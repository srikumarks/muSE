/**
 * @file muse_builtin_networking.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_opcodes.h"
#include "muse_port.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/** @addtogroup Networking */
/*@{*/
muse_boolean muse_network_startup( muse_env *env );
void muse_network_shutdown( muse_env *env );
muse_cell fn_open( muse_env *env, void *context, muse_cell args );
muse_cell fn_with_incoming_connections_to_port( muse_env *env, void *context, muse_cell args );
muse_cell fn_wait_for_input( muse_env *env, void *context, muse_cell args );
muse_cell fn_multicast_group( muse_env *env, void *context, muse_cell args );
muse_cell fn_reply( muse_env *env, void *context, muse_cell args );
muse_cell fn_multicast_group_p( muse_env *env, void *context, muse_cell args );
/*@}*/

#ifndef MUSE_PLATFORM_WINDOWS
#	include <unistd.h>
#	include <signal.h>
#	include <sys/socket.h>
#	include <sys/filio.h>
#	include <netdb.h>
#	include <netinet/in.h>
#	include <arpa/inet.h>
#	define SOCKET int
#	define SOCKET_ERROR (-1)
#	define INVALID_SOCKET (-1)
#	define closesocket(x) close(x)
#	define ioctlsocket(a,b,c) ioctl(a,b,c)
extern int errno;
#else
#	include <winsock2.h>
#	include <ws2tcpip.h>
#	pragma comment(lib, "ws2_32")
	typedef long suseconds_t;
	typedef int socklen_t;
#endif

#define MUSE_DEFAULT_MULTICAST_PORT		31415
#define MUSE_DEFAULT_MULTICAST_GROUP	"231.41.59.26"

#ifdef MUSE_PLATFORM_WINDOWS
#	define MUSE_SO_REUSE SO_REUSEADDR
typedef unsigned long in_addr_t;

/** For Windows, we have to explicitly startup the socket
library! What a stupid thing to have to do! */
	muse_boolean muse_network_startup( muse_env *env )
	{
        /* We ask for sockets version 2.0. */
        const WORD kRequestedVersion = MAKEWORD(2,0);
		WSADATA wsadata;
		
        /* Startup the socket API. This makes sure we have the
        requested sockets version installed on the system. */
        if (WSAStartup(kRequestedVersion, &wsadata) != 0)
        {
			MUSE_DIAGNOSTICS3({
	            fprintf(stderr,	"Error: Socket API can't startup. "
						"Need version %x.\n", 
						kRequestedVersion
						);
			});
			return MUSE_FALSE;
        }
		
		return MUSE_TRUE;
	}

	void muse_network_shutdown(muse_env *env)
	{
        /* Shutdown the sockets API. */
        WSACleanup();
	}
#else
/** On MacosX, it looks like we have to use SO_REUSEPORT instead of 
 * SO_REUSEADDR to achieve the same effect.
 */
#	define MUSE_SO_REUSE SO_REUSEPORT

/**
 * In the POSIX version, we initially setup to block the
 * SIGPIPE signal. The SIGPIPE signal will be sent if we
 * try to write into a socket whose read end has closed.
 * This behaviour, when not masked out, causes the server
 * to exit, and we don't want that to happen.
 *
 * send() should then return EPIPE, which will be transformed
 * into an IOException class.
 *
 * More info can be found at -
 * <a href="http://www.faqs.org/faqs/unix-faq/socket/">Unix-socket-faq</a>
 * <a href="http://www.faqs.org/faqs/unix-faq/programmer/faq/">Unix programming FAQ</a>
 */
	muse_boolean muse_network_startup( muse_env *env )
	{
		/* We ignore the SIGPIPE signal so that the application won't abort
		on socket termination errors. */
		struct sigaction ignoreSIG;
		ignoreSIG.sa_handler	= SIG_IGN;
		ignoreSIG.sa_flags		= 0;
		sigemptyset(&ignoreSIG.sa_mask);

		if ( sigaction(SIGPIPE, &ignoreSIG, NULL) < 0 ) 
		{
			MUSE_DIAGNOSTICS3({ fprintf(stderr, "FATAL ERROR: Couldn't ignore SIGPIPE!\n"); });
			exit(0);
		}
		
		return MUSE_TRUE;
	}

	void muse_network_shutdown(muse_env *env)
	{
		/* Nothing to do. */
	}
#endif

typedef struct _muse_net_t
{
	fd_set fdsets[3]; // 0 = read, 1 = write, 2 = except
	int reductions;
} muse_net_t;


static void prepare_for_network_poll( muse_env *env, SOCKET s, int cat )
{
	FD_CLR( s, &(env->net->fdsets[cat]) );
	FD_CLR( s, &(env->net->fdsets[2]) );
}

static muse_boolean poll_network( muse_env *env, SOCKET s, int cat )
{
	while ( !FD_ISSET(s, &(env->net->fdsets[cat])) )
	{
		FD_SET(s, &(env->net->fdsets[cat]) );
		FD_SET(s, &(env->net->fdsets[2]) );

		{
			int current_reductions = env->net->reductions;
			procrastinate(env);

			if ( env->net->reductions == current_reductions )
			{
				/* We have to do the reduction. */
				static const struct timeval g_tv = {0,500};
				int nfds = select( FD_SETSIZE, &(env->net->fdsets[0]), &(env->net->fdsets[1]), &(env->net->fdsets[2]), &g_tv );
				env->net->reductions++;

				if ( nfds == SOCKET_ERROR )
				{
					FD_ZERO( &(env->net->fdsets[0]) );
					FD_ZERO( &(env->net->fdsets[1]) );
					FD_ZERO( &(env->net->fdsets[2]) );
					return MUSE_FALSE;
				}
			}
			else
			{
				/* Reduction was done by another polling process. */
			}

			if ( FD_ISSET( s, &(env->net->fdsets[2]) ) )
				return MUSE_FALSE;
		}
	}

	return MUSE_TRUE;
}

/**
 * @name Point to point communication
 *
 * Made possible through the functions
 *	- \ref fn_with_incoming_connections_to_port "with-incoming-connections-to-port" and
 *	- \ref fn_with_connection_to_server "with-connection-to-server"
 */
/*@{*/
typedef struct
{
	muse_port_base_t base;
	SOCKET socket;
	struct sockaddr_in address;
} socketport_t;


static void socket_init( muse_env *env, void *s, muse_cell args )
{
	muse_port_t p = (muse_port_t)s;
	port_init( env, p );
	
	p->eof = 0;
	p->error = 0;
}

static void socket_close( void *s );
static void socket_destroy( muse_env *env, void *s )
{
	socket_close(s);
	port_destroy( (muse_port_t)s );
}

static void socket_close( void *s )
{
	socketport_t *p = (socketport_t*)s;
	
	if ( p->socket )
	{
		closesocket( p->socket );
		p->socket = 0;
	}
}

static size_t socket_read( void *buffer, size_t nbytes, void *s )
{
	socketport_t *p = (socketport_t*)s;
	
	muse_int result = SOCKET_ERROR;
	
	while ( result < 0 && poll_network( p->base.env, p->socket, 0 ) )
	{
		result = recv( p->socket, buffer, (int)nbytes, 0 );
		if ( result == 0 ) {
			/* Socket closed from the other end. */
			break;
		} else if ( result < 0 ) {
			/* The poll_network() hasn't updated to the new state 
			that no more data is available. So clear the state flag. */
			FD_CLR( p->socket, &(p->base.env->net->fdsets[0]) );
		}
	}
	
	if ( result <= 0 )
	{
		/* Some error occurred. */
		p->base.error = SOCKET_ERROR;
		p->base.eof = EOF;
		return 0;
	}
	
	return (size_t)result;
}

static size_t socket_write( void *buffer, size_t nbytes, void *s )
{
	/* Ensures that all given bytes are written before returning. */
	
	socketport_t *p = (socketport_t*)s;
	unsigned char *b = buffer;
	unsigned char *b_end = b + nbytes;
	muse_int bytes_sent = 0;
	
	while ( b < b_end )
	{
		bytes_sent = poll_network( p->base.env, p->socket, 1 ) ? send( p->socket, b, (int)(b_end - b), 0 ) : SOCKET_ERROR;
		
		if ( bytes_sent <= 0 )
		{
			p->base.error = SOCKET_ERROR;
			return b - (unsigned char *)buffer;
		}
		
		b += bytes_sent;
	}
	
	return nbytes;
}

static int socket_flush( void *s )
{
	/* Nothing to do. Accumulated buffers will have already been 
	written to the socket by now. */
	return 0;
}

static muse_port_type_t g_socket_type =
{
	{
		'muSE',
		'port',
		sizeof(socketport_t),
		NULL,
		NULL,
		socket_init,
		NULL,
		socket_destroy,
		NULL
	},
	
	socket_close,
	socket_read,
	socket_write,
	socket_flush
};

/**
 * (open "server.somewhere.com" port)
 * (open "231.41.59.26" 31415)
 *
 * Opens a TCP connection to the given server on the given port
 * and returns a port object using which you can communicate with
 * the server. The port can be closed by calling (close p). If a
 * port number is omitted, 31415 (= \p MUSE_DEFAULT_MULTICAST_PORT)
 * is used as the default.
 */
muse_cell fn_open( muse_env *env, void *context, muse_cell args )
{
	int sp					= _spos();
	muse_cell result		= MUSE_NIL;
	muse_cell servername	= _evalnext(&args);
	muse_cell portnum		= _evalnext(&args);

	muse_cell portcell		= _mk_functional_object( &g_socket_type.obj, MUSE_NIL );
	socketport_t *port		= (socketport_t*)_port(portcell);
	
	in_addr_t addr = INADDR_NONE;
	char serverStringAddress[256];
	short portshort = 0;
	int length = 0;
	const muse_char *serverWstringAddress;
	
	/* If we're given a symbol for the server name, we use the string form of
	the symbol and look it up in the DNS. */
	if ( _cellt(servername) == MUSE_SYMBOL_CELL )
		servername = _symname(servername);
		
	serverWstringAddress = _text_contents( servername, &length );
	muse_unicode_to_utf8( serverStringAddress, 256, serverWstringAddress, length );
	
	portshort = (short)(portnum ? _intvalue(portnum) : MUSE_DEFAULT_MULTICAST_PORT);
	
	addr = inet_addr( serverStringAddress );
	if ( addr == INADDR_NONE ) {
		/* Address not in 123.234.12.23 form. Treat it as a name and look it up in the name server. */
		struct hostent *ent = gethostbyname( serverStringAddress );
		if ( ent == NULL ) {
			/* Invalid server address. */
			MUSE_DIAGNOSTICS3({ fprintf( stderr, "Connection to server '%s:%d' failed!\n", serverStringAddress, portshort ); herror("inet error:"); });
			goto UNDO_CONN;
		}
		if ( ent->h_length > 0 ) {
			addr = *(in_addr_t*)ent->h_addr;
		} else {
			MUSE_DIAGNOSTICS3({ fprintf( stderr, "Invalid address '%s:%d'!\n", serverStringAddress, portshort ); });
			goto UNDO_CONN;
		}
	}
	
	port->address.sin_family		= AF_INET;
	port->address.sin_addr.s_addr	= addr;
	port->address.sin_port			= htons( portshort );
	
	port->socket = socket(AF_INET, SOCK_STREAM, 0);

	/* Set a zero timeout period using the SO_LINGER parameter. */
    if ( port->socket > 0 ) 
	{
        struct linger lingerParams = { 1, 0 };
		u_long nbmode = 1;
        setsockopt( port->socket, SOL_SOCKET, SO_LINGER, (const char *)&lingerParams, sizeof(struct linger) );
		ioctlsocket( port->socket, FIONBIO, &nbmode );
    }

	/* Connect to the server. Connection will proceed asynchronously. 
	We poll the network for writeability of this socket in order to determine
	whether it is connected. */
	prepare_for_network_poll( env, port->socket, 1 );
	connect( port->socket, (struct sockaddr*)&(port->address), sizeof(port->address) );
	
	if ( !poll_network( env, port->socket, 1 ) ) 
	{
		MUSE_DIAGNOSTICS3({ fprintf( stderr, "Connection to server '%s:%d' failed!\n", serverStringAddress, portshort ); });
		goto UNDO_CONN;
	}

	port->base.mode = MUSE_PORT_READ_WRITE;

	/* Connection succeeded. Return the port. */
	_unwind(sp);
	_spush(portcell);
	return portcell;

UNDO_CONN:
	_unwind(sp);
	/* Close the connection and return. */
	if ( port )
	{
		port_close( (muse_port_t)port );
	}
	
	return MUSE_NIL;
}


typedef struct muse_server_stream_socket__
{
	SOCKET		listenSocket;		/**< We listen for connections on this one. */
	
	struct sockaddr_in	localSocketAddress;	/**< The local socket address we bind to. */
	
} muse_server_stream_socket_t;

/**
 * (with-incoming-connections-to-port port-number service-fn).
 * Listens for connections to the given port and invokes the
 * service function with the connection information..
 * @code
 * (with-incoming-connections-to-port 1234
 *   (fn (port client-info)
 *      ...))
 * @endcode
 * If the service function returns (), the server is terminated,
 * otherwise the server will accept the next connection and service it.
 *
 * @see fn_with_connection_to_server()
 */
muse_cell fn_with_incoming_connections_to_port( muse_env *env, void *context, muse_cell args )
{
	short listenPort		= (short)_intvalue( _evalnext(&args) );
	muse_cell handler		= _evalnext(&args);
	int sp					= _spos();
	muse_server_stream_socket_t *conn = (muse_server_stream_socket_t*)calloc( 1, sizeof(muse_server_stream_socket_t) );
	muse_cell result = MUSE_NIL;
	
	/* Create the socket we should listen to. */
	conn->listenSocket = socket(AF_INET, SOCK_STREAM, 0);
	if ( conn->listenSocket == INVALID_SOCKET )
	{
		MUSE_DIAGNOSTICS3({ fprintf( stderr, "Couldn't create socket to listen for connections.\n" ); });
		goto BAIL;
	}
	
	/* Bind the socket just created (listen socket) to a 
	local address and port combination. */
	
	/* Setup the localSocketAddress */
	conn->localSocketAddress.sin_family			= AF_INET;
	conn->localSocketAddress.sin_addr.s_addr	= htonl(INADDR_ANY);
	conn->localSocketAddress.sin_port			= htons(listenPort);
	
	/* Bind listenSocket to the local address/port combo. */
	if ( bind( conn->listenSocket, (struct sockaddr*)&(conn->localSocketAddress), sizeof(conn->localSocketAddress) ) 
		== SOCKET_ERROR
		)
	{
		MUSE_DIAGNOSTICS3({ fprintf( stderr, "Couldn't bind listen-socket to local address.\n" ); });
		goto SHUTDOWN_SERVER;
	}
	
	/* Use "listen" to set the connection backlog buffer. */
	if ( listen( conn->listenSocket, SOMAXCONN ) == SOCKET_ERROR )
	{
		MUSE_DIAGNOSTICS3({ fprintf( stderr, "'listen' failed.\n" ); });
		goto SHUTDOWN_SERVER;
	}
	
	/* Accept connections on the server socket.
	 * Once one connection happens, we are through and we
	 * won't listen for any more. 
	 */
	ACCEPT_CONNECTIONS:
	MUSE_DIAGNOSTICS3({ fprintf( stderr, "Waiting for connections to port %d ...\n", ntohs(conn->localSocketAddress.sin_port) ); });
	{
		u_long nbmode = 1;
		ioctlsocket( conn->listenSocket, FIONBIO, &nbmode );
	}

	{
		socklen_t sockAddrSize = sizeof(struct sockaddr_in);
		struct sockaddr_in client_address;
		SOCKET client = INVALID_SOCKET;
		
		prepare_for_network_poll( env, conn->listenSocket, 0 );
		client = accept( conn->listenSocket, (struct sockaddr *)&client_address, &sockAddrSize );
		if ( client == INVALID_SOCKET && poll_network( env, conn->listenSocket, 0 ) )
		{
			client = accept( conn->listenSocket, (struct sockaddr *)&client_address, &sockAddrSize );
		}

		if ( client != INVALID_SOCKET )
		{
			muse_cell client_port_cell = MUSE_NIL;
			socketport_t *client_port = NULL;

			MUSE_DIAGNOSTICS3({ 
				fprintf( stderr,
						 "Accepted connection from machine %s on port %d.\n",
						 inet_ntoa( client_address.sin_addr ),
						 ntohs( client_address.sin_port )
						);
			});

			client_port_cell = _mk_functional_object( &g_socket_type.obj, MUSE_NIL );
			client_port = (socketport_t*)_port( client_port_cell );
	
			client_port->socket = client;
			client_port->base.mode = MUSE_PORT_READ_WRITE;
			client_port->base.eof = 0;
			client_port->base.error = 0;

			/* Client connection accepted successfully. Call the handler. */
			{
				muse_cell handlerArgs = muse_list( env, "ct", 
													client_port_cell, 
													inet_ntoa( client_address.sin_addr ) );			

				result = _apply( handler, handlerArgs, MUSE_TRUE );
				
				/* Save the result. The handler must return a non-NIL value to indicate that
					subsequent connections have to be processed. It should return MUSE_NIL
					to terminate the server. */
				_unwind(sp);
				_spush(result);
			}
		}
		
		if ( result )
		{
			goto ACCEPT_CONNECTIONS;
		}
	}
	
	/* We've completed processing all client connections. Shutdown the server. */
SHUTDOWN_SERVER:
	if ( conn->listenSocket ) closesocket( conn->listenSocket );
	memset( conn, 0, sizeof(muse_server_stream_socket_t) );
	
BAIL:
	free(conn);
	return result;
}
/*@}*/

/**
 * @name Multicast messaging
 *
 * A simple multicast mechanism is implemented using UDP datagrams.
 * You join a multicast group by creating a muSE port using the function
 * \ref fn_multicast_group "multicast-group". Once the join succeeds, you can broadcast
 * a message to the group and can receive broadcast messages from any member
 * of the group using the standard \ref fn_read "read" and \ref fn_write "write"
 * functions. 
 *
 * Immediately after a read, information about the sender is
 * stored internally and you can reply to the sender alone by using the
 * \ref fn_reply "reply" function. \ref fn_reply "reply" works only with multicast group
 * ports. It is a programming error to use it with other ports.
 *
 * You can use the \ref fn_wait_for_input "wait-for-input" function to wait for a message
 * from a multicast group as well.
 */
/*@{*/
typedef struct
{
	muse_port_base_t base;
	SOCKET socket;
	struct sockaddr_in dst_addr;
	struct sockaddr_in src_addr;
	int src_addr_len;
	struct ip_mreq mreq;
	int reply;
} multicast_socket_port_t;

static void multicast_socket_init( muse_env *env, void *p, muse_cell args )
{
	multicast_socket_port_t *s = (multicast_socket_port_t*)p;
	muse_cell group		= _evalnext(&args);
	muse_cell mcport	= _evalnext(&args);
	char group_address[32];
	u_short group_port;
	int result = 0;

	if ( mcport )
	{
		/* Get the port number to use. */
		group_port = (u_short)_intvalue(mcport);		
	}
	else
	{
		/* No port specified. Use default port 31415. */
		group_port = MUSE_DEFAULT_MULTICAST_PORT;
		MUSE_DIAGNOSTICS3({ fprintf( stderr, "No port specified for multicast. Using default port 31415.\n" ); });
	}
	
	if ( group )
	{
		/* The group is supposed to specify an internet numeric address.
		Convert the wide string to a narrow string. */
		int length = 0;
		muse_unicode_to_utf8( group_address, 32, _text_contents( group, &length ), 32 );
		muse_assert( length < 16 );
		if ( length >= 16 )
		{
			s->base.error = 1;
			return;
		}
	}
	else
	{
		/* No group specified. Use a default group. */
		strcpy( group_address, MUSE_DEFAULT_MULTICAST_GROUP );
		MUSE_DIAGNOSTICS3({ fprintf( stderr, "No group specified. Using default group %s\n", group_address ); });
	}
		
	port_init( env,&s->base);

	/* Set to read/write mode. All sockets support this. */
	s->base.mode = MUSE_PORT_READ_WRITE;

	/* Disable pretty printing. Has adverse effects on flushing operation. */
	s->base.pretty_print = 0;
	s->base.tab_size = 0;

	/* Create an ordinary datagram socket for sending and receiving messages. */
	s->socket = socket( AF_INET, SOCK_DGRAM, 0 );
	if ( s->socket < 0 )
	{
		s->base.error = 1;
		goto BAIL;
	}

	/* Allow multiple sockets to use the same port number. */
	{
		int yes = 1;
		if ( (result = setsockopt( s->socket, SOL_SOCKET, MUSE_SO_REUSE, (const char *)&yes, sizeof(yes) )) < 0 )
		{
			MUSE_DIAGNOSTICS3({ fprintf( stderr, "Cannot reuse same address for multiple sockets!\n" ); });
		}
	}

	/* Setup destination address. */
	memset( &s->dst_addr, 0, sizeof(s->dst_addr) );
	s->dst_addr.sin_family		= AF_INET;
	s->dst_addr.sin_addr.s_addr	= inet_addr(group_address);
	s->dst_addr.sin_port		= htons(group_port);

	/* Setup the source address. */
	memset( &s->src_addr, 0, sizeof(s->src_addr) );
	s->src_addr.sin_family		= AF_INET;
	s->src_addr.sin_addr.s_addr	= htonl(INADDR_ANY);
	s->src_addr.sin_port		= htons(group_port);

	/* Bind the socket to the address. */
	if ( (result = bind( s->socket, (struct sockaddr*)&s->src_addr, sizeof(s->src_addr) )) < 0 )
	{
		MUSE_DIAGNOSTICS3({ fprintf( stderr, "Binding to multicast group port failed!\n" ); });
		s->base.error = 2;
		goto CLOSE_SOCKET_AND_BAIL;
	}

	/* Disable loopback. */
	{
		u_char loop = 0;
		if ( (result = setsockopt(s->socket, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof(loop))) < 0 )
		{
			MUSE_DIAGNOSTICS3({ fprintf( stderr, "Failed to disable multicast loop back!\n" ); });
		}
	}

	/* Request the kernel to join the multicast group. */
	s->mreq.imr_interface.s_addr	= htonl(INADDR_ANY);
	s->mreq.imr_multiaddr.s_addr	= inet_addr(group_address);
	if ( (result = setsockopt( s->socket, IPPROTO_IP, IP_ADD_MEMBERSHIP, (const char *)&(s->mreq), sizeof(s->mreq) )) < 0 )
	{
		MUSE_DIAGNOSTICS3({ fprintf( stderr, "Couldn't join the multicast group!\n" ); });
		s->base.error = 3;
		goto CLOSE_SOCKET_AND_BAIL;
	}

	/* Everything went well. */
	return;
	
CLOSE_SOCKET_AND_BAIL:
	closesocket( s->socket );
	s->socket = 0;
	
BAIL:
	s->base.eof = EOF;
}

static void multicast_socket_close( void *p )
{
	multicast_socket_port_t *s = (multicast_socket_port_t*)p;

	if ( s->socket )
	{
		/* We added the kernel to the multicast group. Now the port is
		being destroyed, so remove it from the group. */
		int result = 0;
		if ( (result = setsockopt( s->socket, IPPROTO_IP, IP_DROP_MEMBERSHIP, (const char *)&(s->mreq), sizeof(s->mreq) )) < 0 )
		{
			MUSE_DIAGNOSTICS3({ fprintf( stderr, "Couldn't remove kernel from multicast group!\n" ); });
		}
		
		/* We're done with the socket. */
		closesocket( s->socket );
		s->socket = 0;
	}
}

static void multicast_socket_destroy( muse_env *env, void *p )
{
	multicast_socket_port_t *s = (multicast_socket_port_t*)p;

	/* Close the socket. */
	if ( s->socket )
		multicast_socket_close(p);

	port_destroy(p);
}

static size_t multicast_socket_read( void *buffer, size_t nbytes, void *p )
{
	multicast_socket_port_t *s = (multicast_socket_port_t*)p;
	muse_env *env = s->base.env;

	/* Wait for and read a datagram from any of the group. */
	s->src_addr_len = sizeof(s->src_addr);

	{
		int result = 
			poll_network( env, s->socket, 0 )
				? recvfrom( s->socket, (char*)buffer, (int)nbytes, 0, (struct sockaddr*)&(s->src_addr), &s->src_addr_len )
				: SOCKET_ERROR;

		if ( result < 0 )
		{
			s->base.eof		= EOF;
			s->base.error	= EOF;
			return 0;
		}

		MUSE_DIAGNOSTICS3({ fprintf( stderr, "Received datagram from %s on port %d\n", inet_ntoa(s->src_addr.sin_addr), ntohs(s->src_addr.sin_port) ); });
		return (size_t)result;
	}
}

static size_t multicast_socket_write( void *buffer, size_t nbytes, void *p )
{
	multicast_socket_port_t *s = (multicast_socket_port_t*)p;
	muse_env *env = s->base.env;

	/* Multicast the datagram to the group or the individual. */
	const struct sockaddr *addr = (const struct sockaddr *)(s->reply ? &s->src_addr : &s->dst_addr);
	int addr_len = s->reply ? s->src_addr_len : sizeof(s->dst_addr);
	int result = 
			poll_network( env, s->socket, 1 )
				? sendto( s->socket, buffer, (int)nbytes, 0, addr, addr_len )
				: SOCKET_ERROR;

	if ( result < 0 )
	{
		s->base.error = 1;
		return 0;
	}

	return (size_t)result;
}

static int multicast_socket_flush( void *p )
{
	/* Nothing to do, but mandatory implementation. */
	return 0;
}

static muse_port_type_t g_multicast_socket_type =
{
	{
		'muSE',
		'port',
		sizeof(multicast_socket_port_t),
		NULL,
		NULL,
		multicast_socket_init,
		NULL,
		multicast_socket_destroy,
		NULL
	},
	
	multicast_socket_close,
	multicast_socket_read,
	multicast_socket_write,
	multicast_socket_flush
};

/**
 * (multicast-group [address] [port]).
 * Creates a port that represents a multicast group. You can
 * send messages to the group by writing to the port and you
 * can receive messages to the group by reading from the port.
 * Loopback is disabled, so you won't receive your own messages.
 * If you don't specify the port, the port 31415 is used.
 * If you don't specify the address as well, the group
 * address "231.41.59.26" is used.
 *
 * Note that you should use relatively small expressions.
 * The safest expressions are those that fit within about 512 bytes.
 * If you use multicast-groups like you use SMS, you'll be fine.
 * If you give too large a message, then the write will fail
 * and return () and no message will be sent. You can check for
 * the condition, therefore, at runtime.
 *
 * You should only use multicast group addresses in the range -
 * 225.0.0.0 to 231.255.255.255. For details, see
 * http://www.iana.org/assignments/multicast-addresses
 */
muse_cell fn_multicast_group( muse_env *env, void *context, muse_cell args )
{
	return _mk_functional_object( (muse_functional_object_type_t*)&g_multicast_socket_type, args );
}

/**
 * (multicast-group? p).
 * Returns \c p if it is a multicast port and MUSE_NIL if it isn't.
 */
muse_cell fn_multicast_group_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell p = _evalnext(&args);
	muse_port_t port = _port(p);
	if ( p && port && port->base.type_info == &g_multicast_socket_type.obj )
		return p;
	else
		return MUSE_NIL;
}

/**
 * (reply port).
 *
 * Immediately after a multicast read succeeds, you have the chance
 * to reply specifically to that client. This is possible because the
 * sender's information is stored in the multicast port structure.
 */
muse_cell fn_reply( muse_env *env, void *context, muse_cell args )
{
	muse_cell port = _evalnext(&args);
	multicast_socket_port_t *s = (multicast_socket_port_t*)_port(port);

	muse_assert( s && s->base.base.type_info == &g_multicast_socket_type.obj );
	/**< We support only multicast sockets. */

	s->reply = 1;

	/* Behave like write w.r.t. the remaining arguments. */
	while ( args )
	{
		muse_pwrite( (muse_port_t)s, _evalnext(&args) );
		if ( args )
			port_putc( ' ', (muse_port_t)s );
	}

	port_putc( '\n', (muse_port_t)s );

	s->reply = 0;
	return MUSE_NIL;
}
/*@}*/

/**
 * (wait-for-input network-port [timeout-microseconds]).
 * Returns T if input is available and 'timeout if the wait timed out.
 * Returns () upon error. Works only for network ports. It won't
 * wait for file-based ports.
 */
muse_cell fn_wait_for_input( muse_env *env, void *context, muse_cell args )
{
	muse_cell port = _evalnext(&args);
	muse_cell timeout = args ? _evalnext(&args) : MUSE_NIL;
	
	/* Wait for 10 minutes in the default case. */
	muse_int timeout_us = timeout ? _intvalue(timeout) : (1000000 * 60 * 10);
	
	muse_port_t p = _port(port);
	if ( p && (p->base.type_info == &g_socket_type.obj || p->base.type_info == &g_multicast_socket_type.obj) )
	{
		/* This is a socket. Don't wait for anything else. */
		socketport_t *s = (socketport_t*)p;
		
		fd_set fds;
		struct timeval tv = { (time_t)(timeout_us / 1000000), (suseconds_t)(timeout_us % 1000000) };
		FD_ZERO(&fds);
		FD_SET( s->socket, &fds );
		
		{
			int result = select( (int)(s->socket + 1), &fds, NULL, NULL, &tv );
			switch ( result )
			{
				case 1 : /* Success. */ return _t();
				case 0 : /* Timed out. */ return _builtin_symbol(MUSE_TIMEOUT);
				default: /* Error. */ return MUSE_NIL;
			}
		}
	}
	else
		return _t();
}

/**
 * Shutsdown the socket API. No need to call this as its called
 * at GC time when the muse environment is destroyed.
 */
static muse_cell fn_network_shutdown( muse_env *env, void *context, muse_cell args )
{
	free(env->net);
	env->net = NULL;
	muse_network_shutdown(env);
	return MUSE_NIL;
}

void muse_define_builtin_networking(muse_env *env)
{
	struct funs_t { const muse_char *name; muse_nativefn_t fn; };

	static const struct funs_t k_networking_funs[] =
	{
		{		L"open",								fn_open									},
		{		L"with-incoming-connections-to-port",	fn_with_incoming_connections_to_port	},
		{		L"wait-for-input",						fn_wait_for_input						},
		{		L"multicast-group",						fn_multicast_group						},
		{		L"reply",								fn_reply								},
		{		L"multicast-group?",					fn_multicast_group_p					},
		{		NULL,									NULL									}
	};

	muse_network_startup(env);

	env->net = (muse_net_t*)calloc( 1, sizeof(muse_net_t) );
	FD_ZERO( &(env->net->fdsets[0]) );	// read
	FD_ZERO( &(env->net->fdsets[1]) );	// write
	FD_ZERO( &(env->net->fdsets[2]) );	// except

	/* Define a destructor function to shutdown the network when everything is done. 
	Using braces in the symbol name ensures that this symbol cannot be written directly
	in muSE code. */
	_define( _csymbol(L"{{network-shutdown}}"), _mk_destructor(fn_network_shutdown,NULL) );

	{
		const struct funs_t *funs = k_networking_funs;
		while ( funs->name )
		{
			_define( _csymbol(funs->name), _mk_nativefn(funs->fn,NULL) );
			++funs;
		}
	}

}
