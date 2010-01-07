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
#include "muse_utils.h"
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
#	ifdef MUSE_PLATFORM_BSD
#		include <sys/filio.h>
#		include <sys/types.h>
#	else
#		include <sys/ioctl.h>
#	endif
#	include <netdb.h>
#	include <netinet/in.h>
#	include <arpa/inet.h>
#	define SOCKET int
#	define SOCKET_ERROR (-1)
#	define INVALID_SOCKET (-1)
#	define closesocket(x) close(x)
#	define ioctlsocket(a,b,c) ioctl(a,b,c)
extern int errno;
#	define WSAGetLastError() errno
#	define _snprintf snprintf
#else
#	include <winsock2.h>
#	include <ws2tcpip.h>
#	include <wininet.h>
#	include <shlwapi.h>
#	pragma comment(lib,"wininet")
#	pragma comment(lib,"shlwapi")
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
/** On *BSD (including MacosX), it looks like we have to use 
 * SO_REUSEPORT instead of SO_REUSEADDR to achieve the same effect.
 */
#	ifdef MUSE_PLATFORM_BSD
#		define MUSE_SO_REUSE SO_REUSEPORT
#	else
#		define MUSE_SO_REUSE SO_REUSEADDR
#	endif

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

typedef enum 
{
	POLL_SOCKET_FAILED,
	POLL_SOCKET_ERROR,
	POLL_SOCKET_SET
} poll_socket_status_t;

static poll_socket_status_t poll_network( muse_env *env, SOCKET s, int cat )
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
				int nfds = select( FD_SETSIZE, &(env->net->fdsets[0]), &(env->net->fdsets[1]), &(env->net->fdsets[2]), (struct timeval *)&g_tv );
				env->net->reductions++;

				if ( nfds == SOCKET_ERROR )
				{
					FD_ZERO( &(env->net->fdsets[0]) );
					FD_ZERO( &(env->net->fdsets[1]) );
					FD_ZERO( &(env->net->fdsets[2]) );
					return POLL_SOCKET_FAILED;
				}
			}
			else
			{
				/* Reduction was done by another polling process. */
			}

			if ( FD_ISSET( s, &(env->net->fdsets[2]) ) )
				return POLL_SOCKET_ERROR;
		}
	}

	return POLL_SOCKET_SET;
}

/**
 * @name Point to point communication
 *
 * Made possible through the functions
 *	- \ref fn_with_incoming_connections_to_port "with-incoming-connections-to-port" and
 *	- \ref fn_open "open"
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
		port_destroy( (muse_port_t)s );
	}
}

static size_t socket_read( void *buffer, size_t nbytes, void *s )
{
	socketport_t *p = (socketport_t*)s;
	
	muse_int result = SOCKET_ERROR;
	
	while ( result < 0 && poll_network( p->base.env, p->socket, 0 ) == POLL_SOCKET_SET )
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
		bytes_sent = (poll_network( p->base.env, p->socket, 1 ) == POLL_SOCKET_SET) ? send( p->socket, b, (int)(b_end - b), 0 ) : SOCKET_ERROR;
		
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
 * @code (open "server.somewhere.com" port)
 * (open "231.41.59.26" 31415) @endcode
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
	
	if ( poll_network( env, port->socket, 1 ) != POLL_SOCKET_SET ) 
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
 * @code (with-incoming-connections-to-port port-number service-fn) @endcode
 * Listens for connections to the given port and invokes the
 * service function with the connection information..
 * @code
 * (with-incoming-connections-to-port 1234
 *   (fn (port client-info)
 *      ...)
 *   (fn ()   ; The "OnListening" function.
 *      ...))
 * @endcode
 * If the service function returns (), the server is terminated,
 * otherwise the server will accept the next connection and service it.
 *
 * @param port-number The port-number can be one of a few things -
 *		- An integer giving the port number on which to accept connections
 *		  from any internet address.
 *		- A string such as "12345" giving a port number serving the same prupose as above.
 *		- A string of the form "1.2.3.4:12345" where the portion before the
 *		  colon ':' gives the IP address from which connections are accepted
 *		  and the portion after the colon gives the port number on which
 *		  connections are accepted. For example, if you pass "127.0.0.1:12345",
 *		  then the server will only accept connections from the local machine
 *		  on the port 12345.
 *
 * @see fn_open()
 */
muse_cell fn_with_incoming_connections_to_port( muse_env *env, void *context, muse_cell args )
{
	muse_cell portSpec, handler, onlistening, result;
	short listenPort; 
	unsigned long bindAddr;
	int sp;
	muse_server_stream_socket_t *conn;


	portSpec		= _evalnext(&args);
	bindAddr		= htonl(INADDR_ANY);
	switch ( _cellt(portSpec) ) {
		case MUSE_INT_CELL: 
			/* A simple port number. Allow binding to any address. */
			listenPort = (short)_intvalue(portSpec); 
			break;
		case MUSE_TEXT_CELL:
			/* A string in "1.2.3.4:12345" format giving the IP address
			from which connections are allowed and the port on which to listen. */
			{
				char address[128];
				int portSpecLen;
				const muse_char *portSpecStr = _text_contents( portSpec, &portSpecLen );
				size_t len = muse_unicode_to_utf8( address, 128, portSpecStr, portSpecLen ); 
				address[len] = '\0';
				{
					char *colon = strchr( address, ':' );
					if ( colon ) {
						colon[0] = '\0';
						++colon;
						listenPort = atoi(colon);
						bindAddr = inet_addr(address);
						if ( bindAddr == INADDR_NONE ) {
							/* Address not in 123.234.12.23 form. Treat it as a name and look it up in the name server. */
							struct hostent *ent = gethostbyname( address );

							if ( ent == NULL || ent->h_length == 0 )
								return muse_raise_error( env, _csymbol(L"error:invalid-port-spec"), _cons( portSpec, MUSE_NIL ) );

							bindAddr = *(in_addr_t*)ent->h_addr;
						}
					} else {
						listenPort = atoi(address);
					}
				}
				break;
			}
			break;
		default:
			return muse_raise_error( env, _csymbol(L"error:invalid-port-spec"), _cons( portSpec, MUSE_NIL ) );
	}

	handler		= _evalnext(&args);
	onlistening	= _evalnext(&args);
	sp			= _spos();
	conn		= (muse_server_stream_socket_t*)calloc( 1, sizeof(muse_server_stream_socket_t) );
	result		= MUSE_NIL;
		
	TRY_AGAIN_FROM_THE_BEGINNING:

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
	conn->localSocketAddress.sin_addr.s_addr	= bindAddr;
	conn->localSocketAddress.sin_port			= htons(listenPort);
	
	/* Bind listenSocket to the local address/port combo. */
	if ( bind( conn->listenSocket, (struct sockaddr*)&(conn->localSocketAddress), sizeof(conn->localSocketAddress) ) 
		== SOCKET_ERROR
		)
	{
		muse_raise_error( env, _csymbol(L"error:bind-socket"), _cons( _mk_int(WSAGetLastError()), MUSE_NIL ) );
		goto SHUTDOWN_SERVER;
	}
	
	/* Use "listen" to set the connection backlog buffer. */
	if ( listen( conn->listenSocket, SOMAXCONN ) == SOCKET_ERROR )
	{
		muse_raise_error( env, _csymbol(L"error:listen"), _cons( _mk_int(WSAGetLastError()), MUSE_NIL ) );
		goto SHUTDOWN_SERVER;
	}
	
	/* Accept connections on the server socket.
	 * Once one connection happens, we are through and we
	 * won't listen for any more. 
	 */
	ACCEPT_CONNECTIONS:
	// Waiting for connections to port conn->localSocketAddress.sin_port
	{
		u_long nbmode = 1;
		ioctlsocket( conn->listenSocket, FIONBIO, &nbmode );
	}

	{
		socklen_t sockAddrSize = sizeof(struct sockaddr_in);
		struct sockaddr_in client_address;
		SOCKET client = INVALID_SOCKET;
		
		client = accept( conn->listenSocket, (struct sockaddr *)&client_address, &sockAddrSize );
		
		if ( onlistening ) {
			int sp = _spos();
			muse_apply( env, onlistening, MUSE_NIL, MUSE_TRUE, MUSE_FALSE );
			_unwind(sp);
			onlistening = MUSE_NIL;
		}

		POLL_AGAIN:
		if ( client == INVALID_SOCKET ) {
			prepare_for_network_poll( env, conn->listenSocket, 0 );
			switch ( poll_network( env, conn->listenSocket, 0 ) ) {
				case POLL_SOCKET_SET:
					client = accept( conn->listenSocket, (struct sockaddr *)&client_address, &sockAddrSize );
					break;
				case POLL_SOCKET_FAILED:
					goto POLL_AGAIN;
				case POLL_SOCKET_ERROR:
					if ( conn->listenSocket ) closesocket(conn->listenSocket);
					memset( conn, 0, sizeof(muse_server_stream_socket_t) );
					goto TRY_AGAIN_FROM_THE_BEGINNING;
			}
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

			muse_push_recent_scope(env);

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

			muse_pop_recent_scope(env,handler,result);
		}
		else
		{
			result = muse_raise_error( env, _csymbol(L"error:invalid-socket"), _cons( _mk_int(WSAGetLastError()), MUSE_NIL ) );
		}
		
		if ( result )
		{
			_unwind(sp);
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
	socklen_t src_addr_len;
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
			poll_network( env, s->socket, 0 ) == POLL_SOCKET_SET
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
			poll_network( env, s->socket, 1 ) == POLL_SOCKET_SET
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
 * @code (multicast-group [address] [port]) @endcode
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
 * @code (multicast-group? p) @endcode
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
 * @code (reply port) @endcode
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
 * @code (wait-for-input network-port [timeout-microseconds]) @endcode
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

#ifdef MUSE_PLATFORM_WINDOWS
typedef struct
{
	HINTERNET net;
} internet_handle_t;
static muse_cell destroy_internet_handle( muse_env *env, internet_handle_t *net, muse_cell args )
{
	if ( muse_doing_gc(env) )
	{
		InternetCloseHandle( net->net );
		free(net);
	}
	return MUSE_NIL;
}

static size_t format_http_headers( muse_env *env, muse_char *buffer, size_t sz, muse_cell headers )
{
	size_t n = 0;

	while ( headers && n+1 < sz )
	{
		muse_cell header = muse_head( env, headers );

		{
			int sp = muse_stack_pos(env);
			while ( muse_cell_type(header) != MUSE_TEXT_CELL )
			{
				header = muse_raise_error( env, muse_csymbol( env, L"fetch-uri:bad-header" ), muse_cons( env, header, MUSE_NIL ) );
				muse_set_head( env, headers, header );
				muse_stack_unwind( env, sp );
			}
		}

		{
			int len = 0;
			const muse_char *line = muse_text_contents( env, header, &len );
			if ( n + len + 3 > sz )
			{
				muse_raise_error( env, muse_csymbol( env, L"fetch-uri:unused-headers" ), muse_cons( env, headers, MUSE_NIL ) );
				buffer[n] = '\0';
				return n;
			}
			else
			{
				wcscpy_s( buffer + n, sz-n, line );
				n += len;
				buffer[n++] = '\x0d';
				buffer[n++] = '\x0a';
			}
		}

		headers = muse_tail( env, headers );
	}

	buffer[n] = '\0';
	return n;
}

typedef struct 
{
	HINTERNET huri;
	FILE *cache_file;
} fetch_uri_finalizer_data_t;

static muse_cell fetch_uri_finalizer( muse_env *env, fetch_uri_finalizer_data_t *data, muse_cell args )
{
	if ( data->cache_file ) 
	{
		fclose(data->cache_file);
		data->cache_file = NULL;
	}

	if ( data->huri )
	{
		InternetCloseHandle(data->huri);
		data->huri = NULL;
	}

	// We allow override by passing some arbitrary arg to disable the free call.
	if ( args == MUSE_NIL )
		free(data);

	return MUSE_NIL;
}

typedef struct 
{
	muse_cell uri;
	muse_boolean cached;
	const muse_char *mimePattern;
	muse_cell headers;
} fetch_uri_args_t;

static muse_cell fetch_uri( muse_env *env, fetch_uri_args_t *args, muse_cell args_unused )
{
	muse_cell uri = args->uri;
	muse_boolean cached = args->cached;
	const muse_char *mimePattern = args->mimePattern;
	muse_cell headers = args->headers;

	HINTERNET net = 0;
	HINTERNET huri = 0;
	muse_char mimeType[256];
	muse_char ext[10];
	DWORD mimeType_size = 256;
	muse_char localfile[MAX_PATH+1];
	fetch_uri_finalizer_data_t *finalizer_data = NULL;
	
	int uri_len = 0;
	const muse_char *uri_str = muse_text_contents( env, uri, &uri_len );

	if ( UrlIsW( uri_str, URLIS_URL ) == TRUE  )
	{
		if ( cached )
		{
			// Check for cached version of URL.
			INTERNET_CACHE_ENTRY_INFO entry;
			DWORD size = sizeof(entry);
			if ( GetUrlCacheEntryInfoW( uri_str, &entry, &size ) == TRUE )
			{
				// Cache hit.
				// Note: Doesn't seem to happen at all, though it should.
				return muse_mk_ctext( env, entry.lpszLocalFileName );
			}
			else
			{
				// Cache miss.
			}
		}

		// Get the internet handle.
		{
			muse_cell netsym = _csymbol(L"{{wininet}}");
			if ( _symval(netsym) == netsym ) {
				// Not initialized yet. Do it.
				net = InternetOpen( L"muSE", INTERNET_OPEN_TYPE_DIRECT, NULL, NULL, 0 );
				// If initialization fails, allow replacement of uri with a default file.
				if ( net == NULL )
					return muse_raise_error( env, _csymbol(L"fetch-uri:network-error"), MUSE_NIL );
				
				{
					internet_handle_t *h = (internet_handle_t*)calloc( 1, sizeof(internet_handle_t) );
					h->net = net;
					_define( netsym, _mk_destructor( (muse_nativefn_t)destroy_internet_handle, h ) );
				}
			} else {
				net = ((internet_handle_t*)muse_nativefn_context( env, _symval(netsym), NULL ))->net;
			}
		}

		{
			muse_char headerText[1024];
			size_t len = format_http_headers( env, headerText, 1024, headers );
			huri = InternetOpenUrlW( net, uri_str, (len > 0 ? headerText : NULL), (DWORD)len, INTERNET_FLAG_NEED_FILE | (cached ? 0 : INTERNET_FLAG_RESYNCHRONIZE), (DWORD_PTR)NULL );
		}

		// If opening the uri failed, allow replacement of the uri
		// with a default uri or file.
		if ( huri == NULL )	
		{
			args->uri = muse_raise_error( env, muse_csymbol( env, L"fetch-uri:bad-resource"), uri );
			return fetch_uri( env, args, args_unused );
		}

		// Add the finalizer call for cleanup operations.
		{
			finalizer_data = (fetch_uri_finalizer_data_t*)calloc( 1, sizeof(fetch_uri_finalizer_data_t) );
			finalizer_data->huri = huri;
			muse_add_finalizer_call( env, (muse_nativefn_t)fetch_uri_finalizer, finalizer_data );
		}

		// Find out type of uri.
		if ( HttpQueryInfoW( huri, HTTP_QUERY_CONTENT_TYPE, (LPVOID)mimeType, &mimeType_size, 0 ) == TRUE )
		{
			ext[0] = '\0';

			if ( wcslen(uri_str) < MAX_PATH )
			{
				const wchar_t *uriext = PathFindExtensionW(uri_str);

				if ( uriext[0] == '.' && wcslen(uriext) <= 5 )
				{
					// Valid extension found.
					wcscpy( ext, uriext+1 );
				}
			}


			if ( mimePattern != NULL )
			{
				if ( wcsstr( mimeType, mimePattern ) != NULL )
				{
					// MIME pattern matched.
				}
				else
				{
					// Not of the appropriate mime type. Allow replacement of the uri
					// with a default uri or file
					fetch_uri_finalizer( env, finalizer_data, MUSE_T );
					args->uri = muse_raise_error( env, muse_csymbol( env, L"fetch-uri:bad-resource"), uri );
					return fetch_uri( env, args, args_unused );
				}
			}

			if ( wcsstr( mimeType, L"text" ) != NULL )
			{
				// Text content.
				wcscpy( ext, L"txt" );
			}
			else if ( wcsstr( mimeType, L"xml" ) != NULL )
			{
				// XML content
				wcscpy( ext, L"xml" );
			}
			else if ( wcsstr( mimeType, L"image/jpeg" ) != NULL )
			{
				// JPG file.
				wcscpy( ext, L"jpg" );
			}
			else if ( wcsstr( mimeType, L"image/png" ) != NULL )
			{
				// PNG file.
				wcscpy( ext, L"png" );
			}
			else if ( wcsstr( mimeType, L"image/gif" ) != NULL )
			{
				// GIF file.
				wcscpy( ext, L"gif" );
			}
			else if ( ext[0] == '\0' )
			{
				// No extension found in uri itself. General binary file.
				wcscpy( ext, L"bin" );
			}
			else
			{
				// Leave the extension as found from the uri itself.
				muse_assert( ext[0] != '\0' );
			}
		}
		else
		{
			// Not a Http request I guess. Get the extension from the URL itself.
			const wchar_t *cext = PathFindExtensionW( uri_str );
			if ( cext[0] == '.' )
				wcscpy( ext, cext+1 );
			else
			{
				// Not a supported file. Use the generic ".bin" extension.
				wcscpy( ext, L"bin" );
			}
		}

		if ( CreateUrlCacheEntryW( uri_str, 0, ext, localfile, 0 )  == TRUE )
		{
			FILE *f = _wfopen( localfile, L"wb" );

			// If _wfopen failed, allow replacement of uri with a default file path.
			if ( f )
			{
				BYTE buffer[4096];
				DWORD bytesRead = 0;

				finalizer_data->cache_file = f;

				while ( InternetReadFile( huri, buffer, 4096, &bytesRead ) == FALSE || bytesRead > 0 )
				{
					fwrite( buffer, 1, bytesRead, f );
					yield_process(env,1);
				}

				// We have to close the file and the huri handle before committing
				// to the cache. The finalizers are supposed to take care of closing
				// them, but we need to close them *before* the CommitUrlCacheEntry
				// call. If the InternetCloseHandle() call happens after the
				// CommitUrlCacheEntry call, then the cache file gets immediately
				// deleted after InternetCloseHandle() ... only within Reveal!!
				// Within the muSE command-line, all call orders work just fine!
				//
				// <sigh> .. yet another Win32 quirk to learn about and live with.
				fetch_uri_finalizer( env, finalizer_data, MUSE_T );

				// Cache the URL.
				{
					FILETIME expireTime = {0,0}, modifiedTime = {0,0};

					if ( CommitUrlCacheEntryW( uri_str, localfile, expireTime, modifiedTime, NORMAL_CACHE_ENTRY, NULL, 0, NULL, uri_str ) == TRUE )
					{
						// Commit succeeded.
						return muse_mk_ctext( env, localfile );
					}
					else
					{
						// Commit failed.
						return muse_raise_error( env, muse_csymbol( env, L"fetch-uri:cache-failure" ), uri );
					}
				}
			}
			else
			{
				return muse_raise_error( env, muse_csymbol( env, L"fetch-uri:cache-failure" ), uri );
			}
		}
		else
		{
			// Failed.
			return muse_raise_error( env, muse_csymbol( env, L"fetch-uri:cache-failure" ), uri );
		}
	}
	else
	{
		return uri;
	}
}
#endif

/**
 * Usage: (fetch-uri uri ['refresh] [headers])
 *
 * Evaluates to the cached local file path for the uri.
 * If 'refresh is passed, then it downloads the uri again if it
 * is out of date, even if it is already in the cache.
 * \p headers is a list of strings, each expected to contain
 * one line of the extra HTTP header to be passed to the request.
 * The header strings should not end with CRLF.
 *
 * muSE exceptions:
 *
 *	'fetch-uri:network-error uri
 *		Indicates a fundamental problem with connecting to the net.
 *		Allows you to replace the uri with a default file path.
 *
 *	'fetch-uri:bad-resource uri
 *		Indicates that the uri is either badly formatted or is
 *		referring to an invalid resource. In this case, you can
 *		continue by replacing the uri with either a file or a default
 *		uri that's known to work.
 *
 *	'fetch-uri:unsupported uri
 *		Indicates that the uri is not a http uri or of a type
 *		that can't be supported. In this case, you are allowed to 
 *		replace the uri with a file.
 *
 *	'fetch-uri:cache-failure uri
 *		Indicates a problem downloading the uri and storing it in
 *		the system cache. In this case, you're allowed to replace it with
 *		a file.
 *
 * @note Supports \ref fn_the "the"
 */
muse_cell fn_fetch_uri( muse_env *env, void *context, muse_cell args )
{
#ifdef MUSE_PLATFORM_WINDOWS
	muse_trace_push( env, L"fetch-uri", MUSE_NIL, args );
	{
		muse_cell uri = muse_evalnext( env, &args );
		muse_boolean cached = MUSE_TRUE;
		muse_cell headers = MUSE_NIL;
		muse_cell sym_refresh = muse_csymbol( env, L"refresh" );
		while ( args ) {
			muse_cell flag = muse_evalnext(env, &args);
			if ( flag == sym_refresh ) {
				cached = MUSE_FALSE;
			} else if ( muse_cell_type(flag) == MUSE_CONS_CELL ) {
				headers = flag;
			}
		}
		

		{
			muse_cell result;
			fetch_uri_args_t args;
			args.uri = uri;
			args.cached = cached;
			args.mimePattern = NULL;
			args.headers = headers;

			result = muse_try( env, MUSE_NIL, (muse_nativefn_t)fetch_uri, &args, MUSE_NIL );
			muse_trace_pop(env);
			return muse_add_recent_item( env, (muse_int)fn_fetch_uri, result );
		}
	}
#else
	return muse_raise_error( env, _csymbol(L"error:not-implemented"), _cons( _csymbol(L"fetch-uri"), MUSE_NIL ) );
#endif
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

static int http_readline( muse_port_t p, buffer_t *b, int limit )
{
	while ( !port_eof(p) && limit > 0 ) {
		int c = port_getc(p);
		if ( c == 0x0D ) {
			c = port_getc(p);
			if ( c == 0x0A )
				return limit;
			else {
				buffer_putc( b, 0x0D );
				--limit;
			}
		} else if ( c == 0x0A )
			return limit;
		else {
			buffer_putc( b, (muse_char)c );
			--limit;
		}
	}

	return limit;
}

static int http_readheader( muse_port_t p, buffer_t *b, int limit )
{
	int count = 0;
	while ( !port_eof(p) && limit > 3 ) {
		int c = port_getc(p);
		if ( c == 0x0D ) {
			c = port_getc(p);
			if ( c == 0x0A ) {
				if ( count > 0 ) {
					c = port_getc(p);
					if ( c == ' ' || c == '\t' ) {
						/* Continue. */
						buffer_putc( b, 0x0D );
						buffer_putc( b, 0x0A );
						buffer_putc( b, c );
						limit -= 3;
						count += 3;
					} else {
						/* End of header. */
						port_ungetc( c, p );
						return limit;
					}
				} else {
					/* New line reached right at start. */
					return limit;
				}
			} else {
				buffer_putc( b, 0x0D );
				buffer_putc( b, c );
				limit -= 2;
				count += 2;
			}
		} else if ( c == 0x0A ) {
			if ( count > 0 ) {
				c = port_getc(p);
				if ( c == ' ' || c == '\t' ) {
					/* Continue. */
					buffer_putc( b, 0x0A );
					buffer_putc( b, c );
					limit -= 2;
					count += 2;
				} else {
					/* End of header. */
					port_ungetc( c, p );
					return limit;
				}
			} else {
				/* New line reached right at start. */
				return limit;
			}
		} else {
			buffer_putc( b, c );
			--limit;
			++count;
		}
	}

	return (limit > 3) ? limit : 0;
}

static int http_headerkey( buffer_t *b )
{
	int len = buffer_length(b);
	int i = 0;
	while ( i < len ) {
		muse_char c = buffer_char( b, i );
		if ( c == ':' )
			return i;
		else
			++i;
	}

	return i;
}

static int http_headerval( buffer_t *b, int colon_pos ) 
{
	int len = buffer_length(b);
	int i = colon_pos;
	if ( buffer_char( b, i ) != ':' )
		return len;
	else {
		++i;
		while ( i < len ) {
			muse_char c = buffer_char( b, i );
			if ( c == ' ' || c == '\t' )
				++i;
			else 
				return i;
		}
		return i;
	}
}

static int http_readword( buffer_t *in, int from, buffer_t *out )
{
	int inlen = buffer_length(in);
	while ( from < inlen ) {
		muse_char c = buffer_char( in, from );
		if ( c == ' ' || c == '\t' || c == 0x0D || c == 0x0A ) {
			while ( c == ' ' || c == '\t' || c == 0x0D || c == 0x0A ) {
				++from;
				if ( from+1 < inlen ) 
					c = buffer_char( in, from );
				else
					return from;
			}
			return from;
		} else {
			buffer_putc( out, c );
			++from;
		}
	}

	return from;
}

static void lowercase_text( muse_env *env, muse_cell text )
{
	int len = 0;
	muse_char *str = (muse_char*)muse_text_contents( env, text, &len );
	int i = 0;
	for ( i = 0; i < len; ++i ) {
		muse_char c = str[i];
		if ( c >= 'A' && c <= 'Z' )
			str[i] = (c - 'A') + 'a';
	}
}

muse_cell fn_symbol( muse_env *env, void *context, muse_cell args );

static muse_cell http_parse( muse_port_t p )
{
	muse_env *env = p->env;

	int sp = _spos();
	buffer_t *reqline = buffer_alloc();
	if ( http_readline( p, reqline, 4096 ) > 0 ) {
		muse_cell req = MUSE_NIL;

		/* Parse the request line into its components. */
		{
			muse_cell get_or_post, url, ver;
			buffer_t *word = buffer_alloc();
			int nextWord = http_readword( reqline, 0, word );
			get_or_post = buffer_to_symbol( word, env );
			buffer_free(word); word = buffer_alloc();
			nextWord = http_readword( reqline, nextWord, word );
			url = buffer_to_string( word, env );
			buffer_free(word); word = buffer_alloc();
			nextWord = http_readword( reqline, nextWord, word );
			ver = buffer_to_string( word, env );
			buffer_free(word);
			req = muse_cons( env, muse_list( env, "ccc", get_or_post, url, ver ), MUSE_NIL );
			_unwind(sp);
			_spush(req);
		}

		buffer_free(reqline);

		{
			muse_cell reqlast = req;
			
			/* Parse each header and append to the list. */
			reqline = buffer_alloc();
			while ( http_readheader( p, reqline, 4096 ) > 0 ) {
				int len = buffer_length(reqline);
				if ( len > 0 ) {
					/* Valid header line read in. */
					int pos = http_headerkey(reqline);
					if ( pos < len ) {
						muse_cell header = buffer_substring( reqline, env, 0, pos );
						muse_cell value;
						lowercase_text( env, header );
						pos = http_headerval( reqline, pos );
						value = buffer_substring( reqline, env, pos, len - pos );
						header = _cons( _cons( fn_symbol( env, NULL, _cons(header,MUSE_NIL) ), value ), MUSE_NIL );
						_sett( reqlast, header );
						reqlast = header;
						_unwind(sp);
						_spush(req);
						buffer_free(reqline);
						reqline = buffer_alloc();
					} else {
						break;
					}
				} else {
					/* Current read position is end of all headers 
					   including the trailing extra newline. */
					break;
				}
			}

			buffer_free(reqline);
			return req;
		}
	} else {
		buffer_free(reqline);
		return MUSE_NIL;
	}
}

/**
 * @code (http-parse port) -> (('GET url "HTTP/1.1") ('Header1 "value1") ('Header2 "value2") ...) @endcode
 * @code (http-parse port) -> (('POST url "HTTP/1.1") ('Header1 "value1") ('Header2 "value2") ...) @endcode
 */
muse_cell fn_http_parse( muse_env *env, void *context, muse_cell args )
{
	muse_cell pcell = _evalnext(&args);
	muse_port_t p = _port(pcell);

	return http_parse(p);
}

static const char *http_codedesc( int code )
{
	switch ( code ) {
      case 100: return "Continue";
      case 101: return "Switching Protocols";
      case 200: return "OK";
      case 201: return "Created";
      case 202: return "Accepted";
      case 203: return "Non-Authoritative Information";
      case 204: return "No Content";
      case 205: return "Reset Content";
      case 206: return "Partial Content";
      case 300: return "Multiple Choices";
      case 301: return "Moved Permanently";
      case 302: return "Found";
      case 303: return "See Other";
      case 304: return "Not Modified";
      case 305: return "Use Proxy";
      case 307: return "Temporary Redirect";
      case 400: return "Bad Request";
      case 401: return "Unauthorized";
      case 402: return "Payment Required";
      case 403: return "Forbidden";
      case 404: return "Not Found";
      case 405: return "Method Not Allowed";
      case 406: return "Not Acceptable";
      case 407: return "Proxy Authentication Required";
      case 408: return "Request Time-out";
      case 409: return "Conflict";
      case 410: return "Gone";
      case 411: return "Length Required";
      case 412: return "Precondition Failed";
      case 413: return "Request Entity Too Large";
      case 414: return "Request-URI Too Large";
      case 415: return "Unsupported Media Type";
      case 416: return "Requested range not satisfiable";
      case 417: return "Expectation Failed";
      case 500: return "Internal Server Error";
      case 501: return "Not Implemented";
      case 502: return "Bad Gateway";
      case 503: return "Service Unavailable";
      case 504: return "Gateway Time-out";
      case 505: return "HTTP Version not supported";
	  default: return NULL;
	}
}

static void crlf( muse_port_t p )
{
	port_putc( 0x0d, p );
	port_putc( 0x0a, p );
}

muse_cell fn_format( muse_env *env, void *context, muse_cell args );

/**
 * @code (http-respond port code headers-alist crlf?) @endcode
 * Pass 0 for the code in to skip the response line
 * and only write out headers.
 */
muse_cell fn_http_respond( muse_env *env, void *context, muse_cell args )
{
	muse_port_t p = (muse_port_t)muse_port( env, _evalnext(&args) );
	int code = (int)muse_int_value( env, _evalnext(&args) );
	muse_cell headers = _evalnext(&args);

	/* Write the response line first. */
	{
		const char *codedesc = http_codedesc(code);
		if ( codedesc ) {
			char buffer[256];
			int n = _snprintf( buffer, 256, "HTTP/1.1 %d %s", code, codedesc );
			port_write( buffer, n, p ); 
			crlf(p);
		}
	}

	/* Write out all the headers given. */
	{
		int sp = _spos();
		while ( headers ) {
			muse_cell h = _next(&headers);
			muse_cell key = _head(h);
			muse_cell value = fn_format( env, NULL, _cons(_tail(h),MUSE_NIL) );
			
			if ( _cellt(key) != MUSE_SYMBOL_CELL ) continue; // Silently ignore the header.
			if ( _cellt(value) != MUSE_TEXT_CELL ) continue; // Silenty ignore the header;

			{
				const muse_char *s = muse_symbol_name( env, key );
				while (*s) { port_putc( *s++, p ); } 
				port_putc( ':', p );
				port_putc( ' ', p );
				s = muse_text_contents( env, value, NULL );
				while (*s) { port_putc( *s++, p ); }
				crlf(p);
			}

			_unwind(sp);
		}
	}

	if ( _evalnext(&args) )
		crlf(p);

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
		{		L"fetch-uri",							fn_fetch_uri							},
		{		L"http-parse",							fn_http_parse							},
		{		L"http-respond",						fn_http_respond							},
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
