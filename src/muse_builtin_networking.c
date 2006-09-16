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
muse_boolean muse_network_startup();
void muse_network_shutdown();
muse_cell fn_with_connection_to_server( muse_env *env, void *context, muse_cell args );
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
#	include <netinet/in.h>
#	include <arpa/inet.h>
#	define SOCKET int
#	define SOCKET_ERROR (-1)
#	define INVALID_SOCKET (-1)
#	define closesocket(x) close(x)
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

/** For Windows, we have to explicitly startup the socket
library! What a stupid thing to have to do! */
	muse_boolean muse_network_startup()
	{
        /* We ask for sockets version 2.0. */
        const WORD kRequestedVersion = MAKEWORD(2,0);
		WSADATA wsadata;
		
        /* Startup the socket API. This makes sure we have the
        requested sockets version installed on the system. */
        fprintf(stderr, "Starting up the socket system...\n");
        if (WSAStartup(kRequestedVersion, &wsadata) != 0)
        {
            fprintf(stderr,	"Error: Socket API can't startup. "
					"Need version %x.\n", 
					kRequestedVersion
					);
			return MUSE_FALSE;
        }
		
		return MUSE_TRUE;
	}

	void muse_network_shutdown()
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
	muse_boolean muse_network_startup()
	{
		/* We ignore the SIGPIPE signal so that the application won't abort
		on socket termination errors. */
		struct sigaction ignoreSIG;
		ignoreSIG.sa_handler	= SIG_IGN;
		ignoreSIG.sa_flags		= 0;
		sigemptyset(&ignoreSIG.sa_mask);

		if ( sigaction(SIGPIPE, &ignoreSIG, NULL) < 0 ) 
		{
			fprintf(stderr, "FATAL ERROR: Couldn't ignore SIGPIPE!\n");
			exit(0);
		}
		
		return MUSE_TRUE;
	}

	void muse_network_shutdown()
	{
		/* Nothing to do. */
	}
#endif


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


static void socket_init( void *s, muse_cell args )
{
	muse_port_t p = (muse_port_t)s;
	port_init( p );
	
	p->eof = 0;
	p->error = 0;
}

static void socket_close( void *s );
static void socket_destroy( void *s )
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
	
	muse_int result = recv( p->socket, buffer, (int)nbytes, 0 );
	
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
	
	while ( b < b_end )
	{
		muse_int bytes_sent = send( p->socket, b, (int)(b_end - b), 0 );
		
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
		socket_init,
		NULL,
		socket_destroy
	},
	
	socket_close,
	socket_read,
	socket_write,
	socket_flush
};

static muse_cell fn_network_client_receive( muse_env *env, void *context, muse_cell args );
static muse_cell fn_network_client_send( muse_env *env, void *context, muse_cell args );

/**
 * (with-connection-to-server server-port-string service-fn).
 * Connects to the given server on the given port and invokes the
 * given service-fn with the reader and writer for the connection.
 * Works only with s-expression streams.
 * 
 * For example -
 * @code
 * (with-connection-to-server "123.234.134.23:8080"
 *     (fn (port)
 *        (write port '(hello world))
 *        (case (read port)
 *           ('hi (print "Success!\n"))
 *           (()  (print "Failed!\n")))))
 * @endcode
 * 
 * @see fn_with_incoming_connections_to_port()
 */
muse_cell fn_with_connection_to_server( muse_env *env, void *context, muse_cell args )
{
	int sp					= _spos();
	muse_cell result		= MUSE_NIL;
	muse_cell serverport	= muse_evalnext(&args);

	muse_cell portcell		= muse_mk_functional_object( &g_socket_type.obj, MUSE_NIL );
	socketport_t *port		= (socketport_t*)muse_port(portcell);
	
	char serverStringAddress[32];
	char *serverPortAddress;
	int length = 0;
	const muse_char *serverWstringAddress = muse_text_contents( serverport, &length );
	if ( length > 17 )
	{
		fprintf( stderr, "Invalid server address spec '%S'\n", serverWstringAddress );
		fprintf( stderr, "Address must have the form NNN.NNN.NNN.NNN:PPPP\n");
		goto UNDO_CONN;
	}
	
	muse_unicode_to_utf8( serverStringAddress, 32, muse_text_contents(serverport,NULL), length );
	serverPortAddress = strchr( serverStringAddress, ':' );
	if ( serverPortAddress )
	{
		serverPortAddress[0] = '\0';
		++serverPortAddress;
		/* We have now split the server:port string to separate
			server and port strings. */
	}
	
	port->address.sin_family		= AF_INET;
	port->address.sin_addr.s_addr	= inet_addr(serverStringAddress);
	port->address.sin_port			= htons( (short)(serverPortAddress ? atoi(serverPortAddress) : MUSE_DEFAULT_MULTICAST_PORT) );
	
	port->socket = socket(AF_INET, SOCK_STREAM, 0);
	
	/**
	 * NOTE:
	 *
	 * Windows doesn't close blocking sockets properly if you ask it
	 * to do a "graceful close". This affects the other side of the network.
	 * For example, Java doesn't get to throw an IOException upon a 
	 * sincere plain ::closesocket() call and just hangs on the
	 * InputStream's read() method. The painful part is that this behaviour 
	 * is not consistent from socket to socket. For one socket the close
	 * operation might succeed gracefully whereas for another it might fail,
	 * for no particular fault of yours.
	 *
	 * Therefore the default way we'll close client sockets in Windows is 
	 * by setting it to do a "hard close". 
	 * 
	 * To do such a "hard close", you set the SO_LINGER parameter of the socket 
	 * and give a zero timeout period. This is what the following ::setsockopt()
	 * call does.
	 *
	 * CORRECTION: Sorry! It doesn't seem to be a Windows problem.It is supposed
	 * to be standard socket behaviour. The ::closesocket() call will ultimately
	 * result in the local socket being close only after the remote socket closes.
	 * How does the remote socket know when to close? He should check the return
	 * value of the ::read() call. When ::read() returns 0, it is supposed to 
	 * indicate closing time (other possibility is a ::shutdown()). But Java doesn't
	 * return from read at all!!
	 */
	
    if ( port->socket > 0 ) 
	{
        struct linger lingerParams = { 1, 0 };
        setsockopt( port->socket, SOL_SOCKET, SO_LINGER, (const char *)&lingerParams, sizeof(struct linger) );
    }
	
	/* Connect to the server. */
	if ( connect( port->socket, (struct sockaddr*)&(port->address), sizeof(port->address) ) == SOCKET_ERROR )
	{
		fprintf( stderr, "Connection to server '%s:%s' failed!\n", serverStringAddress, serverPortAddress );
		goto UNDO_CONN;
	}
	
	port->base.mode = MUSE_PORT_READ_WRITE;

	/* Connection succeeded. Call the service function. */
	{
		muse_cell handler = muse_evalnext(&args);
		muse_cell port_args = muse_cons( portcell, MUSE_NIL );
		
		result = muse_apply( handler, port_args, MUSE_TRUE );
		
		/* Save the result. */
		_unwind(sp);
		_spush(result);
	}
	
	
UNDO_CONN:
	/* Close the connection and return. */
	if ( port )
	{
		port_close( (muse_port_t)port );
	}
	
	return result;
}


typedef struct muse_server_stream_socket__
{
	SOCKET		listenSocket;		/**< We listen for connections on this one. */
	
	struct sockaddr_in	localSocketAddress;	/**< The local socket address we bind to. */
			
	muse_cell	client_port_cell;
	socketport_t *client_port;
	
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
	short listenPort		= (short)muse_int_value( muse_evalnext(&args) );
	muse_cell handler		= muse_evalnext(&args);
	int sp					= _spos();
	muse_server_stream_socket_t *conn = (muse_server_stream_socket_t*)calloc( 1, sizeof(muse_server_stream_socket_t) );
	muse_cell result = MUSE_NIL;
	
	conn->client_port_cell = muse_mk_functional_object( &g_socket_type.obj, MUSE_NIL );
	conn->client_port = (socketport_t*)muse_port( conn->client_port_cell );
	
	/* Create the socket we should listen to. */
	conn->listenSocket = socket(AF_INET, SOCK_STREAM, 0);
	if ( conn->listenSocket == INVALID_SOCKET )
	{
		fprintf( stderr, "Couldn't create socket to listen for connections.\n" );
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
		fprintf( stderr, "Couldn't bind listen-socket to local address.\n" );
		goto SHUTDOWN_SERVER;
	}
	
	/* Use "listen" to set the connection backlog buffer. */
	if ( listen( conn->listenSocket, SOMAXCONN ) == SOCKET_ERROR )
	{
		fprintf( stderr, "'listen' failed.\n" );
		goto SHUTDOWN_SERVER;
	}
	
	/* Accept connections on the server socket.
	 * Once one connection happens, we are through and we
	 * won't listen for any more. 
	 */
	ACCEPT_CONNECTIONS:
	fprintf( stderr, "Waiting for connections to port %d ...\n", ntohs(conn->localSocketAddress.sin_port) );
	{
		socklen_t sockAddrSize = sizeof(struct sockaddr_in);
		struct sockaddr_in client_address;
		SOCKET client = accept( conn->listenSocket, (struct sockaddr *)&client_address, &sockAddrSize );

		if ( client >= 0 )
		{
			fprintf( stderr,
					 "Accepted connection from machine %s on port %d.\n",
					 inet_ntoa( client_address.sin_addr ),
					 ntohs( client_address.sin_port )
					);

			conn->client_port->socket = client;
			conn->client_port->base.mode = MUSE_PORT_READ_WRITE;
			conn->client_port->base.eof = 0;
			conn->client_port->base.error = 0;
			
			if ( client == SOCKET_ERROR )
				goto CLOSE_CLIENT_CONTINUE;
		}
		
		/* Client connection accepted successfully. Call the handler. */
		{
			muse_cell handlerArgs = muse_list( "ct", 
												conn->client_port_cell, 
												inet_ntoa( client_address.sin_addr ) );			

			result = muse_apply( handler, handlerArgs, MUSE_TRUE );
			
			/* Save the result. The handler must return a non-NIL value to indicate that
				subsequent connections have to be processed. It should return MUSE_NIL
				to terminate the server. */
			_unwind(sp);
			_spush(result);
		}
		
	CLOSE_CLIENT_CONTINUE:
		muse_assert( conn->client_port );
		
		/* Recycle the same port object for use with the next connection. */
		port_close( (muse_port_t)conn->client_port );
		printf( "\nConnection closed.\n" );
		if ( result )
		{
			port_init( (muse_port_t)conn->client_port );
			goto ACCEPT_CONNECTIONS;
		}
	}
	
	/* We've completed processing all client connections. Shutdown the server. */
SHUTDOWN_SERVER:
	port_destroy( (muse_port_t)conn->client_port );
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

static void multicast_socket_init( void *p, muse_cell args )
{
	multicast_socket_port_t *s = (multicast_socket_port_t*)p;
	muse_cell group		= muse_evalnext(&args);
	muse_cell mcport	= muse_evalnext(&args);
	char group_address[32];
	u_short group_port;
	int result = 0;

	if ( mcport )
	{
		/* Get the port number to use. */
		group_port = (u_short)muse_int_value(mcport);		
	}
	else
	{
		/* No port specified. Use default port 31415. */
		group_port = MUSE_DEFAULT_MULTICAST_PORT;
		fprintf( stderr, "No port specified for multicast. Using default port 31415.\n" );
	}
	
	if ( group )
	{
		/* The group is supposed to specify an internet numeric address.
		Convert the wide string to a narrow string. */
		int length = 0;
		muse_unicode_to_utf8( group_address, 32, muse_text_contents( group, &length ), 32 );
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
		fprintf( stderr, "No group specified. Using default group %s\n", group_address );
	}
		
	port_init(&s->base);

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
			fprintf( stderr, "Cannot reuse same address for multiple sockets!\n" );
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
		fprintf( stderr, "Binding to multicast group port failed!\n" );
		s->base.error = 2;
		goto CLOSE_SOCKET_AND_BAIL;
	}

	/* Disable loopback. */
	{
		u_char loop = 0;
		if ( (result = setsockopt(s->socket, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof(loop))) < 0 )
		{
			fprintf( stderr, "Failed to disable multicast loop back!\n" );
		}
	}

	/* Request the kernel to join the multicast group. */
	s->mreq.imr_interface.s_addr	= htonl(INADDR_ANY);
	s->mreq.imr_multiaddr.s_addr	= inet_addr(group_address);
	if ( (result = setsockopt( s->socket, IPPROTO_IP, IP_ADD_MEMBERSHIP, (const char *)&(s->mreq), sizeof(s->mreq) )) < 0 )
	{
		fprintf( stderr, "Couldn't join the multicast group!\n" );
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
			fprintf( stderr, "Couldn't remove kernel from multicast group!\n" );
		}
		
		/* We're done with the socket. */
		closesocket( s->socket );
		s->socket = 0;
	}
}

static void multicast_socket_destroy( void *p )
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

	/* Wait for and read a datagram from any of the group. */
	s->src_addr_len = sizeof(s->src_addr);

	{
		int result = recvfrom( s->socket, (char*)buffer, (int)nbytes, 0, (struct sockaddr*)&(s->src_addr), &s->src_addr_len );
		if ( result < 0 )
		{
			s->base.eof		= EOF;
			s->base.error	= EOF;
			return 0;
		}

		fprintf( stderr, "Received datagram from %s on port %d\n", inet_ntoa(s->src_addr.sin_addr), ntohs(s->src_addr.sin_port) );
		return (size_t)result;
	}
}

static size_t multicast_socket_write( void *buffer, size_t nbytes, void *p )
{
	multicast_socket_port_t *s = (multicast_socket_port_t*)p;

	/* Multicast the datagram to the group or the individual. */
	const struct sockaddr *addr = (const struct sockaddr *)(s->reply ? &s->src_addr : &s->dst_addr);
	int addr_len = s->reply ? s->src_addr_len : sizeof(s->dst_addr);
	int result = sendto( s->socket, buffer, (int)nbytes, 0, addr, addr_len );
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
		multicast_socket_init,
		NULL,
		multicast_socket_destroy
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
	return muse_mk_functional_object( (muse_functional_object_type_t*)&g_multicast_socket_type, args );
}

/**
 * (multicast-group? p).
 * Returns \c p if it is a multicast port and MUSE_NIL if it isn't.
 */
muse_cell fn_multicast_group_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell p = muse_evalnext(&args);
	muse_port_t port = muse_port(p);
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
	muse_cell port = muse_evalnext(&args);
	multicast_socket_port_t *s = (multicast_socket_port_t*)muse_port(port);

	muse_assert( s && s->base.base.type_info == &g_multicast_socket_type.obj );
	/**< We support only multicast sockets. */

	s->reply = 1;

	/* Behave like write w.r.t. the remaining arguments. */
	while ( args )
	{
		muse_pwrite( (muse_port_t)s, muse_evalnext(&args) );
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
	muse_cell port = muse_evalnext(&args);
	muse_cell timeout = args ? muse_evalnext(&args) : MUSE_NIL;
	
	/* Wait for 10 minutes in the default case. */
	muse_int timeout_us = timeout ? muse_int_value(timeout) : (1000000 * 60 * 10);
	
	muse_port_t p = muse_port(port);
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
				case 0 : /* Timed out. */ return muse_builtin_symbol(MUSE_TIMEOUT);
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
	muse_network_shutdown();
	return MUSE_NIL;
}

void muse_define_builtin_networking()
{
	struct funs_t { const muse_char *name; muse_nativefn_t fn; };

	static const struct funs_t k_networking_funs[] =
	{
		{		L"with-connection-to-server",			fn_with_connection_to_server			},
		{		L"with-incoming-connections-to-port",	fn_with_incoming_connections_to_port	},
		{		L"wait-for-input",						fn_wait_for_input						},
		{		L"multicast-group",						fn_multicast_group						},
		{		L"reply",								fn_reply								},
		{		L"multicast-group?",					fn_multicast_group_p					},
		{		NULL,									NULL									}
	};

	muse_network_startup();

	/* Define a destructor function to shutdown the network when everything is done. 
	Using braces in the symbol name ensures that this symbol cannot be written directly
	in muSE code. */
	muse_define( muse_csymbol(L"{{network-shutdown}}"), muse_mk_destructor(fn_network_shutdown,NULL) );

	{
		const struct funs_t *funs = k_networking_funs;
		while ( funs->name )
		{
			muse_define( muse_csymbol(funs->name), muse_mk_nativefn(funs->fn,NULL) );
			++funs;
		}
	}

}