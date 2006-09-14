/**
 * @file muse_builtin_fileport.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-scheme.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * Implements file ports - a wrapper for I/O using FILE*.
 */

#include "muse_port.h"
#include <stdlib.h>
#include <memory.h>

/** @addtogroup Ports */
/*@{*/

typedef struct
{
	muse_port_base_t base;
	int desc;
	FILE *file;
	int reserved0;
} fileport_t;

typedef struct
{
	muse_port_type_t port;
	muse_cell sym_read, sym_write;
} fileport_type_t;

static void write_utf8_header( fileport_t *p );
static void discard_utf8_header( fileport_t *p );

static void check_for_ezscheme_file( fileport_t *p )
{
	if ( p->base.in.avail > 0 && p->base.in.bytes[0] == '#' )
		p->base.mode |= MUSE_PORT_EZSCHEME;
}

static void fileport_init( void *ptr, muse_cell args )
{
	fileport_t *p = (fileport_t*)ptr;

	muse_boolean read_flag = MUSE_FALSE;
	muse_boolean write_flag = MUSE_FALSE;
	muse_cell filename = muse_evalnext(&args);

	/* Get the read/write flags. */
	while ( args )
	{
		muse_cell flag = muse_evalnext(&args);
		if ( flag == ((fileport_type_t*)p->base.base.type_info)->sym_read )
			read_flag = MUSE_TRUE;
		else if ( flag == ((fileport_type_t*)p->base.base.type_info)->sym_write )
			write_flag = MUSE_TRUE;
	}

	if ( read_flag ) p->base.mode |= MUSE_PORT_READ;
	if ( write_flag ) p->base.mode |= MUSE_PORT_WRITE;

	port_init( (muse_port_base_t*)p );
	
	/* Open the file. */
	{
		p->file = muse_fopen( muse_text_contents(filename,NULL), (read_flag ? (write_flag ? L"rwb" : L"rb") : (write_flag ? L"wb" : L"rb")) );
		if ( !p->file )
			return;
		p->desc = fileno( p->file );
		
		p->base.error	= 0;
		p->base.eof		= 0;

		if ( write_flag )
			write_utf8_header(p);
		if ( read_flag )
		{
			discard_utf8_header(p);
			check_for_ezscheme_file(p);
		}
	}
}

static void fileport_destroy( void *ptr )
{
	fileport_t *p = (fileport_t*)ptr;
	fileport_type_t *t = (fileport_type_t*)p->base.base.type_info;

	if ( p->file && t->port.close )
	{
		t->port.close(p);
	}

	port_destroy( (muse_port_base_t*)p );
}

static void fileport_close( void *ptr )
{
	fileport_t *p = (fileport_t*)ptr;

	if ( p->file )
	{
		fclose( p->file );
		p->file = NULL;		
		p->desc = 0;
	}
}

static size_t fileport_read( void *buffer, size_t nbytes, void *port )
{
	fileport_t *p = (fileport_t*)port;

	muse_assert( p->file != NULL );

	{
		/* Read the requested number of bytes into the given target buffer. */
		size_t bytes_read = read( p->desc, buffer, (unsigned int)nbytes );

		return bytes_read;
	}
}

static size_t fileport_write( void *buffer, size_t nbytes, void *port )
{
	fileport_t *p = (fileport_t*)port;

	muse_assert( p->file != NULL );
	
	return write( p->desc, buffer, (unsigned int)nbytes );
}

static int fileport_flush( void *port )
{
	fileport_t *p = (fileport_t*)port;

	muse_assert( p->file != NULL);

	return fflush( p->file );
}

static fileport_type_t g_fileport_type =
{
	{
		{
			'muSE',
			'port',
			sizeof(fileport_t),
			NULL,
			fileport_init,
			NULL,
			fileport_destroy
		},

		fileport_close,
		fileport_read,
		fileport_write,
		fileport_flush
	},

	MUSE_NIL,
	MUSE_NIL
};

static fileport_type_t g_port_type_stdin =
{
	{
		{
			'muSE',
			'port',
			sizeof(fileport_t),
			NULL,
			NULL,
			NULL,
			NULL
		},

		NULL,
		fileport_read,
		NULL,
		NULL
	},

	MUSE_NIL,
	MUSE_NIL
};

static fileport_type_t g_port_type_stdout =
{
	{
		{
			'muSE',
			'port',
			sizeof(fileport_t),
			NULL,
			NULL,
			NULL,
			NULL
		},

		NULL,
		NULL,
		fileport_write,
		fileport_flush
	},

	MUSE_NIL,
	MUSE_NIL
};

static fileport_t g_muse_stdports[3] =
{
	{	{ 'muSE', (muse_functional_object_type_t*)&g_port_type_stdin },
		MUSE_STDIN_PORT, NULL, 0
	},
	{	{ 'muSE', (muse_functional_object_type_t*)&g_port_type_stdout },
		MUSE_STDOUT_PORT, NULL, 0
	},
	{	{ 'muSE', (muse_functional_object_type_t*)&g_port_type_stdout },
		MUSE_STDERR_PORT, NULL, 0
	}
};

muse_port_t muse_stdport( muse_stdport_t descriptor )
{
	muse_assert( descriptor >= MUSE_STDIN_PORT && descriptor <= MUSE_STDERR_PORT );
	return (muse_port_t)&g_muse_stdports[descriptor];
}

/**
 * (open-file "filename.txt" ['for-reading 'for-writing]).
 * Returns a new file port for reading or writing to it.
 * Use \c read and \c write with the returned port and
 * when you're done with it, call \c close.
 *
 * For example -
 * @code
 * (let ((f (open-file "output.txt" 'for-writing)))
 *     (write f '(hello world))
 *     (close f))
 * @endcode
 */
static muse_cell fn_open_file( muse_env *env, void *context, muse_cell args )
{
	return muse_mk_functional_object( (muse_functional_object_type_t*)&g_fileport_type, args );
}

/**
 * The destructor that will be called at environment destruction time
 * to free up the standard ports.
 */
static muse_cell fn_destroy_stdports( muse_env *env, void *context, muse_cell args )
{
	int i;
	for ( i = MUSE_STDIN_PORT; i <= MUSE_STDERR_PORT; ++i )
	{
		if ( i != MUSE_STDIN_PORT )
			port_flush( (muse_port_t)&g_muse_stdports[i] );

		port_destroy( (muse_port_t)&g_muse_stdports[i] );
	}

	return MUSE_NIL;
}

void muse_define_builtin_fileport()
{
	g_fileport_type.sym_read = 
		g_port_type_stdout.sym_read = 
		g_port_type_stdin.sym_read = muse_csymbol(L"for-reading");

	g_fileport_type.sym_write = 
		g_port_type_stdout.sym_write = 
		g_port_type_stdin.sym_write = muse_csymbol(L"for-writing");

	g_muse_stdports[0].file	= stdin;
	g_muse_stdports[1].file	= stdout;
	g_muse_stdports[2].file	= stderr;

	port_init( (muse_port_t)&g_muse_stdports[0] );
	port_init( (muse_port_t)&g_muse_stdports[1] );
	port_init( (muse_port_t)&g_muse_stdports[2] );

	g_muse_stdports[0].base.mode |= MUSE_PORT_TRUSTED_INPUT;
	g_muse_stdports[1].base.tab_size = 8;
	g_muse_stdports[2].base.tab_size = 8;
	
	/* Define the "open-file" function. This is the only file specific function needed.
	After this the generic port functions take over. */
	muse_define( muse_csymbol(L"open-file"), muse_mk_nativefn( fn_open_file, NULL ) );

	/* We add a destructor for the standard ports and set the value of an internal symbol
	to the destructor. We do this so that the destructor will be invoked only at environment
	destruction time. If we don't assign it to a symbol, the destructor will be invoked
	the next time garbage collection kicks in, since there will be no active reference to
	the destructor. */
	muse_define( muse_csymbol(L"{(##standard-ports##)}"), muse_mk_destructor( fn_destroy_stdports, NULL ) );
}


/**
 * Creates a port definition that you can use to read/write stuff
 * from a given file pointer. The returned port is only for use
 * by the API when it has a file pointer and must use the
 * port calls such as muse_pread() several times on the same port.
 *
 * Ports assigned using \c muse_assign_port must be released
 * using \c muse_unassign_port.
 *
 * @param f The file that the new port should read from.
 * @param mode A combination of \c muse_port_mode_bits_t indicating 
 * properties of the file port.
 */
muse_port_t muse_assign_port( FILE *f, int mode )
{
	fileport_t *port = calloc( 1, sizeof(fileport_t) );
	
	port->base.base.magic_word	= 'muSE';
	port->base.base.type_info	= &g_fileport_type.port.obj;
	port->file					= f;
	port->desc					= fileno(f);
	
	port_init( &port->base );
	
	if ( mode & MUSE_PORT_READ )
	{
		discard_utf8_header(port);
		check_for_ezscheme_file(port);
	}

	/* The mode bits is a constrained number. If you set bits that don't exist,
	its a programming error. */
	muse_assert( mode < MUSE_PORT_READ_DETECT_MACROS * 2 );

	port->base.mode |= mode;

	if ( mode & MUSE_PORT_WRITE )
	{
		write_utf8_header( port );
	}

	return &port->base;
}

/**
 * Unassigns a file port which was earlier assigned using
 * \c muse_assign_port. Should not be used with other
 * ports.
 */
void muse_unassign_port( muse_port_t p )
{
	if ( p->mode & MUSE_PORT_WRITE )
		port_flush(p);

	port_destroy( p );
	free( p );
}
/*@}*/

/**
 * Reads all symbolic expressions in the stream and evaluates
 * them one by one, until end of stream. Returns the result
 * of evaluating the last s-expression in the stream.
 * 
 * Use this to load definitions from files.
 */
muse_cell muse_load( FILE *f )
{
	muse_port_t in = muse_assign_port(f, MUSE_PORT_TRUSTED_INPUT );
	int sp = _spos();
	muse_cell result = MUSE_NIL;
	
	while ( port_eof(in) == 0 )
	{
		muse_cell expr = MUSE_NIL;

		expr = muse_pread(in);

		if ( expr >= 0 )
		{
			_unwind(sp);
			_spush(expr);
			result = muse_eval( expr );
			_unwind(sp);
			_spush(result);
		}
		else
			break;
	}
	
	muse_unassign_port(in);
	return result;
}

static void write_utf8_header( fileport_t *p )
{
#ifdef MUSE_PLATFORM_WINDOWS
	if ( tell(p->desc) == 0 )
	{
		/* We're at the beginning of an output file. Under windows,
		make sure that Notepad and other applications know that its
		UTF8 encoded using the 3 byte header - 0xef 0xbb 0xbf at the
		beginning. We don't do this under unices. */
		static const unsigned char k_utf8_header[3] = { 0xef, 0xbb, 0xbf };
		write( p->desc, k_utf8_header, sizeof(k_utf8_header) );
	}
#endif
}

static void discard_utf8_header( fileport_t *p )
{
	if ( ftell(p->file) == 0 )
	{
		/* We're at the head. Check if the file has the UTF8 3-byte indicator.
		If so, strip it out, since we don't need it. We only accept UTF8 anyway. 
		This is Windows specific really, but it helps for unix implementations
		to read windows generated files as well, so I'm keeping it for all 
		platforms. */

		unsigned char c[3];
		int nbytes = read( p->desc, c, 3 );

		if ( nbytes == 3 )
		{
			if ( c[0] == 0xef && c[1] == 0xbb && c[2] == 0xbf )
			{
				/* Yes its the UTF8 header. Discard it. */
				nbytes = 0;
			}
		}

		if ( nbytes > 0 )
		{
			muse_assert( p->base.in.pos == 0 && p->base.in.avail == 0 );
			memcpy( p->base.in.bytes, c, nbytes );
			p->base.in.avail += nbytes;
		}
	}
}
