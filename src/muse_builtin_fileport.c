/**
 * @file muse_builtin_fileport.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
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
} fileport_type_t;

static void write_utf8_header( muse_env *env, fileport_t *p );
static void discard_utf8_header( muse_env *env, fileport_t *p );

static void fileport_init( muse_env *env, void *ptr, muse_cell args )
{
	fileport_t *p = (fileport_t*)ptr;

	muse_boolean read_flag		= MUSE_FALSE;
	muse_boolean write_flag		= MUSE_FALSE;
	muse_boolean binary_flag	= MUSE_FALSE;
	muse_boolean expand_braces	= MUSE_FALSE;
	muse_boolean detect_macros	= MUSE_FALSE;
	muse_boolean tab_syntax		= MUSE_FALSE;

	muse_cell sym_for_reading	= _csymbol(L"for-reading");
	muse_cell sym_for_writing	= _csymbol(L"for-writing");
	muse_cell sym_binary		= _csymbol(L"binary");
	muse_cell sym_trust			= _csymbol(L"trust");
	muse_cell sym_expand_braces = _csymbol(L"expand-braces");
	muse_cell sym_detect_macros = _csymbol(L"detect-macros");
	muse_cell sym_tab_syntax	= _csymbol(L"tab-syntax");

	muse_cell filename = _evalnext(&args);

	/* Get the read/write flags. */
	while ( args )
	{
		muse_cell flag = _evalnext(&args);
		if ( flag == sym_for_reading )
			read_flag = MUSE_TRUE;
		else if ( flag == sym_for_writing )
			write_flag = MUSE_TRUE;
		else if ( flag == sym_binary )
			binary_flag = MUSE_TRUE;
		else if ( flag == sym_expand_braces )
			expand_braces = MUSE_TRUE;
		else if ( flag == sym_detect_macros )
			detect_macros = MUSE_TRUE;
		else if ( flag == sym_trust ) {
			expand_braces = MUSE_TRUE;
			detect_macros = MUSE_TRUE;
		} else if ( flag == sym_tab_syntax ) 
			tab_syntax = MUSE_TRUE;
	}

	if ( read_flag ) p->base.mode |= MUSE_PORT_READ;
	if ( write_flag ) p->base.mode |= MUSE_PORT_WRITE;
	if ( read_flag ) {
		/* Macro and brace expansion is only applicable when reading a file. */
		if ( expand_braces ) p->base.mode |= MUSE_PORT_READ_EXPAND_BRACES;
		if ( detect_macros ) p->base.mode |= MUSE_PORT_READ_DETECT_MACROS;
		if ( tab_syntax )
			p->base.mode |= MUSE_PORT_EZSCHEME;
	}

	port_init( env, (muse_port_base_t*)p );
	
	/* Open the file. */
	while ( p->file == NULL )
	{
		p->file = muse_fopen( _text_contents(filename,NULL), (read_flag ? (write_flag ? L"rwb" : L"rb") : (write_flag ? L"wb" : L"rb")) );
		if ( !p->file )
		{
			filename = muse_raise_error( env, _csymbol(L"error:open-file"), _cons(filename,MUSE_NIL) );
			if ( _cellt(filename) == MUSE_TEXT_CELL )
				continue;
			else {
				p->base.error = -1;
				p->base.eof = -1;
				return;
			}
		}
		p->desc = fileno( p->file );
		
		p->base.error	= 0;
		p->base.eof		= 0;

		if ( !binary_flag )
		{
			if ( write_flag )
				write_utf8_header(env,p);
			if ( read_flag )
			{
				discard_utf8_header(env,p);
			}
		}
	}
}

static void fileport_destroy( muse_env *env, void *ptr )
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

	/* Read the requested number of bytes into the given target buffer. */
	if ( p->file )
		return fread( buffer, 1, nbytes, p->file );
	else 
		return read( p->desc, buffer, (unsigned int)nbytes );
}

static size_t uc16_fileport_read( void *buffer, size_t nbytes, void *port )
{
	fileport_t *p = (fileport_t*)port;

	/* We're reading a unicode file. We need to read in 16-bit 
	data and convert it into utf-8 before storing it into buffer. 
	Kind of inefficient, but simplest way to support 16-bit text
	files given that utf8 is native for us. */
	if ( p->file )
	{
		unsigned char *b = (unsigned char *)buffer;
		size_t n = 0;
		FILE *f = p->file;

		while ( !feof(f) && n < nbytes )
		{
			int c = (fgetc(f) & 0xFF);
			c |= (fgetc(f) & 0xFF) << 8;

			{
				int nc = uc16_to_utf8( c, b+n, (int)(nbytes-n) );
				if ( nc == 0 )
				{
					ungetc( c >> 8, f );
					ungetc( c & 0xFF, f );
					return n;
				}
				else if ( nc < 0 )
				{
					p->base.error = -1;
					return 0;
				}
				else
					n += nc;
			}
		}

		return n;
	}
	else
		return 0;
}

static size_t fileport_write(void *buffer, size_t nbytes, void *port )
{
	fileport_t *p = (fileport_t*)port;
	
	if ( p->file )
		return fwrite( buffer, 1, nbytes, p->file );
	else
		return write( p->desc, buffer, (unsigned int)nbytes );
}

static int fileport_flush( void *port )
{
	fileport_t *p = (fileport_t*)port;

	if ( p->file )
		return fflush( p->file );
	else
		return 0;
}

static fileport_type_t g_fileport_type =
{
	{
		{
			'muSE',
			'port',
			sizeof(fileport_t),
			NULL,
			NULL,
			fileport_init,
			NULL,
			fileport_destroy,
			NULL
		},

		fileport_close,
		fileport_read,
		fileport_write,
		fileport_flush
	}
};

static fileport_type_t g_uc16_fileport_type =
{
	{
		{
			'muSE',
			'port',
			sizeof(fileport_t),
			NULL,
			NULL,
			fileport_init,
			NULL,
			fileport_destroy,
			NULL
		},

		fileport_close,
		uc16_fileport_read,
		NULL,
		NULL
	}
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
			NULL,
			NULL,
			NULL
		},

		NULL,
		fileport_read,
		NULL,
		NULL
	}
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
			NULL,
			NULL,
			NULL
		},

		NULL,
		NULL,
		fileport_write,
		fileport_flush
	}
};

static fileport_t g_muse_stdports[3] =
{
	{	{ 'muSE', (muse_functional_object_type_t*)&g_port_type_stdin, MUSE_NIL },
		MUSE_STDIN_PORT, NULL, 0
	},
	{	{ 'muSE', (muse_functional_object_type_t*)&g_port_type_stdout, MUSE_NIL },
		MUSE_STDOUT_PORT, NULL, 0
	},
	{	{ 'muSE', (muse_functional_object_type_t*)&g_port_type_stdout, MUSE_NIL },
		MUSE_STDERR_PORT, NULL, 0
	}
};

MUSEAPI muse_port_t muse_stdport( muse_env *env, muse_stdport_t descriptor )
{
	muse_assert( descriptor >= MUSE_STDIN_PORT && descriptor <= MUSE_STDERR_PORT );
	return env->stdports[descriptor];
}

/**
 * @code (open-file "filename.txt" ['for-reading 'for-writing 'binary 'expand-braces 'detect-macros 'trust 'tab-syntax]) @endcode
 *
 * Returns a new file port for reading or writing to it.
 * Use \c read and \c write with the returned port and
 * when you're done with it, call \c close. If you use the 'binary
 * flag, then any header that might be present at the start of
 * the file won't be processed. Also, no such header will be written
 * out if you're opening the file for writing.
 *
 * You can allow brace expansion during reading by passing the 
 * @code 'expand-braces @endcode and macro detection and expansion
 * using the @code 'detect-macros @endcode flag. The @code 'trust @endcode
 * flag is an abbreviation for setting both of them. If both are not
 * set, the reader will not execute any code. These flags only affect
 * the port when opened in read mode.
 *
 * For example -
 * @code
 * (let ((f (open-file "output.txt" 'for-writing)))
 *     (write f '(hello world))
 *     (close f))
 * @endcode
 *
 * If \c open-file fails, it raises the @code ('error:open-file <filename>) @endcode
 * exception. You can resume the exception by 
 *	-# continuing with a different filename,
 *	-# continuing with () which will cause the port to be in eof state.
 * You can also, of course, abort.
 *
 * Supports \ref fn_the "the"
 *
 * @exception error:open-file
 * Handler format: @code (fn (resume 'error:open-file path) ...) @endcode
 * Raised when, for some reason, the given path couldn't be opened as a file.
 * The handler can resume by resolving the correct path to use.
 */
muse_cell fn_open_file( muse_env *env, void *context, muse_cell args )
{
	return muse_add_recent_item( env, (muse_int)fn_open_file, _mk_functional_object( (muse_functional_object_type_t*)&g_fileport_type, args ) );
}

void muse_define_builtin_fileport(muse_env *env)
{
	int sp = _spos();

	{
		int i;
		const muse_char *stdnames[] = { L"*stdin*", L"*stdout*", L"*stderr*" };
		for ( i = 0; i < 3; ++i )
		{
			env->stdports[i] = malloc(sizeof(fileport_t));
			memcpy( env->stdports[i], &g_muse_stdports[i], sizeof(fileport_t) );
			port_init( env, env->stdports[i] );

			/* Define *stdin*, *stdout* and *stderr* to the respective ports. */
			{
				muse_cell ref = _mk_destructor( (muse_nativefn_t)NULL, env->stdports[i] );
				env->stdports[i]->base.ref = ref;
				_define( _csymbol(stdnames[i]), ref );
				_unwind(sp);
			}
		}
	}

	env->stdports[0]->mode |= MUSE_PORT_TRUSTED_INPUT;
	env->stdports[1]->tab_size = 8;
	env->stdports[2]->tab_size = 8;
	
	muse_current_port( env, MUSE_STDIN_PORT, env->stdports[0] );
	muse_current_port( env, MUSE_STDOUT_PORT, env->stdports[1] );
	muse_current_port( env, MUSE_STDERR_PORT, env->stdports[2] );
	muse_current_port( env, MUSE_INPUT_PORT, env->stdports[0] );

	/* Define the "open-file" function. This is the only file specific function needed.
	After this the generic port functions take over. */
	_define( _csymbol(L"open-file"), _mk_nativefn( fn_open_file, NULL ) );
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
MUSEAPI muse_port_t muse_assign_port( muse_env *env, FILE *f, int mode )
{
	fileport_t *port = calloc( 1, sizeof(fileport_t) );
	
	port->base.base.magic_word	= 'muSE';
	port->base.base.type_info	= &g_fileport_type.port.obj;
	port->file					= f;
	port->desc					= fileno(f);
	
	port_init( env, &port->base );
	
	if ( mode & MUSE_PORT_READ )
	{
		port->base.in.fpos = ftell(port->file);
		discard_utf8_header(env, port);
	}

	/* The mode bits is a constrained number. If you set bits that don't exist,
	its a programming error. */
	muse_assert( mode < MUSE_PORT_READ_DETECT_MACROS * 2 );

	port->base.mode |= mode;

	if ( mode & MUSE_PORT_WRITE )
	{
		port->base.out.fpos = ftell(port->file);
		write_utf8_header( env, port );
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
MUSEAPI muse_cell muse_load( muse_env *env, FILE *f )
{
	muse_port_t in = muse_assign_port(env, f, MUSE_PORT_TRUSTED_INPUT );
	int sp = _spos();
	muse_cell result = MUSE_NIL;
	muse_port_t prevIn = muse_current_port( env, MUSE_INPUT_PORT, in );
	
	while ( port_eof(in) == 0 )
	{
		muse_cell expr = MUSE_NIL;

		expr = muse_pread(in);

		if ( expr >= 0 )
		{
			_unwind(sp);
			_spush(expr);
			result = _eval( expr );
			_unwind(sp);
			_spush(result);
		}
		else
			break;
	}
	
	muse_current_port( env, MUSE_INPUT_PORT, prevIn );
	muse_unassign_port(in);
	return result;
}

static void write_utf8_header( muse_env *env, fileport_t *p )
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

static void discard_utf8_header( muse_env *env, fileport_t *p )
{
	if ( ftell(p->file) == 0 )
	{
		/* We're at the head. Check if the file has the UTF8 3-byte indicator.
		If so, strip it out, since we don't need it. We only accept UTF8 anyway. 
		This is Windows specific really, but it helps for unix implementations
		to read windows generated files as well, so I'm keeping it for all 
		platforms. */

		unsigned char c[3];
		int nbytes = read( p->desc, c, 2 );

		if ( nbytes == 2 )
		{
			if ( c[0] == 0xff && c[1] == 0xfe )
			{
				/* 16-bit unicode file! Change its type. */
				p->base.base.type_info = (muse_functional_object_type_t*)&g_uc16_fileport_type;
				nbytes = 0;
			}
			else if ( c[1] == 0x00 )
			{
				/* Guessing that it is a 16-bit unicode stream, unix style. */
				p->base.base.type_info = (muse_functional_object_type_t*)&g_uc16_fileport_type;
			}
			else if ( c[0] == 0xef && c[1] == 0xbb )
			{
				/* Could be utf8. */
				nbytes += read( p->desc, c+2, 1 );
				if ( nbytes == 3 && c[2] == 0xbf )
				{
					/* Yes it is utf8. Discard it. */
					nbytes = 0;
				}
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
