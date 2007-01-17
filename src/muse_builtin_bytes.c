/**
 * @file muse_builtin_bytes.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * Implements an object type to represent an arbitrary sequence of bytes.
 */

#include "muse_builtins.h"
#include "muse_port.h"
#include <stdlib.h>
#include <memory.h>
#include <limits.h>

/** @addtogroup FunctionalObjects */
/*@{*/
/**
 * @defgroup ByteArray Byte array
 *
 * A byte array is finite sized sequence of raw byte data. Raw data can be
 * embedded into a muSE text stream using the construct 
 * @code {bytes 11 "blahblah..."} @endcode. You can also read and write
 * raw byte data (without the bracketing information) using
 * \c read-bytes and \c write-bytes.
 */
/*@{*/

typedef struct 
{
	muse_functional_object_t base;
	muse_int size;
	unsigned char *bytes;
	muse_cell ref; /**< The bytes object to the data of which this one refers. */
} bytes_t;

extern muse_functional_object_type_t g_bytes_type;

static bytes_t *bytes_alloc( bytes_t *b, muse_int size )
{
	b->size = size;
	if ( size > 0 )
		b->bytes = (unsigned char *)malloc( (size_t)size );
	return b;
}

static void bytes_free( bytes_t *b )
{
	free(b->bytes);
}

#define _bytes_data(b) bytes_data(env,b)
static bytes_t *bytes_data( muse_env *env, muse_cell b )
{
	return (bytes_t*)muse_functional_object_data( env, b, 'barr' );
}

#define _mk_bytes(size) mk_bytes(env,size)
muse_cell mk_bytes( muse_env *env, muse_int size )
{
	muse_cell bytes = _mk_functional_object( &g_bytes_type, MUSE_NIL );
	bytes_t *b = _bytes_data(bytes);
	b->ref = bytes;
	b->size = size;
	if ( size > 0 )
		b->bytes = (unsigned char *)malloc((size_t)size);
	return bytes;
}

#define _bytes_ptr(b) bytes_ptr(env,b)
unsigned char *bytes_ptr( muse_env *env, muse_cell b )
{
	return _bytes_data(b)->bytes;
}


void bytes_set_size( muse_env *env, muse_cell b, muse_int size )
{
	_bytes_data(b)->size = size;
}

#define _mk_slice(b,offset,size) mk_slice(env,b,offset,size)
static muse_cell mk_slice( muse_env *env, muse_cell b, muse_int offset, muse_int size )
{
	bytes_t *bdata = _bytes_data(b);
	muse_assert( offset >= 0 && offset <= bdata->size );
	muse_assert( size >= 0 && offset + size <= bdata->size );

	{
		muse_cell slice = _mk_functional_object( &g_bytes_type, MUSE_NIL );
		bytes_t *sliceData = _bytes_data(slice);

		muse_assert( sliceData != NULL );

		sliceData->ref = b;
		sliceData->size = size;
		sliceData->bytes = bdata->bytes + (size_t)offset;

		return slice;
	}
}


/**
 * (bytes size)
 * Creates an uninitialized bytes-array of the given size.
 */
static void bytes_init( muse_env *env, void *ptr, muse_cell args )
{
	bytes_t *b = (bytes_t*)ptr;

	b->size = args ? _intvalue(_evalnext(&args)) : (muse_int)0;

	if ( b->size > 0 )
		b->bytes = (unsigned char *)malloc( (size_t)b->size );
	else
		b->size = 0;
}

static void bytes_mark( muse_env *env, void *ptr )
{
	muse_mark( env, ((bytes_t*)ptr)->ref );
}

static void bytes_destroy( muse_env *env, void *ptr )
{
	bytes_t *b = (bytes_t*)ptr;

	if ( _bytes_data(b->ref) == b && b->bytes != NULL )
	{
		free(b->bytes);
		b->size = 0;
		b->bytes = NULL;
	}
}

/**
 * Writes out an expression of the form
 * @code #nnn[data] @endcode which is
 * read back correctly as a byte array by the reader.
 */
static void bytes_write( muse_env *env, void *ptr, void *port )
{
	bytes_t *b = (bytes_t*)ptr;
	muse_port_t p = (muse_port_t)port;
	
	/* Write the opening sequence. */
	port_putc( '#', p );

	/* Write the byte size. */
	{
		char buffer[32];
		int len = sprintf( buffer, MUSE_FMT_INT, b->size );
		muse_assert( len > 0 );
		port_write( buffer, len, p );
	}

	/* Write out the byte array as a string. Quote
	characters are allowed in the middle of the byte array
	because we've already written out the size. */
	port_putc( '[', p );
	if ( b->size > 0 )
		port_write( b->bytes, (size_t)b->size, p );

	/* Write the closing sequence. */
	port_putc( ']', p );
}

/**
 * (bytes-object byte-offset field-type [value])
 * (bytes-object byte-offset size)
 * (bytes-object byte-offset)
 *
 * The offset and offset + sizeof(field-type) must be within range.
 * field-type is one of - 'byte, 'short, 'int, 'long, 'float, 'double.
 * The type names and sizes match the Java specification.
 * If a value is given the byte object is modified at the location.
 * If size is omitted, it is taken to be up to the end of the object.
 */
static muse_cell fn_bytes_fn( muse_env *env, bytes_t *b, muse_cell args )
{
	muse_int offset = _intvalue( _evalnext(&args) );
	muse_cell field_type = MUSE_NIL;

	if ( args == MUSE_NIL )
	{
		bytes_t *refBytes = _bytes_data(b->ref);
		muse_int size = b->size - offset;
		muse_assert( offset >= 0 );
		if ( size < 0 )
		{
			size = 0;
			offset = b->size;
		}

		return _mk_slice( b->ref, (b->bytes - refBytes->bytes) + offset, size );
	}

	field_type = _evalnext(&args);

	switch ( muse_cell_type(field_type) )
	{
	case MUSE_INT_CELL :
		{
			muse_int size = _intvalue(field_type);
			bytes_t *refBytes = _bytes_data(b->ref);

			return _mk_slice( b->ref, (b->bytes - refBytes->bytes) + offset, size );
		}
		break;
	case MUSE_SYMBOL_CELL :
		{
			muse_cell value = args ? _evalnext(&args) : MUSE_NIL;
			const muse_char *name = muse_symbol_name(env,field_type);

			muse_assert( (wcscmp(name,L"byte") == 0 && offset+1 <= b->size)
						|| (wcscmp(name,L"short") == 0 && offset+2 <= b->size)
						|| (wcscmp(name,L"int") == 0 && offset+4 <= b->size)
						|| (wcscmp(name,L"long") == 0 && offset+8 <= b->size)
						|| (wcscmp(name,L"float") == 0 && offset+4 <= b->size)
						|| (wcscmp(name,L"double") == 0 && offset+8 <= b->size)
					);

			if ( value )
			{
				muse_int i = _intvalue(value);

				switch ( name[0] )
				{
				case 'b':
					muse_assert( i >= CHAR_MIN && i <= CHAR_MAX );
					b->bytes[(size_t)offset] = (char)i;
					return value;

				case 's':
					muse_assert( i >= SHRT_MIN && i <= SHRT_MAX );
					*(short*)(b->bytes + (size_t)offset) = (short)i;
					return value;

				case 'i':
					muse_assert( i >= INT_MIN && i <= INT_MAX );
					*(int*)(b->bytes + (size_t)offset) = (int)i;
					return value;

				case 'l':
					muse_assert( i >= LLONG_MIN && i <= LLONG_MAX );
					*(muse_int*)(b->bytes + (size_t)offset) = i;
					return value;

				case 'f':
					*(float*)(b->bytes + (size_t)offset) = (float)_floatvalue(value);
					return value;

				case 'd':
					*(double*)(b->bytes + (size_t)offset) = (double)_floatvalue(value);
					return value;
				}
			}
			else
			{
				switch ( name[0] )
				{
				case 'b': return _mk_int( (char)b->bytes[(size_t)offset] );
				case 's': return _mk_int( ((short*)(b->bytes + (size_t)offset))[0] );
				case 'i': return _mk_int( ((int*)(b->bytes + (size_t)offset))[0] );
				case 'l': return _mk_int( ((muse_int*)(b->bytes + (size_t)offset))[0] );
				case 'f': return _mk_float( ((float*)(b->bytes + (size_t)offset))[0] );
				case 'd': return _mk_float( ((double*)(b->bytes + (size_t)offset))[0] );
				}
			}
		}
		break;
	}

	muse_assert( !"Field type must be one of 'byte, 'short, 'int, 'long, 'float, 'double or an integer size value!" );
	return MUSE_NIL;
}

static muse_functional_object_type_t g_bytes_type =
{
	'muSE',
	'barr',
	sizeof(bytes_t),
	(muse_nativefn_t)fn_bytes_fn,
	NULL,
	bytes_init,
	bytes_mark,
	bytes_destroy,
	bytes_write
};

/**
 * (bytes? bytes)
 *
 * Evaluates to \p bytes if it is a valid byte data object and
 * to () if it isn't one.
 */
static muse_cell fn_bytes_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell b = _evalnext(&args);
	bytes_t *data = _bytes_data(b);

	return (data == NULL) ? MUSE_NIL : b;
}

/**
 * (bytes-size bytes) -> int
 */
static muse_cell fn_bytes_size( muse_env *env, void *context, muse_cell args )
{
	muse_cell b = _evalnext(&args);
	bytes_t *bdata = _bytes_data(b);

	muse_assert( bdata != NULL );

	return _mk_int(bdata->size);
}

/**
 * Force the entire buffer down the throat of the port.
 */
static size_t port_write_force( muse_env *env, unsigned char *buffer, size_t size, muse_port_t p )
{
	size_t total = 0;

	while ( p->error == 0 && !port_eof(p) && size > 0 )
	{
		size_t nwritten = port_write( buffer, size, p );

		if ( nwritten == 0 )
			return total;

		size -= nwritten;
		total += nwritten;
		buffer += nwritten;
	}

	return total;
}

/**
 * (write-bytes [port] bytes [start-offset] [num-bytes])
 *
 * Write the raw byte sequence to the port.
 */
static muse_cell fn_write_bytes( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg1 = _evalnext(&args);

	muse_port_t p = _port(arg1);

	if ( p )
		arg1 = _evalnext(&args);
	else
		p = _stdport( MUSE_STDOUT_PORT );

	{
		bytes_t *b = _bytes_data(arg1);
		muse_int start_offset = 0, size = b->size;

		muse_assert( b != NULL );

		if ( args )
		{
			start_offset = _intvalue(_evalnext(&args));
			size = b->size - start_offset;
		}

		muse_assert( start_offset >= 0 && start_offset <= b->size );

		if ( args )
		{
			size = _intvalue(_evalnext(&args));

			if ( start_offset + size > b->size )
				size = b->size - start_offset;
		}

		muse_assert( size >= 0 && start_offset + size <= b->size );

		if ( size > 0 )
		{
			size_t nbytes = port_write_force( env, b->bytes + (size_t)start_offset, (size_t)size, p );		
			return _mk_int(nbytes);
		}
		else
			return MUSE_NIL;
	}
}

static size_t port_read_force( muse_env *env, unsigned char *buffer, size_t size, muse_port_t p )
{
	size_t nread = 0;

	while ( !port_eof(p) && size > 0 )
	{
		size_t n = port_read( buffer, size, p );
		size -= n;
		buffer += n;
		nread += n;
	}

	return nread;
}

/**
 * (read-bytes [port] [bytes]) -> [bytes]
 *
 * Reads raw bytes from the given port or stdin. If a bytes
 * object is given, it attempts to fill it. If no bytes object
 * is given, it reads from the port until eof and returns a
 * new bytes object containing the data.
 */
static muse_cell fn_read_bytes( muse_env *env, void *context, muse_cell args )
{
	muse_cell port_arg = MUSE_NIL, bytes_arg = MUSE_NIL;
	muse_port_t p = NULL;
	bytes_t *result = NULL;
	muse_int max_bytes = (muse_int)-1;

	port_arg	= _evalnext(&args);
	bytes_arg	= _evalnext(&args);

	if ( port_arg )
	{
		p = _port(port_arg);
		if ( !p )
		{
			p = _stdport( MUSE_STDIN_PORT );
			bytes_arg = port_arg;
			port_arg = MUSE_NIL;
		}
	}

	if ( bytes_arg )
	{
		result = _bytes_data(bytes_arg);
	}

	if ( result )
		max_bytes = result->size;

	if ( port_eof(p) || max_bytes == 0 )
		return MUSE_NIL;

	{
		int chunkSize = 4096;
		int maxChunks = 1;
		muse_int total_size = 0;
		bytes_t *chunks = NULL;
		bytes_t *chunks_iter = NULL;
		muse_cell result = MUSE_NIL;
		
		chunks = (bytes_t*)calloc( maxChunks, sizeof(bytes_t) );
		chunks_iter = chunks;

		while ( !port_eof(p) && (p->error == 0) && (max_bytes == -1 || max_bytes > 0) )
		{
			if ( chunks_iter - chunks >= maxChunks )
			{
				maxChunks *= 2;
				chunks = (bytes_t*)realloc( chunks, sizeof(bytes_t) * maxChunks );
			}

			bytes_alloc( chunks_iter, (max_bytes == -1) ? chunkSize : max_bytes );

			{
				size_t n = port_read_force( env, chunks_iter->bytes, (size_t)chunks_iter->size, p );
				if ( max_bytes >= 0 )
					max_bytes -= n;
				total_size += n;

				if ( n < chunks_iter->size )
					chunks_iter->size = n;

			}

			++chunks_iter;
		}

		if ( total_size > 0 )
		{
			result = _mk_bytes(total_size);

			{
				bytes_t *c = chunks;
				unsigned char *b = _bytes_ptr(result);
				size_t offset = 0;
				while ( c < chunks_iter )
				{
					memcpy( b + offset, c->bytes, (size_t)c->size );
					offset += (size_t)c->size;
					bytes_free(c++);
				}
			}
		}

		free(chunks);
		return result;
	}
}

/**
 * (bytes size)
 * Creates a new uninitialized byte array of the given size.
 */
static muse_cell fn_bytes( muse_env *env, void *context, muse_cell args )
{
	muse_cell b = _mk_functional_object( &g_bytes_type, args );

	/* We make the reference of a new object point to itself.
	This lets us use the byte object directly to create a slice
	of itself. */
	_bytes_data(b)->ref = b;

	return b;
}

/**
 * (copy-bytes size src src-offset dest dest-offset)
 */
static muse_cell fn_copy_bytes( muse_env *env, void *context, muse_cell args )
{
	muse_int size		= _intvalue( _evalnext(&args) );
	bytes_t *src		= _bytes_data( _evalnext(&args) );
	muse_int srcOffset	= _intvalue( _evalnext(&args) );
	bytes_t *dest		= _bytes_data( _evalnext(&args) );
	muse_int destOffset = _intvalue( _evalnext(&args) );

	if ( size <= 0 )
		return MUSE_NIL;

	muse_assert( src != NULL && dest != NULL );
	muse_assert( srcOffset >= 0 && srcOffset < src->size );
	muse_assert( srcOffset + size <= src->size );
	muse_assert( destOffset >= 0 && destOffset < dest->size );
	muse_assert( destOffset + size <= dest->size );

	memcpy( dest->bytes + destOffset, src->bytes + srcOffset, (size_t)size );
	return MUSE_NIL;
}

typedef struct 
{
	muse_nativefn_t fn;
	const muse_char *name;
} _defn_t;

void muse_define_builtin_type_bytes( muse_env *env )
{
	static const _defn_t k_defns[] =
	{
		{ fn_bytes,			L"bytes"		},
		{ fn_bytes_p,		L"bytes?"		},
		{ fn_bytes_size,	L"bytes-size"	},
		{ fn_write_bytes,	L"write-bytes"	},
		{ fn_read_bytes,	L"read-bytes"	},
		{ fn_copy_bytes,	L"copy-bytes"	},
		{ NULL,				NULL			}
	};

	{
		int sp = _spos();
		const _defn_t *d = k_defns;
		while ( d->name )
		{
			_define( _csymbol(d->name), _mk_nativefn(d->fn, NULL) );
			_unwind(sp);
			++d;
		}
	}
}

/*@}*/
/*@}*/