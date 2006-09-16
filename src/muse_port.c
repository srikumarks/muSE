/**
 * @file muse_port.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_port.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/** @addtogroup Ports */
/*@{*/

/*============ Begin Port Basics =================*/
enum
{
	PORT_BUFFER_SIZE		= 4096,
	PORT_BUFFER_MASK		= PORT_BUFFER_SIZE - 1,
	PORT_BUFFER_UNGET_SIZE	= 16
};

static void port_init_buffer( muse_port_buffer_t *buffer )
{
	buffer->size		= PORT_BUFFER_SIZE;
	buffer->avail		= 0;
	buffer->pos			= 0;
	buffer->fpos		= 0;
	buffer->bytes = (unsigned char *)calloc( buffer->size, 1 );
}

static void port_destroy_buffer( muse_port_buffer_t *buffer )
{
	if ( buffer->bytes )
		free(buffer->bytes);
	
	memset( buffer, 0, sizeof(muse_port_buffer_t) );
}

#define portfn(p,fn) ((muse_port_type_t*)((muse_functional_object_t*)p)->type_info)->fn

/**
 * Intended to be called inside a particular port's init function
 * at port creation time. Sets up the port's buffers.
 */
void port_init( muse_port_base_t *p )
{
	/* Allocate an input buffer if you can read from this port. */
	if ( ((muse_port_type_t*)p->base.type_info)->read )
		port_init_buffer( &p->in );
	
	/* Allocate an output buffer if you can write to this port. */
	if ( ((muse_port_type_t*)p->base.type_info)->write )
		port_init_buffer( &p->out );

	p->pretty_print = _env()->parameters[MUSE_PRETTY_PRINT];
	p->tab_size		= _env()->parameters[MUSE_TAB_SIZE];
}

/**
 * Intended to be called inside a particular ports destroy function
 * at port destruction time. Destroys the port's buffers.
 */
void port_destroy( muse_port_base_t *p )
{
	port_destroy_buffer( &p->in );
	port_destroy_buffer( &p->out );
}

/**
 * Same protocol as the system \c getc function,
 * but works with the given port.
 */
int	port_getc( muse_port_base_t *p )
{
	muse_port_buffer_t *b	= &p->in;
	
	muse_assert( b->bytes );
	
	if ( p->error || p->eof )
		return EOF;
	
	/* First check if we have anything in the buffer. */
	if ( b->avail > 0 )
	{
		int result = b->bytes[b->pos];
		b->pos = (b->pos + 1) & PORT_BUFFER_MASK;
		--(b->avail);
		++(b->fpos);
		return result;
	}
	
	/* Nothing in the buffer fill it in a single system call. */
	{
		b->pos = PORT_BUFFER_UNGET_SIZE;
		b->avail = (int)portfn(p,read)( b->bytes + PORT_BUFFER_UNGET_SIZE, PORT_BUFFER_SIZE - PORT_BUFFER_UNGET_SIZE, p );
		if ( b->avail < 0 )
			b->avail = 0; /* Unix returns 0. Windows may not. */
		
		if ( b->avail == 0 )
		{
			p->eof = EOF;
			return EOF;
		}
		
		return port_getc( p );
	}
}

/**
 * Same protocol as the system \c ungetc function.
 * The only difference from the system version is that
 * infinite ungetting is not supported. Only a maximum
 * of 16 characters may be "ungot".
 */
int port_ungetc( int c, muse_port_base_t *p )
{
	if ( c != EOF )
	{
		muse_port_buffer_t *b = &p->in;
	
		muse_assert( b->bytes );
		muse_assert( b->avail + 1 <= PORT_BUFFER_SIZE );
		
		if ( p->error || b->avail + 1 > PORT_BUFFER_SIZE )
			return EOF; /* Exceeded the unget limit. */
		
		b->pos = ((b->pos - 1) + PORT_BUFFER_SIZE) & PORT_BUFFER_MASK;
		++(b->avail);
		b->bytes[b->pos] = (unsigned char)c;
		--(b->fpos);
		p->eof = 0;
	}

	return c;
}

/**
 * Same protocol as the system \c putc function.
 * Flushes the port whenever a '\n' character is
 * put.
 */
int port_putc( int c, muse_port_base_t *p )
{
	muse_port_buffer_t *b = &p->out;
	
	muse_assert( b->bytes );

	if ( b->avail == PORT_BUFFER_SIZE )
		port_flush( p );
	
	muse_assert( b->avail < PORT_BUFFER_SIZE );
	
	if ( p->error )
		return EOF; /* Need to return EOF as the error code for putc. */
	
	b->bytes[b->pos] = (unsigned char)c;
	b->pos = (b->pos + 1) & PORT_BUFFER_MASK;
	++(b->avail);
	
	if ( b->avail == PORT_BUFFER_SIZE || c == '\n')
		port_flush( p );
	
	if ( p->error )
		return EOF;
	
	return c;
}

/**
 * Returns EOF if the port has reached end of stream.
 * similar to \c feof.
 */
int port_eof( muse_port_base_t *port )
{
	return port->eof;
}

/**
 * Flushes the port and call's the port's close function
 * to release system resources.
 */
void port_close( muse_port_base_t *p )
{
	port_flush(p);
	portfn(p,close)(p);

	/* Once a port is closed. We don't need
	the input and output buffers any more. 
	So call destroy right away. */
	port_destroy( (muse_port_base_t*)p );

	p->error = 0;
	p->eof = EOF;
}

/**
 * Wraps the port specific read function. Any data pending
 * in the input buffer is copied first before requesting the
 * port for more data.
 */
size_t port_read( void *buffer, size_t nbytes, muse_port_base_t *port )
{
	muse_port_buffer_t *in	= &port->in;
	unsigned char *b		= (unsigned char *)buffer;
	size_t bytes_to_copy	= nbytes;
	size_t bytes_read		= 0;
	
	muse_assert( buffer && port && (nbytes > 0) );
	
	if ( port->error || port->eof )
		return 0;
	
	/* First copy whatever is in our buffer to the given buffer. */
	if ( in->avail > 0 )
	{
		int ready_bytes = (int)((int)bytes_to_copy > in->avail ? in->avail : bytes_to_copy);
		int bytes_read_sofar = 0;
		
		while ( bytes_read_sofar < ready_bytes )
		{
			int this_ready_bytes = in->pos + ready_bytes - bytes_read_sofar > PORT_BUFFER_SIZE 
									? PORT_BUFFER_SIZE - in->pos 
									: ready_bytes - bytes_read_sofar;
			
			memcpy( b, in->bytes + in->pos, ready_bytes - bytes_read_sofar );
			
			in->pos				= (in->pos + this_ready_bytes) & PORT_BUFFER_MASK;
			bytes_read_sofar	+= this_ready_bytes;
			b					+= this_ready_bytes;
		}
		
		in->fpos		+= ready_bytes;
		bytes_read		+= ready_bytes;
		in->avail		-= ready_bytes;
		bytes_to_copy	-= ready_bytes;
		
	}
	
	muse_assert( in->avail == 0 );
	in->pos = 0;
	
	/* Read remaining data directly into the output buffer. */
	if ( bytes_to_copy > 0 )
	{
		size_t remaining_bytes_read = portfn(port,read)( b, bytes_to_copy, port );
		
		bytes_read	+= remaining_bytes_read;
		in->fpos	+= remaining_bytes_read;
	}

	return bytes_read;
}

/**
 * Wraps the port specific write function. Writes any data in
 * the output buffer before calling the port-specific write function.
 * If the buffer can hold all the given data as well, then the
 * port's write function is not called and the given bytes are
 * simply copied to the port buffer.
 */
size_t port_write( void *buffer, size_t nbytes, muse_port_base_t *port )
{
	muse_port_buffer_t *out	= &port->out;
	
	muse_assert( buffer && port && (nbytes > 0) );
	
	if ( port->error || port->eof )
		return 0;
	
	/* First write out whatever we've accumulated in the buffer. */
	if ( out->avail + nbytes > PORT_BUFFER_SIZE )
	{
		port_flush( port );
		
		muse_assert( out->avail == 0 );
		
		/* Write out the remaining data directly from the buffer. */
		{
			size_t bytes_written = portfn(port,write)(buffer, nbytes, port );
			out->fpos += bytes_written;
			return bytes_written;
		}		
	}
	else
	{
		/* Simply copy the data to the buffer. We'll write
		at one go at flush time. */
		memcpy( out->bytes + out->pos, buffer, nbytes );
		out->pos	= (int)((out->pos + nbytes) & PORT_BUFFER_MASK);
		out->avail	+= (int)nbytes;
		return nbytes;
	}
}

/**
 * Writes any data pending in the port's output buffer
 * and then calls the port-specific flush function.
 */
int port_flush( muse_port_base_t *port )
{
	muse_port_buffer_t *out = &port->out;
	
	if ( port->error )
		return EOF;
	
	/* Write out all bytes in the buffer to the port. */
	if ( !port->error && out->avail > 0 )
	{
		size_t bytes_written = portfn(port,write)( out->bytes, out->avail, port );
		
		out->avail	= 0;
		out->pos	= 0;
		out->fpos	+= bytes_written;
	}
	
	return portfn(port,flush)( port );
}

#undef portfn
/*============ End Port Basics =================*/

enum 
{ 
	PARSE_EOF						= -1000,
	PARSE_ERROR						= -1001,
	PARSE_ERROR_BUFFER_OVERFLOW		= -1002,
	PARSE_ERROR_EXPECTED_STRING		= -1003,
	PARSE_ERROR_EXPECTED_LIST		= -1004,
	PARSE_ERROR_BAD_CONS_SYNTAX		= -1005,
	PARSE_ERROR_EXPECTED_SYMBOL		= -1006,
	
	PARSE_EMPTY_GROUP				= -10000,
	PARSE_END_OF_GROUP				= -10001,
	PARSE_END_OF_LIST				= -10002,
	PARSE_LIST_ITEM_SEPARATOR		= -10003
};

/** @name Pretty print support. */
/*@{*/
enum { MAX_INDENT_COLS = 128 };

static int g_align_cols[MAX_INDENT_COLS];
static int g_align_level = 0;
static muse_boolean g_pretty_printer_enabled = MUSE_TRUE;
static int g_tab_size = 4;

static void pretty_printer_reset( muse_port_t p )
{
	g_pretty_printer_enabled		= p->pretty_print ? MUSE_TRUE : MUSE_FALSE;
	g_align_level					= 0;
	g_align_cols[g_align_level]		= 0;
	g_tab_size						= p->tab_size;
}

static void pretty_printer_indent()
{
	if ( g_pretty_printer_enabled )
	{
		muse_assert( g_align_level < MAX_INDENT_COLS-1 );
		g_align_cols[g_align_level+1] = g_align_cols[g_align_level];
		++g_align_level;
	}
}

static void pretty_printer_unindent()
{
	if ( g_pretty_printer_enabled )
	{
		muse_assert( g_align_level > 0 );
		--g_align_level;
	}
}

static void pretty_printer_line_break( muse_port_t f )
{
	if ( g_pretty_printer_enabled )
	{
		port_putc('\n',f);
		
		{
			int i = 0, N = g_align_cols[g_align_level] - g_tab_size;

			for ( ; i < N; i += g_tab_size ) 
				port_putc( '\t', f );

			N += g_tab_size;

			for ( ; i < N; ++i )
				port_putc( ' ', f );
		}
	}
}

static void pretty_printer_move( int numc )
{
	if ( g_pretty_printer_enabled )
		g_align_cols[g_align_level] += numc;
}
/*@}*/

/**
 * @name ezscheme structures
 * @{ 
 */
typedef struct
{
	muse_cell expr;
	int col_start, col_end;
} ez_result_t;

int ez_update_col( int ch, int col )
{
	static const int k_tab_size = 4;

	switch ( ch )
	{
	case '\t'	: return col + k_tab_size;
	case '\n'	:
	case '\r'	: return 0;
	default		: return col + 1;
	}
}

static ez_result_t ez_result( muse_cell expr, int col_start, int col_end )
{
	ez_result_t r = { expr, col_start, col_end };
	return r;
}

ez_result_t ez_parse( muse_port_t p, int col, muse_boolean is_head );
muse_cell ez_parse_expr( muse_port_t p );
/*@}*/

static int peekc( muse_port_t f )
{
	if ( port_eof(f) )
		return EOF;
	else
	{
		int c = port_getc(f);
		port_ungetc(c,f);
		return c;
	}
}

typedef struct
{
	int lines;
	int col;
} white_space_t;

/**
 * Skips all white space and comments following
 * the current stream position in the given stream
 * Returns the column position at which the next
 * non-white space character starts.
 */
white_space_t ez_skip_whitespace( muse_port_t f, int line, int col )
{
	int c = 0;

	c = port_getc(f);
	while ( c != EOF && isspace(c) )
	{
		col = ez_update_col( c, col );
		
		if ( c == '\n' )
			++line;

		c = port_getc(f);
	}

	if ( (f->mode & MUSE_PORT_EZSCHEME) ? c == '#' : c == ';' )
	{
		/* Skip comment to end of line. */
		while ( c != EOF && c != '\n' && c != '\r' )
			c = port_getc(f);
		
		if ( c != EOF )
		{
			return ez_skip_whitespace( f, line + 1, 0 );
		}
	}
	else
		port_ungetc(c,f);

	{
		white_space_t w = {line, col};
		return w;
	}
}

static inline int _token_begin_list( int c )
{
	return c == '(' || c == '{';
}

static inline int _token_end_list( int c )
{
	return c == ')' || c == '}';
}

/**
 * Reads a decimal number starting from the
 * current stream position.
 */
static ez_result_t _read_number( muse_port_t f, int col )
{
	char buffer[128];
	int count = 0, digit_count = 0, denom_pos = -1;
	muse_boolean is_fractional = MUSE_FALSE;
	char c = port_getc(f);

	if ( c == EOF )
		return ez_result( MUSE_NIL, col, col );

	/* Optional leading minus sign. */
	if ( c == '-' || c == '+' )
	{
		buffer[count++] = c;
		c = port_getc(f);
	}

	/* ... followed by decimal digits. */
	while ( count < 35 && c >= '0' && c <= '9' )
	{
		buffer[count++] = c;
		++digit_count;
		c = port_getc(f);
	}

	if ( count < 36 && c == '/' )
	{
		/* If followed by a '/' and then a positive decimal number,
		we're dealing with a literal fraction. Convert it to a float. */
		denom_pos = count;
		buffer[count++] = c;
		
		c = port_getc(f);

		while ( count < 75 && c >= '0' && c <= '9' )
		{
			buffer[count++] = c;
			++digit_count;
			c = port_getc(f);
		}
	}
	else
	{
		/* Optional decimal point followed by decimal digits. */
		if ( count < 36 && c == '.' )
		{
			is_fractional = MUSE_TRUE;
			buffer[count++] = c;
			c = port_getc(f);

			while ( count < 75 && c >= '0' && c <= '9' )
			{
				buffer[count++] = c;
				++digit_count;
				c = port_getc(f);
			}
		}

		/* Optional exponent specification. */
		if ( count < 76 && (c == 'e' || c == 'E') )
		{
			is_fractional = MUSE_TRUE;
			buffer[count++] = c;
			c = port_getc(f);

			/* Optional exponent sign. */
			if ( count < 77 && (c == '-' || c == '+') )
			{
				buffer[count++] = c;
				c = port_getc(f);
			}

			/* ... followed by decimal digits */
			while ( count < 100 && c >= '0' && c <= '9' )
			{
				buffer[count++] = c;
				++digit_count;
				c = port_getc(f);
			}
		}
	}

	if ( c != EOF )
		port_ungetc(c,f);

	muse_assert( count <= 127 );

	if ( digit_count > 0 )
	{
		buffer[count] = '\0';

		if ( denom_pos >= 0 )
		{
			/* We've got a fraction like 4/3. */
			muse_int num = 1, denom = 1;

			if ( denom_pos == 0 )
				sscanf( buffer, "/" MUSE_FMT_INT, &denom );
			else if ( denom_pos == count - 1 )
				sscanf( buffer, MUSE_FMT_INT "/", &num );
			else
				sscanf( buffer, MUSE_FMT_INT "/" MUSE_FMT_INT, &num, &denom );

			muse_assert( denom > 0 );

			if ( denom == 0 )
			{
				fprintf( stderr, "Syntax error: Zero denominator used in literal fraction '%s'! Not a number!\n", buffer );
			}
			else
				return ez_result( muse_mk_float( (muse_float)num / (muse_float)denom ), col, col + count );
		}
		else
		{
			if ( is_fractional )
			{
				muse_float f = atof(buffer);
				return ez_result( muse_mk_float(f), col, col + count );
			}
			else
			{
				muse_int i;
				sscanf( buffer, MUSE_FMT_INT, &i );
				return ez_result( muse_mk_int(i), col, col + count );
			}
		}
	}

	{
		/* Not a number. Unget all the characters and return MUSE_NIL. */
		while ( count > 0 )
			port_ungetc( buffer[--count], f );

		return ez_result( MUSE_NIL, col, col );
	}
}

static int hex( int c )
{
	if ( c >= '0' && c <= '9' )
		return c - '0';
	if ( c >= 'a' && c <= 'f' )
		return 10 + (c - 'a');
	if ( c >= 'A' && c <= 'F' )
		return 10 + (c - 'A');
	return -1;
}

/**
 * Reads a hexadecimal number starting from the 
 * current stream position. This does not include
 * the "0x" prefix.
 */
static ez_result_t _read_hex( muse_port_t f, int col )
{
	int c = port_getc(f);

	/* First character sequence must be "0x" */
	if ( c != '0' )
	{
		port_ungetc( c, f );
		return ez_result( MUSE_NIL, col, col );
	}

	c = port_getc(f);

	if ( c != 'x' )
	{
		port_ungetc( c, f );
		port_ungetc( '0', f );
		return ez_result( MUSE_NIL, col, col );
	}

	/* Lead sequence "0x" is satisfied. Read the hexadecimal
	number until no hex characters can be found. */
	{
		muse_int n = 0;
		int len = 0, h = 0;

		while ( len < 16 )
		{
			c = port_getc(f);
			h = hex(c);

			if ( c < 0 )
				break;

			if ( h < 0 )
			{
				port_ungetc( c, f );
				break;
			}

			n = (n << 4) | h;
			++len;
		}

		muse_assert( len > 0 || !"No valid hex characters after '0x'!\n" );
		return ez_result( muse_mk_int(n), col, col + len + 2 );
	}
}

/**
 * Reads and returns the next string delimited
 * by double quote characters.
 */
static ez_result_t _read_string( muse_port_t f, int col )
{
	int col_end = col;
	char c = port_getc(f);
	if ( c != '"' )
	{
		port_ungetc(c,f);
		return ez_result( MUSE_NIL, col, col );
	}
	else if ( port_eof(f) )
	{
		muse_assert( !PARSE_ERROR_EXPECTED_STRING );
		return ez_result( MUSE_NIL, col, col );
	}
	else
	{
		int maxlen = 32;
		int endpos = 0;
		char *s = (char*)malloc( maxlen+1 );
		s[endpos] = '\0';
		
		c = port_getc(f);
		
		while ( c != EOF )
		{
			if ( c == '"' )
			{
				/* Two consecutive " characters ("") is the escape sequence
				for a double quote character within a string. So if you want
				the following text -
						hello "world"
				you have to type the expression
						"hello ""world"""
				All other characters are taken literally.
				*/
				if ( peekc(f) == '"' )
				{
					c = port_getc(f);
					++col_end;

					MUSE_DIAGNOSTICS({
						muse_char *temp = alloca( sizeof(muse_char) * (endpos + 1) );
						s[endpos] = c;
						s[endpos+1] = '\0';
						swprintf( temp, endpos + 1, L"%S", s );

						muse_message( L"Parser", L"There are two consecutive double quote characters in\n"
												 L"[%s]. Did you really mean to use a double-quote character\n"
												 L"inside a literal string?",
												 temp );
					});
				}
				else
				{
					/* End of string. */
					break;
				}
			}

			col_end = ez_update_col( c, col_end );
			
			s[endpos++] = (char)c;
			if ( endpos > maxlen )
			{
				/* Realloc the string buffer */
				maxlen *= 2;
				s = (char*)realloc( s, maxlen+1 );
				s[endpos] = '\0';
			}
			
			c = port_getc(f);
		}
		
		s[endpos] = '\0';
		
		{
			muse_cell result = muse_mk_text_utf8( s, s + endpos );
			free(s);
			return ez_result( result, col, col_end );
		}
	}
}

static muse_boolean is_symbol_char( int c )
{
	if ( c != EOF && !_token_begin_list(c) && !_token_end_list(c)
		&& c != '\'' && c != '"' && c != ',' && c != ';'
		&& c != '[' && c != ']' && !isspace(c) )
		return MUSE_TRUE;
	else
		return MUSE_FALSE;
}

/**
 * Reads a symbol from the current stream position.
 * Only named symbols are possible to be read by this.
 * Anonymous symbols are never created by read.
 */
static ez_result_t _read_symbol( muse_port_t f, int col )
{
	int maxlen = 32;
	int pos = 0;
	int c = '\0';
	char *s = (char*)calloc( maxlen+1, 1 );
	
	col = ez_skip_whitespace(f,0,col).col;
	
	c = port_getc(f);
	
	while ( is_symbol_char(c) )
	{
		if ( pos == maxlen )
		{
			maxlen = 2 * maxlen;
			s = (char*)realloc( s, maxlen+1 );
			s[pos] = '\0';
		}
		
		s[pos++] = (char)c;
		c = port_getc(f);
	}
	
	port_ungetc(c,f); /* The last character we read wasn't processed. */
	
	if ( pos > 0 )
	{
		muse_cell sym = muse_symbol_utf8( s, s + pos );
		free(s);
		return ez_result( sym, col, col + pos );
	}
	else
	{
		/* Gobble the problematic character. */
		c = port_getc(f);

		MUSE_DIAGNOSTICS({
			muse_char temp[2];

			temp[0] = c;
			temp[1] = '\0';

			muse_message( L"Parser", L"Expecting a symbol, number or a string, but I get '%s' instead.\n"
									 L"Mismatched parentheses somewhere? Maybe you should use the\n"
									 L"\"Check Syntax\" feature in DrScheme.", temp );
		});

		return ez_result( PARSE_ERROR_EXPECTED_SYMBOL, col, col + pos );
	}
}

/**
 * An atom is a number, symbol or a string.
 */
static ez_result_t _read_atom( muse_port_t f, int col )
{
	char c;
	ez_result_t result = { MUSE_NIL, col, col };

	col = ez_skip_whitespace(f,0,col).col;
	c = peekc(f);

	if ( c == EOF )
		return ez_result( PARSE_EOF, col, col );

	/* Our checking order is
		- string	-> ""
		- hex		-> 0x
		- number	-> 1, 2.0, .2, 0.2, 2. 2.e5 -2, -2.0, -2e5, etc.
		- symbol	-> hello, hello->world, etc.

		All the read functions below return MUSE_NIL
		if the input at this point doesn't parse accordingly,
		a symbol being the most forgiving, pretty much
		accepting anything other than the others.
	*/

	result = _read_string(f,col);
	if ( result.expr > 0 )
		return result;

	result = _read_hex(f,col);
	if ( result.expr > 0 )
		return result;

	result = _read_number(f,col);
	if ( result.expr > 0 )
		return result;

	/* If its none of the above, treat it as a symbol. */
	return _read_symbol(f,col);
}

/**
 * Reads a list delimited by parentheses. The list
 * items are all separated by white space. The last
 * pair of the list can be specified using the special
 * dot notation - ex: @code (a b c . d) @endcode
 */
static muse_cell _read_list( muse_port_t f )
{
	char c = port_getc(f);
	
	if ( !_token_begin_list(c) )
	{
		port_ungetc(c,f);
		muse_assert( !PARSE_ERROR_EXPECTED_LIST );
		return PARSE_ERROR_EXPECTED_LIST;
	}
	else
	{
		muse_cell h, t, next_element;
		int sp;
		
		ez_skip_whitespace(f,0,0);
		c = port_getc(f);
		if ( c == EOF )
		{
			MUSE_DIAGNOSTICS({
				muse_message( L"Parser", L"File ended soon after a '('. Maybe you should use\n"
										 L"the \"Check syntax\" feature of DrScheme." );
			});
			return PARSE_ERROR_EXPECTED_LIST;
		}
		else if ( _token_end_list(c) )
		{
			/* Nil expression of the form () */
			return MUSE_NIL;
		}
		else
			port_ungetc(c,f);
		
		h = t = muse_cons(MUSE_NIL,MUSE_NIL);
		
		sp = _spos();
		next_element = muse_pread( f );
		if ( next_element < 0 )
		{
			MUSE_DIAGNOSTICS({
				muse_message( L"Parser", L"Error while trying to read elements of a list.\n"
										 L"The last term is\n\t%m",
										 muse_head(t) );
			});
			return next_element; /* Parse error. */
		}
		
		_seth( h, next_element );
		_unwind(sp);
		
		/* Read more list elements and add to the list tail. */
		while ( !port_eof(f) )
		{
			ez_skip_whitespace(f,0,0);
			c = port_getc(f);
			
			if ( _token_end_list(c) )
				break; /* End of list. */
			else if ( c == '.' )
			{
				/* Check if this is a cons pair. */
				char c2 = peekc(f);
				if ( c2 == EOF )
				{
					MUSE_DIAGNOSTICS({
						muse_message( L"Parser", L"Didn't expect the file to end after\n\t%m\nwhile parsing a list.", _head(t) );
					});
					return PARSE_ERROR;
				}
				
				if ( isspace(c2) )
				{
					/* Yes its a cons pair. Set the last cons cell's tail and finish up. */
					next_element = muse_pread( f );
					if ( next_element < 0 )
						return next_element; /* Parse error. */
					
					_sett( t, next_element );
					_unwind(sp);
					
					ez_skip_whitespace(f,0,0);
					c = port_getc(f);
					if ( !_token_end_list(c) )
					{
						MUSE_DIAGNOSTICS({
							muse_message( L"Parser", L"The list should end after\n\t. %m", _tail(t) );
						});
						return PARSE_ERROR_BAD_CONS_SYNTAX;
						/* Next token after second item of cons pair must be ')' */
					}
					
					break;
				}
				else
				{
					/* Its not a cons pair. The last thing that starts with a period
					might be a symbol or a number. */
					port_ungetc( '.', f );
					next_element = _read_atom(f,0).expr;
				}
			}
			else
			{
				port_ungetc(c,f);
				
				/* Add one more element to the list. */
				next_element = muse_pread( f );
				if ( next_element < 0 )
					return next_element; /* Parse error. */
			}

			/* Add to the end of the list. */
			{
				muse_cell c = muse_cons( next_element, MUSE_NIL );
				_sett( t, c );
				t = c;
				_unwind(sp);
			}
		}

		/* Return the list. */
		return h;
	}
}

/**
 * Returns \c MUSE_TRUE if the given sexpr is a macro
 * expression. A macro expression is a list expression
 * which has a symbol at its head that is defined to
 * be a macro.
 */
static muse_boolean is_macro_sexpr( muse_cell sexpr )
{
	/* sexpr must not be (). */
	if ( !sexpr )
		return MUSE_FALSE;

	/* sexpr must be a list. Minimum condition for that is for it to be a cons cell. */
	if ( _cellt(sexpr) != MUSE_CONS_CELL )
		return MUSE_FALSE;

	{
		muse_cell sym = _head(sexpr);

		/* The head of the list must be a symbol. */
		if ( _cellt(sym) != MUSE_SYMBOL_CELL )
			return MUSE_FALSE;

		{
			muse_cell val = _symval(sym);
			
			/* The symbol's value must be a lambda function. */
			if ( _cellt(val) != MUSE_LAMBDA_CELL )
				return MUSE_FALSE;

			/* The lambda function must be a macro. The head of the
			lambda cons cell gives the formal arguments list, which
			will be "quick-quoted" in the case of macros and hence will
			be -ve. For normal functions, it'll be >= 0. */
			if ( _head(val) < 0 )
				return MUSE_TRUE;
		}
	}

	return MUSE_FALSE;
}

/**
 * Reads the next symbolic expression at the current
 * stream position, ignoring white space and comment
 * lines.
 * 
 * @param f The port from which the expression is to be read.
 */
muse_cell muse_pread( muse_port_t f )
{
	char c;

	if ( f->mode & MUSE_PORT_EZSCHEME )
		return ez_parse_expr(f);

	ez_skip_whitespace(f,0,0);
	
	c = port_getc(f);
	
	if ( c == '\'' )
	{
		/* Quoted expression. We have to disable auto-detection of macro expressions
		within the quoted sub-expression. Otherwise it'll not be possible to give a 
		quoted expression that looks like a macro expression but is supposed to be
		literally just that. If you actually want the macro expression to be evaluated
		within the quoted expression, use curly braces {} around it rather than 
		parens (). */
		muse_cell expr = MUSE_NIL;
		int saved_mode = f->mode;
		f->mode &= ~MUSE_PORT_READ_DETECT_MACROS;
		expr = muse_pread( f );
		f->mode = saved_mode;
		return muse_quote(expr);
	}
	else if ( c == '(' || c == '{' )
	{
		/* List expression. */
		port_ungetc( c, f );
		
		{
			muse_cell sexpr = _read_list(f);

			if ( sexpr < 0 )
			{
				MUSE_DIAGNOSTICS({
					muse_message( L"Parser", L"Expected a list. Got error %d.", (muse_int)sexpr );
				});
				return sexpr; /**< Some error happened. */
			}

			/* 
			We need to evaluate a list expression at read time if one
			of the following conditions is satisfied -
				1.	The expression is delimited by {} and we've been
					asked to expand braces.
				2.	The expression is delimited by (), but we've been
					asked to detect and expand macro expressions (braces
					or parens) and the head of the list is a macro symbol. 
			*/
			if ( (f->mode & MUSE_PORT_READ_EXPAND_BRACES)
					&& (c == '{' || ((f->mode & MUSE_PORT_READ_DETECT_MACROS)
										&& c == '(' 
										&& is_macro_sexpr(sexpr))) )
			{
				return muse_eval(sexpr);
			}
			else
				return sexpr;
		}
	}
	else 
	{
		port_ungetc(c,f);

		{
			muse_cell expr = _read_atom(f,0).expr;

			MUSE_DIAGNOSTICS({
				if ( expr < 0 && expr != PARSE_EOF )
				{
						muse_message( L"Parser", L"Expected a symbol, number or a string.\n"
												 L"Mismatched parentheses somewhere? Maybe you should\n"
												 L"use the \"Check syntax\" feature of DrScheme.");
				}
			});

			return expr;
		}
	}
}

static void muse_print_q( muse_port_t f, muse_cell sexpr, muse_boolean quote );

static void muse_print_int( muse_port_t f, muse_cell i )
{
	char buffer[64];
	int count = sprintf( buffer, MUSE_FMT_INT, _ptr(i)->i );
	pretty_printer_move(count);
	port_write( buffer, count, f );
}

static void muse_print_float( muse_port_t s, muse_cell f )
{
	char buffer[128];
	int count = sprintf( buffer, MUSE_FMT_FLOAT, _ptr(f)->f );

	if ( !(strchr( buffer, '.' ) || strchr( buffer, 'e' )) )
	{
		/* The printed number is an integer. Make sure it will
		be read back as a float by appending ".0" */
		buffer[count++] = '.';
		buffer[count++] = '0';
	}

	pretty_printer_move(count);
	port_write( buffer, count, s );
}

static size_t muse_print_text( muse_port_t f, muse_cell t, muse_boolean quote )
{
	const char dquote		= '"';
	muse_text_cell *tc		= &_ptr(t)->text;
	size_t size				= muse_utf8_size(tc->start, (int)(tc->end - tc->start));
	size_t count			= 0;
	size_t total			= 0;
	char *utf8				= alloca( size );

	muse_char *c			= tc->start;
	muse_char *q			= wcschr( c, dquote );

	if ( quote )
	{
		port_putc( dquote, f );

		while ( c != tc->end )
		{
			if ( q )
			{
				/* Convert upto the double quote character. */
				(*q) = 0;
				count = muse_unicode_to_utf8( utf8, (int)size, c, (int)(q-c) );
				(*q) = dquote;
				port_write( utf8, count, f );
				total += count;

				/* Write out an escaped double quote character. */
				port_write( "\"\"", 2, f );
				total += 2;

				/* Move to the next portion of the string. */
				c = q + 1;
				q = wcschr( c, dquote );
			}
			else
			{
				/* The last stretch of the string without any double quote characters. */
				count = muse_unicode_to_utf8( utf8, (int)size, c, (int)(tc->end - c) );
				port_write( utf8, count, f );
				total += count;
				break;
			}
		}

		port_putc( dquote, f );

		return total + 2;
	}
	else
	{
		count = muse_unicode_to_utf8( utf8, (int)size, c, (int)(tc->end - c) );
		port_write( utf8, count, f );
		total = count;
		return total;
	}
}

static void muse_print_list ( muse_port_t f, muse_cell l, muse_boolean quote )
{
	if ( l )
	{
		muse_boolean need_line_break = MUSE_FALSE;

		if ( _isquote( _head(l) ) )
		{
			port_putc( '\'', f );
			pretty_printer_move(1);
			muse_print_q( f, _tail(l), quote );
			return;
		}
		
		pretty_printer_indent();
		port_putc( '(', f );
		pretty_printer_move(1);
		
		while ( l )
		{
			muse_cell h = _next(&l);

			if ( need_line_break ) pretty_printer_indent();
			muse_print_q( f, h, quote );
			if ( need_line_break ) pretty_printer_unindent();
			
			if ( l && _cellt(l) != MUSE_CONS_CELL )
			{
				port_write( " . ", 3, f );
				pretty_printer_move(3);
				muse_print_q( f, l, quote );
				break;
			}
			
			if ( l ) 
			{
				if ( need_line_break || (_cellt(h) == MUSE_CONS_CELL && !_isquote(_head(h))) || _cellt(h) == MUSE_TEXT_CELL )
				{
					pretty_printer_line_break(f);
					need_line_break = MUSE_TRUE;
				}
				else
				{
					port_putc( ' ', f );
					pretty_printer_move(1);
				}
			}
		}
		
		port_putc( ')', f );
		pretty_printer_move(1);
		pretty_printer_unindent();
	}
	else
	{
		pretty_printer_indent();
		port_putc( '(', f );
		port_putc( ')', f );
		pretty_printer_move(2);
		pretty_printer_unindent();
	}
}

static void muse_print_sym( muse_port_t f, muse_cell s )
{
	muse_cell name = _symname(s);
	if ( name )
		pretty_printer_move( (int)muse_print_text( f, name, MUSE_FALSE ) );
	else
	{
		char buffer[64];
		int count = sprintf( buffer, "<sym:%x>", s );
		port_write( buffer, count, f );
		pretty_printer_move(count);
	}
}

static void muse_print_lambda( muse_port_t f, muse_cell l, muse_boolean quote )
{
	char buffer[64];
	int count = sprintf( buffer, "(fn ", l );
	pretty_printer_indent();
	port_write( buffer, count, f );
	pretty_printer_move(count);
	while ( l )
	{
		pretty_printer_indent();
		muse_print_q( f, _next(&l), quote );
		pretty_printer_unindent();
		if ( l ) pretty_printer_line_break(f);
	}
	port_putc( ')', f );
	pretty_printer_unindent();
}

static void muse_print_nativefn( muse_port_t f, muse_cell l )
{
	muse_functional_object_t *obj = _fnobjdata(l);
	
	if ( obj && obj->type_info->write )
	{
		obj->type_info->write( obj, f );
	}
	else
	{
		char buffer[64];
		int count = sprintf( buffer, "<prim:%x>", l );
		port_write( buffer, count, f );
		pretty_printer_move(count);
	}
}

/**
 * Prints the given s-expr to the given stream.
 * The quote parameter detemines whether strings
 * should be printed with double-quote delimiters
 * or just as strings.
 */
static void muse_print_q( muse_port_t f, muse_cell sexpr, muse_boolean quote )
{
	/* If the expression has been quick-quoted, unquote
	it to print the contents.*/
	if ( sexpr < 0 )
	{
		sexpr = -sexpr;
		port_putc( '\'', f );
		pretty_printer_move(1);
	}
	
	/*pretty_printer_indent();*/
	switch ( _cellt(sexpr) )
	{
		case MUSE_INT_CELL		:	muse_print_int( f, sexpr ); break;
		case MUSE_FLOAT_CELL	:	muse_print_float( f, sexpr ); break;
		case MUSE_TEXT_CELL		:	muse_print_text( f, sexpr, quote ); break;
		case MUSE_CONS_CELL		:	muse_print_list( f, sexpr, quote ); break;
		case MUSE_SYMBOL_CELL	:	muse_print_sym( f, sexpr ); break;
		case MUSE_LAMBDA_CELL	:	muse_print_lambda( f, sexpr, quote ); break;
		case MUSE_NATIVEFN_CELL	:	muse_print_nativefn( f, sexpr ); break;			
	}
	/*pretty_printer_unindent();*/
}

void muse_pprint( muse_port_t port, muse_cell sexpr )
{
	/* Temporary implementation. */
	pretty_printer_reset(port);
	muse_print_q( port, sexpr, MUSE_FALSE );
}

/**
 * Writes the given s-expression to the given
 * stream. If the expression contains only basic
 * objects such as numbers, lists and symbols,
 * then output of write is suitable to read 
 * back by \c muse_pread(). Otherwise, the only
 * difference between write and print is that
 * write encloses strings in quotes and print
 * doesn't.
 */
void muse_pwrite( muse_port_t f, muse_cell sexpr )
{
	/* Temporary implementation. */
	pretty_printer_reset(f);
	muse_print_q( f, sexpr, MUSE_TRUE );
}

/**
 * Takes a muSE port represented as a cell and
 * returns the internal port object that it uses.
 * If the given thing is not a port, then it
 * returns NULL.
 */
muse_port_t muse_port( muse_cell p )
{
	return (muse_port_t)muse_functional_object_data(p,'port');
}

/*@}*/

/************************************************/
/* EZScheme syntax                              */
/************************************************/
/*

symbol = value
symbol = [	1 : "hello"
			2 : "world"
			3 : "there" ]

add := fn [x y] (x + y)

rest (assoc symbol 1)
add 2 3

let [	a		:: 53
		b		:: 45
		(c : d) :: list 
	]
   print "hello"
		(2 + 3)
		"end of hello"
	


*/

ez_result_t ez_parse( muse_port_t p, int col, muse_boolean is_head );
muse_cell ez_parse_expr( muse_port_t p );

static ez_result_t ez_expr( muse_cell expr, int col_start, int col_end )
{
	if ( is_macro_sexpr(expr) )
		expr = muse_eval(expr);

	{
		ez_result_t r = { expr, col_start, col_end };
		return r;
	}
}

muse_cell ez_parse_expr( muse_port_t p )
{
	return ez_parse( p, 0, MUSE_TRUE ).expr;
}

/**
 * A ezscheme atom is a normal scheme atom - symbol, number, string.
 */
ez_result_t ez_parse_atom( muse_port_t p, int col )
{
	return _read_atom( p, col );
}

/**
 * An ezscheme group is an expression enclosed in parentheses.
 * ezscheme uses parentheses to "group" as opposed to defining
 * function heads. This means that enclosing an expression in
 * multiple set of parentheses has no effect compared to 
 * just usig a single set - i.e. (hello) is exactly the same
 * as (((hello))).
 */
ez_result_t ez_parse_group( muse_port_t p, int col )
{
	int c = port_getc(p);
	
	muse_assert( c == '(' );
	
	{
		ez_result_t r = ez_parse( p, col + 1, MUSE_TRUE );
		if ( r.expr == PARSE_END_OF_GROUP )
		{
			port_getc(p);
			r.expr = PARSE_EMPTY_GROUP;
			r.col_start = col;
			return r;
		}
		
		if ( r.expr < 0 )
		{
			if ( !port_eof(p) )
				fprintf( stderr, "Syntax error %d: Expected end of group character ')' or term.\n", r.expr );
			return r;
		}
		
		{
			ez_result_t eog = ez_parse( p, r.col_end, MUSE_FALSE );
			
			if ( eog.expr != PARSE_END_OF_GROUP )
			{
				if ( !port_eof(p) )
					fprintf( stderr, "Syntax error %d: Expected end of group character ')'.\n", eog.expr );
			}
			else
				port_getc(p); /* Get the pending ')' character. */
				
			r.col_end = eog.col_end;
			return r;
		}
	}
}
		
/**
 * An ezscheme list is a bunch of stuff enclosed in
 * square brackets - []. For example,
 *		[a,b,c,d]
 */		
ez_result_t ez_parse_list( muse_port_t p, int col )
{
	int c = port_getc(p);
	
	muse_assert( c == '[' );
	
	{
		muse_cell h = MUSE_NIL;
		muse_cell t = MUSE_NIL;
		ez_result_t r;
		
		white_space_t ws = ez_skip_whitespace( p, 0, col+1 );

		do
		{
			/* Parse terms and collect them into a list. */
			r = ez_parse( p, ws.col, MUSE_TRUE );
			
			if ( r.expr == PARSE_END_OF_LIST )
			{
				port_getc(p);
				return ez_expr( h, col, r.col_end );
			}
			
			if ( r.expr == PARSE_LIST_ITEM_SEPARATOR )
			{
				port_getc(p);
				r = ez_parse( p, r.col_end, MUSE_TRUE );
			}

			if ( r.expr < 0 )
			{
				if ( !port_eof(p) )
					fprintf( stderr, "Syntax error %d: Expecting list terms.\n", r.expr );
					
				return ez_expr( h, col, r.col_end );
			}
			
			{
				muse_cell nt = muse_cons( r.expr, MUSE_NIL );
				
				if ( t )
					_sett( t, nt );
				else
					h = nt;

				t = nt;
				col = r.col_end;
			}
		} while ( !port_eof(p) );
		
		/* EOF ends list. */
		fprintf( stderr, "Syntax error: No ']' before end of file.\n" );
		return ez_expr( h, col, r.col_end );
	}
}

/**
 * An ezscheme operator is any symbol that does not
 * have alphanumeric characters or underscores.
 */					
static muse_boolean is_operator( const muse_char *name )
{
	muse_char c = *name;

	while ( c )
	{
		/* Alpha numeric character present in symbol. So its not an operator. */
		if ( isalnum(c) || c == '_' )
			return MUSE_FALSE;

		c = *(++name);
	}

	return MUSE_TRUE;
}

static muse_boolean is_data_cell( muse_cell c )
{
	muse_cell_t t = _cellt(c);
	switch ( t )
	{
	case MUSE_INT_CELL :
	case MUSE_FLOAT_CELL :
	case MUSE_TEXT_CELL :
		return MUSE_TRUE;
	default:
		return MUSE_FALSE;
	}
}

/**
 * Indentation dependent parsing of expressions - ezscheme.
 */
ez_result_t ez_parse( muse_port_t p, int col, muse_boolean is_head )
{
	white_space_t ws = ez_skip_whitespace( p, 0, col );
	
	int c = peekc(p);
	
	if ( c == EOF )
		return ez_result( PARSE_EOF, col, col );
		
	if ( c == ',' )
	{
		/* Comma is list item separator. */
		return ez_result( PARSE_LIST_ITEM_SEPARATOR, col, col+1 );
	}

	if ( c == ')' )
	{
		/* Keep the group end character visible until we escape to the
		matching group declaration. */
		return ez_result( PARSE_END_OF_GROUP, col, ws.col+1 );
	}
	
	if ( c == ']' )
	{
		return ez_result( PARSE_END_OF_LIST, col, ws.col+1 );
	}
		
	/* Parse a head expression. */
	{
		ez_result_t head;
		
		if ( c == '(' )
			head = ez_parse_group( p, col );
		else if ( c == '[' )
			/* Square brackets used for representing lists. */
			head = ez_parse_list( p, col );
		else if ( c == '\'' )
		{
			/* Quoted expression. */
			c = port_getc(p);
			
			head = ez_parse( p, col + 1, MUSE_FALSE );
			head.expr = muse_quote(head.expr);
			head.col_start = col;
		}	
		else
			head = ez_parse_atom( p, col );
		
		if ( head.expr < 0 || !is_head )
			return head;

		/* This head could be the LHS of an infix expression
		or a function head to apply to the rest of the arguments. */
		
		{
			white_space_t ws2 = ez_skip_whitespace( p, 0, head.col_end );
			muse_boolean collect_args = MUSE_FALSE;

			if ( ws2.lines > 0 )
			{
				if ( ws2.col <= col )
				{
					/* A line break occurred after the head expression.
					Return the head as is if its group has ended. */
					head.col_end = ws2.col;
					return head;
				}
				else
					/* A line break occurred, but we're still within 
					the head's group. We now need to treat the next term
					as a head term. */
					collect_args = MUSE_TRUE;
			}

			{
				ez_result_t oparg = ez_parse( p, ws2.col, collect_args );
				
				// A head term suffixed with an empty group is parenthesized.
				while ( !collect_args && oparg.expr == PARSE_EMPTY_GROUP )
				{
					head = ez_expr( muse_cons( head.expr, MUSE_NIL ), col, oparg.col_end );
					oparg = ez_parse( p, oparg.col_end, MUSE_FALSE );
				}

				/* An operator is a symbol which has no alphanumeric
				characters. */
				if ( !collect_args && oparg.expr >= 0 && _cellt(oparg.expr) == MUSE_SYMBOL_CELL )
				{
					/* Yes its a symbol. Check if its an operator. */
					const muse_char *name = muse_symbol_name(oparg.expr);
					
					if ( is_operator(name) )
					{
						/* Its an operator. This has to be interpreted as an infix expression.
						So parse the expression after this one and compose the result into a
						tripler list. */
						white_space_t ws3 = ez_skip_whitespace( p, 0, oparg.col_end );
						
						if ( (ws3.lines > 0 && ws3.col <= head.col_start) || port_eof(p) )
						{
							/* A line break when interpreting an infix expression means
							we have a postfix operator. */
							return ez_expr( muse_cons( oparg.expr, muse_cons( head.expr, MUSE_NIL ) ), col, ws3.col );
						}
						
						{
							/* Parse the remaining expression. */
							ez_result_t arg = ez_parse( p, ws3.col, MUSE_TRUE );
							
							if ( arg.expr < 0 )
							{
								/* Something happened! Treat it as though there is a line break. */
								return ez_expr( muse_cons( oparg.expr, muse_cons( head.expr, MUSE_NIL ) ), col, ws3.col );
							}

							/* Check for special operators ':' and '0x2261' */
							if ( wcscmp( name, L":" ) == 0 )
							{
								/* : is the cons operator. */
								return ez_result( muse_cons( head.expr, arg.expr ), col, arg.col_end );								
							}
							else if ( wcscmp( name, L"::" ) == 0 )
							{
								/* :: creates a 2-element list. */
								return ez_result( muse_cons( head.expr, muse_cons( arg.expr, MUSE_NIL ) ), col, arg.col_end );
							}
							else if ( wcscmp( name, L":=" ) == 0 )
							{
								/* Creates a define. */
								return ez_result( muse_cons( muse_builtin_symbol(MUSE_DEFINE), 
															 muse_cons(	head.expr, 
																		muse_cons( arg.expr , MUSE_NIL ) ) ), 
													col, 
													arg.col_end );
							}
							else
								/* Compose the infix operator with the lhs and rhs. */
								return ez_expr( 	muse_cons( 	oparg.expr, 
																muse_cons( 	head.expr, 
																			muse_cons( arg.expr, MUSE_NIL ) ) ), 
													col, 
													arg.col_end );
						}
					}
				}
			
				/* The oparg is not an infix symbol. It is an argument list item. */
				{
					/* First check if its the empty group (). If so, the 
					expression will be PARSE_EMPTY_GROUP. */
					if ( oparg.expr == PARSE_EMPTY_GROUP )
					{
						/* Its a function call with no arguments. */
						return ez_expr( muse_cons( head.expr, MUSE_NIL ), col, oparg.col_end );
					}
					else if ( oparg.expr < 0 )
					{
						head.col_end = oparg.col_end;
						return head;
					}
					else
					{
						int col_end = oparg.col_end;
						muse_cell t = muse_cons( oparg.expr, MUSE_NIL );
						muse_cell h = muse_cons( head.expr, t );
						is_head = MUSE_FALSE;

						do 
						{
							white_space_t ws3 = ez_skip_whitespace( p, 0, col_end );
							
							/* Parse any more arguments to add on to this function call. */
							if ( ws3.lines > 0 || ws3.col <= oparg.col_start )
							{
								/* There's been a line break. Check if we've moved
								behind column wise. If we have, then the argument list
								has terminated. */
								if ( ws3.col <= head.col_start )
								{
									/* Terminate arg list. */
									return ez_expr( h, col, ws3.col );
								}

								if ( ws3.col > head.col_start )
									is_head = MUSE_TRUE;	
							}
							
							{
								/* Continue collecting arguments. */
								ez_result_t nextarg = ez_parse( p, ws3.col, is_head );
								is_head = MUSE_FALSE;

								if ( nextarg.expr < 0 )
								{
									if ( nextarg.expr == PARSE_LIST_ITEM_SEPARATOR )
									{
										port_getc(p);
										nextarg = ez_parse( p, nextarg.col_end, MUSE_TRUE );
									}

									if ( nextarg.expr < 0 )
										return ez_expr( h, col, nextarg.col_end );
								}

								// Process the next argument.
								{
									muse_cell nt = muse_cons( nextarg.expr, MUSE_NIL );
									_sett( t, nt );
									t = nt;

									/* Check if the next indent level is less than the current one. */
									if ( nextarg.col_end > head.col_start )
									{
										col_end = nextarg.col_end;		
										is_head = MUSE_TRUE; /* Treat the next one as "head". */
									}
									else
										return ez_expr( h, col, nextarg.col_end );
								}
							}
						} while ( !port_eof(p) );
						
						/* We've reached end of file. End-of-file implicitly terminates argument list. */
						return ez_expr( h, col, col_end );
					}
				}
			}
		}
	}
}

