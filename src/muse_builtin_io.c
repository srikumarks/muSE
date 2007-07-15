/**
 * @file muse_builtin_io.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_builtins.h"
#include "muse_port.h"
#include <stdlib.h>

/**
 * (port? p).
 * Returns p if it is a port indeed, and () otherwise.
 */
muse_cell fn_port_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell p = _evalnext(&args);
	
	return p && _port(p) ? p : MUSE_NIL;
}

/**
 * Uses the given writer to write a single argument.
 * This function's sole purpose is to implement
 * \ref fn_print "print" and \ref fn_write "write".
 */
static muse_cell fn_output( muse_env *env, muse_cell args, void (*writer)( muse_port_t p, muse_cell arg ) )
{
	muse_port_t port = NULL;
	
	muse_cell arg1 = _evalnext(&args);
	
	port = _port(arg1);
	if ( !port )
	{
		port = _stdport(MUSE_STDOUT_PORT);
		writer( port, arg1 );
		if ( args )
			port_putc( ' ', port );
	}
	
	while ( args )
	{
		writer( port, _evalnext(&args) );
		if ( args )
			port_putc( ' ', port );
	}
	
	/* This last new-line is important in order to indicate to any
	readers that the previous term has ended. Otherwise if an integer
	is written out, then the reader will end up waiting for more digits
	because it doesn't know that the integer has ended. So we use the
	mother-of-all white space characters - the newline - to indicate the
	end of a term. */
	port_putc( '\n', port );

	/* If there is a write error, return () and clear the error code. */
	if ( port->error )
	{
		MUSE_DIAGNOSTICS3({ fprintf( stderr, "IO: write error %d!\n", port->error ); });
		port->error = 0;
		return MUSE_NIL;
	}
	else
		return _t();
}

/**
 * (print -expressions-).
 * Prints each expression to the standard output in sequence.
 * Strings are printed without the quotes, so \c print can be
 * used to print out messages for the user.
 */
muse_cell fn_print( muse_env *env, void *context, muse_cell args )
{
	return fn_output( env, args, muse_pprint );
}

/**
 * (write -expressions-).
 * Writes the s-expressions to the standard output in sequence.
 * Tries to write an s-expression such that it can be read 
 * back in using \ref fn_read "read", meaning strings will be enclosed
 * in double quotes.
 */
muse_cell fn_write( muse_env *env, void *context, muse_cell args )
{
	return fn_output( env, args, muse_pwrite );
}

/**
 * (read [port]) 
 *
 * Reads a single sexpr from the standard input stream.
 */
muse_cell fn_read( muse_env *env, void *context, muse_cell args )
{
	muse_port_t port = NULL;
	
	if ( args )
	{
		port = _port( _evalnext(&args) );
		muse_assert( port != NULL && "Read can only take a port argument.");
	}
	else
		port = _stdport( MUSE_STDIN_PORT );
	
	{
		muse_cell result = muse_pread( port );
		return result < 0 ? MUSE_NIL : result;
	}
}

/**
 * (close port).
 * Closes the given port.
 */
muse_cell fn_close( muse_env *env, void *context, muse_cell args )
{
	muse_cell port = _evalnext(&args);
	muse_port_t p = _port(port);
	
	muse_assert( p && "argument must be a port." );

	port_close( p);

	return MUSE_NIL;
}

/**
 * (eof? port).
 * Returns T if the port has reached end of stream.
 * Returns () if it hasn't reached end of stream.
 */
muse_cell fn_eof_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell port = _evalnext(&args);
	muse_port_t p = _port(port);
	
	muse_assert( p && "close's argument must be a port." );

	return port_eof(p) ? _t() : MUSE_NIL;
}

/**
 * (flush [port]).
 * Flushes the accumulated output on the given port.
 * If no port is given, flushes stdout.
 */
muse_cell fn_flush( muse_env *env, void *context, muse_cell args )
{
	muse_cell portcell = _evalnext(&args);
	muse_port_t port = portcell ? _port(portcell) : _stdport( MUSE_STDOUT_PORT );
	
	muse_assert( port != NULL );
	
	port_flush( port );
	
	return portcell;
}

/**
 * (load "file.lisp")
 *
 * Reads and evaluates all expressions in the given file
 * and returns the value of the last expression.
 */
muse_cell fn_load( muse_env *env, void *context, muse_cell args )
{
	int sp				= _spos();
	muse_cell filename	= _evalnext(&args);
	FILE *f				= NULL;
	
	MUSE_DIAGNOSTICS({
		muse_expect( env, L"(load >>file<<)", L"v?", filename, MUSE_TEXT_CELL );
	});

	f = muse_fopen( _text_contents(filename,NULL), L"rb" );

	if ( f )
	{
		muse_cell result = muse_load( env, f );
		fclose(f);
		_unwind(sp);
		return _spush(result);
	}
	else
	{
		_unwind(sp);

		MUSE_DIAGNOSTICS({
			muse_message( env,L"(load >>file<<)", L"The file [%m] doesn't exist!", filename );
		});

		return MUSE_NIL;
	}
}

/**
 * (mickey inport outport)
 *
 * Reads in the stream from the input port, expands all 
 * mickey expressions and outputs to the output port.
 *
 * @see muse_mickey
 */
muse_cell fn_mickey( muse_env *env, void *context, muse_cell args )
{
	muse_port_t in = _port(_evalnext(&args));
	muse_port_t out = _port(_evalnext(&args));

	muse_mickey( in, out );

	return MUSE_NIL;
}

static void mickey_mode( muse_port_t in, muse_port_t out );

/**
 * Processes mickey streams from input port to output port.
 * Mickey streams let you use muSE as a powerful yet simple 
 * macro expander. You may find this useful in scripting,
 * dynamic documents in web applications, etc.
 * 
 * A mickey stream is a stream of text with embedded "mickey"
 * codes - of the form @...expr...@ where the portion between 
 * the "@" signs (which look like Mickey's ears and hence the
 * name) is interpreted as a muSE expression. The result 
 * of evaluating the expression is used in place of the 
 * @ expression itself (without the mickey ears). 
 *
 * If you put a sequence of expressions within @..expr..@,
 * the mickey block will evaluate to the value of the last 
 * expression - behaving like a (do ..) block.
 *
 * Note that scheme symbols and atoms are themselves allowed 
 * to use the @ character within a mickey expression. So, for
 * example, if you simply want a symbol Hello to expand to 
 * its value "World", you should write it like this -
 *		@Hello;@
 * The ; character will terminate the symbol name and ignore 
 * all characters up to the next new line or @ whichever comes 
 * first. In general, it is a good idea to end a mickey
 * expression with ;@ instead of just @. Stuff like @(+ 1 2)@
 * will work correctly however. 
 *
 * If you have a symbol starting with an @ character 
 * (say @rate), you can place its value in the output 
 * using @(eval '@rate)@.
 *
 * If you want to place a literal @ character in the output 
 * stream, simply use @@ in the input stream wherever you need
 * the literal @. For example, an email address would look
 * like somebody@@somewhere.com in the input stream.
 */
void muse_mickey( muse_port_t in, muse_port_t out )
{
	muse_env *env = in->env;

	while ( !port_eof(in) && in->error == 0 )
	{
		/* Free state - no mickey expression.
		Pass through all characters. */
		int c = port_getc(in);

		if ( c == '@' )
		{
			/* Start of mickey expression. */
			mickey_mode( in, out );			
		}
		else if ( c > 0 )
		{
			port_putc( c, out );
		}
	}
}

static size_t skip_whitespace( muse_port_t in )
{
	size_t n = 0;
	while ( !port_eof(in) && in->error == 0 )
	{
		int c = port_getc(in);
		if ( isspace(c) )
		{
			++n;
		}
		else
		{
			port_ungetc(c,in);
			return n;
		}
	}

	return n;
}

/**
 * The meaning of the ; character inside of a 
 * mickey expression is to skip everything till 
 * either the closing mickey character or a newline,
 * whicever comes first. 
 */
static size_t skip_mickey_comment( muse_port_t in )
{
	size_t n = skip_whitespace(in);
	
	if ( !port_eof(in) && in->error == 0 )
	{
		int c = port_getc(in);
		if ( c == ';' )
		{
			/* Skip comment. */
			++n;

			while ( !port_eof(in) && in->error == 0 )
			{
				c = port_getc(in);

				if ( c == '@' )
				{
					/* End of mickey expression. Don't include
					the @ character itself. */
					port_ungetc(c,in);
					break;
				}
				else if ( c == '\n' )
				{
					/* End of line. Comment finished. */
					break;
				}

				++n;
			}
		}
		else
		{
			port_ungetc(c,in);
		}
	}

	return n;
}

/**
 * Skips one or more comment expressions and all white space.
 */
static void skip_mickey_comments( muse_port_t in )
{
	size_t commentline = skip_mickey_comment(in);

	while ( commentline > 0 )
	{
		commentline = skip_mickey_comment(in);
	}
}

/**
 * The previous character was a '@', so we're now in
 * "mickey mode" and must read and process all expressions
 * up to the next @ character. If we immediately encounter 
 * an @ character (module white space), we treat it as
 * a literal @ character. 
 */
static void mickey_mode( muse_port_t in, muse_port_t out )
{
	muse_env *env = in->env;
	muse_cell result = MUSE_NIL;
	int sp = _spos();
	int numexprs = 0;
	
	skip_mickey_comments( in );

	while ( !port_eof(in) && in->error == 0 )
	{
		int c = port_getc(in);

		if ( c == '@' )
		{
			/* End of mickey mode. */
			if ( numexprs > 0 )
			{
				if ( result ) muse_pprint( out, result );
			}
			else
			{
				/* If no expressions were evaluated, treat it
				as a an escaped @ character. */
				port_putc( '@', out );
			}

			_unwind(sp);
			return;
		}
		else
		{
			port_ungetc(c,in);

			/* Process expression. */
			_unwind(sp);
			result = muse_eval( env, muse_pread(in), MUSE_FALSE );
			skip_mickey_comments(in);
			++numexprs;
		}
	}
}
