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
	muse_cell p = muse_evalnext(&args);
	
	return p && muse_port(p) ? p : MUSE_NIL;
}

/**
 * Uses the given writer to write a single argument.
 * This function's sole purpose is to implement
 * \ref fn_print "print" and \ref fn_write "write".
 */
static muse_cell fn_output( muse_env *env, muse_cell args, void (*writer)( muse_port_t p, muse_cell arg ) )
{
	muse_port_t port = NULL;
	
	muse_cell arg1 = muse_evalnext(&args);
	
	port = muse_port(arg1);
	if ( !port )
	{
		port = muse_stdport(MUSE_STDOUT_PORT);
		writer( port, arg1 );
		if ( args )
			port_putc( ' ', port );
	}
	
	while ( args )
	{
		writer( port, muse_evalnext(&args) );
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
		port = muse_port( muse_evalnext(&args) );
		muse_assert( port != NULL && "Read can only take a port argument.");
	}
	else
		port = muse_stdport( MUSE_STDIN_PORT );
	
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
	muse_cell port = muse_evalnext(&args);
	muse_port_t p = muse_port(port);
	
	muse_assert( p && "argument must be a port." );

	port_close(p);

	return MUSE_NIL;
}

/**
 * (eof? port).
 * Returns T if the port has reached end of stream.
 * Returns () if it hasn't reached end of stream.
 */
muse_cell fn_eof_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell port = muse_evalnext(&args);
	muse_port_t p = muse_port(port);
	
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
	muse_cell portcell = muse_evalnext(&args);
	muse_port_t port = portcell ? muse_port(portcell) : muse_stdport( MUSE_STDOUT_PORT );
	
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
	muse_cell filename	= muse_evalnext(&args);
	FILE *f				= NULL;
	
	MUSE_DIAGNOSTICS({
		muse_expect( L"(load >>file<<)", L"v?", filename, MUSE_TEXT_CELL );
	});

	f = muse_fopen( muse_text_contents(filename,NULL), L"rb" );

	if ( f )
	{
		muse_cell result = muse_load( f );
		fclose(f);
		_unwind(sp);
		return _spush(result);
	}
	else
	{
		_unwind(sp);

		MUSE_DIAGNOSTICS({
			muse_message( L"(load >>file<<)", L"The file [%m] doesn't exist!", filename );
		});

		return MUSE_NIL;
	}
}
