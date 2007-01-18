/**
 * @file muse_repl.c
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

/**
 * The muSE REPL - Read-Eval-Print-Loop.
 * It reads symbolic expressions from standard input,
 * evaluates them and writes the result of the evaluation
 * to the standard output.
 * 
 * The only way to return from the function is
 * to exit the program by typing @code (exit) @endcode
 * into the command line.
 */
void muse_repl(muse_env *env)
{
	int sp = _spos();
	int error_count = 0;
	muse_cell c;
	muse_cell current_expr = _csymbol(L"$");
	muse_port_t pstdin = muse_stdport(env,0);
	muse_port_t pstdout = muse_stdport(env,1);
	
	fprintf( stderr, "--------------------------------------------------------------------------------\n"
 			 "muSE (muvee Symbolic Expressions) version %s\n"
			 "See LICENSE.txt or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt\n"
			 "for terms and conditions under which this software is distributed.\n"
			 "\n"
			 "(process support)\n"
			 "--------------------------------------------------------------------------------\n",
			 MUSE_VERSION_STRING );

	while ( !feof(stdin) )
	{
		fprintf( stderr, "\n> " );
		fflush( stderr );
		c = muse_pread( pstdin );
		if ( c < 0 )
		{
			++ error_count;
			fprintf( stderr, "Parse error %d\n", c );
			port_getc( pstdin );

			if ( error_count > 5 )
				break;
		}
		else
		{
			error_count = 0;

			if ( c != current_expr )
			{
				_define( current_expr, c );
			}
			c = _eval(c);
			switch ( _cellt(c) )
			{
				case MUSE_INT_CELL :
				case MUSE_FLOAT_CELL :
				case MUSE_TEXT_CELL :
				case MUSE_CONS_CELL :
				case MUSE_NATIVEFN_CELL :
				case MUSE_SYMBOL_CELL :
					muse_pprint( pstdout, c );
					port_flush( pstdout );
				default:;
			};
		}
				
		_unwind(sp);
	}
}

