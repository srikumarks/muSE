/**
 * @file muse_builtin_math.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_builtin_math.h"
#include <math.h>
#include <stdlib.h>

/**
 * @code (+ ...numbers...) @endcode
 * add sums up all of its arguments and returns the result.
 * The result will be an integer if all of its arguments
 * are integers. Otherwise it'll be float.
 */
muse_cell fn_add( muse_env *env, void *context, muse_cell args )
{
	muse_int i = 0;
	muse_float f = 0.0;
	muse_boolean result_is_float = MUSE_FALSE;
	
	while ( args )
	{
		muse_cell arg = _evalnext(&args);
		switch ( _cellt(arg) )
		{
			case MUSE_INT_CELL :
				i += _ptr(arg)->i;
				break;
			case MUSE_FLOAT_CELL :
				result_is_float = MUSE_TRUE;
				f += _ptr(arg)->f;
				break;
			default:
				MUSE_DIAGNOSTICS({ 
					if ( !muse_expect( env, L"(+ ... >>arg<< ...)", L"v|??|", arg, MUSE_INT_CELL, MUSE_FLOAT_CELL ) )
						muse_message( env,L"(+ ... >>arg<< ...)", 
										L"[+] can only work with integers and floats.\n"
										L"You gave [%m] which is of type [%t].", arg, arg );
				});
		}
	}
	
	if ( result_is_float )
		return _mk_float( f + i );
	else
		return _mk_int(i);
}

/**
 * @code (- ...numbers...) @endcode
 * Takes 1 or more arguments.
 * 	- <tt>(- m)</tt> gives <tt>-m</tt>
 * 	- <tt>(- m n)</tt> gives <tt>(m-n)</tt>
 * 	- <tt>(- m n o p)</tt> gives <tt>(m - (n+o+p))</tt>
 */
muse_cell fn_sub( muse_env *env, void *context, muse_cell args )
{
	muse_int i = 0;
	muse_float f = 0.0;
	muse_boolean result_is_float = MUSE_FALSE;
	muse_cell c = MUSE_NIL;
	
	if ( !args )
		return _mk_int(0);
	
	c = _evalnext(&args);
	switch ( _cellt(c) )
	{
		case MUSE_INT_CELL		: i += _ptr(c)->i; break;
		case MUSE_FLOAT_CELL	: f += _ptr(c)->f; result_is_float = MUSE_TRUE; break;
		default:
				MUSE_DIAGNOSTICS({ 
					if ( !muse_expect( env, L"(- ... >>arg<< ...)", L"v|??|", c, MUSE_INT_CELL, MUSE_FLOAT_CELL ) )
						muse_message( env,L"(- ... >>arg<< ...)", 
										L"[-] can only work with integers and floats.\n"
										L"You gave [%m] which is of type [%t].", c, c );
				});
	}
	
	if ( !args )
	{
		if ( result_is_float )
			return _mk_float( -f );
		else
			return _mk_int( -i );
	}
	
	while ( args )
	{
		c = _evalnext(&args);
		switch ( _cellt(c) )
		{
			case MUSE_INT_CELL		: i -= _ptr(c)->i; break;
			case MUSE_FLOAT_CELL	: f -= _ptr(c)->f; result_is_float = MUSE_TRUE; break;
			default:
				MUSE_DIAGNOSTICS({ 
					if ( !muse_expect( env, L"(- ... >>arg<< ...)", L"v|??|", c, MUSE_INT_CELL, MUSE_FLOAT_CELL ) )
						muse_message( env,L"(- ... >>arg<< ...)", 
										L"[-] can only work with integers and floats.\n"
										L"You gave [%m] which is of type [%t].", c, c );
				});
		}
	}
	
	if ( result_is_float )
		return _mk_float( i + f );
	else
		return _mk_int(i);
}

/**
 * @code (* ...numbers...) @endcode
 * Multiplies all arguments. Result is float if
 * at least one of the arguments is float. Otherwise
 * it is an integer.
 */
muse_cell fn_mul( muse_env *env, void *context, muse_cell args )
{
	muse_int i = 1;
	muse_float f = 1.0;
	muse_boolean result_is_float = MUSE_FALSE;
	
	while ( args )
	{
		muse_cell arg = _evalnext(&args);
		switch ( _cellt(arg) )
		{
			case MUSE_INT_CELL :
				i *= _ptr(arg)->i;
				break;
			case MUSE_FLOAT_CELL :
				result_is_float = MUSE_TRUE;
				f *= _ptr(arg)->f;
				break;
			default:
				MUSE_DIAGNOSTICS({ 
					if ( !muse_expect( env, L"(* ... >>arg<< ...)", L"v|??|", arg, MUSE_INT_CELL, MUSE_FLOAT_CELL ) )
						muse_message( env,L"(* ... >>arg<< ...)", 
										L"[*] can only work with integers and floats.\n"
										L"You gave [%m] which is of type [%t].", arg, arg );
				});
		}
	}
	
	if ( result_is_float )
		return _mk_float( f * i );
	else
		return _mk_int(i);
}


/**
 * @code (/ ...numbers...) @endcode
 * Takes 1 or more arguments.
 * 	- <tt>(/ m)</tt> gives <tt>1/m</tt>
 * 	- <tt>(/ m n)</tt> gives <tt>(m/n)</tt>
 * 	- <tt>(/ m n o p)</tt> gives <tt>(m / (n * o * p))</tt>
 */
muse_cell fn_div( muse_env *env, void *context, muse_cell args )
{
	muse_float f = 1.0;
	muse_cell c = MUSE_NIL;
	
	if ( !args )
		return _mk_int(0);
	
	c = _evalnext(&args);
	switch ( _cellt(c) )
	{
		case MUSE_INT_CELL		: f = (muse_float)(_ptr(c)->i); break;
		case MUSE_FLOAT_CELL	: f = _ptr(c)->f; break;
		default:
				MUSE_DIAGNOSTICS({ 
					if ( !muse_expect( env, L"(/ ... >>arg<< ...)", L"v|??|", c, MUSE_INT_CELL, MUSE_FLOAT_CELL ) )
						muse_message( env,L"(/ ... >>arg<< ...)", 
										L"[/] can only work with integers and floats.\n"
										L"You gave [%m] which is of type [%t].", c, c );
				});
	}
	
	if ( !args )
		return _mk_float( 1.0 / f );
	
	while ( args )
	{
		c = _evalnext(&args);
		switch ( _cellt(c) )
		{
			case MUSE_INT_CELL		: f /= _ptr(c)->i; break;
			case MUSE_FLOAT_CELL	: f /= _ptr(c)->f; break;
			default:
				MUSE_DIAGNOSTICS({ 
					if ( !muse_expect( env, L"(/ ... >>arg<< ...)", L"v|??|", c, MUSE_INT_CELL, MUSE_FLOAT_CELL ) )
						muse_message( env,L"(/ ... >>arg<< ...)", 
										L"[/] can only work with integers and floats.\n"
										L"You gave [%m] which is of type [%t].", c, c );
				});
		}
	}
	
	return _mk_float(f);
}

/**
 * @code (i/ numerator denominator) @endcode
 * Takes 2 arguments. Divides first by the second and returns
 * the quotient. Arguments must be integers.
 */
muse_cell fn_idiv( muse_env *env, void *context, muse_cell args )
{
	muse_cell a1 = _evalnext(&args);
	muse_cell a2 = _evalnext(&args);
	
	muse_int q = _ptr(a1)->i / _ptr(a2)->i;
	
	return _mk_int(q);;
}

/**
 * @code (% numerator denominator) @endcode
 * Takes 2 arguments and returns the floating point or
 * integer remainder on dividing first by the second.
 * It does a proper modulo operation unlike C/C++
 * - the sign of the denominator is discarded
 * and the result is always in the range [0,denom)
 * irrespective of the sign of the numerator.
 */
muse_cell fn_mod( muse_env *env, void *context, muse_cell args )
{
	muse_int n = _intvalue(_evalnext(&args));
	muse_int m = _intvalue(_evalnext(&args));

	muse_assert( m != 0 );
	m = (m > 0) ? m : -m;
	n = (n % m);

	return _mk_int( (n < 0) ? (n + m) : n );
}

/**
 * @code (++ c) @endcode
 *
 * Increments the integer contents of the given cell.
 */
muse_cell fn_inc( muse_env *env, void *context, muse_cell args )
{
	muse_cell c = _evalnext(&args);
	++(_ptr(c)->i);
	return c;
}

/**
 * @code (-- c) @endcode
 *
 * Decrements the integer contents of the given cell.
 */
muse_cell fn_dec( muse_env *env, void *context, muse_cell args )
{
	muse_cell c = _evalnext(&args);
	--(_ptr(c)->i);
	return c;
}

/**
 * @code (trunc float-value) @endcode
 * Converts a float to int. If given an integer argument.
 * returns it as is.
 */
muse_cell fn_trunc( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = _evalnext(&args);
	switch ( _cellt(arg) )
	{
		case MUSE_FLOAT_CELL : return _mk_int((muse_int)_ptr(arg)->f);
		default : return arg;
	}
}

/**
 * @code (rand M [N]) @endcode
 * Generates a random number.
 * 	- <tt>(rand N)</tt> returns an integer random number in the range [0,N)
 * 	- <tt>(rand M N)</tt> returns an integer random number in the range [M,N)
 * 	- <tt>(rand F)</tt> returns a float random number in the range [0,F).
 * 	- <tt>(rand G F)</tt> returns a float random number in the range [G,F).
 */
muse_cell fn_rand( muse_env *env, void *context, muse_cell args )
{
	muse_cell N = _evalnext(&args);
	muse_cell M = MUSE_NIL;
	
	if ( args )
	{
		M = N;
		N = _evalnext(&args);
		muse_assert( _cellt(M) == _cellt(N) );
	}
	
	switch ( _cellt(N) )
	{
		case MUSE_INT_CELL : 
		{
			muse_int m = 0;
			if ( M )
				m = _ptr(M)->i;

			{
				muse_int dn = _ptr(N)->i - m;
				return _mk_int( m + (muse_int)(rand() * dn / (1.0+RAND_MAX)) );
			}
		}
		break;
			
		case MUSE_FLOAT_CELL :
		{
			muse_float m = 0.0;
			if ( M )
				m = _ptr(M)->f;
			return _mk_float( m + rand() * (_ptr(N)->f - m) / (1.0+RAND_MAX) );
		}
		break;
			
		default:
			return MUSE_NIL;
	}
}

/**
 * @code (pow base exponent) @endcode
 * Computes base ^ exponent.
 * The result is always a float.
 */
muse_cell fn_pow( muse_env *env, void *context, muse_cell args )
{
	muse_cell base		= _evalnext(&args);
	muse_cell exponent	= _evalnext(&args);

	return _mk_float( pow( _floatvalue(base), _floatvalue(exponent) ) );
}

/**
 * The type unary math operators.
 */
typedef double (*unary_math_op_t)(double);

/**
 * The function that calls the supplied unary math operator
 * in the "context" field on the given argument. The supported
 * unary math operations are -
 * 	- 	sqrt
 * 	-	log
 * 	-	log10
 * 	-	exp
 * 	-	sin
 * 	-	cos
 * 	-	tan
 * 	-	asin
 * 	-	acos
 * 	-	atan
 * 	-	sinh
 * 	-	cosh
 * 	-	tanh
 * 	-	asinh
 * 	-	acosh
 * 	-	atanh
 * 	-	fabs
 * 	-	floor
 * 	-	ceil
 */
muse_cell fn_unary_math( muse_env *env, void *fn, muse_cell args )
{
	unary_math_op_t op = (unary_math_op_t)fn;
	
	muse_float f = _floatvalue(_evalnext(&args));
	
	return _mk_float( op(f) );
}

#if defined(_MSC_VER) && _MSC_VER >= 1400
/**
 * Something very peculiar about Microsoft Visual Studio 2005 forced
 * me to define this function wrapper for the floor math function.
 * Maybe it has to do with the ambiguity between floor(float) and
 * floor(double). What's even stranger, the same ambiguity doesn't exist
 * for ceil() in VC2003, but exists for VC2005!!
 * - Kumar
 */
static double muse_floor( double x )
{
	return floor(x);
}
static double muse_ceil( double x )
{
	return ceil(x);
}
#else
#	define muse_floor floor
#	define muse_ceil ceil
#endif

/**
 * Defines log, log10, sqrt, sin, cos, tan, etc.
 */
void muse_math_load_common_unary_functions( muse_env *env )
{
	struct _unary_op { const muse_char *name; unary_math_op_t f; };
	
	static const struct _unary_op k_unary_ops[] =
	{
	{	L"sqrt",	sqrt	},
	{	L"log",		log		},
	{	L"log10",	log10	},
	{	L"exp",		exp		},
	{	L"sin",		sin		},
	{	L"cos",		cos		},
	{	L"tan",		tan		},
	{	L"asin",	asin	},
	{	L"acos",	acos	},
	{	L"atan",	atan	},
	{	L"sinh",	sinh	},
	{	L"cosh",	cosh	},
	{	L"tanh",	tanh	},
#ifndef MUSE_PLATFORM_WINDOWS
	{	L"asinh",	asinh	},
	{	L"acosh",	acosh	},
	{	L"atanh",	atanh	},
#endif
	{	L"fabs",	fabs	},
	{	L"floor",	muse_floor	},
	{	L"ceil",	muse_ceil	},
	{	NULL,		NULL	}
	};
	
	{
		const struct _unary_op *op = k_unary_ops;
		while ( op->name )
		{
			int sp = _spos();
			_define( _csymbol(op->name), _mk_nativefn( fn_unary_math, (void*)(op->f) ) );
			_unwind(sp);
			++op;
		}
	}
}
