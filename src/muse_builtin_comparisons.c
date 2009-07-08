/**
 * @file muse_builtin_comparisons.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_builtins.h"

int compare_text( muse_env *env, muse_cell lhs, muse_cell rhs )
{
	muse_char *ls = _ptr(lhs)->text.start;
	muse_char *rs = _ptr(rhs)->text.start;
	return wcscmp( ls, rs );
}

static inline int deep_compare_int( muse_int i1, muse_int i2 )
{
	return (i1 < i2) ? -1 : (i1 > i2 ? 1 : 0);
}

static int deep_compare( muse_env *env, muse_cell lhs, muse_cell rhs )
{
	int lhs_t, rhs_t, compare_t;
	
	if ( lhs == rhs )
		return 0;
	
	if ( lhs == MUSE_NIL || rhs == MUSE_NIL )
		return lhs - rhs;
	
	lhs_t = _cellt(lhs);
	rhs_t = _cellt(rhs);
	compare_t = lhs_t;
	
	if ( lhs_t != rhs_t )
	{
		/* If one of them is not a number, then return a comparison of their types. */
		if ( !_isnumbert(lhs_t) || !_isnumbert(rhs_t) )
			return lhs_t - rhs_t;
	}

	if ( lhs_t == MUSE_FLOAT_CELL || rhs_t == MUSE_FLOAT_CELL )
		compare_t = MUSE_FLOAT_CELL;

	switch ( compare_t )
	{
		case MUSE_INT_CELL		:  
			return deep_compare_int( _intvalue(lhs), _intvalue(rhs) );
		case MUSE_FLOAT_CELL	:  
		{
			muse_float f1 = _floatvalue(lhs);
			muse_float f2 = _floatvalue(rhs);
			return (f1 < f2) ? -1 : (f1 > f2 ? 1 : 0);
		}
		case MUSE_TEXT_CELL		:
			return compare_text(env,lhs,rhs);
		case MUSE_CONS_CELL		:
		{
			int c = deep_compare( env, muse_head( env, lhs ), muse_head( env, rhs ) );
			if ( c != 0 )
				return c;
			else
				return deep_compare( env, muse_tail( env, lhs ), muse_tail( env, rhs ) );
		}
		case MUSE_SYMBOL_CELL	:
		{
			/* Compare the string form of the symbols. */
			muse_cell lt = _tail(_head(_tail(lhs)));
			muse_cell rt = _tail(_head(_tail(rhs)));
			
			if ( lt == MUSE_NIL && rt == MUSE_NIL )
				return lhs - rhs;
			else
				return deep_compare( env, lt, rt );
		}
		case MUSE_LAZY_CELL		:
			return deep_compare( env, _force(lhs), _force(rhs) );

		default					:
			return lhs - rhs;
	}	
}

int fn_deep_compare( muse_env *env, muse_cell args )
{
	muse_cell lhs = _evalnext(&args);
	muse_cell rhs = _evalnext(&args);
	int result = deep_compare( env, lhs, rhs );

	return result;
}

/**
 * @code (eq? x y) @endcode
 * Compares x and y for referential equality. Additionally x and y
 * will be equal even if their references are different, but
 * they have the same integer values.
 */
muse_cell fn_eq( muse_env *env, void *context, muse_cell args )
{
	muse_cell lhs = _evalnext(&args);
	muse_cell rhs = _evalnext(&args);
	return muse_eq( env, lhs, rhs ) ? _t() : MUSE_NIL;
}

/**
 * @code (= x y) @endcode
 * Compares x and y for value equality. Compound structures
 * such as lists are deep compared.
 */
muse_cell fn_equal( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( env, args ) == 0 ? _t() : MUSE_NIL;
}

/**
 * @code (!= x y) @endcode
 * Evaluates to T if x and y are not the same (deep comparison)
 * and to () if they are.
 */
muse_cell fn_ne( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( env, args ) != 0 ? _t() : MUSE_NIL;
}

/**
 * @code (< x y) @endcode
 * T if x compares less than y and () otherwise.
 */
muse_cell fn_lt( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( env, args ) < 0 ? _t() : MUSE_NIL;
}

/**
 * @code (> x y) @endcode
 * T if x compares greater than y and () otherwise.
 */
muse_cell fn_gt( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( env, args ) > 0 ? _t() : MUSE_NIL;
}

/**
 * @code (<= x y) @endcode
 * T if x compares less than or equal to y and () otherwise.
 */
muse_cell fn_le( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( env, args ) <= 0 ? _t() : MUSE_NIL;
}

/**
 * @code (>= x y) @endcode
 * T if x compares greater than or equal to y and () otherwise.
 */
muse_cell fn_ge( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( env, args ) >= 0 ? _t() : MUSE_NIL;
}

/**
 * @code (and e1 e2 .. eN) @endcode
 *
 * The conjunction of all the given values -
 * i.e. evaluates to () if even one of them
 * is () and if none of them is (), it evaluates
 * to eN.
 */
muse_cell fn_and( muse_env *env, void *context, muse_cell args )
{
	muse_cell result = MUSE_NIL;
	int sp = _spos();

	while ( args )
	{
		_unwind(sp);
		result = _evalnext(&args);
		if ( !result )
			return MUSE_NIL;
	}
	
	return result;
}

/**
 * @code (or e1 e2 .. eN) @endcode
 *
 * Evaluates to the first of all the e1 e2 etc which
 * is not (). If all of them are (), then the result
 * is also ().
 */
muse_cell fn_or( muse_env *env, void *context, muse_cell args )
{
	int sp = _spos();
	while ( args )
	{
		_unwind(sp);
		{
			muse_cell c = _evalnext(&args);
			if ( c )
				return c;
		}
	}
	
	return MUSE_NIL;
}

/**
 * @code (not x) @endcode
 *
 * Evaluates to T if x is () and to () if x is anything else.
 */
muse_cell fn_not( muse_env *env, void *context, muse_cell args )
{
	muse_cell c = _evalnext(&args);
	return c ? MUSE_NIL : _t();
}

/**
 * @code (min x0 ... xn) @endcode
 * Returns the minimum element x,
 * where x <= x0 <= ... <= xn
 */
muse_cell fn_min( muse_env *env, void *context, muse_cell args )
{
	int sp = _spos();
	muse_cell result = _evalnext(&args);

	while ( args )
	{
		muse_cell candidate = _evalnext(&args);

		if ( deep_compare( env, result, candidate ) > 0 ) 
		{
			result = candidate;
			_unwind(sp);
			_spush(result);
		}
	}

	return result;
}

/**
 * @code (max x0 ... xn) @endcode
 * Returns the maximum element x,
 * where x0 <= ... <= xn <= x
 */
muse_cell fn_max( muse_env *env, void *context, muse_cell args )
{
	int sp = _spos();
	muse_cell result = _evalnext(&args);

	while ( args )
	{
		muse_cell candidate = _evalnext(&args);

		if ( deep_compare( env, result, candidate ) < 0 )
		{
			result = candidate;
			_unwind(sp);
			_spush(result);
		}
	}

	return result;
}
