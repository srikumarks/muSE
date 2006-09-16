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

int compare_text( muse_cell lhs, muse_cell rhs )
{
	muse_char *ls = _ptr(lhs)->text.start;
	muse_char *rs = _ptr(rhs)->text.start;
	return wcscmp( ls, rs );
}

static inline int deep_compare_int( muse_int i1, muse_int i2 )
{
	return (i1 < i2) ? -1 : (i1 > i2 ? 1 : 0);
}

static int deep_compare( muse_cell lhs, muse_cell rhs )
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
		if ( !(_isnumbert(lhs_t) || _isnumbert(rhs_t)) )
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
			return compare_text(lhs,rhs);
		case MUSE_CONS_CELL		:
		{
			int c = deep_compare( _head(lhs), _head(rhs) );
			if ( c != 0 )
				return c;
			else
				return deep_compare( _tail(lhs), _tail(rhs) );
		}
		case MUSE_SYMBOL_CELL	:
		{
			muse_cell lt = _tail(_head(_tail(lhs)));
			muse_cell rt = _tail(_head(_tail(rhs)));
			
			if ( lt == MUSE_NIL && rt == MUSE_NIL )
				return lhs - rhs;
			else
				return deep_compare( lt, rt );
		}
		default					:
			return lhs - rhs;
	}	
}

int fn_deep_compare( muse_cell args )
{
	muse_cell lhs = muse_evalnext(&args);
	muse_cell rhs = muse_evalnext(&args);
	int result = deep_compare( lhs, rhs );

	return result;
}

/**
 * (eq? x y ).
 * Compares x and y for referential equality. Additionally x and y
 * will be equal even if their references are different, but
 * they have the same integer values.
 */
muse_cell fn_eq( muse_env *env, void *context, muse_cell args )
{
	muse_cell lhs = muse_evalnext(&args);
	muse_cell rhs = muse_evalnext(&args);
	return muse_eq( lhs, rhs ) ? _t() : MUSE_NIL;
}

/**
 * (= x y).
 * Compares x and y for value equality. Compound structures
 * such as lists are deep compared.
 */
muse_cell fn_equal( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( args ) == 0 ? _t() : MUSE_NIL;
}

muse_cell fn_ne( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( args ) != 0 ? _t() : MUSE_NIL;
}

muse_cell fn_lt( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( args ) < 0 ? _t() : MUSE_NIL;
}

muse_cell fn_gt( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( args ) > 0 ? _t() : MUSE_NIL;
}

muse_cell fn_le( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( args ) <= 0 ? _t() : MUSE_NIL;
}

muse_cell fn_ge( muse_env *env, void *context, muse_cell args )
{
	return fn_deep_compare( args ) >= 0 ? _t() : MUSE_NIL;
}

muse_cell fn_and( muse_env *env, void *context, muse_cell args )
{
	while ( args )
	{
		muse_cell c = muse_evalnext(&args);
		if ( !c )
			return MUSE_NIL;
	}
	
	return _t();
}

muse_cell fn_or( muse_env *env, void *context, muse_cell args )
{
	while ( args )
	{
		muse_cell c = muse_evalnext(&args);
		if ( c )
			return _t();
	}
	
	return MUSE_NIL;
}

muse_cell fn_not( muse_env *env, void *context, muse_cell args )
{
	muse_cell c = muse_evalnext(&args);
	return c ? MUSE_NIL : _t();
}
