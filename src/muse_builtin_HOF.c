/**
 * @file muse_builtin_HOF.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-scheme.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_builtins.h"
#include <stdlib.h>

static muse_cell produce_items( muse_cell *seq, muse_boolean *more_to_go )
{
	switch ( _cellt(*seq) )
	{
		case MUSE_CONS_CELL		: 
			if ( *seq )
			{
				*more_to_go = MUSE_TRUE;
				return _next(seq);
			}
			else
			{
				*more_to_go = MUSE_FALSE;
				return MUSE_NIL;
			}
		case MUSE_LAMBDA_CELL	: 
		case MUSE_NATIVEFN_CELL	:
		{
			muse_cell result = muse_apply( *seq, MUSE_NIL, MUSE_TRUE );
			*more_to_go = (result != MUSE_NIL) ? MUSE_TRUE : MUSE_FALSE;
			*seq = _tail(result);
			return result;
		}
		default					: 
			*more_to_go = MUSE_FALSE;
			*seq = MUSE_NIL;
			return MUSE_NIL;
	}
}

/**
 * (map fn list).
 * Creates a new list whose elements are determined by applying the given
 * function to each argument of the list. 
 * For example -
 * @code
 * (define squares (map (fn (x) (* x x)) (list 1 2 3 4 5)))
 * (print squares)
 * @endcode
 * will print 
 * @code
 * (1 4 9 16 25)
 * @endcode
 */
muse_cell fn_map( muse_env *env, void *context, muse_cell args )
{
	int sp = _spos();
	muse_cell h = MUSE_NIL, t = MUSE_NIL, r, c;
	muse_cell fn = muse_evalnext(&args);
	muse_cell sequence = muse_evalnext(&args);
	muse_boolean more_to_go = MUSE_FALSE;
	muse_cell map_args = muse_cons( MUSE_NIL, MUSE_NIL );
	
	_seth( map_args, produce_items( &sequence, &more_to_go ) ); 
	if ( more_to_go )
	{
		h = t = muse_cons( MUSE_NIL, MUSE_NIL );
		sp = _spos();
		r = muse_apply( fn, map_args, MUSE_TRUE );
		_seth(h,r);
		_unwind(sp);		
		_seth( map_args, produce_items( &sequence, &more_to_go ) ); 
	}
	
	while ( more_to_go )
	{
		r = muse_apply( fn, map_args, MUSE_TRUE );
		c = muse_cons( r, MUSE_NIL );
		_sett(t,c);
		t = c;
		_unwind(sp);
		_seth( map_args, produce_items( &sequence, &more_to_go ) ); 
	}
	
	return h;
}

/**
 * (collect list predicate [mapper])
 */
muse_cell fn_collect( muse_env *env, void *context, muse_cell args )
{
	int		  sp		= _spos();
	muse_cell list		= muse_evalnext(&args);
	muse_cell predicate = muse_evalnext(&args);
	muse_cell mapper	= args ? muse_evalnext(&args) : MUSE_NIL;
	muse_cell h			= MUSE_NIL;
	muse_cell t			= MUSE_NIL;
	muse_cell argcell	= MUSE_NIL;
	muse_boolean more_to_go = MUSE_FALSE;
	
	muse_assert( _cellt(list) == MUSE_CONS_CELL && _isfn(predicate) && (!mapper || _isfn(mapper)) );

	argcell = muse_cons( MUSE_NIL, MUSE_NIL );
	_seth( argcell, produce_items( &list, &more_to_go ) );
	
	while ( more_to_go )
	{
		if ( muse_apply( predicate, argcell, MUSE_TRUE ) )
		{
			if ( !h )
			{
				/* Create the result list. */
				h = t = muse_cons( MUSE_NIL, MUSE_NIL );
			}
			else
			{
				/* Add a cell to the tail of the result list. */
				muse_cell temp = muse_cons( MUSE_NIL, MUSE_NIL );
				_sett( t, temp );
				t = temp;
			}
			
			/* If we have a mapper, apply it to the list element. Otherwise 
			act like you're filtering the list using the predicate. */
			_seth( t, mapper ? muse_apply( mapper, argcell, MUSE_TRUE ) : _head(argcell) );
		}
		
		_seth( argcell, produce_items( &list, &more_to_go ) );
	}
	
	_unwind(sp);
	if ( h ) 
		_spush(h);
	return h;
}

/**
 * (find predicate list) -> list.
 *
 * Returns a reference to the first element of the list whose
 * head satisfies the given predicate.
 * 
 * For example -
 * @code
 * (print (find (fn (x) (> x 4)) (list 1 2 3 4 5 6 7 8)))
 * @endcode
 * will print
 * @code
 * (5 6 7 8)
 * @endcode
 */
muse_cell fn_find( muse_env *env, void *context, muse_cell args )
{
	int sp				= _spos();
	muse_cell predicate	= muse_evalnext(&args);
	muse_cell list		= muse_evalnext(&args);
	muse_cell result	= MUSE_NIL;
	
	if ( list )
	{
		muse_cell argcell = muse_cons( MUSE_NIL, MUSE_NIL );
		
		while (list)
		{
			_seth( argcell, _head(list) );
			
			if ( muse_apply( predicate, argcell, MUSE_TRUE ) )
			{
				result = list;
				break;
			}
			else
				list = _tail(list);
		}
	}
	
	_unwind(sp);
	if ( result )
		return _spush(result);
	else
		return MUSE_NIL;
}

/**
 * (andmap predicate list).
 *
 * Evaluates the predicate on each element of the list.
 * Returns T if everything satisfied the predicate and ()
 * if even one element didn't satisfy the predicate. If
 * an element didn't satisfy the predicate, \c andmap 
 * does not evaluate the predicate on subsequent elements.
 */
muse_cell fn_andmap( muse_env *env, void *context, muse_cell args )
{
	muse_cell predicate = muse_evalnext(&args);
	muse_cell list = muse_evalnext(&args);
	int sp = _spos();
	
	while ( list )
	{
		muse_cell p = muse_apply( predicate, _next(&list), MUSE_TRUE );
		if ( !p )
		{
			_unwind(sp);
			return MUSE_NIL;
		}
	}
	
	_unwind(sp);
	return _t();
}

/**
 * (ormap predicate list).
 *
 * Evaluates the predicate on each element of the list.
 * Returns T if anything satisfied the predicate and () if nothing
 * did. If any one element satisfied the predicate, then \c ormap
 * does not evaluate the predicate on the other elements of the list.
 */
muse_cell fn_ormap( muse_env *env, void *context, muse_cell args )
{
	muse_cell predicate = muse_evalnext(&args);
	muse_cell list = muse_evalnext(&args);
	int sp = _spos();
	
	while ( list )
	{
		muse_cell p = muse_apply( predicate, _next(&list), MUSE_TRUE );
		if ( p )
		{
			_unwind(sp);
			return _t();
		}
	}
	
	_unwind(sp);
	return MUSE_NIL;
}

/**
* (for-each fn list [result]).
 *
 * Same as fn_map(), but doesn't collect results into a list.
 * You can optionally give a result expression which will be
 * used as the result of the \c for-each expression.
 */
muse_cell fn_for_each( muse_env *env, void *context, muse_cell args )
{
	muse_cell fn	= muse_evalnext(&args);
	muse_cell list	= muse_evalnext(&args);
	
	while ( list )
	{
		int sp2 = _spos();
		muse_apply( fn, muse_cons( _next(&list), MUSE_NIL ), MUSE_TRUE );
		_unwind(sp2);
	}
	
	return muse_evalnext(&args);
}

/**
 * (transpose -lists-).
 * 
 * Treats the given lists like the rows of a matrix, transposes
 * the lists and returns a list of the rows of the transposed
 * matrix.
 * 
 * For example -
 * @code
 * (transpose '(1 2 3) '(4 5 6)) => '((1 4) (2 5) (3 6))
 * (transpose '(1 4) '(2 5) '(3 6)) => '((1 2 3) (4 5 6))
 * @endcode
 *
 * Not exactly a HOF, but it can be put to good use to
 * implement arbitrary argument mapping like this -
 *
 * @code
 * (define map* (fn (f . args) (map (fn (x) (apply f x)) (apply transpose args)))
 * @endcode
 * 
 * You can use \c map* as follows -
 * @code
 * (print (map* + (list 1 2 3 4) (list 10 20 30 40)))
 * @endcode
 * which is equivalent to 
 * @code
 * (print (list (+ 1 10) (+ 2 20) (+ 3 30) (+ 4 40)))
 * @endcode
 * i.e. the result will be -
 * @code
 * (11 22 33 44)
 * @endcode
 */
muse_cell fn_transpose( muse_env *env, void *context, muse_cell args )
{
	int sp = _spos();
	muse_cell matrix = muse_eval_list(args);
	
	if ( !matrix )
	{
		_unwind(sp);
		return MUSE_NIL;
	}
	else
	{
		int rows = muse_list_length(matrix);
		int cols = muse_list_length(_head(matrix));
		int r, c;
		
		muse_cell *array = (muse_cell*)malloc( rows * cols * sizeof(muse_cell) );
		muse_cell m = matrix;
		
		for ( r = 0; r < rows; ++r )
		{
			muse_list_extract( cols, _next(&m), 1, array + cols * r, 1 );
		}
		
		{
			muse_cell h, t, *m;
			h = t = muse_cons( MUSE_NIL, MUSE_NIL );
			m = array;
			
			_seth( t, muse_array_to_list( rows, m, cols ) );
			
			for ( c = cols-1, ++m; c > 0; --c, ++m )
			{
				muse_cell temp = muse_cons( muse_array_to_list( rows, m, cols ), MUSE_NIL );
				_sett( t, temp );
				t = temp;
			}

			_unwind(sp);
			free(array);
			return _spush(h);
		}
	}
}
