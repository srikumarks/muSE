/**
 * @file muse_builtin_HOF.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_builtins.h"
#include <stdlib.h>

static muse_monad_view_t *get_monad_view( muse_cell obj, muse_functional_object_t **objptr_out )
{
	muse_functional_object_t *objptr = _fnobjdata(obj);
		
	if ( objptr_out )
		(*objptr_out) = objptr;
	
	if ( objptr && objptr->type_info->view )
		return (muse_monad_view_t*)objptr->type_info->view( 'mnad' );
	else
		return NULL;
}

/**
 * (size obj)
 *
 * Returns the size of the given list (i.e. its length) or vector
 * (same as vector-length) or hashtable (same as hashtable-size).
 */
muse_cell fn_size( muse_env *env, void *context, muse_cell args )
{
	muse_cell obj = muse_evalnext(&args);
	
	if ( _cellt(obj) == MUSE_CONS_CELL )
	{
		/* Its a list. */
		return muse_mk_int( muse_list_length(obj) );
	}
	else
	{
		muse_functional_object_t *objptr = NULL;
		muse_monad_view_t *monad = get_monad_view( obj, &objptr );
		
		if ( monad )
			return monad->size( objptr );
	}
	
	return MUSE_NIL;
}

static muse_cell list_map( muse_cell list, muse_cell fn, muse_cell h, muse_cell t )
{
	if ( list )
	{
		{
			int sp = muse_stack_pos();
			
			muse_stack_push(h);

			muse_cell val = muse_cons( muse_apply( fn, 
												   muse_cons( muse_head(list), MUSE_NIL ),
												   MUSE_TRUE ),
									   MUSE_NIL );
			
			muse_stack_unwind(sp);
			
			if ( t )
			{
				muse_set_tail( t, val );
				t = val;
			}
			else
				h = t = val;
		}
		
		return list_map( muse_tail(list), fn, h, t );
	}
	else
	{
		return h;
	}
}

/**
 * (map fn obj).
 * The object can be a list, vector or hashtable and the return value will
 * be of the corresponding type. When the object is a list, the function
 * is expected to take a single argument and provide a mapped value
 * that is collected into 
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
 *
 * map also works with vectors and hashtables. In those cases, it transforms
 * the "values" of those data structures and creates an isomorphic structure 
 * with the transformed values.
 */
muse_cell fn_map( muse_env *env, void *context, muse_cell args )
{
	muse_cell fn = muse_evalnext(&args);
	muse_cell obj = muse_evalnext(&args);
	
	if ( _cellt(obj) == MUSE_CONS_CELL )
	{
		/* Map being done on a list. */
		return list_map( obj, fn, MUSE_NIL, MUSE_NIL );
	}
	else
	{
		muse_functional_object_t *objptr = NULL;
		muse_monad_view_t *monad = get_monad_view( obj, &objptr );
		
		if ( monad )
			return monad->map( objptr, fn );		
	}
	
	return MUSE_NIL;
}

struct list_append_generator_context_t
{
	muse_cell lists;
	muse_cell iter;
};

muse_cell list_append_generator( struct list_append_generator_context_t *ctxt, int i, muse_boolean *eol )
{
	while ( ctxt->lists && !ctxt->iter )
	{
		ctxt->lists = muse_tail( ctxt->lists );
		ctxt->iter = muse_head( ctxt->lists );
	}
	
	if ( ctxt->lists )
	{
		(*eol) = MUSE_FALSE;
		return _next( &(ctxt->iter) );
	}
	else
	{
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
}

static muse_cell list_join( muse_cell lists )
{
	struct list_append_generator_context_t ctxt = { lists, muse_head(lists) };
	return muse_generate_list( (muse_list_generator_t)list_append_generator, &ctxt );
}

/**
 * (join [reduction-fn] obj1 obj2 ...)
 *
 * Joins the given objects, all of which must be of the same type.
 * Objects can be lists, vectors or hashtables. 
 *
 * In the case of lists, the lists are all appended in the given
 * order and a new list that is a concatenation of the given lists
 * results.
 *
 * In the case of vectors, they are all joined end to end to
 * create a new vector whose length is the sum of the lengths of
 * the individual vectors.
 *
 * In the case of hashtables, a new hashtable with all the key-value 
 * pairs in all the given hashtables is created. If two hashtables
 * contain the same key, the value stored in the later specified 
 * hashtable takes precedence over the value stored in an earlier 
 * one. The join function optionally accepts a reduction function
 * as its first argument. When such a function is given, it is used
 * to combined the values associated with the same key in multiple
 * hashtables. If for a given key K the hashtables has values 
 * V1, V2, ... VN, then the result hashtable will have the value
 * R(..R(R(V1, V2), V3)...,VN) where R is the reduction function.
 */
muse_cell fn_join( muse_env *env, void *context, muse_cell args )
{
	muse_cell evaled_args = muse_eval_list( args );
	
	muse_cell fn = muse_head( evaled_args );
	
	if ( _isfn(fn) && !_fnobjdata(fn) )
	{
		/* We have a reduction function as the first argument. */
		evaled_args = muse_tail( evaled_args );
	}
	else
	{
		/* We don't have a reduction function. */
		fn = MUSE_NIL;
	}
	
	if ( _cellt( muse_head(evaled_args) ) == MUSE_CONS_CELL )
	{
		return list_join( evaled_args );
	}
	else
	{
		muse_cell obj = muse_head(evaled_args);
		muse_functional_object_t *objptr = NULL;
		muse_monad_view_t *monad = get_monad_view( obj, &objptr );
		
		if ( monad )
			return monad->join( objptr, muse_tail(evaled_args), fn );
	}
	
	return MUSE_NIL;
	
}

static muse_cell list_collect( muse_cell list, muse_cell predicate, muse_cell mapper, muse_cell h, muse_cell t )
{
	if ( list )
	{
		int sp = muse_stack_pos();
		muse_cell thing = muse_head(list);
		muse_cell args = MUSE_NIL;
		
		muse_stack_push(h);
		args = muse_cons( thing, MUSE_NIL );
		
		if ( !predicate || muse_apply( predicate, args, MUSE_TRUE ) )
		{
			if ( mapper )
				muse_set_head( args, muse_apply( mapper, args, MUSE_TRUE ) );
			
			if ( t )
			{
				muse_set_tail( t, args );
				t = args;
			}
			else
				h = t = args;
		}

		muse_stack_unwind(sp);
		
		return list_collect( muse_tail(list), predicate, mapper, h, t );
	}
	else
	{
		return h;
	}
}

/**
 * (collect obj predicate mapper [reduction-fn])
 * EXPERIMENTAL
 * Intended for more general iteration over the collection objects.
 * The \p obj is either a list or a vector or a hashtable.
 *
 * The \p predicate is a function that selects items from the 
 * object. For lists, the predicate is a f(x) where x is a list
 * item. For vectors, the predicate is a f(i . x) where i is
 * the index and x is the value of the vector at the index. For
 * hashtables, the predicate is a f(k . v) where k is a key and v its
 * corresponding value in the hashtable.
 *
 * The \p mapper is a function that transforms items into
 * other items. For lists, the mapper is a f(x) where x is a
 * list item. For vectors, the mapper is a f(i . x) where i is
 * the index and x is the value at the index. The result
 * is expected to be a similar (j . y) pair. The result vector
 * is automatically resized to fit the result indices. 
 * For hashtables, the mapper is a f(k . v) where k is a key
 * and v is its corresponding value in the hashtable. Similar
 * to vectors, the mapper has to evaluate to a (k' . v') pair
 * that will be placed into the result hashtable.
 *
 * The optional \p reduction-fn is a f(v1 v2) that is used to
 * combined multiple values that may be assigned to the same index
 * or key in the cases of vectors and hashtable respectively. If the
 * reduction function is not specified, f(v1 v2) = v2 is used as the
 * function - i.e. replacement.
 */
muse_cell fn_collect( muse_env *env, void *context, muse_cell args )
{
	muse_cell obj = muse_evalnext(&args);
	muse_cell predicate = muse_evalnext(&args);
	muse_cell mapper = muse_evalnext(&args);
	
	if ( _cellt(obj) == MUSE_CONS_CELL )
	{
		return list_collect( obj, predicate, mapper, MUSE_NIL, MUSE_NIL );
	}
	else
	{
		muse_functional_object_t *objptr = NULL;
		muse_monad_view_t *monad = get_monad_view( obj, &objptr );
		
		if ( monad )
			return monad->collect( objptr, predicate, mapper, muse_evalnext(&args) );
	}

	return MUSE_NIL;
}

static muse_cell list_reduce( muse_cell obj, muse_cell reduction_fn, muse_cell acc )
{
	if ( obj )
	{
		{
			int sp = muse_stack_pos();
			
			muse_stack_push(acc);
			
			acc = muse_apply( reduction_fn, muse_cons( acc, muse_cons( muse_head(obj), MUSE_NIL ) ), MUSE_TRUE );
		
			muse_stack_unwind(sp);
		}
		
		return list_reduce( muse_tail(obj), reduction_fn, acc );
	}
	else
		return acc;
}

/**
 * (reduce fn initial obj)
 *
 * Reduces the values of the given collection \p obj using the given
 * reduction function and the \p initial value. You can use it with
 * lists, vectors and hashtables. The reduction occurs only over the
 * values of the collection. Indices (in the case of vectors) and keys
 * (in the case of hashtables) are not touched at all.
 */
muse_cell fn_reduce( muse_env *env, void *context, muse_cell args )
{
	muse_cell fn		= muse_evalnext(&args);
	muse_cell initial	= muse_evalnext(&args);
	muse_cell obj		= muse_evalnext(&args);
	
	if ( _cellt(obj) == MUSE_CONS_CELL )
		return list_reduce( obj, fn, initial );
	else
	{
		muse_functional_object_t *objptr = NULL;
		muse_monad_view_t *monad = get_monad_view( obj, &objptr );
		
		if ( monad )
			return monad->reduce( objptr, fn, initial );
	}
	
	return MUSE_NIL;
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
