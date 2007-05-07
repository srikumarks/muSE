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

static muse_cell list_iterator( muse_env *env, void *self, muse_iterator_callback_t callback, void *context )
{
	muse_cell list = (muse_cell)self;
	int sp = _spos();
	int cont = MUSE_TRUE;
	
	while ( list )
	{
		cont = callback( env, self, context, _head(list) );
		_unwind(sp);
		if ( !cont )
			return list;
		
		list = _tail(list);
	}	
	
	return MUSE_NIL;
}

static void *get_view( muse_env *env, int view, muse_cell obj, muse_functional_object_t **objptr_out )
{
	muse_functional_object_t *objptr = _fnobjdata(obj);
	
	if ( objptr_out )
		(*objptr_out) = objptr;
	
	if ( objptr && objptr->type_info->view )
		return (muse_iterator_t)objptr->type_info->view( env, view );
	else
		return NULL;	
}

static muse_iterator_t get_iterator_view( muse_env *env, muse_cell obj, muse_functional_object_t **objptr_out )
{
	if ( _cellt(obj) == MUSE_CONS_CELL )
	{
		if ( objptr_out )
			(*objptr_out) = (void*)obj;
		return list_iterator;
	}
	else
		return (muse_iterator_t)get_view( env, 'iter', obj, objptr_out );
}

static muse_monad_view_t *get_monad_view( muse_env *env, muse_cell obj, muse_functional_object_t **objptr_out )
{
	return (muse_monad_view_t*)get_view( env, 'mnad', obj, objptr_out );
}

/**
 * (size obj) or (length obj)
 *
 * Returns the size of the given list (i.e. its length) or vector
 * (same as vector-length) or hashtable (same as hashtable-size).
 */
muse_cell fn_length( muse_env *env, void *context, muse_cell args )
{
	muse_cell obj = _evalnext(&args);
	
	if ( _cellt(obj) == MUSE_CONS_CELL )
	{
		/* Its a list. */
		return _mk_int( _list_length(obj) );
	}
	else
	{
		muse_functional_object_t *objptr = NULL;
		muse_monad_view_t *monad = get_monad_view( env, obj, &objptr );
		
		if ( monad )
			return monad->size( env, objptr );
	}
	
	return MUSE_NIL;
}

static muse_cell list_map( muse_env *env, muse_cell list, muse_cell fn, muse_cell h, muse_cell t )
{
	if ( list )
	{
		{
			int sp = _spos();
			muse_cell val;
			
			_spush(h);

			val = _cons( _apply( fn, 
										 _cons( _head(list), MUSE_NIL ),
										 MUSE_TRUE ),
							 MUSE_NIL );
			
			_unwind(sp);
			
			if ( t )
			{
				_sett( t, val );
				t = val;
			}
			else
				h = t = val;
		}
		
		return list_map( env, _tail(list), fn, h, t );
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
	muse_cell fn = _evalnext(&args);
	muse_cell obj = _evalnext(&args);
	
	if ( _cellt(obj) == MUSE_CONS_CELL )
	{
		/* Map being done on a list. */
		return list_map( env, obj, fn, MUSE_NIL, MUSE_NIL );
	}
	else
	{
		muse_functional_object_t *objptr = NULL;
		muse_monad_view_t *monad = get_monad_view( env, obj, &objptr );
		
		if ( monad )
			return monad->map( env, objptr, fn );		
	}
	
	return MUSE_NIL;
}

struct list_append_generator_context_t
{
	muse_cell lists;
	muse_cell iter;
};

muse_cell list_append_generator( muse_env *env, struct list_append_generator_context_t *ctxt, int i, muse_boolean *eol )
{
	while ( ctxt->lists && !ctxt->iter )
	{
		ctxt->lists = _tail( ctxt->lists );
		ctxt->iter = _head( ctxt->lists );
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

static muse_cell list_join( muse_env *env, muse_cell lists )
{
	struct list_append_generator_context_t ctxt = { lists, _head(lists) };
	return muse_generate_list( env, (muse_list_generator_t)list_append_generator, &ctxt );
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
	muse_cell evaled_args = muse_eval_list( env, args );
	
	muse_cell fn = _head( evaled_args );
	
	if ( _isfn(fn) && !_fnobjdata(fn) )
	{
		/* We have a reduction function as the first argument. */
		evaled_args = _tail( evaled_args );
	}
	else
	{
		/* We don't have a reduction function. */
		fn = MUSE_NIL;
	}
	
	if ( _cellt( _head(evaled_args) ) == MUSE_CONS_CELL )
	{
		return list_join( env, evaled_args );
	}
	else
	{
		muse_cell obj = _head(evaled_args);
		muse_functional_object_t *objptr = NULL;
		muse_monad_view_t *monad = get_monad_view( env, obj, &objptr );
		
		if ( monad )
			return monad->join( env, objptr, _tail(evaled_args), fn );
	}
	
	return MUSE_NIL;
	
}

static muse_cell list_collect( muse_env *env, muse_cell list, muse_cell predicate, muse_cell mapper, muse_cell h, muse_cell t )
{
	if ( list )
	{
		int sp = _spos();
		muse_cell thing = _head(list);
		muse_cell args = MUSE_NIL;
		
		_spush(h);
		args = _cons( thing, MUSE_NIL );
		
		if ( !predicate || _apply( predicate, args, MUSE_TRUE ) )
		{
			if ( mapper )
				_seth( args, _apply( mapper, args, MUSE_TRUE ) );
			
			if ( t )
			{
				_sett( t, args );
				t = args;
			}
			else
				h = t = args;
		}

		_unwind(sp);
		
		return list_collect( env, _tail(list), predicate, mapper, h, t );
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
	muse_cell obj = _evalnext(&args);
	muse_cell predicate = _evalnext(&args);
	muse_cell mapper = _evalnext(&args);
	
	if ( _cellt(obj) == MUSE_CONS_CELL )
	{
		return list_collect( env, obj, predicate, mapper, MUSE_NIL, MUSE_NIL );
	}
	else
	{
		muse_functional_object_t *objptr = NULL;
		muse_monad_view_t *monad = get_monad_view( env, obj, &objptr );
		
		if ( monad )
			return monad->collect( env, objptr, predicate, mapper, _evalnext(&args) );
	}

	return MUSE_NIL;
}

static muse_cell list_reduce( muse_env *env, muse_cell obj, muse_cell reduction_fn, muse_cell acc )
{
	if ( obj )
	{
		{
			int sp = _spos();
			
			_spush(acc);
			
			acc = _apply( reduction_fn, _cons( acc, _cons( _head(obj), MUSE_NIL ) ), MUSE_TRUE );
		
			_unwind(sp);
		}
		
		return list_reduce( env, _tail(obj), reduction_fn, acc );
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
 *
 * For convenience, the reduction function is expected to be 
 * associative and commutative. If the function isn't so, then
 * know that the first argument of the function is the accumulator.
 * i.e. @code (reduce f init (list 1 2 3 4 5)) @endcode
 * will give you -
 * @code (f (f (f (f (f init 1) 2) 3) 4) 5) @endcode
 */
muse_cell fn_reduce( muse_env *env, void *context, muse_cell args )
{
	muse_cell fn		= _evalnext(&args);
	muse_cell initial	= _evalnext(&args);
	muse_cell obj		= _evalnext(&args);
	
	if ( _cellt(obj) == MUSE_CONS_CELL )
		return list_reduce( env, obj, fn, initial );
	else
	{
		muse_functional_object_t *objptr = NULL;
		muse_monad_view_t *monad = get_monad_view( env, obj, &objptr );
		
		if ( monad )
			return monad->reduce( env, objptr, fn, initial );
	}
	
	return MUSE_NIL;
}


static muse_boolean finder( muse_env *env, void *self, void *what, muse_cell thing )
{
	return muse_equal( env, (muse_cell)what, thing ) ? MUSE_FALSE : MUSE_TRUE;
}

/**
 * (find item collection) -> reference.
 *
 * \c find can be used to locate an object in a list, vector or hashtable.
 * It uses muse_equal() to determine whether an object is present in the
 * collection. 
 *
 * When used with a list as the collection, it evaluates to
 * the sublist with the given object at the head - or \c () if
 * the object is not present in the list.
 *
 * When used with a vector, it evaluates to the index of the object
 * in the vector if it is present in the vector, or to \c () if it isn't.
 *
 * When used with a hashtable, it evaluates to the key for which the
 * object is the value in the hashtable. If the object isn't there, it
 * evaluates to \c ()/
 * 
 * List example -
 * @code
 * (print "Sub list = " (find 5 (list 1 2 3 4 5 6 7 8)))
 * (print "Vector pos = " (find 5 (vector 1 2 3 4 5 6 7 8)))
 * @endcode
 * will print
 * @code
 * Sub list = (5 6 7 8)
 * Vector pos = 4
 * @endcode
 */
muse_cell fn_find( muse_env *env, void *context, muse_cell args )
{
	muse_cell object	= _evalnext(&args);
	muse_cell coll		= _evalnext(&args);
	muse_cell result	= MUSE_NIL;
	
	muse_functional_object_t *collObj	= NULL;
	muse_iterator_t iter				= get_iterator_view( env, coll, &collObj );
	
	if ( iter )
	{
		result = iter( env, collObj, finder, (void*)object );
	}
	
	return result;
}

typedef struct
{
	muse_cell fn;
	muse_cell temp;
} mapinfo_t;

static muse_boolean domapper( muse_env *env, void *self, mapinfo_t *info, muse_cell thing )
{
	_seth( info->temp, thing );
	_apply( info->fn, info->temp, MUSE_TRUE );
	return MUSE_TRUE;
}

static muse_boolean andmapper( muse_env *env, void *self, mapinfo_t *info, muse_cell thing )
{
	_seth( info->temp, thing );
	return _apply( info->fn, info->temp, MUSE_TRUE ) ? MUSE_TRUE : MUSE_FALSE;
}

static muse_boolean ormapper( muse_env *env, void *self, mapinfo_t *info, muse_cell thing )
{
	_seth( info->temp, thing );
	return _apply( info->fn, info->temp, MUSE_TRUE ) ? MUSE_FALSE : MUSE_TRUE;
}

/**
 * (andmap predicate collection).
 *
 * Evaluates the predicate on each element of the collection.
 * The collection can be a list, vector or a hashtable.
 * Returns T if everything satisfied the predicate and ()
 * if even one element didn't satisfy the predicate. If
 * an element didn't satisfy the predicate, \c andmap 
 * does not evaluate the predicate on subsequent elements.
 *
 * The predicate is a \c fn(x) and is given the value objects in 
 * lists, vectors or hashtables.
 */
muse_cell fn_andmap( muse_env *env, void *context, muse_cell args )
{
	muse_cell predicate = _evalnext(&args);
	muse_cell list = _evalnext(&args);
	
	mapinfo_t info = { predicate, _cons(MUSE_NIL, MUSE_NIL) };
	muse_functional_object_t *collObj = NULL;
	muse_iterator_t iter = get_iterator_view( env, list, &collObj );
	if ( iter )
	{
		return iter( env, collObj, (muse_iterator_callback_t)andmapper, &info ) ? MUSE_NIL : _t();
	}
	
	return MUSE_NIL;
}

/**
 * (ormap predicate collection).
 *
 * Evaluates the predicate on each element of the collection.
 * The collection can be a list, vector or a hashtable.
 * Returns T if anything satisfied the predicate and () if nothing
 * did. If any one element satisfied the predicate, then \c ormap
 * does not evaluate the predicate on the other elements of the list.
 *
 * The predicate is a \c fn(x) and is given the value objects in 
 * lists, vectors or hashtables.
 */
muse_cell fn_ormap( muse_env *env, void *context, muse_cell args )
{
	muse_cell predicate = _evalnext(&args);
	muse_cell list = _evalnext(&args);
	
	mapinfo_t info = { predicate, _cons(MUSE_NIL, MUSE_NIL) };
	muse_functional_object_t *collObj = NULL;
	muse_iterator_t iter = get_iterator_view( env, list, &collObj );
	if ( iter )
	{
		return iter( env, collObj, (muse_iterator_callback_t)ormapper, &info );
	}
	
	return MUSE_NIL;
}

 
/**
 * (for-each fn collection [result]).
 *
 * Same as fn_map(), but doesn't collect results.
 * The collection can be a list, vector or a hashtable.
 * You can optionally give a result expression which will be
 * used as the result of the \c for-each expression.
 *
 * The operation is a \c fn(x) and is given the value objects
 * of lists, vectors or hashtables.
 */
muse_cell fn_for_each( muse_env *env, void *context, muse_cell args )
{
	muse_cell fn = _evalnext(&args);
	muse_cell list = _evalnext(&args);
	
	mapinfo_t info = { fn, _cons(MUSE_NIL, MUSE_NIL) };
	muse_functional_object_t *collObj = NULL;
	muse_iterator_t iter = get_iterator_view( env, list, &collObj );
	if ( iter )
	{
		iter( env, collObj, (muse_iterator_callback_t)domapper, &info );
	}
	
	return _evalnext(&args);
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
	muse_cell matrix = muse_eval_list(env,args);
	
	if ( !matrix )
	{
		_unwind(sp);
		return MUSE_NIL;
	}
	else
	{
		int rows = _list_length(matrix);
		int cols = _list_length(_head(matrix));
		int r, c;
		
		muse_cell *array = (muse_cell*)malloc( rows * cols * sizeof(muse_cell) );
		muse_cell m = matrix;
		
		for ( r = 0; r < rows; ++r )
		{
			muse_list_extract( env, cols, _next(&m), 1, array + cols * r, 1 );
		}
		
		{
			muse_cell h, t, *m;
			h = t = _cons( MUSE_NIL, MUSE_NIL );
			m = array;
			
			_seth( t, muse_array_to_list( env, rows, m, cols ) );
			
			for ( c = cols-1, ++m; c > 0; --c, ++m )
			{
				muse_cell temp = _cons( muse_array_to_list( env, rows, m, cols ), MUSE_NIL );
				_sett( t, temp );
				t = temp;
			}

			_unwind(sp);
			free(array);
			return _spush(h);
		}
	}
}

/**
 * (funcspec [vector|hashtable] (fn ([key|index]) ...))
 *
 * Establishes that the entries in the vector or the hashtable should
 * be determined by the given function. The function is only computed
 * on demand.
 */
muse_cell fn_funcspec( muse_env *env, void *context, muse_cell args )
{
	muse_cell obj = _evalnext(&args);
	muse_cell funcspec = _evalnext(&args);

	muse_functional_object_t *objPtr = NULL;
	muse_funcspec_t funcspec_impl = (muse_funcspec_t)get_view( env, 'spec', obj, &objPtr );
	if ( funcspec_impl )
	{
		funcspec_impl( env, objPtr, funcspec );
		return muse_builtin_symbol(env, MUSE_T);
	}
	else
	{
		MUSE_DIAGNOSTICS({
			muse_message( env, L"(funcspec >>object<< fn)", L"Object %m doesn't support the 'spec' view!", obj );
		});
		return MUSE_NIL;
	}
}