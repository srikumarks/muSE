/**
 * @file muse_builtin_vector.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-scheme.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * Implements functional vectors for constant time random access
 * to a collection of objects.
 */

#include "muse_builtins.h"
#include <stdlib.h>

/** @addtogroup FunctionalObjects */
/*@{*/
/**
 * @defgroup Vectors
 *
 * A functional vector is a fixed size array of cells.
 * A vector can be used like a function. When given one argument which should
 * be an index, it'll return the vaue of the slot at that index. When given
 * two arguments where the first argument is an index, it sets the slot
 * at the given index to the value determined by the second argument.
 * 
 * Examples -
 * @code
 * (define vec (mk-vector 5))
 * (print (vector-length vec))
 *      > 5
 * (vec 3 'three)
 * (vec 0 'zero)
 * (vec 1 'one)
 * (vec 2 'two)
 * (vec 4 'four)
 * (print (vec 2))
 *      > two
 * (print (vector->list vec))
 *      > (zero one two three four)
 * (print (vector->list vec 3 2))
 *      > (three four)
 * @endcode
 */
/*@{*/

typedef struct 
{
	muse_functional_object_t base;
	int length;
	muse_cell *slots;
} vector_t;

static void vector_init_with_length( void *ptr, int length )
{
	vector_t *v = (vector_t*)ptr;
	if ( length > 0 )
	{
		v->length = length;
		v->slots = (muse_cell*)calloc( v->length, sizeof(muse_cell) );
	}
}

static void vector_init( void *ptr, muse_cell args )
{
	vector_init_with_length( ptr, args ? (int)muse_int_value(muse_evalnext(&args)) : 0 );
}

static void vector_mark( void *ptr )
{
	vector_t *v = (vector_t*)ptr;
	muse_cell *cptr = v->slots;
	muse_cell *cptr_env = cptr + v->length;

	while ( cptr < cptr_env )
	{
		muse_mark( *cptr++ );
	}
}

static void vector_destroy( void *ptr )
{
	vector_t *v = (vector_t*)ptr;
	free(v->slots);
	v->length = 0;
	v->slots = NULL;
}

/**
 * The function that implements vector slot access.
 */
muse_cell fn_vector( muse_env *env, vector_t *v, muse_cell args )
{
	if ( args )
	{
		int index = (int)muse_int_value(muse_evalnext(&args));

		muse_assert( index >= 0 && index < v->length );

		if ( args )
		{
			/* We're setting a slot. */
			muse_cell result = muse_evalnext(&args);
			v->slots[index] = result;
			return result;
		}
		else
		{
			/* We're getting a slot. */
			return v->slots[index];
		}
	}

	return MUSE_NIL;
}

static muse_functional_object_type_t g_vector_type =
{
	'muSE',
	'vect',
	sizeof(vector_t),
	(muse_nativefn_t)fn_vector,
	vector_init,
	vector_mark,
	vector_destroy
};

/**
 * (mk-vector N).
 * Creates a new vector of length N. All slots in the vector
 * are initially NIL. The returned object is a nativefn
 * (functional object). If \c vec is the returned object,
 * then @code (vec i) @endcode yields the value at the slot
 * \c i and @code (vec i val) @endcode sets the value at
 * the slot \c i to \c val and returns \c val.
 */
muse_cell fn_mk_vector( muse_env *env, void *context, muse_cell args )
{
	return muse_mk_functional_object( &g_vector_type, args ); 
}

/**
 * (vector a1 a2 a3 --- aN).
 * Makes an N-length vector from the arguments with the arguments
 * as the initial values. Useful and compact for small vectors.
 */
muse_cell fn_vector_from_args( muse_env *env, void *context, muse_cell args )
{
	int i, length			= muse_list_length(args);
	muse_cell length_arg	= muse_cons( muse_mk_int( length ), MUSE_NIL );
	muse_cell vec			= muse_mk_functional_object( &g_vector_type, length_arg );
	vector_t *v				= (vector_t*)muse_functional_object_data(vec,'vect');

	for ( i = 0; i < length; ++i )
	{
		v->slots[i] = muse_evalnext(&args);
	}

	return vec;
}

/**
 * (vector? fv).
 * Returns fv if it is a functional vector. Returns () if it isn't.
 */
muse_cell fn_vector_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell fv = muse_evalnext(&args);
	vector_t *v = (vector_t*)muse_functional_object_data( fv, 'vect' );

	return v ? fv : MUSE_NIL;
}

/**
 * (vector-length v).
 * Evaluates to the length of the given functional vector.
 */
muse_cell fn_vector_length( muse_env *env, void *context, muse_cell args )
{
	return muse_mk_int( muse_vector_length( muse_evalnext(&args) ) );
}

/**
 * (list->vector ls).
 * Converts the given list into a vector and returns the vector.
 */
muse_cell fn_list_to_vector( muse_env *env, void *context, muse_cell args )
{
	muse_cell list = muse_evalnext(&args);
	int length = muse_list_length(list);
	
	if ( length > 0 )
	{
		muse_cell fv = fn_mk_vector( env, NULL, MUSE_NIL );
		vector_t *v = (vector_t*)muse_functional_object_data(fv,'vect');

		vector_init_with_length( v, length );

		muse_list_extract( length, list, 1, v->slots, 1 );

		return fv;
	}
	else
		return MUSE_NIL;
}

/**
 * (vector->list fv [from count step]).
 * Given a functional vector, it returns a list of elements of the vector.
 * If no index range is given, the entire vector is converted into
 * a list. If an index range or step is given, the elements in the
 * range with the given step are converted. The conversion will
 * start with index \c i and end with index \c j-1, in steps of \c step.
 */
muse_cell fn_vector_to_list( muse_env *env, void *context, muse_cell args )
{
	muse_cell fv = muse_evalnext(&args);
	vector_t *v = (vector_t*)muse_functional_object_data(fv,'vect');
	muse_assert( v != NULL && "First argument must be a functional vector!" );

	{
		int from	= 0;
		int count	= v->length;
		int step	= 1;

		if ( args ) from	= (int)muse_int_value(muse_evalnext(&args));
		
		/* Make sure count stays within valid limits even if
		it isn't specified explcitly. */
		count = v->length - from;
		
		if ( args ) count	= (int)muse_int_value(muse_evalnext(&args));
		if ( args ) step	= (int)muse_int_value(muse_evalnext(&args));

		muse_assert( count >= 0 && from >= 0 && from + step * count <= v->length );

		return muse_array_to_list( count, v->slots + from, step );
	}
}


static const struct vector_fns_t { const muse_char *name; muse_nativefn_t fn; } g_vector_fns[] =
{
	{	L"mk-vector",			fn_mk_vector		},
	{	L"vector",				fn_vector_from_args	},
	{	L"vector?",				fn_vector_p			},
	{	L"vector-length",		fn_vector_length	},
	{	L"vector->list",		fn_vector_to_list	},
	{	L"list->vector",		fn_list_to_vector	},
	{	NULL,					NULL				},
};

void muse_define_builtin_type_vector()
{
	int sp = _spos();
	const struct vector_fns_t *fns = g_vector_fns;
	for ( ; fns->name; ++fns )
	{
		muse_define( muse_csymbol(fns->name), muse_mk_nativefn( fns->fn, NULL ) );
		_unwind(sp);
	}
}

/**
 * Creates a new vector object that has enough slots allocated to hold
 * the given number of objects. All slots are initialized to MUSE_NIL.
 */
muse_cell muse_mk_vector( int length )
{
	muse_assert( length >= 0 );
	{
		int sp = _spos();
		muse_cell result = fn_mk_vector( _env(), NULL, muse_cons( muse_mk_int(length), MUSE_NIL ) );
		_unwind(sp);
		_spush(result);
		return result;
	}
}

/**
 * Returns the number of slots the vector has.
 */
int muse_vector_length( muse_cell vec )
{
	vector_t *v = (vector_t*)muse_functional_object_data( vec, 'vect' );
	muse_assert( v != NULL && "v must be a vector!" );
	return v ? v->length : 0;
}

/**
 * Returns the value occupying the slot at the given 0-based index.
 */
muse_cell muse_vector_get( muse_cell vec, int index )
{
	vector_t *v = (vector_t*)muse_functional_object_data( vec, 'vect' );
	muse_assert( v != NULL && "v must be a vector!" );
	if ( v )
	{
		muse_assert( index >= 0 && index < v->length );
		return v->slots[index];
	}
	else
		return MUSE_NIL;
}

/**
 * Replaces the value in the slot at the given index with the new value.
 */
muse_cell muse_vector_put( muse_cell vec, int index, muse_cell value )
{
	vector_t *v = (vector_t*)muse_functional_object_data( vec, 'vect' );
	muse_assert( v != NULL && "v must be a vector!");
	if ( v )
	{
		muse_assert( index >= 0 && index < v->length );
		v->slots[index] = value;
		return value;
	}
	else
		return MUSE_NIL;
}

/*@}*/
/*@}*/

