/**
 * @file muse_builtin_vector.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * Implements functional vectors for constant time random access
 * to a collection of objects.
 */

#include "muse_builtins.h"
#include "muse_port.h"
#include <stdlib.h>
#include <memory.h>

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
	muse_cell datafn;
} vector_t;

static muse_cell vector_force( muse_env *env, vector_t *v, int index, muse_cell val );
static void vector_forceall( muse_env *env, vector_t *v );

static vector_t *vector_init_with_length( void *ptr, int length )
{
	vector_t *v = (vector_t*)ptr;
	if ( length > 0 )
	{
		v->length = length;
		v->slots = (muse_cell*)calloc( v->length, sizeof(muse_cell) );
	}
	
	return v;
}

static void vector_init( muse_env *env, void *ptr, muse_cell args )
{
	vector_t *v = vector_init_with_length( ptr, args ? (int)_intvalue(_evalnext(&args)) : 0 );
	if ( args ) {
		muse_cell datafn = _evalnext(&args);
		if ( _isfn(datafn) ) {
			v->datafn = datafn;
		}
	}
}

static void vector_mark( muse_env *env, void *ptr )
{
	vector_t *v = (vector_t*)ptr;
	muse_cell *cptr = v->slots;
	muse_cell *cptr_env = cptr + v->length;

	while ( cptr < cptr_env )
	{
		muse_mark( env, *cptr++ );
	}

	muse_mark( env, v->datafn );
}

static void vector_destroy( muse_env *env, void *ptr )
{
	vector_t *v = (vector_t*)ptr;
	free(v->slots);
	v->length = 0;
	v->slots = NULL;
}

/**
 * Writes out the vector to the given port in such a
 * way that the expression written out is converted
 * to a vector by a trusted read operation.
 */
static void vector_write( muse_env *env, void *ptr, void *port )
{
	vector_t *v = (vector_t*)ptr;
	muse_port_t p = (muse_port_t)port;
	
	port_putc( '{', p );
	port_write( "vector", 6, p );	
	
	{
		int i;
		for ( i = 0; i < v->length; ++i )
		{
			port_putc( ' ', p );
			muse_pwrite( p, vector_force( env, v, i, v->slots[i] ) );
		}
	}
	
	port_putc( '}', p );
}

static muse_cell vector_size( muse_env *env, void *self )
{
	return _mk_int( ((vector_t*)self)->length );
}

static void vector_resize( vector_t *self, int new_size )
{
	if ( self->length >= new_size )
		return;
	
	self->slots = (muse_cell*)realloc( self->slots, sizeof(muse_cell) * new_size );
	memset( self->slots + self->length, 0, sizeof(muse_cell) * (new_size - self->length) );
	self->length = new_size;
}

/**
 * Forces the slot at the given index. If the vector is specified functionally,
 * and the slot doesn't have a value yet, the function is used to compute the value.
 * 
 * @param index The index of a slot in the vector to force the value.
 * @param val The current value of the slot or MUSE_NIL if slot doesn't exist.
 */
static muse_cell vector_force( muse_env *env, vector_t *v, int index, muse_cell val )
{
	if ( !val )
	{
		if ( v->datafn )
		{
			int sp = _spos();
			val = _force(muse_apply( env, v->datafn, _cons(_mk_int(index),MUSE_NIL), MUSE_TRUE, MUSE_TRUE )); 
			_unwind(sp);
		}

		if ( index >= v->length && val )
			vector_resize( v, index+1 );
	}

	return v->slots[index] = val;
}


/**
 * Forces all the slots of the vector in index order.
 */
static void vector_forceall( muse_env *env, vector_t *v )
{
	if ( v->datafn )
	{
		int i = 0;
		for ( i = 0; i < v->length; ++i )
		{
			vector_force( env, v, i, v->slots[i] );
		}
	}
}

/**
 * The function that implements vector slot access.
 */
muse_cell fn_vector( muse_env *env, vector_t *v, muse_cell args )
{
	int indexcell = _evalnext(&args);
	int index = (int)_intvalue(indexcell);
	muse_cell *slot = NULL;

	muse_assert( index >= 0 );

	if ( index < v->length )
	{
		slot = v->slots + index;
	}

	if ( args )
	{
		/* Set value. */
		muse_cell newval = _evalnext(&args);

		if ( newval )
		{
			if ( !slot )
			{
				vector_resize( v, index+1 );
				slot = v->slots + index;
			}

			muse_assert( slot != NULL );
			(*slot) = newval;
		}
		
		return newval;
	}
	else
	{
		/* Get value. */
		if ( slot && *slot )
			return *slot;
		else
			return vector_force( env, v, index, MUSE_NIL );
	}
}


static void vector_merge_one( muse_env *env, vector_t *v, int i, muse_cell new_value, muse_cell reduction_fn )
{
	if ( reduction_fn && v->slots[i] )
	{
		/* Value exists at the specified slot. */
		v->slots[i] = 
			_apply( reduction_fn,
						_cons( v->slots[i],
								   _cons( new_value,
											  MUSE_NIL ) ),
						MUSE_TRUE );
	}
	else
	{
		/* No reduction function, or the slot is empty. */
		v->slots[i] = new_value;						
	}	
}

static void vector_trim( vector_t *v )
{
	while ( v->length > 0 && !v->slots[v->length - 1] )
		--(v->length);
}

static muse_cell vector_map( muse_env *env, void *self, muse_cell fn )
{
	vector_t *v = (vector_t*)self;
	
	muse_cell result = muse_mk_vector( env, v->length );
	vector_t *result_ptr = (vector_t*)_functional_object_data( result, 'vect' );
	
	muse_cell args = _cons( MUSE_NIL, MUSE_NIL );
	
	int sp = _spos();
	int i = 0;
	for ( i = 0; i < v->length; ++i )
	{
		/* Initialize the arguments to the mapper function. */
		_seth( args, vector_force( env, v, i, v->slots[i] ) );
		
		result_ptr->slots[i] = _apply( fn, args, MUSE_TRUE );
		
		_unwind(sp);
	}
	
	if ( v->datafn != MUSE_NIL ) {
		/* Apply the mapper to the datafn so that the new vector's
		elements are generated accordingly. */
		result_ptr->datafn = muse_eval( env, muse_list( env, "S(S)(c(cS))", 
															 L"fn", L"$x", 
															   fn, v->datafn, L"$x" ),
										     MUSE_FALSE );
	}
	
	return result;
}

static muse_cell vector_join( muse_env *env, void *self, muse_cell objlist, muse_cell reduction_fn )
{
	vector_t *v1 = (vector_t*)self;
	muse_cell result;
	vector_t *result_ptr;
	
	/* Compute the required total length. */
	int total_length = v1->length;
	{
		muse_cell temp_objlist = objlist;
		while ( temp_objlist )
		{
			muse_cell obj = _next(&temp_objlist);
			vector_t *v2 = (vector_t*)_functional_object_data( obj, 'vect' );
			total_length += v2->length;
		}
	}
	
	{
		result = muse_mk_vector( env, total_length );
		result_ptr = (vector_t*)_functional_object_data( result, 'vect' );
		vector_forceall( env, v1 );
		
		memcpy( result_ptr->slots, v1->slots, sizeof(muse_cell) * v1->length );
		
		{
			int offset = v1->length;
			while ( objlist )
			{
				muse_cell obj = _next(&objlist);
				vector_t *v2 = (vector_t*)_functional_object_data( obj, 'vect' );
				vector_forceall( env, v2 );
				memcpy( result_ptr->slots + offset, v2->slots, sizeof(muse_cell) * v2->length );
				offset += v2->length;
			}
		}
	}
	
	return result;	
}

static muse_cell vector_collect( muse_env *env, void *self, muse_cell predicate, muse_cell mapper, muse_cell reduction_fn )
{
	vector_t *v1 = (vector_t*)self;
	
	muse_cell result = muse_mk_vector( env, v1->length );
	vector_t *result_ptr = (vector_t*)_functional_object_data( result, 'vect' );
	
	muse_cell args = _cons( MUSE_NIL, MUSE_NIL );
	
	{
		int sp = _spos();
		int i, j;
		for ( i = 0, j = 0; i < v1->length; ++i )
		{
			_setht( args, _mk_int(i), v1->slots[i] );
			
			if ( !predicate || _apply( predicate, args, MUSE_TRUE ) )
			{
				if ( mapper )
				{
					muse_cell m;
					
					_seth( args, _mk_int(j) );
					m = _apply( mapper, args, MUSE_TRUE );
					
					if ( m )
					{
						muse_int new_ix = _intvalue( _head(m) );
						
						vector_resize( result_ptr, (int)(new_ix + 1) );
						
						vector_merge_one( env, result_ptr, (int)new_ix, _tail(m), reduction_fn );
					}
				}
				else
				{
					vector_merge_one( env, result_ptr, j, v1->slots[i], reduction_fn );
				}

				++j;
			}
			
			_unwind(sp);			
		}
	}
	
	vector_trim( result_ptr );
	return result;
}

static muse_cell vector_reduce( muse_env *env, void *self, muse_cell reduction_fn, muse_cell initial )
{
	vector_t *v = (vector_t*)self;
	
	muse_cell result = initial;
	
	vector_forceall( env, v );

	{
		int sp = _spos();
		int i;
		
		_spush(result);
		
		for ( i = 0; i < v->length; ++i )
		{
			result = _apply( reduction_fn,
								 _cons( result, _cons( v->slots[i], MUSE_NIL ) ),
								 MUSE_TRUE );
			_unwind(sp);
			_spush(result);
		}
	}
	
	return result;	
}


static void normalize_index_iterator( int *from, int *count, int *to, int step, int length )
{
	if ( step == 0 || (*count) <= 0 )
	{
		(*count) = 0;
		return;
	}

	if ( (*to) >= (*from) )
	{
		// Forward scan

		// Check for empty case.
		if ( (*from) >= length ) { (*count) = 0; return; }

		if ( (*from) < 0 ) (*from) = (((*from) % step) + step) % step;

		if ( (*to) >= (*from) )
		{
			// Still good
			if ( (*to) >= length ) (*to) = length;

			(*count) = ((*to) - (*from))/step;
		}
		else
		{
			// Empty.
			(*count) = 0;
			return;
		}
	}
	else
	{
		// Backward scan

		// Check for empty case.
		if ( (*from) < 0 ) { (*count) = 0; return; }

		if ( (*from) >= length ) (*from) = (*from) - (-step) * (((*from) - length - step) / (-step));

		if ( (*to) <= (*from) )
		{
			// Still good
			if ( (*to) < 0 ) (*to) = -1;

			(*count) = ((*from) - (*to)) / (-step);
		}
		else
		{
			// Empty
			(*count) = 0;
			return;
		}
	}
}

void get_slice_iterator_from_args( muse_env *env, int *from, int *count, int *step, int *to, int length, muse_cell *argv )
{
	(*from) = 0;
	(*count) = -1;
	(*step) = 1;
	(*to) = -1;

	if ( *argv ) (*from) = (int)_intvalue(_next(argv));
	if ( *argv ) (*count) = (int)_intvalue(_next(argv));
	if ( *argv ) (*step) = (int)_intvalue(_next(argv));

	// Default count such that it covers the whole vector.
	if ( (*count) < 0 ) 
	{
		if ( (*step) < 0 )
			(*count) = (*from);
		else
			(*count) = length - (*from);
	}
	
	(*to) = (*from) + (*step) * (*count);

	normalize_index_iterator( from, count, to, *step, length );
}

static muse_cell vector_slice( muse_env *env, void *self, muse_cell argv )
{
	vector_t *v = (vector_t*)self;

	int from, count, step, to;

	get_slice_iterator_from_args( env, &from, &count, &step, &to, v->length, &argv );

	if ( count <= 0 )
		return muse_mk_vector( env, 0 );
	else
	{
		muse_cell vec = muse_mk_vector( env, count );
		int i = 0;
		int sp = _spos();
		for ( i = 0; i < count; ++i )
		{
			int j = from + i * step;
			muse_vector_put( env, vec, i, vector_force( env, v, j, v->slots[j] ) );
			_unwind(sp);
		}

		return vec;
	}
}

static muse_cell vector_iterator( muse_env *env, vector_t *self, muse_iterator_callback_t callback, void *context )
{
	int sp = _spos();
	int i;
	muse_boolean cont = MUSE_TRUE;
	
	if ( self->datafn )
	{
		for ( i = 0; i < self->length; ++i )
		{
			cont = callback( env, self, context, vector_force( env, self, i, self->slots[i] ) );
			_unwind(sp);
			if ( !cont )
				return _mk_int(i); /**< Return the current index. */
		}
	}
	else
	{
		for ( i = 0; i < self->length; ++i )
		{
			cont = callback( env, self, context, self->slots[i] );
			_unwind(sp);
			if ( !cont )
				return _mk_int(i); /**< Return the current index. */
		}
	}
	
	return MUSE_NIL;
}

static muse_cell vector_datafn( muse_env *env, void *self, muse_cell datafn )
{
	vector_t *v = (vector_t*)self;
	muse_cell olddatafn = v->datafn;
	v->datafn = datafn;
	return olddatafn;
}

static muse_cell vector_get( muse_env *env, void *self, muse_cell key, muse_cell argv )
{
	vector_t *v = (vector_t*)self;
	if ( _cellt(key) == MUSE_INT_CELL ) {
		int index = (int)muse_int_value(env, key);
		return argv ? muse_get( env, v->slots[index], _head(argv), _tail(argv) ) : v->slots[index];
	} else {
		return MUSE_NIL;
	}
}

static muse_cell vector_put( muse_env *env, void *self, muse_cell key, muse_cell argv )
{
	vector_t *v = (vector_t*)self;
	if ( _cellt(key) == MUSE_INT_CELL ) {
		muse_cell val = _next(&argv);
		int index = (int)muse_int_value(env,key);
		return argv ? muse_put( env, v->slots[index], val, argv ) : (v->slots[index] = val); 
	} else {
		return MUSE_NIL;
	}
}

muse_cell fn_vector_to_list( muse_env *env, void *context, muse_cell args );

static muse_cell vector_format( muse_env *env, void *self )
{
	vector_t *v = (vector_t*)self;
	int sp = _spos();
	muse_cell items_arg = _cons( v->base.self, MUSE_NIL );
	muse_cell items = fn_vector_to_list( env, NULL, items_arg );
	muse_cell result = muse_apply( env, muse_mk_nativefn( env, fn_format, NULL ), items, MUSE_TRUE, MUSE_FALSE );
	_unwind(sp);
	_spush(result);

	/* Don't leave garbage. This is possible here 'cos format
	won't use the cells used to pass its arguments. */
	_returncell(items_arg);
	while ( items ) {
		muse_cell n = _tail(items);
		_returncell(items);
		items = n;
	}

	return result;
}

static muse_prop_view_t g_vector_prop_view =
{
	vector_get,
	vector_put
};

static muse_monad_view_t g_vector_monad_view =
{
	vector_size,
	vector_map,
	vector_join,
	vector_collect,
	vector_reduce,
	vector_slice
};

static muse_format_view_t g_vector_format_view =
{
	vector_format
};

static void *vector_view( muse_env *env, int id )
{
	switch ( id )
	{
		case 'mnad' : return &g_vector_monad_view;
		case 'iter' : return vector_iterator;
		case 'dtfn' : return vector_datafn;
		case 'prop' : return &g_vector_prop_view;
		case 'frmt' : return &g_vector_format_view;
		default : return NULL;
	}
}

static muse_functional_object_type_t g_vector_type =
{
	'muSE',
	'vect',
	sizeof(vector_t),
	(muse_nativefn_t)fn_vector,
	vector_view,
	vector_init,
	vector_mark,
	vector_destroy,
	vector_write
};

/**
 * (mk-vector N [datafn]).
 * Creates a new vector of length N. All slots in the vector
 * are initially NIL. The returned object is a nativefn
 * (functional object). If \c vec is the returned object,
 * then @code (vec i) @endcode yields the value at the slot
 * \c i and @code (vec i val) @endcode sets the value at
 * the slot \c i to \c val and returns \c val.
 */
muse_cell fn_mk_vector( muse_env *env, void *context, muse_cell args )
{
	return _mk_functional_object( &g_vector_type, args ); 
}

/**
 * (vector a1 a2 a3 --- aN).
 * Makes an N-length vector from the arguments with the arguments
 * as the initial values. Useful and compact for small vectors.
 *
 * Supports \ref fn_the "the".
 */
muse_cell fn_vector_from_args( muse_env *env, void *context, muse_cell args )
{
	int i, length			= _list_length(args);
	muse_cell length_arg	= _cons( _mk_int( length ), MUSE_NIL );
	muse_cell vec			= _mk_functional_object( &g_vector_type, length_arg );
	vector_t *v				= (vector_t*)_functional_object_data(vec,'vect');
	int sp					= _spos();

	for ( i = 0; i < length; ++i )
	{
		v->slots[i] = _evalnext(&args);
		_unwind(sp);
	}

	return muse_add_recent_item( env, (muse_int)fn_vector_from_args, vec );
}

/**
 * (vector? fv).
 * Returns fv if it is a functional vector. Returns () if it isn't.
 */
muse_cell fn_vector_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell fv = _evalnext(&args);
	vector_t *v = (vector_t*)_functional_object_data( fv, 'vect' );

	return v ? fv : MUSE_NIL;
}

/**
 * (vector-length v).
 * Evaluates to the length of the given functional vector.
 */
muse_cell fn_vector_length( muse_env *env, void *context, muse_cell args )
{
	return _mk_int( muse_vector_length( env, _evalnext(&args) ) );
}

/**
 * (list->vector ls).
 * Converts the given list into a vector and returns the vector.
 */
muse_cell fn_list_to_vector( muse_env *env, void *context, muse_cell args )
{
	muse_cell list = _evalnext(&args);
	int length = _list_length(list);
	
	if ( length > 0 )
	{
		muse_cell fv = fn_mk_vector( env, NULL, MUSE_NIL );
		vector_t *v = (vector_t*)_functional_object_data(fv,'vect');

		vector_init_with_length( v, length );

		{
			muse_cell *slots = v->slots;
			while ( list ) {
				slots[0] = muse_head(env,list);
				list = muse_tail(env,list);
			}
		}

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
	muse_cell fv = _evalnext(&args);
	vector_t *v = (vector_t*)_functional_object_data(fv,'vect');
	muse_assert( v != NULL && "First argument must be a functional vector!" );

	vector_forceall( env, v );

	{
		int from	= 0;
		int count	= v->length;
		int step	= 1;

		if ( args ) from	= (int)_intvalue(_evalnext(&args));
		
		/* Make sure count stays within valid limits even if
		it isn't specified explcitly. */
		count = v->length - from;
		
		if ( args ) count	= (int)_intvalue(_evalnext(&args));
		if ( args ) step	= (int)_intvalue(_evalnext(&args));

		muse_assert( count >= 0 && from >= 0 && from + step * count <= v->length );

		return muse_array_to_list( env, count, v->slots + from, step );
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

void muse_define_builtin_type_vector(muse_env *env)
{
	int sp = _spos();
	const struct vector_fns_t *fns = g_vector_fns;
	for ( ; fns->name; ++fns )
	{
		_define( _csymbol(fns->name), _mk_nativefn( fns->fn, NULL ) );
		_unwind(sp);
	}
}

/**
 * Creates a new vector object that has enough slots allocated to hold
 * the given number of objects. All slots are initialized to MUSE_NIL.
 */
MUSEAPI muse_cell muse_mk_vector( muse_env *env, int length )
{
	muse_assert( length >= 0 );
	{
		int sp = _spos();
		muse_cell result = fn_mk_vector( env, NULL, _cons( _mk_int(length), MUSE_NIL ) );
		_unwind(sp);
		_spush(result);
		return result;
	}
}

/**
 * Returns the number of slots the vector has.
 */
MUSEAPI int muse_vector_length( muse_env *env, muse_cell vec )
{
	vector_t *v = (vector_t*)_functional_object_data( vec, 'vect' );
	muse_assert( v != NULL && "v must be a vector!" );
	return v ? v->length : 0;
}

/**
 * Returns the value occupying the slot at the given 0-based index.
 */
MUSEAPI muse_cell muse_vector_get( muse_env *env, muse_cell vec, int index )
{
	vector_t *v = (vector_t*)_functional_object_data( vec, 'vect' );
	muse_assert( v != NULL && "v must be a vector!" );
	if ( v )
	{
		muse_assert( index >= 0 && index < v->length );
		return vector_force( env, v, index, v->slots[index] );
	}
	else
		return MUSE_NIL;
}

/**
 * Replaces the value in the slot at the given index with the new value.
 */
MUSEAPI muse_cell muse_vector_put( muse_env *env, muse_cell vec, int index, muse_cell value )
{
	vector_t *v = (vector_t*)_functional_object_data( vec, 'vect' );
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

