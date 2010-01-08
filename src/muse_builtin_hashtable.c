/**
 * @file muse_builtin_hashtable.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * Implements functional hash tables for constant time random access
 * to a property list.
 */

#include "muse_builtins.h"
#include "muse_port.h"
#include <stdlib.h>
#include <memory.h>

/** @addtogroup FunctionalObjects */
/*@{*/
/**
 * @defgroup Hashtables
 *
 * A functional hash table is a function from symbols or integers
 * to arbitrary values. When given only a single \c key
 * argument, it returns the value associated with the key. If no
 * value is associated with the key, it returns <tt>()</tt>. Note
 * that this means you can't distinguish between a key being associated
 * with a <tt>()</tt> value and the key not being present in the
 * hash table.
 *
 * When given two arguments \c key and \c value, adds the association
 * to the hash table and returns the \c value. If you want to remove
 * a key's association from the hash table, pass a second argument
 * of <tt>()</tt>.
 *
 * Examples -
 * @code
 * (define ht (mk-hashtable))
 * (ht 'ceo 'pete)
 * (ht 'coo 'terence)
 * (ht 'company 'muvee)
 * (print (ht 'ceo))
 *      > pete
 * (print (hashtable-size ht))
 *      > 3
 * (print (hashtable->alist ht))
 *      > ((coo . terence) (ceo . pete) (company . muvee))
 * (ht 'company ())
 * (print (hashtable-size ht))
 *      > 2
 * (print (hashtable->alist ht))
 *      > ((coo . terence) (ceo . pete))
 * @endcode
 */
/*@{*/

typedef struct 
{
	muse_functional_object_t base;
	int			count;			/**< The number of kvpairs in the hash table. */
	int			bucket_count;	/**< The number of buckets in the hash table. */
	muse_cell	*buckets;		/**< The array holding the buckets. Each bucket is
									simply an assoc list. */
	muse_cell	datafn;			/**< fn(key)->value for use when key is not found. */
} hashtable_t;

static void get_size_or_datafn( muse_env *env, hashtable_t *h, muse_cell *args )
{
	if ( *args ) {
		muse_cell arg = _evalnext(args);
		if ( _isfn(arg) ) {
			h->datafn = arg;
		} else if ( _cellt(arg) == MUSE_INT_CELL ) {
			h->bucket_count = (int)_intvalue( arg );
		}
	}
}

static void hashtable_init( muse_env *env, void *p, muse_cell args )
{
	hashtable_t *h = (hashtable_t*)p;
	
	h->bucket_count = 7;

	get_size_or_datafn( env, h, &args );
	get_size_or_datafn( env, h, &args );
	
	muse_assert( h->bucket_count > 0 );
	h->buckets = (muse_cell*)calloc( h->bucket_count, sizeof(muse_cell) );	
}

static void hashtable_mark( muse_env *env, void *p )
{
	hashtable_t *h = (hashtable_t*)p;
	
	if ( h->count > 0 )
	{
		muse_cell *buckets = h->buckets;
		muse_cell *buckets_end = buckets + h->bucket_count;
	
		while ( buckets < buckets_end )
		{
			muse_mark( env, *buckets++ );
		}
	}

	muse_mark( env, h->datafn );
}

static void hashtable_destroy( muse_env *env, void *p )
{
	hashtable_t *h = (hashtable_t*)p;
	
	if ( h->bucket_count > 0 )
	{
		free(h->buckets);
	}
}

/**
 * Writes the hashtable out to the given port in the form
 * @code
 *    {hashtable '((key1 . value1) (key2 . value2) ... (keyN . valueN))}
 * @endcode
 * Since it uses braces, a trusted read operation will 
 * automatically give the hashtable object in the position that
 * this expression is inserted.
 */
static void hashtable_write( muse_env *env, void *ptr, void *port )
{
	hashtable_t *h = (hashtable_t*)ptr;
	muse_port_t p = (muse_port_t)port;
	
	port_putc( '{' , p );
	port_write( "hashtable '(", 12, p );
	
	{
		int b, i;
		
		/* Step through the buckets. */
		for ( b = 0, i = 0; b < h->bucket_count && i < h->count; ++b )
		{
			muse_cell alist = h->buckets[b];
			
			/* Step through the pairs in each bucket. */
			while ( alist )
			{
				if ( i > 0 ) port_putc( ' ', p );
				muse_pwrite( p, _head(alist) );
				alist = _tail(alist);
				++i;
			}
		}
	}
	
	port_putc( ')', p );
	port_putc( '}', p );
}

static int bucket_for_hash( muse_int hash, int modulus )
{
	return (int)(((hash % modulus) + modulus) % modulus);
}

static void hashtable_rehash( muse_env *env, hashtable_t *h, int new_bucket_count )
{
	muse_cell *new_buckets;
	
	new_bucket_count = new_bucket_count | 1;
	new_buckets = (muse_cell*)calloc( new_bucket_count, sizeof(muse_cell) );
	
	/* Note that in the following rehash loop,
	no cons happens. */
	{
		int i = 0, N = h->bucket_count;
		for ( ; i < N; ++i )
		{
			muse_cell alist = h->buckets[i];
						
			while ( alist )
			{
				muse_int hash_i = muse_hash( env, _head( _head( alist ) ) );
				int new_b		= bucket_for_hash( hash_i, new_bucket_count );
				muse_cell next	= _tail(alist);
							
				_sett( alist, new_buckets[new_b] );
				new_buckets[new_b] = alist;
				alist = next;
			}
		}
	}
				
	/* Replace the original buckets with the new buckets. */
	free( h->buckets );
	h->bucket_count = new_bucket_count;
	h->buckets = new_buckets;
}

muse_cell fn_hashtable_stats( muse_env *env, void *context, muse_cell args )
{
	muse_cell ht = _evalnext(&args);
	hashtable_t *h = (hashtable_t*)_functional_object_data(ht,'hash');

	if ( h )
	{
		int collision_count = 0;
		int unused_buckets = 0;
		int b = 0;
		
		for ( ; b < h->bucket_count; ++b )
		{
			int bucket_size = _list_length( h->buckets[b] );
			if ( bucket_size == 0 )
				++unused_buckets;
			else 
				collision_count += (bucket_size - 1);
		}
		
		return muse_list( env, "cccc",
						  muse_list( env, "si", "element-count", h->count ),
						  muse_list( env, "si", "bucket-count", h->bucket_count ),
						  muse_list( env, "si", "unused-buckets", unused_buckets ),
						  muse_list( env, "si", "collisions", collision_count ) );
	}
	
	return MUSE_NIL;
}

static muse_cell *hashtable_add( muse_env *env, hashtable_t *h, muse_cell key, muse_cell value, muse_int *hash_opt )
{
	muse_int hash;
	
	if ( hash_opt )
		hash = *hash_opt;
	else
		hash = muse_hash( env, key );
	
	{
		int bucket = bucket_for_hash( hash, h->bucket_count );
		
		if ( h->count + 1 >= 2 * h->bucket_count )
		{
			/* Need to rehash and determine new bucket. */
			hashtable_rehash( env, h, h->bucket_count * 2 );
			
			bucket = bucket_for_hash( hash, h->bucket_count );
		}

		/* Add the kvpair to the determined bucket. */
		muse_assert( value != MUSE_NIL );
		h->buckets[bucket] = _cons( _cons( key, value ), h->buckets[bucket] );
		++(h->count);
		
		return h->buckets + bucket;
	}
}

static void hashtable_fast_add( muse_env *env, hashtable_t *h, muse_cell *kvpair, muse_cell key, muse_cell value )
{
	muse_assert( *kvpair == MUSE_NIL );

	(*kvpair) = _cons( _cons( key, value ), MUSE_NIL );
	++(h->count);

	if ( h->count >= 2 * h->bucket_count )
		hashtable_rehash( env, h, 2 * h->bucket_count );
}

static muse_cell *hashtable_get( muse_env *env, hashtable_t *h, muse_cell key, muse_int *hash_out )
{
	muse_int hash = muse_hash(env,key);
	int bucket = bucket_for_hash( hash, h->bucket_count );

	if ( hash_out ) 
		(*hash_out) = hash;
	
	return muse_assoc_iter( env, h->buckets + bucket, key );
}

static muse_cell hashtable_get_prop( muse_env *env, void *self, muse_cell key, muse_cell argv ) {
	hashtable_t *h = (hashtable_t*)self;
	muse_int hash = 0;
	muse_cell *kvpair = hashtable_get( env, h, key, &hash );

	if ( *kvpair ) {
		muse_cell val = _tail( _head( *kvpair ) );
		return argv ? muse_get( env, muse_add_recent_item( env, key, val ), muse_head( env, argv ), muse_tail( env, argv ) ) : val;
	} else {
		/* key doesn't exist. Try to compute using the func spec. */
		muse_cell value = MUSE_NIL;

		/* Use the datafn to derive the value for the key. */
		if ( h->datafn )
			value = _force(muse_apply( env, h->datafn, _cons(key,MUSE_NIL), MUSE_TRUE, MUSE_TRUE ));

		/* Cache the value in the hashtable. */
		if ( value )
			hashtable_add( env, h, key, value, &hash );

		return argv ? muse_get( env, muse_add_recent_item( env, key, value ), muse_head( env, argv ), muse_tail( env, argv ) ) 
					: muse_add_recent_item( env, key, value );
	}
}

static muse_cell hashtable_put_prop( muse_env *env, void *self, muse_cell key, muse_cell argv ) {
	muse_cell value = _next(&argv);
	hashtable_t *h = (hashtable_t*)self;

	muse_cell *kvpair = hashtable_get( env, h, key, NULL );
	if ( *kvpair ) {
		if ( value )
		{
			/* It already exists. Simply change the value to the new one. */
			if ( argv ) {
				return muse_put( env, muse_add_recent_item( env, key, _tail(_head(*kvpair)) ), value, argv );
			} else {
				_sett( _head(*kvpair), value );
				return muse_add_recent_item( env, key, value );
			}
		} 
		else
		{
			/* The value is MUSE_NIL. Which means we have to remove
			the kvpair from the hashtable. */
			(*kvpair) = _tail( *kvpair );
			--(h->count);
			return MUSE_NIL;
		}
	} else {
		if ( value )
		{
			/* It doesn't exist. Need to add a new entry.
			Check to see if we need to rehash the table. 
			We rehash if we have to do more than 2 linear
			searches on the average for each access. */
			hashtable_fast_add( env, h, kvpair, key, value );
			return muse_add_recent_item( env, key, value );
		}
		else
		{
			/* The key isn't there in the hashtable and we've
			been asked to remove it. So we don't need to do anything. */
			return MUSE_NIL;
		}
	}
}

static muse_prop_view_t g_hashtable_prop_view = {
	hashtable_get_prop,
	hashtable_put_prop
};

muse_cell fn_hashtable( muse_env *env, hashtable_t *h, muse_cell args )
{
	muse_assert( h != NULL && "Context 'h' must be a hashtable object!" );

	{
		muse_cell key = _evalnext(&args);
		if ( args ) {
			/* Two argument form. */
			return hashtable_put_prop( env, h, key, muse_eval_list( env, args ) );
		} else {
			/* One argument form. */
			return hashtable_get_prop( env, h, key, MUSE_NIL );
		}
	}
}

static muse_cell hashtable_size( muse_env *env, void *self )
{
	return _mk_int( ((hashtable_t*)self)->count );
}

static void hashtable_merge_one( muse_env *env, hashtable_t *h1, muse_cell key, muse_cell new_value, muse_cell reduction_fn )
{
	int sp = _spos();
	muse_int hash = 0;
	muse_cell *new_kv = hashtable_get( env, h1, key, &hash );
	
	if ( new_kv && *new_kv )
	{
		/* Key already exists. */
		if ( reduction_fn )
		{
			/* Set the value to reduction_fn( current_value, new_value ). */
			_sett( _head( *new_kv ), 
						   _apply( reduction_fn,
									   _cons( _tail( _head( *new_kv ) ),
												  _cons( new_value,
															 MUSE_NIL ) ),
									   MUSE_TRUE ) );					
		}
		else
		{
			/* No reduction function. Simply replace the old value with the new one. */
			_sett( _head( *new_kv ), new_value );
		}
	}
	else
	{
		/* Key doesn't exist. Need to add new. */
		hashtable_add( env, h1, key, new_value, &hash );
	}
	
	_unwind(sp);
}

static muse_cell hashtable_map( muse_env *env, void *self, muse_cell fn )
{
	hashtable_t *h = (hashtable_t*)self;
	
	muse_cell result = muse_mk_hashtable( env, h->count );
	hashtable_t *result_ptr = (hashtable_t*)_functional_object_data( result, 'hash' );
	
	muse_cell args = _cons( MUSE_NIL, MUSE_NIL );
	
	int sp = _spos();
	int b;
	for ( b = 0; b < h->bucket_count; ++b )
	{
		muse_cell alist = h->buckets[b];
		
		while ( alist )
		{
			muse_cell kv = _head(alist);
			
			_seth( args, _tail( kv ) );
			
			hashtable_add( env, result_ptr, _head( kv ), _apply( fn, args, MUSE_TRUE ), NULL );
						
			alist = _tail(alist);
			
			_unwind(sp);
		}
	}
	
	if ( h->datafn != MUSE_NIL ) {
		/* Apply the mapper to the datafn so that the new hashtable's
		elements are generated accordingly. */
		result_ptr->datafn =
			muse_eval( env, muse_list( env, "S(S)(c(cS))",
											L"fn", L"$x",
  				 							  fn, h->datafn, L"$x" ),
							MUSE_FALSE );
	}
	
	return result;
}


static void hashtable_merge( muse_env *env, hashtable_t *h1, hashtable_t *h2, muse_cell reduction_fn )
{
	int b = 0;
	int sp = _spos();
	
	for ( b = 0; b < h2->bucket_count; ++b )
	{
		muse_cell alist = h2->buckets[b];
		
		while ( alist )
		{
			muse_cell kv = _head(alist);
			muse_cell key = _head(kv);
			muse_cell value = _tail(kv);
			
			hashtable_merge_one( env, h1, key, value, reduction_fn );
			
			_unwind(sp);
			alist = _tail(alist);
		}
	}
}

static muse_cell hashtable_join( muse_env *env, void *self, muse_cell objlist, muse_cell reduction_fn )
{
	hashtable_t *h1 = (hashtable_t*)self;
	
	muse_cell result = muse_mk_hashtable( env, h1->count );
	hashtable_t *result_ptr = (hashtable_t*)_functional_object_data( result, 'hash' );
	
	/* First add all the elements of h1. */
	hashtable_merge( env, result_ptr, h1, MUSE_NIL );
	
	/* Next add all elements of h2 to the result. */
	while ( objlist )
	{
		muse_cell obj = _next(&objlist);
		hashtable_t *h2 = (hashtable_t*)_functional_object_data( obj, 'hash' );
		hashtable_merge( env, result_ptr, h2, reduction_fn );
	}
	
	return result;
}

static muse_cell hashtable_collect( muse_env *env, void *self, muse_cell predicate, muse_cell mapper, muse_cell reduction_fn )
{
	hashtable_t *h = (hashtable_t*)self;
	
	muse_cell result = muse_mk_hashtable( env, h->count );
	hashtable_t *result_ptr = (hashtable_t*)_functional_object_data( result, 'hash' );
	
	/* Step through self's contents and add all the key-value pairs that satisfy the predicate. */
	{
		int sp = _spos();
		int b = 0;
		for ( b = 0; b < h->bucket_count; ++b )
		{
			muse_cell alist = h->buckets[b];
			
			while ( alist )
			{
				muse_cell kv = _head( alist );
				
				if ( !predicate || _apply( predicate, kv, MUSE_TRUE ) )
				{
					/* Key-value pair satisfied the predicate. */
					if ( mapper )
						kv = _apply( mapper, kv, MUSE_TRUE );
					
					hashtable_merge_one( env, result_ptr, _head(kv), _tail(kv), reduction_fn );
				}
				
				_unwind(sp);
				alist = _tail(alist);
			}
		}
	}
	
	return result;
}

static muse_cell hashtable_reduce( muse_env *env, void *self, muse_cell reduction_fn, muse_cell initial )
{
	hashtable_t *h = (hashtable_t*)self;
	
	muse_cell result = initial;
	muse_cell args = _cons( result, _cons( MUSE_NIL, MUSE_NIL ) );
	muse_cell *arg1 = &(_ptr(args)->cons.head);
	muse_cell *arg2 = &(_ptr(_tail(args))->cons.head);
	
	int sp = _spos();
	int b = 0;
	for ( b = 0; b < h->bucket_count; ++b )
	{
		muse_cell alist = h->buckets[b];
		
		while ( alist )
		{
			(*arg1) = result;
			(*arg2) = _tail( _head( alist ) );
			
			result = _apply( reduction_fn, args, MUSE_TRUE );
			
			_unwind(sp);
			_spush(result);
			
			alist = _tail(alist);
		}
	}
	
	return result;
}

static muse_cell hashtable_iterator( muse_env *env, hashtable_t *self, muse_iterator_callback_t callback, void *context )
{
	int sp = _spos();
	int b;
	muse_boolean cont = MUSE_TRUE;
	
	for ( b = 0; b < self->bucket_count; ++b )
	{
		muse_cell alist = self->buckets[b];
		
		while ( alist )
		{
			cont = callback( env, self, context, _tail(_head(alist)) );
			_unwind(sp);
			if ( !cont )
				return _head(_head(alist)); /**< Return the key. */
			
			alist = _tail(alist);
		}
	}	
	
	return MUSE_NIL;
}

static muse_cell hashtable_datafn( muse_env *env, void *self, muse_cell datafn )
{
	hashtable_t *ht = (hashtable_t*)self;
	muse_cell olddatafn = ht->datafn;
	ht->datafn = datafn;
	return olddatafn;
}

static muse_monad_view_t g_hashtable_monad_view =
{
	hashtable_size,
	hashtable_map,
	hashtable_join,
	hashtable_collect,
	hashtable_reduce
};

static void *hashtable_view( muse_env *env, int id )
{
	switch ( id )
	{
		case 'mnad' : return &g_hashtable_monad_view;
		case 'iter' : return hashtable_iterator;
		case 'dtfn' : return hashtable_datafn;
		case 'prop' : return &g_hashtable_prop_view;
		default : return NULL;
	}
}

static muse_functional_object_type_t g_hashtable_type =
{
	'muSE',
	'hash',
	sizeof(hashtable_t),
	(muse_nativefn_t)fn_hashtable,
	hashtable_view,
	hashtable_init,
	hashtable_mark,
	hashtable_destroy,
	hashtable_write
};

/**
 * (mk-hashtable [datafn] [size]).
 * Creates a new hash table. No arguments are required, but
 * you can give the expected size of the hash table as an argument
 * and/or the datafn to use to map keys to values. You may give
 * the size and the datafn in any order.
 */
muse_cell fn_mk_hashtable( muse_env *env, void *context, muse_cell args )
{
	return _mk_functional_object( &g_hashtable_type, args );
}

/**
 * (hashtable? ht).
 * Returns \c ht if it is a functional hashtable, or () if it isn't.
 */
muse_cell fn_hashtable_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell ht = _evalnext(&args);
	hashtable_t *h = (hashtable_t*)_functional_object_data(ht,'hash');
	
	if ( h )
		return ht;
	else
		return MUSE_NIL;
}

/**
 * (hashtable-size ht).
 * Returns the number of key-value pairs stored in the hash table.
 */
muse_cell fn_hashtable_size( muse_env *env, void *context, muse_cell args )
{
	muse_cell ht = _evalnext(&args);
	hashtable_t *h = (hashtable_t*)_functional_object_data(ht,'hash');
	
	muse_assert( h && h->base.type_info->type_word == 'hash' );
	
	return _mk_int( h->count );
}

typedef struct {
	muse_cell list;
	int count;
} copy_list_data_t;

static muse_cell list_items( muse_env *env, void *context, int i, muse_boolean *eol )
{
	copy_list_data_t *data = (copy_list_data_t*)context;
	if ( data->list ) {
		(*eol) = MUSE_FALSE;
		data->count++;
		return _next(&(data->list));
	} else {
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
}

static muse_cell copy_list( muse_env *env, muse_cell list, int *count )
{
	copy_list_data_t data = { list, 0 };
	muse_cell result = muse_generate_list( env, list_items, &data );
	if ( count ) (*count) = data.count;
	return result;
}

/**
 * (hashtable alist).
 * Returns a hash table with the same contents as the given alist.
 *
 * Supports \ref fn_the "the".
 */
muse_cell fn_alist_to_hashtable( muse_env *env, void *context, muse_cell args )
{
	muse_cell ht = fn_mk_hashtable( env, NULL, MUSE_NIL );
	
	muse_cell alist = _evalnext(&args);
	int count = 0;
	muse_cell alist_copy = copy_list( env, alist, &count );

	{
		hashtable_t *h = (hashtable_t*)_functional_object_data(ht,'hash');
		h->count = count;
		h->buckets[0] = alist_copy;
		hashtable_rehash( env, h, count );
	}
	
	return muse_add_recent_item( env, (muse_int)fn_alist_to_hashtable, ht );
}

typedef struct {
	muse_cell intrabucket_iter;
	muse_cell *buckets_iter;
	muse_cell *buckets_end;
} h2a_data_t;

static muse_cell h2a_generator( muse_env *env, h2a_data_t *data, int i, muse_boolean *eol )
{
	if ( data->intrabucket_iter ) {
		(*eol) = MUSE_FALSE;
		return _next(&(data->intrabucket_iter));
	} else if ( data->buckets_iter + 1 < data->buckets_end ) {
		++(data->buckets_iter);
		data->intrabucket_iter = *(data->buckets_iter);
		return h2a_generator( env, data, i, eol );
	} else {
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
}


/**
 * (hashtable->alist ht).
 * Returns an alist version of the contents of the given hash table.
 * The order of the elements is unpredictable.
 */
muse_cell fn_hashtable_to_alist( muse_env *env, void *context, muse_cell args )
{
	muse_cell ht = _evalnext(&args);
	hashtable_t *h = (hashtable_t*)_functional_object_data(ht,'hash');
	
	muse_assert( h && h->base.type_info->type_word == 'hash' );
	
	{
		h2a_data_t data = { *(h->buckets), h->buckets, h->buckets + h->bucket_count };
		return muse_add_recent_item( env, 
									 (muse_int)fn_hashtable_to_alist, 
									 muse_generate_list( env, (muse_list_generator_t)h2a_generator, &data ) );
	}
}

static const struct _defs 
{
	const muse_char *name;
	muse_nativefn_t fn;
} k_hashtable_funs[] =
{
	{	L"mk-hashtable",		fn_mk_hashtable			},
	{	L"hashtable?",		fn_hashtable_p			},
	{	L"hashtable-size",	fn_hashtable_size		},
	{	L"hashtable",			fn_alist_to_hashtable	},
	{	L"hashtable->alist",	fn_hashtable_to_alist	},
	{	L"hashtable-stats",	fn_hashtable_stats		},
	{	NULL,					NULL					}
};

void muse_define_builtin_type_hashtable(muse_env *env)
{
	int sp = _spos();
	const struct _defs *defs = k_hashtable_funs;
	
	for ( ; defs->name; ++defs )
	{
		_define( _csymbol(defs->name), _mk_nativefn( defs->fn, NULL ) );
		_unwind(sp);
	}
}

/**
 * Creates a hashtable with a bucket count setup according to the
 * given desired length. Note that calling muse_hashtable_length()
 * without first putting anything into the hashtable will always
 * get you 0. The "length" of the hashtable is the number of 
 * key-value pairs put into it.
 */
MUSEAPI muse_cell muse_mk_hashtable( muse_env *env, int length )
{
	int sp = _spos();
	muse_cell ht = fn_mk_hashtable( env, NULL, (length > 0 ? _cons( _mk_int(length), MUSE_NIL ) : MUSE_NIL) );
	_unwind(sp);
	_spush(ht);
	return ht;
}

/** 
 * Returns the number of key-value pairs put into the hash table.
 */
MUSEAPI int muse_hashtable_length( muse_env *env, muse_cell ht )
{
	hashtable_t *h = (hashtable_t*)_functional_object_data( ht, 'hash' );
	muse_assert( h != NULL && "Argument must be a hashtable object!" );
	return h ? h->count : 0;
}

/**
 * Returns the key-value pair with the given key as the head
 * if the key is present in the hash table, otherwise it returns MUSE_NIL.
 * The value associated with the key will be in the tail of the returned
 * pair.
 */
MUSEAPI muse_cell muse_hashtable_get( muse_env *env, muse_cell ht, muse_cell key )
{
	int sp = _spos();
	muse_cell result = _apply( ht, _cons(key,MUSE_NIL), MUSE_TRUE );
	_unwind(sp);
	return result;
}

/**
 * Associates the given value with the given key in the hash table.
 * The given value replaces any previous value that might have been
 * associated with the key.
 */
MUSEAPI muse_cell muse_hashtable_put( muse_env *env, muse_cell ht, muse_cell key, muse_cell value )
{
	int sp = _spos();
	muse_cell result = _apply( ht, _cons(key,_cons(value,MUSE_NIL)), MUSE_TRUE );
	_unwind(sp);
	return result;
}

/*@}*/
/*@}*/
