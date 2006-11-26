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
} hashtable_t;

static void hashtable_init( void *p, muse_cell args )
{
	hashtable_t *h = (hashtable_t*)p;

	if ( args )
		h->bucket_count = (int)muse_int_value( muse_evalnext(&args) );
	else
		h->bucket_count = 7;
	
	h->buckets = (muse_cell*)calloc( h->bucket_count, sizeof(muse_cell) );	
}

static void hashtable_mark( void *p )
{
	hashtable_t *h = (hashtable_t*)p;
	
	if ( h->count > 0 )
	{
		muse_cell *buckets = h->buckets;
		muse_cell *buckets_end = buckets + h->bucket_count;
	
		while ( buckets < buckets_end )
		{
			muse_mark( *buckets++ );
		}
	}
}

static void hashtable_destroy( void *p )
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
static void hashtable_write( void *ptr, void *port )
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

static void hashtable_rehash( hashtable_t *h, int new_bucket_count )
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
				muse_int hash_i = muse_hash( muse_head( muse_head( alist ) ) );
				int new_b		= bucket_for_hash( hash_i, new_bucket_count );
				muse_cell next	= muse_tail(alist);
							
				muse_set_tail( alist, new_buckets[new_b] );
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

#ifndef NDEBUG
muse_cell fn_hashtable_stats( muse_env *env, void *context, muse_cell args )
{
	muse_cell ht = muse_evalnext(&args);
	hashtable_t *h = (hashtable_t*)muse_functional_object_data(ht,'hash');

	if ( h )
	{
		int collision_count = 0;
		int unused_buckets = 0;
		int b = 0;
		
		for ( ; b < h->bucket_count; ++b )
		{
			int bucket_size = muse_list_length( h->buckets[b] );
			if ( bucket_size == 0 )
				++unused_buckets;
			else 
				collision_count += (bucket_size - 1);
		}
		
		return muse_list( "cccc",
						  muse_list( "si", "element-count", h->count ),
						  muse_list( "si", "bucket-count", h->bucket_count ),
						  muse_list( "si", "unused-buckets", unused_buckets ),
						  muse_list( "si", "collisions", collision_count ) );
	}
	
	return MUSE_NIL;
}
#endif

static muse_cell *hashtable_add( hashtable_t *h, muse_cell key, muse_cell value, muse_int *hash_opt )
{
	muse_int hash;
	
	if ( hash_opt )
		hash = *hash_opt;
	else
		hash = muse_hash( key );
	
	{
		int bucket = bucket_for_hash( hash, h->bucket_count );
		
		if ( h->count + 1 >= 2 * h->bucket_count )
		{
			/* Need to rehash and determine new bucket. */
			hashtable_rehash( h, h->bucket_count * 2 );
			
			bucket = bucket_for_hash( hash, h->bucket_count );
		}

		/* Add the kvpair to the determined bucket. */
		muse_assert( value != MUSE_NIL );
		h->buckets[bucket] = muse_cons( muse_cons( key, value ), h->buckets[bucket] );
		++(h->count);
		
		return h->buckets + bucket;
	}
}

static muse_cell *hashtable_get( hashtable_t *h, muse_cell key, muse_int *hash_out )
{
	muse_int hash = muse_hash(key);
	int bucket = bucket_for_hash( hash, h->bucket_count );

	if ( hash_out ) 
		(*hash_out) = hash;
	
	return muse_assoc_iter( h->buckets + bucket, key );
}

muse_cell fn_hashtable( muse_env *env, hashtable_t *h, muse_cell args )
{
	muse_assert( h != NULL && "Context 'h' must be a hashtable object!" );

	{
		muse_cell key = muse_evalnext(&args);
		muse_int hash = 0;

		/* Find the kvpair if it exists in the hash table. */
		muse_cell *kvpair = hashtable_get( h, key, &hash );
				
		if ( args )
		{
			/* We've been asked to set a property. */
			muse_cell value = muse_evalnext(&args);

			/* First see if the key is already in the hash table. */
			if ( kvpair )
			{
				if ( value )
				{
					/* It already exists. Simply change the value to the new one. */
					muse_set_tail( muse_head(*kvpair), value );
					return value;
				} 
				else
				{
					/* The value is MUSE_NIL. Which means we have to remove
					the kvpair from the hashtable. */
					(*kvpair) = muse_tail( *kvpair );
					--(h->count);
					return MUSE_NIL;
				}
			}
			else 
			{
				if ( value )
				{
					/* It doesn't exist. Need to add a new entry.
					Check to see if we need to rehash the table. 
					We rehash if we have to do more than 2 linear
					searches on the average for each access. */
					hashtable_add( h, key, value, &hash );
					
					return value;
				}
				else
				{
					/* The key doesn't exist and the value is ().
					We don't need to do anything. */
					return MUSE_NIL;
				}
			}
		}
		
		/* We've been asked to get a property. */
		if ( kvpair )
			return muse_tail( muse_head( *kvpair ) );
		else
			return MUSE_NIL;
	}
}

static muse_cell hashtable_size( void *self )
{
	return muse_mk_int( ((hashtable_t*)self)->count );
}

static void hashtable_merge_one( hashtable_t *h1, muse_cell key, muse_cell new_value, muse_cell reduction_fn )
{
	int sp = muse_stack_pos();
	muse_int hash = 0;
	muse_cell *new_kv = hashtable_get( h1, key, &hash );
	
	if ( new_kv )
	{
		/* Key already exists. */
		if ( reduction_fn )
		{
			/* Set the value to reduction_fn( current_value, new_value ). */
			muse_set_tail( muse_head( *new_kv ), 
						   muse_apply( reduction_fn,
									   muse_cons( muse_tail( muse_head( *new_kv ) ),
												  muse_cons( new_value,
															 MUSE_NIL ) ),
									   MUSE_TRUE ) );					
		}
		else
		{
			/* No reduction function. Simply replace the old value with the new one. */
			muse_set_tail( muse_head( *new_kv ), new_value );
		}
	}
	else
	{
		/* Key doesn't exist. Need to add new. */
		hashtable_add( h1, key, new_value, &hash );
	}
	
	muse_stack_unwind(sp);
}

static muse_cell hashtable_map( void *self, muse_cell fn )
{
	hashtable_t *h = (hashtable_t*)self;
	
	muse_cell result = muse_mk_hashtable( h->count );
	hashtable_t *result_ptr = (hashtable_t*)muse_functional_object_data( result, 'hash' );
	
	muse_cell args = muse_cons( MUSE_NIL, MUSE_NIL );
	
	int sp = muse_stack_pos();
	int b;
	for ( b = 0; b < h->bucket_count; ++b )
	{
		muse_cell alist = h->buckets[b];
		
		while ( alist )
		{
			muse_cell kv = muse_head(alist);
			
			muse_set_head( args, muse_tail( kv ) );
			
			hashtable_add( result_ptr, muse_head( kv ), muse_apply( fn, args, MUSE_TRUE ), NULL );
						
			alist = muse_tail(alist);
			
			muse_stack_unwind(sp);
		}
	}
	
	return result;
}


static void hashtable_merge( hashtable_t *h1, hashtable_t *h2, muse_cell reduction_fn )
{
	int b = 0;
	int sp = muse_stack_pos();
	
	for ( b = 0; b < h2->bucket_count; ++b )
	{
		muse_cell alist = h2->buckets[b];
		
		while ( alist )
		{
			muse_cell kv = muse_head(alist);
			muse_cell key = muse_head(kv);
			muse_cell value = muse_tail(kv);
			
			hashtable_merge_one( h1, key, value, reduction_fn );
			
			muse_stack_unwind(sp);
			alist = muse_tail(alist);
		}
	}
}

static muse_cell hashtable_join( void *self, muse_cell objlist, muse_cell reduction_fn )
{
	hashtable_t *h1 = (hashtable_t*)self;
	
	muse_cell result = muse_mk_hashtable( h1->count );
	hashtable_t *result_ptr = (hashtable_t*)muse_functional_object_data( result, 'hash' );
	
	/* First add all the elements of h1. */
	hashtable_merge( result_ptr, h1, MUSE_NIL );
	
	/* Next add all elements of h2 to the result. */
	while ( objlist )
	{
		muse_cell obj = _next(&objlist);
		hashtable_t *h2 = (hashtable_t*)muse_functional_object_data( obj, 'hash' );
		hashtable_merge( result_ptr, h2, reduction_fn );
	}
	
	return result;
}

static muse_cell hashtable_collect( void *self, muse_cell predicate, muse_cell mapper, muse_cell reduction_fn )
{
	hashtable_t *h = (hashtable_t*)self;
	
	muse_cell result = muse_mk_hashtable( h->count );
	hashtable_t *result_ptr = (hashtable_t*)muse_functional_object_data( result, 'hash' );
	
	/* Step through self's contents and add all the key-value pairs that satisfy the predicate. */
	{
		int sp = muse_stack_pos();
		int b = 0;
		for ( b = 0; b < h->bucket_count; ++b )
		{
			muse_cell alist = h->buckets[b];
			
			while ( alist )
			{
				muse_cell kv = muse_head( alist );
				
				if ( !predicate || muse_apply( predicate, kv, MUSE_TRUE ) )
				{
					/* Key-value pair satisfied the predicate. */
					if ( mapper )
						kv = muse_apply( mapper, kv, MUSE_TRUE );
					
					hashtable_merge_one( result_ptr, muse_head(kv), muse_tail(kv), reduction_fn );
				}
				
				muse_stack_unwind(sp);
				alist = muse_tail(alist);
			}
		}
	}
	
	return result;
}

static muse_cell hashtable_reduce( void *self, muse_cell reduction_fn, muse_cell initial )
{
	hashtable_t *h = (hashtable_t*)self;
	
	muse_cell result = initial;
	muse_cell args = muse_cons( result, muse_cons( MUSE_NIL, MUSE_NIL ) );
	muse_cell *arg1 = &(_ptr(args)->cons.head);
	muse_cell *arg2 = &(_ptr(_tail(args))->cons.head);
	
	int sp = muse_stack_pos();
	int b = 0;
	for ( b = 0; b < h->bucket_count; ++b )
	{
		muse_cell alist = h->buckets[b];
		
		while ( alist )
		{
			(*arg1) = result;
			(*arg2) = muse_tail( muse_head( alist ) );
			
			result = muse_apply( reduction_fn, args, MUSE_TRUE );
			
			muse_stack_unwind(sp);
			muse_stack_push(result);
			
			alist = muse_tail(alist);
		}
	}
	
	return result;
}

static muse_cell hashtable_iterator( hashtable_t *self, muse_iterator_callback_t callback, void *context )
{
	int sp = _spos();
	int b;
	muse_boolean cont = MUSE_TRUE;
	
	for ( b = 0; b < self->bucket_count; ++b )
	{
		muse_cell alist = self->buckets[b];
		
		while ( alist )
		{
			cont = callback( self, context, _head(alist) );
			_unwind(sp);
			if ( !cont )
				return _head(_head(alist)); /**< Return the key. */
			
			alist = _tail(alist);
		}
	}	
	
	return MUSE_NIL;
}

static muse_monad_view_t g_hashtable_monad_view =
{
	hashtable_size,
	hashtable_map,
	hashtable_join,
	hashtable_collect,
	hashtable_reduce
};

static void *hashtable_view( int id )
{
	switch ( id )
	{
		case 'mnad' : return &g_hashtable_monad_view;
		case 'iter' : return hashtable_iterator;
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
 * (mk-hashtable [size]).
 * Creates a new hash table. No arguments are required, but
 * you can give the expected size of the hash table as an argument.
 */
muse_cell fn_mk_hashtable( muse_env *env, void *context, muse_cell args )
{
	return muse_mk_functional_object( &g_hashtable_type, args );
}

/**
 * (hashtable? ht).
 * Returns \c ht if it is a functional hashtable, or () if it isn't.
 */
muse_cell fn_hashtable_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell ht = muse_evalnext(&args);
	hashtable_t *h = (hashtable_t*)muse_functional_object_data(ht,'hash');
	
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
	muse_cell ht = muse_evalnext(&args);
	hashtable_t *h = (hashtable_t*)muse_functional_object_data(ht,'hash');
	
	muse_assert( h && h->base.type_info->type_word == 'hash' );
	
	return muse_mk_int( h->count );
}


/**
 * (hashtable alist).
 * Returns a hash table with the same contents as the given alist.
 */
muse_cell fn_alist_to_hashtable( muse_env *env, void *context, muse_cell args )
{
	muse_cell ht = fn_mk_hashtable( env, NULL, MUSE_NIL );
	
	muse_cell alist = muse_evalnext(&args);
	int count = 0;
	muse_cell *alistarr = muse_list_to_array( alist, &count );
	muse_cell alist_copy = muse_array_to_list( count, alistarr, 1 );
	free(alistarr);

	{
		hashtable_t *h = (hashtable_t*)muse_functional_object_data(ht,'hash');
		h->count = count;
		h->buckets[0] = alist_copy;
		hashtable_rehash( h, count );
	}
	
	return ht;
}

/**
 * (hashtable->alist ht).
 * Returns an alist version of the contents of the given hash table.
 * The order of the elements is unpredictable.
 */
muse_cell fn_hashtable_to_alist( muse_env *env, void *context, muse_cell args )
{
	muse_cell result = MUSE_NIL;
	muse_cell ht = muse_evalnext(&args);
	hashtable_t *h = (hashtable_t*)muse_functional_object_data(ht,'hash');
	
	muse_assert( h && h->base.type_info->type_word == 'hash' );
	
	{
		muse_cell *kvpairs		= (muse_cell*)malloc( sizeof(muse_cell) * h->count );
		muse_cell *kvpairs_iter = kvpairs;
		muse_cell *buckets_iter = h->buckets;
		muse_cell *buckets_end	= buckets_iter + h->bucket_count;

		/* Collect all kvpairs in the hash table into a single
			array so that we can use array->list conversion. */
		for ( ; buckets_iter < buckets_end; ++buckets_iter )
		{
			muse_cell alist = *buckets_iter;
			
			if ( alist )
			{
				int bucket_size = muse_list_length(alist);
				
				muse_list_extract( bucket_size, alist, 1, kvpairs_iter, 1 );
				
				kvpairs_iter += bucket_size;
			}
		}
		
		muse_assert( kvpairs_iter == kvpairs + h->count );
		
		result = muse_array_to_list( h->count, kvpairs, 1 );
		free(kvpairs);
	}
	
	return result;
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
#ifndef NDEBUG
	{	L"hashtable-stats",	fn_hashtable_stats		},
#endif
	{	NULL,					NULL					}
};

void muse_define_builtin_type_hashtable()
{
	int sp = muse_stack_pos();
	const struct _defs *defs = k_hashtable_funs;
	
	for ( ; defs->name; ++defs )
	{
		muse_define( muse_csymbol(defs->name), muse_mk_nativefn( defs->fn, NULL ) );
		muse_stack_unwind(sp);
	}
}

/**
 * Creates a hashtable with a bucket count setup according to the
 * given desired length. Note that calling muse_hashtable_length()
 * without first putting anything into the hashtable will always
 * get you 0. The "length" of the hashtable is the number of 
 * key-value pairs put into it.
 */
muse_cell muse_mk_hashtable( int length )
{
	int sp = _spos();
	muse_cell ht = fn_mk_hashtable( _env(), NULL, muse_cons( muse_mk_int(length), MUSE_NIL ) );
	_unwind(sp);
	_spush(ht);
	return ht;
}

/** 
 * Returns the number of key-value pairs put into the hash table.
 */
int muse_hashtable_length( muse_cell ht )
{
	hashtable_t *h = (hashtable_t*)muse_functional_object_data( ht, 'hash' );
	muse_assert( h != NULL && "Argument must be a hashtable object!" );
	return h ? h->count : 0;
}

/**
 * Returns the key-value pair with the given key as the head
 * if the key is present in the hash table, otherwise it returns MUSE_NIL.
 * The value associated with the key will be in the tail of the returned
 * pair.
 */
muse_cell muse_hashtable_get( muse_cell ht, muse_cell key )
{
	return fn_hashtable( _env(), 
						(hashtable_t*)muse_functional_object_data( ht, 'hash' ), 
						muse_cons( key, MUSE_NIL ) );
}

/**
 * Associates the given value with the given key in the hash table.
 * The given value replaces any previous value that might have been
 * associated with the key.
 */
muse_cell muse_hashtable_put( muse_cell ht, muse_cell key, muse_cell value )
{
	int sp = _spos();
	muse_cell result =
		fn_hashtable( _env(), 
						(hashtable_t*)muse_functional_object_data( ht, 'hash' ), 
						muse_cons( key, muse_cons( value, MUSE_NIL ) ) );
	_unwind(sp);
	return result;
}

/*@}*/
/*@}*/
