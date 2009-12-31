/**
 * @file muse_builtin_class.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_builtins.h"
#include "muse_port.h"
#include <memory.h>


typedef struct { muse_boolean inherited; muse_cell key, kvpair; } assoc_t;
enum { 
	ASSOC_CACHE_KEY_BITS = 4, 
	ASSOC_CACHE_SIZE = 1 << ASSOC_CACHE_KEY_BITS, 
	ASSOC_CACHE_MASK = ASSOC_CACHE_SIZE - 1 
};

typedef struct 
{
	muse_functional_object_t base;
	muse_cell supers;
	muse_cell plist;
	assoc_t cache[ASSOC_CACHE_SIZE];
} object_t;

static muse_cell object_get_prop( muse_env *env, void *self, muse_cell key, muse_cell argv );
static muse_cell object_put_prop( muse_env *env, void *self, muse_cell key, muse_cell argv );

static muse_prop_view_t g_object_prop_view = {
	object_get_prop,
	object_put_prop
};

static muse_cell object_format( muse_env *env, void *self );

static muse_format_view_t g_object_format_view = { object_format };

static void object_init( muse_env *env, void *ptr, muse_cell args )
{
	object_t *obj = (object_t*)ptr;

	obj->supers = _evalnext(&args);
	obj->plist = MUSE_NIL;
	
	/* Support passing a single object as the super. */
	if ( _cellt(obj->supers) != MUSE_CONS_CELL ) {
		obj->supers = _cons( obj->supers, MUSE_NIL );
	}

	while ( args ) {
		int sp = _spos();
		muse_cell key = _evalnext(&args);
		muse_cell value = _evalnext(&args);

		obj->plist = _cons( _cons( key, value ), obj->plist );
		_unwind(sp);
	}

	memset( obj->cache, 0, sizeof(obj->cache) );
}

static void *object_view( muse_env *env, int id ) 
{
	switch ( id ) {
		case 'prop' : return &g_object_prop_view;
		case 'frmt' : return &g_object_format_view;
		default		: return NULL;
	}
}

static void object_mark( muse_env *env, void *obj )
{
	muse_mark( env, ((object_t*)obj)->supers );
	muse_mark( env, ((object_t*)obj)->plist );
}

static void object_destroy( muse_env *env, void *obj )
{
}

static void object_write( muse_env *env, void *_self, void *_port )
{
	object_t *self = (object_t*)_self;
	muse_port_t port = (muse_port_t)_port;

	port_write( "{object ", 8, port );
	if ( self->supers )
	{
		port_putc( '\'', port );
		muse_pwrite( port, self->supers );
	}
	else
	{
		port_putc( '(', port );
		port_putc( ')', port );
	}

	{
		muse_cell plist = self->plist;
		while ( plist ) {
			muse_cell kv = _next(&plist);
			port_putc( ' ', port );
			port_putc( '\'', port );
			muse_pwrite( port, _head(kv) );
			port_putc( ' ', port );
			{
				muse_cell t = _tail(kv);
				if ( _cellt(t) == MUSE_LAMBDA_CELL ) {
					port_write( "(fn ", 4, port );
					muse_pwrite( port, _head(t) );
					port_putc( ')', port );
				} else {
					port_putc( '\'', port );
					muse_pwrite( port, t );
				}
			}
		}
	}
	port_putc( '}', port );
}

muse_cell object_plist( muse_env *env, muse_cell obj )
{
	return ((object_t*)muse_functional_object_data( env, obj, 'mobj' ))->plist;
}

static muse_cell fn_object_fn( muse_env *env, object_t *obj, muse_cell args )
{
	muse_cell key = _evalnext(&args);
	muse_cell method = object_get_prop( env, obj, key, MUSE_NIL );

	return muse_add_recent_item( env, key, muse_apply( env, method, _cons( obj->base.self, args ), MUSE_FALSE, MUSE_FALSE ) );
}

static muse_functional_object_type_t g_object_type =
{
	'muSE',
	'mobj',
	sizeof(object_t),
	(muse_nativefn_t)fn_object_fn,
	object_view,
	object_init,
	object_mark,
	object_destroy,
	object_write
};


/**
 * @code 
 * (new supers-list prop1 value1 prop2 value2 ...)
 * (new supers-list)
 * (new)
 * @endcode
 *
 * Creates an "object" that you can use for OOP.
 * \ref fn_get "get" gets properties of the object
 * (even if nested) and \ref fn_put "put" sets
 * properties of objects, even if nested. You can
 * use "object" instead of "new" for clarity. They
 * are synonyms.
 *
 * \ref fn_get "get" searches the object's hierarchy
 * for the property if it is not found in the object 
 * itself. \ref fn_put "put" always modifies only
 * the object's own properties, even if nested.
 *
 * The result of the most recent get or put operation 
 * can be obtained using \ref fn_the "the" by giving the
 * property key as the argument. For example -
 * @code
 * > (new () 'message "hi there!")
 * > (get (the object) 'message)
 * hi there!
 * > (write (the 'message))
 * "hi there!"
 * @endcode
 *
 * Using the object in the function position with
 * a method key as the first argument will invoke
 * the corresponding method on the object. The result
 * of the most recent method invocation can be accessed
 * using \ref fn_the "the" by supplying the method key.
 * For example -
 * @code
 * > (define o (object () 'task (fn (self) (print "did task") "result")))
 * > (o 'task)
 * did task
 * result
 * > (write (the 'task))
 * "result"
 * @endcode
 *
 * You can use \ref fn_super_invoke "super-invoke"
 * to call methods defined in the super objects.
 *
 * You can edit an object's supers list using
 * \ref fn_supers "supers".
 *
 * @note Property and method access will be fastest
 * if keyed by a symbol since symbol equality can
 * be tested by an int comparison.
 */
muse_cell fn_new( muse_env *env, void *context, muse_cell args )
{
	muse_cell obj = _mk_functional_object( &g_object_type, args );
	return muse_add_recent_item( env, (muse_int)fn_new, obj );
}

/**
 * @code (supers object) @endcode
 *
 * Gets the object's supers list. 
 *
 * @code (supers object supers-list) @endcode
 *
 * Sets the object's supers list to the one given. If you
 * modify the list returned by (supers object), do call (supers object supers-list)
 * to tell the object that its supers list has changed.
 *
 * @exception error:object-expected
 *	Handler format: @code (fn (resume 'error:object-expected obj) ...) @endcode
 *	Raised if the given thing is not an object. 
 *	The given "object" is passed to the handler in \p obj.
 *	You can resume by resolving the correct object in the handler.
 */
muse_cell fn_supers( muse_env *env, void *context, muse_cell args )
{
	muse_cell orig_args = args;
	muse_cell obj = _evalnext(&args);
	object_t *it = (object_t*)muse_functional_object_data( env, obj, 'mobj' );
	if ( it ) {
		if ( args ) {
			/* Change supers list. */
			it->supers = _evalnext(&args);

			/* Invalidate cache. */
			memset( it->cache, 0, sizeof(it->cache) );
		}

		return it->supers;
	} else {
		return fn_supers( 
					env, 
					NULL, 
					_cons( muse_raise_error( env, _csymbol(L"error:object-expected"), _cons( obj, MUSE_NIL ) ), args ) );
	}
}


static inline int keyindex( muse_cell key )
{
	return (int)(_celli(key) & ASSOC_CACHE_MASK);
}

static muse_cell object_cache( object_t *self, int keyi, muse_cell key, muse_cell kvpair, muse_boolean inherited )
{
	assoc_t *a = self->cache + keyi;
	a->inherited = inherited;
	a->key = key;
	a->kvpair = kvpair;
	return kvpair;
}

static muse_cell object_search_prop( muse_env *env, object_t *self, muse_cell key, muse_boolean *inherited, muse_boolean search_hierarchy )
{
	int keyi = keyindex(key);

	if ( self->cache[keyi].key == key ) {
		/* Cache hit. */
		muse_cell kv = self->cache[keyi].kvpair;
		if ( inherited ) (*inherited) = self->cache[keyi].inherited;
		return kv;
	} else {
		/* Cache miss. */
		muse_cell kv = muse_assoc( env, self->plist, key );
		if ( kv ) {
			/* Found in current object. Store in cache. */
			if ( inherited ) (*inherited) = MUSE_FALSE;
			return object_cache( self, keyi, key, kv, MUSE_FALSE );
		} else if ( search_hierarchy ) {
			/* Not found. Search hierarchy. */
			muse_cell supers = self->supers;
			muse_cell kv = MUSE_NIL;
			while ( supers && !kv ) {
				muse_cell super = _next(&supers);
				object_t *superobj = (object_t*)muse_functional_object_data( env, super, 'mobj' );
				if ( superobj ) {
					kv = object_search_prop( env, superobj, key, NULL, MUSE_TRUE );
					if ( kv ) {
						if ( inherited ) (*inherited) = MUSE_TRUE;
						return object_cache( self, keyi, key, kv, MUSE_TRUE );
					}
				} else {
					return MUSE_NIL;
				}
			}
			return MUSE_NIL;
		} else {
			/* Not found anywhere. */
			return MUSE_NIL;
		}
	}
}

static muse_cell object_get_prop( muse_env *env, void *self, muse_cell key, muse_cell argv )
{
	object_t *obj = (object_t*)self;

	muse_cell kv = object_search_prop( env, obj, key, NULL, MUSE_TRUE );
	if ( kv ) {
		/* Found. */
		if ( argv ) {
			return muse_get( env, muse_add_recent_item( env, key, _tail(kv) ), _head(argv), _tail(argv) );
		} else {
			return muse_add_recent_item( env, key, _tail(kv) );
		}
	} else {
		return MUSE_NIL;
	}
}

static muse_cell object_put_prop( muse_env *env, void *self, muse_cell key, muse_cell argv )
{
	object_t *obj = (object_t*)self;
	muse_boolean inherited = MUSE_FALSE;

	muse_cell kv = object_search_prop( env, obj, key, &inherited, MUSE_FALSE );
	muse_cell val = _next(&argv);
	if ( kv && !inherited ) {
		/* Found. */
		if ( argv ) {
			return muse_put( env, muse_add_recent_item( env, key, _tail(kv) ), val, argv );
		} else {
			_sett( kv, val );
			return muse_add_recent_item( env, key, val );
		}
	} else {
		/* Not found in object (even if found in parent). Add to object. */
		int keyi = keyindex(key);
		if ( argv ) {
			/* If deep property setting, create an object. */
			muse_cell newobj = fn_new( env, NULL, MUSE_NIL );
			kv = _cons( key, newobj );
			obj->plist = _cons( kv, obj->plist );
			object_cache( obj, keyi, key, kv, MUSE_FALSE );
			return muse_put( env, muse_add_recent_item( env, key, newobj ), val, argv );
		} else {
			kv = _cons( key, val );
			obj->plist = _cons( kv, obj->plist );
			object_cache( obj, keyi, key, kv, MUSE_FALSE );
			return muse_add_recent_item( env, key, val );
		}
	}
}

static muse_cell object_format( muse_env *env, void *ptr )
{
	/* Get the "as-string" method of the object. 
	If present, call it to get a string. */
	object_t *self = (object_t*)ptr;
	muse_cell sym_as_string = _csymbol(L"as-string");

	muse_cell as_string_kv = object_search_prop( env, self, sym_as_string, NULL, MUSE_TRUE );

	if ( as_string_kv ) {
		muse_cell as_string = _tail(as_string_kv);
		if ( _cellt(as_string) == MUSE_LAMBDA_CELL ) {
			/* It is a method. Apply it. */
			return muse_apply( env, as_string, _cons( self->base.self, MUSE_NIL ), MUSE_TRUE, MUSE_FALSE );
		} else {
			/* It is a value, just return it. */
			return as_string;
		}
	} else {
		return _csymbol(L"{object}");
	}
}


static muse_cell super_invoke( muse_env *env, object_t *self, muse_cell supers, muse_cell methodkey, muse_cell args )
{
	while ( supers ) {
		muse_cell super = _next(&supers);
		muse_cell method = muse_get( env, super, methodkey, MUSE_NIL );
		if ( method ) {
			return muse_add_recent_item( 
						env, 
						methodkey,
						muse_apply( env, method, _cons( self->base.self, args ), MUSE_FALSE, MUSE_FALSE ) );

		} 
	}

	return muse_raise_error( env, _csymbol(L"error:method-not-found"), _cons( self->base.self, _cons( methodkey, MUSE_NIL ) ) );
}

/**
 * @code (super-invoke obj 'method-name . args) @endcode
 *
 * Invokes a method definition searching the super list
 * without searching the object's own property list.
 *
 * @exception error:method-not-found 
 *	Handler format: @code (fn (resume 'error:method-not-found obj methodkey) ...) @endcode
 *	Raised when the given method is not found in the super tree.
 *	The sole argument is the original method key given. You can
 *	resume by passing the expected result of the invocation.
 *
 * @exception error:object-expected
 *	Handler format: @code (fn (resume 'error:object-expected obj) ...) @endcode
 *	Raised if the given thing is not an object. 
 *	The given "object" is passed to the handler in \p obj.
 *	You can resume by resolving the correct object in the handler.
 */
muse_cell fn_super_invoke( muse_env *env, void *context, muse_cell args )
{
	muse_cell obj = _evalnext(&args);
	object_t *it = (object_t*)muse_functional_object_data( env, obj, 'mobj' );
	if ( it ) {
		muse_cell methodkey = _evalnext(&args);
		return super_invoke( env, it, it->supers, methodkey, args );
	} else {
		return fn_super_invoke( env, NULL, _cons( muse_raise_error( env, _csymbol(L"error:object-expected"), _cons( obj, MUSE_NIL ) ), args ) );
	}
}

/**
 * @code (super-invoke* obj supers-list 'method-name . args) @endcode
 *
 * Invokes a method definition searching the *given* super list
 * without searching the object's own property list.
 * @code (super-invoke obj methodkey . args) @endcode is equivalent to
 * @code (super-invoke* obj (supers obj) methodkey . args) @endcode
 *
 * @exception error:method-not-found 
 *	Handler format: @code (fn (resume 'error:method-not-found obj methodkey) ...) @endcode
 *	Raised when the given method is not found in the super tree.
 *	The sole argument is the original method key given. You can
 *	resume by passing the expected result of the invocation.
 *
 * @exception error:object-expected
 *	Handler format: @code (fn (resume 'error:object-expected obj) ...) @endcode
 *	Raised if the given thing is not an object. 
 *	The given "object" is passed to the handler in \p obj.
 *	You can resume by resolving the correct object in the handler.
 */
muse_cell fn_super_invoke_explicit( muse_env *env, void *context, muse_cell args )
{
	muse_cell obj = _evalnext(&args);
	object_t *it = (object_t*)muse_functional_object_data( env, obj, 'mobj' );
	if ( it ) {
		muse_cell supers = _evalnext(&args);
		muse_cell methodkey = _evalnext(&args);
		return super_invoke( env, it, supers, methodkey, args );
	} else {
		return fn_super_invoke_explicit( env, NULL, _cons( muse_raise_error( env, _csymbol(L"error:object-expected"), _cons( obj, MUSE_NIL ) ), args ) );
	}
}

/**
 * Either x == type or x inherits from type directly or indirectly.
 * The return value is retval if the inheritance condition is satisfied.
 * Oherwise it is MUSE_NIL.
 */
static muse_cell is_parent_of( muse_env *env, muse_cell type, muse_cell x, muse_cell retval )
{
	if ( type == x )
		return retval;
	else {
		muse_cell supers = ((object_t*)muse_functional_object_data( env, x, 'mobj' ))->supers;
		while ( supers ) {
			muse_cell a = is_parent_of( env, type, _next(&supers), retval );
			if ( a ) return retval;
		}
		return MUSE_NIL;              
	}
}

/**
 * @code (isa? type x) @endcode
 *
 * Compares types. If type and x are of the same non object type,
 * it returns x. If type and x are objects and they both have one
 * common super class, it returns the x. If none of these conditions 
 * are satisfied, it returns ().
 */
muse_cell fn_isa_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell type = _evalnext(&args);
	muse_cell x = _evalnext(&args);

	if ( muse_functional_object_data( env, type, 'mobj' ) && muse_functional_object_data( env, x, 'mobj' ) ) {
		return is_parent_of( env, type, x, x );		
	} else {
		return _cellt(x) == _cellt(type) ? type : MUSE_NIL;
	}
}

/**
 * @code (object? obj) @endcode
 *
 * Evalutes to \p obj if it is an object and to () if it isn't.
 */
muse_cell fn_object_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell thing = _evalnext(&args);
	muse_functional_object_t *f = _fnobjdata(thing);
	if ( f && f->type_info->type_word == 'mobj' )
		return thing;
	else
		return MUSE_NIL;
}