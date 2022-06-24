/**
 * @file muse_builtin_box.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * Implements R5RS-style boxes. 
 */
#include "muse_builtins.h"
#include "muse_port.h"

/** @addtogroup FunctionalObjects */
/*@{*/
/**
 * @defgroup Boxes
 *
 * A box is a container for one object.
 * You can use the box like a function - sort of like R5RS "properties".
 * If b is a box, the the expression (b) gets the contents of the box.
 * The expression (b val) puts the value into the box and returns the 
 * new value.
 */
/*@{*/

typedef struct 
{
	muse_functional_object_t base;
	muse_cell contents;
} box_t;

static void box_init( muse_env *env, void *ptr, muse_cell args )
{
	box_t *b = (box_t*)ptr;
	b->contents = _evalnext(&args);
}

static void box_mark( muse_env *env, void *ptr )
{
	box_t *b = (box_t*)ptr;
	muse_mark( env, b->contents );
}

/**
 * Writes out the box's contents to the given port in such a
 * way that the expression written out is converted
 * to a vector by a trusted read operation.
 */
static void box_write( muse_env *env, void *ptr, void *port )
{
	box_t *b = (box_t*)ptr;
	muse_port_t p = (muse_port_t)port;
	
	port_putc( '{', p );
	port_write( "box ", 4, p );
	muse_pwrite( p, muse_quote( env, b->contents ) );
	port_putc( '}', p );
}

/**
 * The function that implements box access.
 */
muse_cell fn_box( muse_env *env, box_t *b, muse_cell args )
{
	if ( args ) {
		/* Set the contents of the box to the new value. */
		return (b->contents = _evalnext(&args));
	} else {
		/* Retrieve the contents of the box. */
		return b->contents;
	}
}

/**
 * @defgroup Box property access
 *
 * A box conforms to the "property access protocol" with id 'prop'.
 * This means you can use `get` and `put` with boxes as though
 * they were normal identifiers for their contents. For example -
 *
 * (define b (box (vector 1 2 3 4)))
 * (print (get b 2))                   ; prints 3
 * (put b 2 42)                        ; changes 3 within the vector to 42
 * (print (b))                         ; prints (vector 1 2 42 4)
 *
 * Furthermore, to make some code clearer, you can use `get` without
 * additional "key" arguments to retrieve the full contents of the box.
 * Similarly you can use `put` with a single additional argument 
 * (without a value) to set the contents of the box. This makes code
 * clearer to read.
 */
/*@{*/
static muse_cell box_get( muse_env *env, void *self, muse_cell key, muse_cell argv )
{
        box_t *b = (box_t*)self;
        if ( key ) {
                return muse_get( env, b->contents, key, argv );
        } else {
                return b->contents;
        }
}

static muse_cell box_put( muse_env *env, void *self, muse_cell key, muse_cell argv )
{
        box_t *b = (box_t*)self;
        if ( !argv ) {
                // Only the key argument given. Use it as the value.
                b->contents = key;
                return key;
        }

        if ( key ) {
                // The arguments could include a sequence of keys with a value at the end.
                // We handle it as though the box was deferenced for its value and used.
                return muse_put( env, b->contents, key, argv );
        }

        return MUSE_NIL;
}

static muse_prop_view_t g_box_prop_view =
{
        box_get,
        box_put
};
/*@}*/

void *box_view( muse_env *env, int id ) 
{
        switch (id) {
                case 'prop': return &g_box_prop_view;
                default: return NULL;
        }
}

static muse_functional_object_type_t g_box_type =
{
	'muSE',
	'boxx',
	sizeof(box_t),
	(muse_nativefn_t)fn_box,
	box_view,
	box_init,
	box_mark,
	NULL,
	box_write
};


/**
 * Creates a new box with the given contents. If you
 * pass MUSE_NIL for the contents, it is equivalent to
 * creating an empty box.
 */
MUSEAPI muse_cell muse_mk_box( muse_env *env, muse_cell contents )
{
	muse_cell b = _mk_functional_object( &g_box_type, MUSE_NIL ); 
	muse_box_set( env, b, contents );
	return b;
}

/**
 * Returns the contents of the given box.
 */
MUSEAPI muse_cell muse_box_get( muse_env *env, muse_cell box )
{
	box_t *b = (box_t*)_functional_object_data(box,'boxx');
	if ( b ) {
		return b->contents;
	} else {
		MUSE_DIAGNOSTICS({
			muse_message( env, L"muse_box_get(env,>>box<<)", L"Argument is not a box!" );
		});
		return MUSE_NIL;
	}
}

/**
 * Changes the contents of the given box to the new value.
 * Returns the new value.
 */
MUSEAPI muse_cell muse_box_set( muse_env *env, muse_cell box, muse_cell contents )
{
	box_t *b = (box_t*)_functional_object_data(box,'boxx');
	if ( b ) {
		return b->contents = contents;
	} else {
		MUSE_DIAGNOSTICS({
			muse_message( env, L"muse_box_set(env,>>box<<,contents)", L"Argument is not a box!" );
		});
		return MUSE_NIL;
	}
}

/**
 * @code (box [contents]) @endcode
 *
 * Creates a new box with the given contents. If contents is
 * not given, a new box with () as the contents will be created.
 * A box with () as its contents is considered to be empty.
 */
muse_cell fn_mk_box( muse_env *env, void *context, muse_cell args )
{
	return _mk_functional_object( &g_box_type, args ); 
}

void muse_define_builtin_type_box(muse_env *env)
{
	int sp = _spos();
	_define( _csymbol(L"box"), _mk_nativefn( fn_mk_box, NULL ) );
	_unwind(sp);
}

/*@}*/
/*@}*/
