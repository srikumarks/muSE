/**
 * @file muse_builtin_algo.c
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

typedef struct _sort_property_t
{
	muse_env *env;
	muse_cell cell;
	muse_cell pty;
} sort_property_t;

static int pty_compare( const void *cell1, const void *cell2 )
{
	muse_env *env = ((sort_property_t *)cell1)->env;
	return _compare( ((const sort_property_t *)cell1)->pty, ((const sort_property_t *)cell2)->pty );
}

static muse_cell listiter( muse_env *env, muse_cell *listptr, int i, muse_boolean *eol )
{
	muse_cell c = *listptr;
	muse_cell result = _head(c);

	(*eol) = c ? MUSE_FALSE : MUSE_TRUE;

	(*listptr) = _tail(c);
	return result;
}

typedef struct 
{
	muse_cell propertyFn;
	muse_cell argcell;
	muse_cell domain;
} propmapper_t;

static muse_cell propiter( muse_env *env, propmapper_t *prop, int i, muse_boolean *eol )
{
	if ( prop->domain )
	{
		_seth( prop->argcell, _head(prop->domain) );
		(*eol) = MUSE_FALSE;
		prop->domain = _tail(prop->domain);
		return _apply( prop->propertyFn, prop->argcell, MUSE_TRUE );
	}
	else
	{
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
}


/** 
 * Creates another list with the same objects as in the given list.
 */
static muse_cell listdup( muse_env *env, muse_cell list )
{
	if ( !list )
		return MUSE_NIL;


	return muse_generate_list( env, (muse_list_generator_t)listiter, &list );
}

static muse_cell sortproplist( muse_env *env, muse_cell propfn, muse_cell list )
{
	propmapper_t pm;
	pm.argcell = _cons( MUSE_NIL, MUSE_NIL );
	pm.domain = list;
	pm.propertyFn = propfn;

	return muse_generate_list( env, (muse_list_generator_t)propiter, &pm );
};

static muse_cell sort_by_property_inplace( muse_env *env, muse_cell list, muse_cell propertyFn )
{
	int	sp				= _spos();
	int	length			= _list_length(list);
	
	/* Allocate a vector to hold a copy of the cells
		so that we can sort fast. */
	sort_property_t *vec = (sort_property_t*)malloc( sizeof(sort_property_t) * length );
	if ( !vec )
		return MUSE_NIL;
	
	/* Fill the vector with the list entries. */
	if ( propertyFn )
	{
		int			i		= 0;
		muse_cell	c		= list; 
		muse_cell	p		= sortproplist( env, propertyFn, list );

		for ( ; i < length; ++i, c = _tail(c), p = _tail(p) )
		{
			vec[i].env		= env;
			vec[i].cell		= _head(c);
			vec[i].pty		= _head(p);
		}
	}
	else
	{
		/*	We're not given a property function. So simply
		compare the cell contents. */
		int			i = 0;
		muse_cell	c = list; 
		for ( ; i < length; ++i, c = _tail(c) )
		{
			vec[i].env = env;
			vec[i].cell	= vec[i].pty = _head(c);
		}
	}
	
	/* Sort using the generic qsort algo. */
	qsort( vec, length, sizeof(sort_property_t), pty_compare );
	
	/* Put the sorted items into the list cells. */
	{
		int			i = 0;
		muse_cell	c = list;
		
		for ( ; i < length; ++i, c = _tail(c) )
		{
			_seth( c, vec[i].cell );
		}
	}
	
	/* Free the temp vector. */
	free(vec);
	
	_unwind(sp);
	return list;
}

/**
 * (sort! list [propertyFn]).
 * Sorts a list in place according to an optional property function.
 * The first argument is the list of objects to sort. The cells
 * in the list are replaced with the same objects in sorted order.
 * The usual object ordering is used, which will sort a numeric
 * list in ascending order. 
 * 
 * A propertyFn can be specified to control the sorting order.
 * The ordering of two objects in the list is determined by the
 * ordering of the objects resulting from the application of the
 * property function to the list objects.
 * 
 * This method is not as flexible as giving a comparator function, but
 * it is good enough for most purposes in a scripting language
 * and is more efficient than giving a comparator function. 
 * For example, if you want to sort a numeric list in descending
 * order instead of ascending order, you can write -
 * 		@code (sort! ls -) @endcode 
 * instead of @code (sort! ls) @endcode
 * If the list consists of lists, then you can use the propertyFn
 * to select an element of the list entries to sort by.
 * 
 */
muse_cell fn_sort_inplace( muse_env *env, void *context, muse_cell args )
{
	muse_cell	list			= _evalnext(&args);
	muse_cell	propertyFn		= args ? _evalnext(&args) : MUSE_NIL;
	return muse_add_recent_item( env, (muse_int)fn_sort_inplace, sort_by_property_inplace( env, list, propertyFn ) );
}

/**
 * Just like fn_sort_inplace(), except that the original list
 * is not modified. A new list of the same objects in sorted
 * order is returned.
 */
muse_cell fn_sort( muse_env *env, void *context, muse_cell args )
{
	muse_cell	list			= listdup( env, _evalnext(&args) );
	muse_cell	propertyFn		= args ? _evalnext(&args) : MUSE_NIL;
	return muse_add_recent_item( env, (muse_int)fn_sort, sort_by_property_inplace( env, list, propertyFn ) );
}


/**
 * (reverse list)
 *
 * Evaluates to a list with items in the reverse order of the given
 * list. The given list is not modified.
 */
muse_cell fn_reverse( muse_env *env, void *context, muse_cell args )
{
	muse_cell list = _evalnext(&args);
	muse_cell result = MUSE_NIL;

	while ( list )
	{
		result = _cons( _next(&list), result );
	}

	return muse_add_recent_item( env, (muse_int)fn_reverse, result );
}

/**
 * (reverse! list)
 *
 * Evaluates to a list with items in the reverse order of the given
 * list. The given list is reversed in-place to conserve memory, 
 * therefore the cell given as input is left pointing to the last
 * element of the list. Use @code (set! x (reverse! x)) @endcode
 * if you want to modify a variable containing a list to its
 * reversed version.
 */
muse_cell fn_reverse_inplace( muse_env *env, void *context, muse_cell args )
{
	muse_cell list = _evalnext(&args);
	muse_cell result = MUSE_NIL;

	while ( list )
	{
		muse_cell next = _tail(list);
		_sett( list, result );
		result = list;
		list = next;
	}

	return muse_add_recent_item( env, (muse_int)fn_reverse_inplace, result );
}
