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
	muse_cell cell;
	muse_cell pty;
} sort_property_t;

static int pty_compare( const void *cell1, const void *cell2 )
{
	return muse_compare( ((const sort_property_t *)cell1)->pty, ((const sort_property_t *)cell2)->pty );
}

static muse_cell sort_by_property_inplace( muse_cell list, muse_cell propertyFn )
{
	int	sp				= _spos();
	int	length			= muse_list_length(list);
	
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
		int			sp		= _spos();
		muse_cell	argcell = muse_cons( MUSE_NIL, MUSE_NIL );

		/* TODO: Danger of exceeding stack limit! We should use
		a vector to do the sorting. */
		for ( ; i < length; ++i, c = _tail(c) )
		{
			muse_cell h		= _head(c);
			
			_seth( argcell, h );
			vec[i].cell		= h;
			vec[i].pty		= muse_apply( propertyFn, argcell, MUSE_TRUE );
		}

		_unwind(sp);
		_returncell( argcell ); /*	We can return it immediately 'cos 
			its only a scaffolding cell. This is
			a constraint on the propertyFn -
			that it should not keep its argument
			list for later use. */
	}
	else
	{
		/*	We're not given a property function. So simply
		compare the cell contents. */
		int			i = 0;
		muse_cell	c = list; 
		for ( ; i < length; ++i, c = _tail(c) )
		{
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
	muse_cell	list			= muse_evalnext(&args);
	muse_cell	propertyFn		= args ? muse_evalnext(&args) : MUSE_NIL;
	return sort_by_property_inplace( list, propertyFn );
}

/**
 * Just like fn_sort_inplace(), except that the original list
 * is not modified. A new list of the same objects in sorted
 * order is returned.
 */
muse_cell fn_sort( muse_env *env, void *context, muse_cell args )
{
	muse_cell	list			= muse_dup( muse_evalnext(&args) );
	muse_cell	propertyFn		= args ? muse_evalnext(&args) : MUSE_NIL;
	return sort_by_property_inplace( list, propertyFn );
}
