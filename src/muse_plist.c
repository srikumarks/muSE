/**
 * @file muse_plist.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#include "muse_opcodes.h"

/**
 * Returns the property list of the given symbol.
 * The symbol can be either a named symbol such as that
 * created by the \c muse_symbol() class of functions,
 * or an anonymous symbol created by \c muse_mk_anon_symbol().
 */
MUSEAPI muse_cell muse_symbol_plist( muse_env *env, muse_cell sym )
{
	/* Skip the first item that has the hash code and symbol name. */
	return _tail(_tail(sym));
}

/**
 * Looks up a particular association by key in an
 * association list. An association list has the form
 * @code ((key1 . value1) (key2 . value2) ... (keyN . valueN)) @endcode
 * When given a key \c keyK, it returns the pair 
 * @code (keyK . valueLK) @endcode 
 * if the key can be found in the association list,
 * or () if it doesn't exist in the list.
 * 
 * @note I'm considering changing this function to
 * return the list whose head is the found key-value
 * pair instead. This will let us search through
 * an assoc list for all instances of a given key.
 */
MUSEAPI muse_cell muse_assoc( muse_env *env, muse_cell alist, muse_cell prop )
{
	if ( !alist )
		return MUSE_NIL;
	else if ( muse_equal( env, _head(_head(alist)), prop ) )
		return _head(alist);
	else
		return muse_assoc( env, _tail(alist), prop );
}

/**
 * Similar to \ref muse_assoc, except that the return
 * value is not the kvpair, but the list position
 * of the kvpair. Since the return value is also
 * an assoc list, you can iterate through the list
 * using this function.
 *
 * The reason the return value is given as a muse_cell*
 * instead of simply a muse_cell is so that you can
 * remove the found cell using a simple pointer
 * setting operation, from the assoc list. 
 * @code
 * muse_cell alist, prop;
 * muse_cell *pos = muse_assoc_iter( &alist, prop );
 * (*pos) = muse_tail(*pos);
 * @endcode
 *
 * If the property is not found in the alist, the return
 * value is a pointer to the tail cell of the end of
 * the alist. This way, you can add the item to the end
 * if the property is not found.
 * @code
 * muse_cell alist, prop, defval;
 * muse_cell *pos = muse_assoc_iter(&alist, prop);
 * if ( !*pos )
 *    (*pos) = muse_cons( muse_cons( prop, defval ), MUSE_NIL );
 * @endcode
 */
muse_cell *muse_assoc_iter( muse_env *env, muse_cell *alist, muse_cell prop )
{
	if ( !*alist )
		return alist;
	else if ( muse_eq( env, _head(_head(*alist)), prop ) )
		return alist;
	else
		return muse_assoc_iter( env, &_ptr(*alist)->cons.tail, prop );
}

/**
 * Looks up the given property in the given symbol's
 * property list. The return value is the pair whose
 * head is the property key and the tail is the property
 * value.
 */
MUSEAPI muse_cell muse_get_prop( muse_env *env, muse_cell sym, muse_cell prop )
{
	return muse_assoc( env, muse_symbol_plist(env, sym), prop );
}

/**
 * Sets or adds the given property-value association to
 * the symbol's property list. If the property already
 * exists in the symbol's plist, the value of the property
 * is changed to the new one. If it doesn't exist, a new cell
 * with the new association is created and added to the symbol's
 * property list.
 * 
 * @return The cell in the symbol's plist that contains 
 * the association.
 */
MUSEAPI muse_cell muse_put_prop( muse_env *env, muse_cell sym, muse_cell prop, muse_cell value )
{
	muse_cell p = _get_prop( sym, prop );
	if ( p )
		_sett( p, value );
	else
	{
		p = _cons(prop,value);
		_sett( _tail(sym), _cons( p, _tail(_tail(sym)) ) );
	}
	return p;
}

/**
 * Shallow compares two cells. Two cells
 * are "eq" if either they refer to the same cell,
 * or they are equal intgers.
 */
MUSEAPI int muse_eq( muse_env *env, muse_cell a, muse_cell b )
{
	if ( a == b )
		return MUSE_TRUE;
	
	if ( _cellt(a) == MUSE_INT_CELL && _cellt(b) == MUSE_INT_CELL && _ptr(a)->i == _ptr(b)->i )
		return MUSE_TRUE;
	
	return MUSE_FALSE;
}

/**
 * Deep compares two cells.
 * @return 1 if the two cells are equal, 0 otherwise.
 */
MUSEAPI int muse_equal( muse_env *env, muse_cell a, muse_cell b )
{
	/* Two cells are eq is they are the same! */
	if ( a == b )
		return MUSE_TRUE;
	
	/* If one of them is MUSE_NIL, we return MUSE_FALSE. */
	if ( !(a && b) )
		return MUSE_FALSE;
	
	/* If two cells are of the same type, we can compare primitive types for eqness. */
	if ( _cellt(a) == _cellt(b) )
	{
		switch ( _cellt(a) )
		{
			case MUSE_INT_CELL		: return _ptr(a)->i == _ptr(b)->i;
			case MUSE_FLOAT_CELL	: return _ptr(a)->f == _ptr(b)->f;
			case MUSE_TEXT_CELL		: return wcscmp( _ptr(a)->text.start, _ptr(b)->text.start ) == 0;
			case MUSE_CONS_CELL		: return muse_equal( env, _head(a), _head(b) ) && muse_equal( env, _tail(a), _tail(b) );
			default					: return a == b;
		}
	}
	else
	{
		/* If two cells are of different types, they can't be compared, unless they are both numeric. */
		switch ( _cellt(a) )
		{
			case MUSE_INT_CELL		:	if ( _cellt(b) == MUSE_FLOAT_CELL )
											return (muse_float)_ptr(a)->i == _ptr(b)->f;
										else
											break;
			case MUSE_FLOAT_CELL	:	if ( _cellt(b) == MUSE_INT_CELL )
											return (muse_float)_ptr(b)->i == _ptr(a)->f;
										else
											break;
			default:;
		}
		
		return MUSE_FALSE;
	}
}

int compare_i_f( muse_int i, muse_float f )
{
	return i < f ? -1 : 1;
}

int compare_i_i( muse_int i, muse_int f )
{
	if ( i < f )
		return -1;
	if ( i > f )
		return 1;
	return 0;
}

int compare_f_f( muse_float i, muse_float f )
{
	if ( i < f )
		return -1;
	if ( i > f )
		return 1;
	return 0;
}

int compare_contents_of_conses( muse_env *env, muse_cell a, muse_cell b )
{
	/* CAUTION! Won't work for data structures with cycles. */
	{
		int c = _compare( _head(a), _head(b) );
		if ( c != 0 )
			return c;
	}
	
	return _compare( _tail(a), _tail(b) );
}

/**
 * Deep compares two cells for ordering.
 * @return 0 if the cells are eq, 1 if b < a and -1 if a < b.
 */
int	muse_compare( muse_env *env, muse_cell a, muse_cell b )
{
	if ( a == b )
		return 0;

	/* If either is nil, we really can't compare contents, so we just do a cell compare. */
	if ( a == MUSE_NIL || b == MUSE_NIL )
		return a - b;
	
	if ( _cellt(a) == _cellt(b) )
	{
		/* If they have the same type, we can deep compare them. */
		switch ( _cellt(a) )
		{
			case MUSE_INT_CELL		: return compare_i_i( _ptr(a)->i, _ptr(b)->i );
			case MUSE_FLOAT_CELL	: return compare_f_f( _ptr(a)->f, _ptr(b)->f );
			case MUSE_TEXT_CELL		: return wcscmp( _ptr(a)->text.start, _ptr(b)->text.start );
			case MUSE_SYMBOL_CELL	: return wcscmp( muse_symbol_name(env,a), muse_symbol_name(env,b) );
			case MUSE_CONS_CELL		: return compare_contents_of_conses( env, a, b );
			default					: return a - b;
		}
	}
	else
	{
		/* If they are different, we can only compare numeric types. */
		switch ( _cellt(a) )
		{
			case MUSE_INT_CELL		:	if ( _cellt(b) == MUSE_FLOAT_CELL )
											return compare_i_f( _ptr(a)->i, _ptr(b)->f );
										else
											break;
			case MUSE_FLOAT_CELL	:	if ( _cellt(b) == MUSE_INT_CELL )
											return - compare_i_f( _ptr(b)->i, _ptr(a)->f );
										else
											break;
			default:;
		}
		
		/* For others, we impose an ordering based on their types. */
		return _cellt(a) - _cellt(b);
	}
}




