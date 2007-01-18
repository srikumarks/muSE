/**
 * @file muse_builtin_plist.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#include "muse_builtins.h"

/**
 * (get symbol property).
 * Looks up the given property for the given symbol.
 * If found, it yields the @code (property . value) @endcode pair,
 * and if not found, it evaluates to ().
 * @see fn_put()
 */
muse_cell fn_get( muse_env *env, void *context, muse_cell args)
{
	muse_cell sym	= _evalnext(&args);
	muse_cell prop	= _evalnext(&args);
	return _get_prop( sym, prop );
}

/**
 * (put symbol property value).
 * Sets the given property of the given symbol to the given value.
 * Subsequently, if you evaluate @code (get symbol property) @endcode,
 * you'll get @code (property . value) @endcode as the result.
 */
muse_cell fn_put( muse_env *env, void *context, muse_cell args)
{
	muse_cell sym	= _evalnext(&args);
	muse_cell prop	= _evalnext(&args);
	muse_cell value = _evalnext(&args);
	return _put_prop( sym, prop, value );
}

/**
 * (assoc plist key).
 * @see muse_assoc()
 */
muse_cell fn_assoc( muse_env *env, void *context, muse_cell args)
{
	muse_cell alist = _evalnext(&args);
	muse_cell prop = _evalnext(&args);
	return muse_assoc(env,alist,prop);
}

/**
 * (plist symbol).
 * @see muse_symbol_plist()
 */ 
muse_cell fn_plist( muse_env *env, void *context, muse_cell args)
{
	return muse_symbol_plist( env, _evalnext(&args) );
}

/**
 * (symbol "symbol-name").
 * Interns the symbol of the given textual name and returns a unique symbol
 * cell.
 */
muse_cell fn_symbol( muse_env *env, void *context, muse_cell args )
{
	muse_cell name = _evalnext(&args);
	int length = 0;
	const muse_char *text = _text_contents( name, &length );
	return muse_symbol( env, text, text + length );
}

/**
 * (name sym).
 * Returns the text name of the given symbol or () if the
 * given thing is not a symbol or doesn't have a name.
 */
muse_cell fn_name( muse_env *env, void *context, muse_cell args )
{
	muse_cell sym = _evalnext(&args);
	if ( sym && _cellt(sym) == MUSE_SYMBOL_CELL )
	{
		return _tail(_head(_tail(sym)));
	}
	else
		return MUSE_NIL;
}

/* Defined in muse.c */
muse_cell muse_intern_symbol( muse_env *env, muse_cell sym, int local_ix, muse_int hash );

/**
 * Generates an interned anonymous symbol that can be
 * used as a variable in macro generated code. Previously,
 * (before processes) it was possible to use (new) to do the 
 * same thing, but after processes were implemented, symbols need
 * be interned before they can be used as variables since
 * any function may be evaluated in multiple processes
 * and it needs to maintain the state of its variables
 * consistently in the process in which it is being evaluated.
 */
muse_cell fn_gensym( muse_env *env, void *context, muse_cell args )
{
	muse_cell sym = _mk_anon_symbol();
	return muse_intern_symbol( env, sym, _newlocal(), sym );
}
