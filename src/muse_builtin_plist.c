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
 * Recursive property getter. First gets the property \c key
 * of the object \c obj. If argv is MUSE_NIL, that becomes the
 * result. Otherwise, that result is used as the target for
 * yet more property keys extracted from \c argv.
 *
 * @see muse_put()
 */
MUSEAPI muse_cell muse_get( muse_env *env, muse_cell obj, muse_cell key, muse_cell argv )
{
	if ( _cellt(obj) == MUSE_SYMBOL_CELL ) {
		muse_cell val = _tail(_get_prop( obj, key ));
		return argv ? muse_get( env, val, _head(argv), _tail(argv) ) : val;
	} else {
		muse_functional_object_t *fobj = NULL;
		muse_prop_view_t *prop_view = _fnobjview( obj, 'prop', fobj );
		if ( prop_view ) {
			return prop_view->get_prop( env, fobj, key, argv );
		} else {
			return MUSE_NIL;
		}
	}
}

/**
 * (get symbol property).
 * Looks up the given property for the given symbol.
 * If found, it yields the @code (property . value) @endcode pair,
 * and if not found, it evaluates to ().
 * @see fn_put()
 */
muse_cell fn_get( muse_env *env, void *context, muse_cell args)
{
	muse_cell sym = _evalnext(&args);
	muse_cell argv = muse_eval_list( env, args );
	return muse_get( env, sym, _head(argv), _tail(argv) );
}

/**
 * Nested property put. \c argv has to have at least one element
 * in it, giving the value to set the property \c prop of object \c obj.
 * It takes much mroe English to describe this function than to
 * just refer to the code, so read it and understand it. Clue - its
 * the counterpart of \c muse_get.
 *
 * @see muse_get();
 */
MUSEAPI muse_cell muse_put( muse_env *env, muse_cell obj, muse_cell prop, muse_cell argv )
{
	if ( _cellt(obj) == MUSE_SYMBOL_CELL ) {
		muse_cell val = _next(&argv);
		if ( argv ) {
			return muse_put( env, muse_get( env, obj, prop, MUSE_NIL ), val, argv );
		} else {
			return _tail(_put_prop( obj, prop, val ));
		}
	} else {
		muse_functional_object_t *fobj = NULL;
		muse_prop_view_t *prop_view = _fnobjview(obj,'prop',fobj);
		if ( prop_view ) {
			return prop_view->put_prop( env, fobj, prop, argv );
		} else {
			return MUSE_NIL;
		}
	}
}

/**
 * Multiple property setting.
 * The argv is expected to be of the form @code ('key1 value1 'key2 value2 ...) @endcode.
 * Returns \p obj.
 */
MUSEAPI	muse_cell muse_put_many( muse_env *env, muse_cell obj, muse_cell argv )
{
	int sp = _spos();
	while ( argv ) {
		muse_cell c = _cons( _head(_tail(argv)), MUSE_NIL );
		muse_put( env, obj, _head(argv), c );
		_unwind(sp);
		_returncell(c);
		argv = _tail(_tail(argv));
	}

	return obj;
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
	return muse_put( env, sym, prop, muse_eval_list( env, args ) );
}

/**
 * (put* obj prop1 value1 prop2 value2 ...)
 *
 * Sets many properties of the object in one go.
 */
muse_cell fn_put_many( muse_env *env, void *context, muse_cell args )
{
	muse_cell obj	= _evalnext(&args);
	int sp = _spos();
	while ( args ) {
		muse_cell key = _evalnext(&args);
		muse_cell val = _evalnext(&args);
		muse_cell c = _cons( val, MUSE_NIL );
		muse_put( env, obj, key, c );
		_unwind(sp);
		_returncell(c);
	}

	return obj;
}

void muse_define_put_macro( muse_env *env )
{
	int sp = _spos();
	
	/*
	 We define put as a macro that collapses nested (get ..)
	 expressions so that you can use (put a.b.c "value") to
	 mean (put a 'b 'c "value"). Since it is a macro, it has
	 no runtime cost and the final expression is much simpler.
	 
	 (define put (fn '$args))
	 (define put (fn '$args
	 (case $args
	 ((('get . $get*) . $put*) (apply put (join $get* $put*)))
	 (_ (cons prim:put $args)))))
	 */
	muse_eval( env, muse_list( env, "SS(S'S)", L"define", L"put", L"fn", L"$args" ), MUSE_FALSE );
	muse_eval( env, 
			  muse_list( env, "SS(S'S(SS(c(SS(SSS)))(S(SSS))))",
						L"define", L"put",
						L"fn", L"$args", 
						L"case", L"$args",
						_cons(_cons(muse_quote(env,_csymbol(L"get")),_csymbol(L"$get*")),_csymbol(L"$put*")),
						L"apply", L"put", L"join", L"$get*", L"$put*",
						L"_", L"cons", L"prim:put", L"$args" ),
			  MUSE_FALSE );
	
	_unwind(sp);
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
