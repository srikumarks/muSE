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
 * @code (get thing key) @endcode
 * Looks up the value for the given \p key in the given \p thing.
 * The \p thing can be any key-value association, including
 * objects, hashtables, modules and vectors. Except for vectors
 * which take integer keys, the others are expected to use symbols
 * as keys.
 *
 * \c get supports deep lookup.
 * @code
 * (get thing key1 key2 ... keyN).
 * @endcode
 * is equivalent to -
 * @code
 * (get ... (get (get thing key1) key2) ... keyN)
 * @endcode
 *
 * If \p thing is a value that is being held in a variable,
 * then the notation <tt>thing.key1.key2</tt> is equivalent to
 * @code
 * (get thing 'key1 'key2)
 * @endcode.
 * 
 * @see \ref fn_put "put"
 */
muse_cell fn_get( muse_env *env, void *context, muse_cell args)
{
	muse_cell sym = _evalnext(&args);
	muse_cell argv = muse_eval_list( env, args );
	return muse_get( env, sym, _head(argv), _tail(argv) );
}

/**
 * Deeply nested value setting support. 
 *	-# If the \p argv has a single element, then that becomes the
 *	   value of the \p prop key of the \p obj.
 *	-# If the \p argv has more than one element, then the call recurses
 *	   into the object that is the \p prop of \p obj.
 *
 * @exception error:readonly-object
 * <table border="0" cellspacing="0" width="80em">
 * <tr><td>\b Handler</td>		<td>@code (fn (resume 'error:readonly-object obj prop . argv) ...) @endcode</td></tr>
 * <tr><td>\b When</td>			<td>You tried to set a property of a read-only object.</td></tr>
 * <tr><td>\b Resume</td>		<td>By performing the expected operation and passing the expected
 *								result of the \c put operation that failed.</td></tr>
 * </table>
 *
 * @see muse_get()
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
			if ( prop_view->put_prop )
				return prop_view->put_prop( env, fobj, prop, argv );
			else
				return muse_raise_error( env, _csymbol(L"error:readonly-object"), _cons( obj, _cons( prop, argv ) ) );
		} else {
			return MUSE_NIL;
		}
	}
}

/**
 * @code (put* thing key1 value1 key2 value2 ... keyN valueN) @endcode
 * Multiple property setting.
 * @see \ref fn_put_many "put*"
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
 * @code (put thing key value) @endcode
 * Sets the value for the given \p key in the given \p thing to the given \p value.
 * \p thing can be anything that \ref fn_get "get" supports and \p key
 * also has the same restrictions as for \ref fn_get "get".
 *
 * \c put also supports deep nesting just like \c get.
 * @code
 * (put thing key1 key2 ... keyN value)
 * @endcode
 * is equivalent to
 * @code
 * (put (get thing key1 key2 ... keyN-1) keyN value)
 * @endcode
 *
 * \c put supports the dot notation too. For example, @code (put thing.key value) @endcode
 * is equivalent to @code (put thing 'key value) @endcode. (\c put is actually a macro
 * that expands to this form at read time.)
 *
 * @see \ref fn_get "get"
 */
muse_cell fn_put( muse_env *env, void *context, muse_cell args)
{
	muse_cell sym	= _evalnext(&args);
	muse_cell prop	= _evalnext(&args);
	return muse_put( env, sym, prop, muse_eval_list( env, args ) );
}

/**
 * @code (put* obj key1 value1 key2 value2 ... keyN valueN) @endcode
 *
 * Sets many properties of the object in one go.
 * @see \ref fn_put "put"
 * @see \ref fn_get "get"
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
						_cons(_cons(muse_quote(env,_builtin_symbol(MUSE_GET)),_csymbol(L"$get*")),_csymbol(L"$put*")),
						L"apply", L"put", L"join", L"$get*", L"$put*",
						L"_", L"cons", L"prim:put", L"$args" ),
			  MUSE_FALSE );
	
	_unwind(sp);
}

/**
 * The API call muse_assoc doesn't support lazy alists.
 * This function supports it by using muse_head() and muse_tail()
 * instead of _head() and _tail(). This function is used
 * by fn_assoc.
 */
static muse_cell muse_assoc_with_lazy_support( muse_env *env, muse_cell alist, muse_cell prop )
{
	if ( !alist )
		return MUSE_NIL;
	else {
		muse_cell h = muse_head( env, alist );
		if ( muse_equal( env, muse_head( env, h ), prop ) )
			return h;
		else
			return muse_assoc_with_lazy_support( env, muse_tail( env, alist ), prop );
	}
}

/**
 * @code (assoc plist key) @endcode
 * @see muse_assoc()
 */
muse_cell fn_assoc( muse_env *env, void *context, muse_cell args)
{
	muse_cell alist = _evalnext(&args);
	muse_cell prop = _evalnext(&args);
	return muse_assoc_with_lazy_support(env,alist,prop);
}

/**
 * @code (plist symbol) @endcode
 * @see muse_symbol_plist()
 */ 
muse_cell fn_plist( muse_env *env, void *context, muse_cell args)
{
	return muse_symbol_plist( env, _evalnext(&args) );
}

/**
 * @code (symbol "symbol-name") @endcode
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
 * @code (name sym) @endcode
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
