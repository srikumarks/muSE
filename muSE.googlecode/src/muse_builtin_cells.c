/**
 * @file muse_builtin_cells.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_builtins.h"

enum
{
	DEFINE_NORMAL,
	DEFINE_EXTENSION,
	DEFINE_OVERRIDE
};

/**
 * Does normal definition as well as extensions and overrides.
 */
static muse_cell _defgen( muse_env *env, int option, muse_cell sym, muse_cell gen, muse_cell fn )
{
	if ( (_cellt(gen) != MUSE_LAMBDA_CELL) || (_quq(_head(gen)) != _csymbol(L"{{generic-args}}")) )
	{
		MUSE_DIAGNOSTICS({
			if ( option > DEFINE_NORMAL && _cellt(gen) == MUSE_LAMBDA_CELL )
				muse_message( env, option == DEFINE_EXTENSION 
										? L"(define-extension >>genfn<< ...)" 
										: L"(define-override >>genfn<< ...)",
									L"Expecting a generic function, but got a normal function instead!" );

			if ( sym != gen )
				muse_message( env, L"define", L"Symbol '%m' is expected to be undefined." );
		});

		/*	First mark the symbol as a fresh symbol by defining it to be itself.
			This way, if the symbol is referred to in the body of the value,
			it will remain unchanged so that recursive functions can be written. 
			Note that the value is evaluated after this assignment is made in order
			to make the self-substitution happen.
			
			NOTE: There is no implementation of tail-recursion. So beware of
			stack blowup. It usually safe to use recursion for small bound
			routines such as syntax transformers. */
		_define( sym, sym );

		fn = _eval(fn);

		_define( sym, fn );
		return fn;
	}

	/* We're working with a generic function, so
	we don't need to worry about recursive references. 
	Evaluate the function in the context of preceding definitions. */
	fn = _eval(fn);

	/* Check that the function is indeed a function. */
	muse_assert( _cellt(fn) == MUSE_LAMBDA_CELL );

	/* The function can't be a generic itself. */
	muse_assert( _quq(_head(fn)) != _csymbol(L"{{generic-args}}") );

	/* The generic and the extension function must both either be normal
	functions, or macros. They can't be different. */
	muse_assert( _head(gen) < 0 ? _head(fn) < 0 : _head(fn) >= 0 );

	{
		muse_cell case_e = _head(_tail(gen));
		muse_cell case_arg_e = _tail(case_e);

		switch ( option )
		{
		case DEFINE_NORMAL :
			/* (define ...) behaves like (define-extension ...) when given a generic. */
		case DEFINE_EXTENSION : 
			/* Extend. */
			muse_list_append( env, _tail(case_arg_e), _cons( _cons( _quq(_head(fn)), _tail(fn) ), MUSE_NIL ) );
			break;
		case DEFINE_OVERRIDE :
			/* Override. */
			_sett( case_arg_e, _cons( _cons( _quq(_head(fn)), _tail(fn) ), _tail(case_arg_e) ) );
			break;
		}

		_define( sym, gen );
	}

	return fn;
}

/**
 * (define symbol value).
 * Sets the current value of the symbol. \c symbol is not evaluated, \c value is
 * evaluated.
 * 
 * Examples -
 * 
 * This example simply evaluates \c value and assigns it as the
 * value of the given \c symbol. 
 * @code
 * (define symbol value)
 * @endcode 
 * 
 * The second example provides additional documentation about the
 * symbol being defined, using an arbitrary property structure
 * identified by the keyword \c doc at the head of the documentation
 * block, followed by an association list of the documentation writer's
 * own choosing. Typical documentation property words include
 * \c usage for a brief description of the usage syntax, \c descr
 * for a more elaborate description of the object being created,
 * and \c brief for a brief textual description of the purpose
 * of the object. 
 * @code
 * (define symbol 
 *   (doc (usage "(symbol param1 param2)")
 *        (descr "Computes symbol")
 *        (param1 "The first value")
 *        (param2 "The second value"))
 *   (fn (p1 p2) (+ p1 p2)))
 * @endcode
 * The defined function takes two arguments and returns their sum.
 * Pretty lame function, but serves to illustrate.
 * 
 * If you want the documentation block to be ignored by the reader,
 * then you should call \ref muse_init_env with the \c MUSE_DISCARD_DOC
 * parameter set to \c MUSE_TRUE.
 *
 * \par Generic functions
 * If the given symbol is already bound to a 
 * \ref syntax_generic_lambda "generic function", \c define behaves like
 * \ref fn_define_extension "define-extension" - i.e. it extends the
 * definition of the generic function with the new function's case.
 */
muse_cell fn_define( muse_env *env, void *context, muse_cell args )
{
	muse_cell sym = _next(&args);

	muse_cell oldval = MUSE_NIL;

	int sp = _spos();
	
	oldval = _symval(sym);
	_spush(oldval);

	/* Process documentation if specified. */
	if ( _cellt(_head(args)) == MUSE_CONS_CELL && _head(_head(args)) == _builtin_symbol(MUSE_DOC) )
	{
		/* We have some documentation. We process documentation only if we've
		been asked to. */
		if ( env->parameters[MUSE_DISCARD_DOC] )
		{
			args = _tail(args); /* Skip the doc. */
		}
		else
		{
			/* Simply add the documentation to the plist of the symbol,
			discarding any current documentation. */
			_sett( _tail(sym), _tail(_head(args)) );
			
			/* Skip to the code part. */
			args = _tail(args);
			
			/* Also add the code itself to the plist for reference. */
			_put_prop( sym, _builtin_symbol(MUSE_CODE), _head(args) );
		}
	}
	
	/* Define the value of the symbol. */
	{
		muse_cell value = _defgen( env, (int)context, sym, oldval, _head(args) );

		MUSE_DIAGNOSTICS({
			if ( _tail(args) )
			{
				muse_char ctxt[128];
				muse_sprintf( env, ctxt, 128, L"(define %m ...)", sym );

				muse_message( env,ctxt, L"You've given the following extra parameters -\n"
									L"\t%m\n"
									L"that will all be ignored. Maybe you meant to do something else.",
									_tail(args) );
			}
		});

		_unwind(sp);
		return value;
	}
}

/** @addtogroup GenericFns Generic functions */
/*@{*/

/**
 * (define-extension generic-fn-symbol fn-extension)
 *
 * Obeys the same syntax as \ref fn_define "define", except that
 * the symbol for extension is expected to be bound to a 
 * \ref syntax_generic_lambda "generic function".
 * Only generic functions can be extended.
 *
 * "Extension" means that the case presented by the given function
 * is examined \b after all the preceding cases. So if the extension function
 * has the same argument pattern as any of the preceding cases,
 * will not have any effect on the behaviour of the generic function.
 *
 * You can also use \ref fn_define "define" instead of
 * \c define-extension.
 *
 * @see \ref syntax_generic_lambda "gfn", \ref syntax_generic_block "gfn:",
 *  \ref fn_define_override "define-override"
 */
muse_cell fn_define_extension( muse_env *env, void *context, muse_cell args )
{
	return fn_define( env, (void*)DEFINE_EXTENSION, args );
}

/**
 * (define-override generic-fn-symbol fn-override)
 *
 * Obeys the same syntax as \ref fn_define "define", except that
 * the symbol for extension is expected to be bound to a 
 * \ref syntax_generic_lambda "generic function".
 * Only generic functions can be overridden.
 *
 * "Override" means that the case presented by the given function
 * is examined \b before all the preceding cases of the generic function.
 * This means that if an overriding function has the same argument pattern
 * as any preceding case, it'll completely take over and not give the
 * preceding case a chance.
 *
 * @see \ref syntax_generic_lambda "gfn", \ref syntax_generic_block "gfn:",
 *  \ref fn_define_extension "define-extension"
 */
muse_cell fn_define_override( muse_env *env, void *context, muse_cell args )
{
	return fn_define( env, (void*)DEFINE_OVERRIDE, args );
}

/*@}*/

/**
 * (set! symbol value).
 *
 * Inside a function body or let or case block, it changes the value of the
 * given locally declared symbol. Cannot in general be used to set the value of 
 * a global symbol, unless that symbol is, up to this point, undefined.
 */
muse_cell fn_set_M( muse_env *env, void *context, muse_cell args )
{
	muse_cell sym = _next(&args);

	MUSE_DIAGNOSTICS({ muse_expect( env, L"set!", L"s:", sym, L"something" ); });

	{
		muse_cell value = _eval(_head(args));
		_define(sym,value);
		return value;
	}
}

/**
 * (setf! cell value).
 *
 * Sets the head of the given cons cell to the given value.
 */
muse_cell fn_setf_M( muse_env *env, void *context, muse_cell args )
{
	muse_cell c = _evalnext(&args);
	muse_cell v = _evalnext(&args);
	_seth( c, v );
	return v;
}

/**
 * (setr! cell value).
 *
 * Sets the tail of the given cons cell to the given value.
 */
muse_cell fn_setr_M( muse_env *env, void *context, muse_cell args )
{
	muse_cell c = _evalnext(&args);
	muse_cell v = _evalnext(&args);
	_sett( c, v );
	return v;
}

/**
 * (first list).
 * Gets the \c head of the list, which is the first element.
 */
muse_cell fn_first( muse_env *env, void *context, muse_cell args )
{
	return _head( _eval(_head(args)) );
}

/**
 * (rest list).
 * 
 * Gets the tail of the list. This is intended to be read as 
 * "rest of the list".
 */
muse_cell fn_rest( muse_env *env, void *context, muse_cell args )
{
	return _tail( _eval(_head(args)) );
}

/**
 * (nth n list).
 * Gets the n-th element of a list.
 * 
 * n is zero based. Returns item at index n in the list.
 * Could be thought of as "skip n, then return head".
 */
muse_cell fn_nth( muse_env *env, void *context, muse_cell args )
{
	int N = (int)_ptr(_evalnext(&args))->i;
	muse_cell c = _evalnext(&args);
	
	while ( N-- > 0 )
		c = _tail(c);

	return _head(c);
}

/**
 * (take N ls).
 * 
 * Returns a new list consisting of a maximum of N items of
 * the given list "ls".
 */
muse_cell fn_take( muse_env *env, void *context, muse_cell args )
{
	int sp = _spos();
	muse_int N = _intvalue(_evalnext(&args));
	muse_cell list = _evalnext(&args);

	if ( list && (--N) >= 0 )
	{
		muse_cell h = _cons( _next(&list), MUSE_NIL );
		muse_cell t = h;
		int sp2 = _spos();

		while ( list && (--N) >= 0 )
		{
			_sett( t, _cons( _next(&list), MUSE_NIL ) );
			t = _tail(t);
			_unwind(sp2);
		}

		_unwind(sp);
		_spush(h);
		return h;
	}
	else
	{
		_unwind(sp);
		return MUSE_NIL;
	}
}

/**
 * (drop N list).
 *
 * Literally does that - drops N items from the given list
 * and returns a list of the remaining items. 
 * Examples -
 * 	- <tt>(drop 0 ls)</tt> = <tt>ls</tt>
 * 	- <tt>(drop 2 ls)</tt> = <tt>(rest (rest ls))</tt>
 * 	- etc.
 * 
 * Does not create a new list like \c take does. 
 */
muse_cell fn_drop( muse_env *env, void *context, muse_cell args )
{
	int N = (int)_ptr(_evalnext(&args))->i;
	muse_cell c = _evalnext(&args);
	
	while ( N-- > 0 )
		c = _tail(c);

	return c;
}

/**
 * (dup arg).
 *
 * Creates a structural duplicate of the given argument.
 */
muse_cell fn_dup( muse_env *env, void *context, muse_cell args )
{
	return muse_dup( env, _evalnext(&args) );
}

/**
 * (list a1 a2 ... )
 * Returns a list of the given items - the list of items is a
 * copy, though the items refer to the same objects.
 */
muse_cell fn_list( muse_env *env, void *context, muse_cell args )
{
	return muse_eval_list( env, args );
}

/**
 * (append! list1 list2 -so-on- listN).
 * Modifies the tail of list1 to point to list2, tail of list2 to list3 etc.
 * All except listN are modified.
 * 
 * For example -
 * @code
 * (define a (list 1 2 3 4 5))
 * (define b (list 10 20 30 40 50))
 * (append! a b)
 * (print a)
 * @endcode
 * will print
 * @code
 * (1 2 3 4 5 10 20 30 40 50)
 * @endcode
 */
muse_cell fn_append_M( muse_env *env, void *context, muse_cell args )
{
	muse_cell head = MUSE_NIL;
	muse_cell last = MUSE_NIL;

	if ( args )
	{
		head = last = _evalnext(&args);
	}

	while ( args )
	{
		muse_cell tail = _evalnext(&args);
		if ( tail )
		{
			muse_list_append( env, last, tail );
			last = tail;
		}
	}

	return head;		
}
