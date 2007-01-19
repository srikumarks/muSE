/**
 * @file muse_builtin_module.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * Implements a simple first-class module system for use at read-time.
 */

#include "muse_builtins.h"
#include <stdlib.h>

/** @addtogroup FunctionalObjects */
/*@{*/
/**
 * @defgroup ModuleSystem A simple module system
 *
 * A module is modelled as a function that introduces a set of symbols
 * into the scope of evaluation of its arguments. Modules are first class
 * objects, but are intended to be used at read-time, so usually you should
 * use modules within {} expressions and not within parentheses.
 *
 * You define a module using \ref fn_module "module", \ref fn_import "import"
 * definitions from a module, and use a module \c M as @code {M expr1 expr2 ...} @endcode
 * to provide the scope for evaluating the expressions. The \c {} expression itself
 * evaluates to the value of the last expression.
 *
 * For example -
 * @code
 * (define Stats 
 *     (module '(mean var) 
 *         (define mean (fn args (/ (apply + args) (length args))))
 *         (define var (fn args (- (apply mean (map (fn (x) (* x x)) args)) 
 *                                 (let ((mx (apply mean args)))
 *								      (* mx mx)))))
 *     ))
 *
 * {Stats
 *     (let ((numbers (do (print "Enter a list of numbers: ") (read))))
 *        (print "Mean = " (apply mean numbers))
 *        (print "Variance = " (apply var numbers)))
 * }
 * @endcode
 */
/*@{*/

typedef struct 
{
	muse_functional_object_t base;
	muse_cell exported_symbols; /**< A list of symbols which this module provides. */
	int num_exports; /**< Number of exported symbols. Equals length of the list \c exported_symbols. */
	muse_cell *values; /**< The values of the exported symbols in the order in which they appear in the list. */
} module_t;

static module_t *module_data( muse_env *env, muse_cell m )
{
	return (module_t*)_functional_object_data(m,'modu');
}

/**
 * This implementation of "define" saves the old definition so
 * it can be restored once the module's scope is over.
 */
static muse_cell fn_module_define( muse_env *env, module_t *m, muse_cell args )
{
	muse_cell sym = _head(args);

	/* Save the symbol's current value. */
	_push_binding(sym);

	/* Do the regular define operation. */
	return fn_define( env, NULL, args );
}

static void module_init( muse_env *env, void *ptr, muse_cell args )
{
	module_t *m = (module_t*)ptr;

	/* Get the list of exported symbols. */
	m->exported_symbols = _evalnext(&args);

	m->num_exports = muse_list_length(env, m->exported_symbols);

	m->values = (muse_cell*)calloc( m->num_exports, sizeof(muse_cell) );

	{
		int sp = _spos();
		int bsp = _bspos();

		/* Change the meaning of "define" to be specific to this module. */
		muse_pushdef( env, _csymbol(L"define"), _mk_nativefn( (muse_nativefn_t)fn_module_define, m ) );

		/* Evaluate all the expressions in the scope. */
		_do(args);

		/* Save the values of the exported symbols before the old
		values are restored. */
		{
			int i;
			muse_cell symbols = m->exported_symbols;
			for ( i = 0; symbols != MUSE_NIL; symbols = _tail(symbols), ++i )
			{
				m->values[i] = _symval(_head(symbols));
			}
		}

		/* Restore the old values. */
		_unwind_bindings(bsp);
		_unwind(sp);
	}
}

static void module_mark( muse_env *env, void *ptr )
{
	module_t *m = (module_t*)ptr;

	muse_mark( env, m->exported_symbols );

	{
		int i;
		for ( i = 0; i < m->num_exports; ++i )
		{
			muse_mark( env, m->values[i] );
		}
	}
}

static void module_destroy( muse_env *env, void *ptr )
{
	module_t *m = (module_t*)ptr;

	free(m->values);
}

/**
 * Brings the module's exported definitions into scope.
 * @return The bindings stack pointer before the old
 * values of exported symbols were saved. If you do
 * _unwind_bindings() with this values, all symbols
 * introduced by the module will be restored to their
 * old values.
 */
static int module_enter( muse_env *env, module_t *m )
{
	int bsp = _bspos();
	muse_cell symbols = m->exported_symbols;
	int i;

	for ( i = 0; symbols != MUSE_NIL; symbols = _tail(symbols), ++i )
	{
		muse_pushdef( env, _head(symbols), m->values[i] );
	}

	return bsp;
}

/**
 * {a-module expr1 expr2 etc}
 *
 * A module is a function which makes a set of bindings 
 * current before evaluating its arguments, returning the value
 * of the last one. Modules must always be scoped using the
 * read-time evaluation syntax for consistency. They are first
 * class and so you may pass them around as you like, but be
 * aware of the way they work. Modules are best only used at
 * read-time.
 *
 * Unused modules are simply garbage collected. Note that if a module
 * is named, it will remain "used" until the name is used for a
 * a different purpose.
 */
static muse_cell fn_module_fn( muse_env *env, module_t *m, muse_cell args )
{
	/* Make the module's definitions current. */
	int bsp = module_enter( env, m );

	{
		muse_cell result = _do( args );
		_unwind_bindings(bsp);
		return result;
	}
}

static muse_functional_object_type_t g_module_type =
{
	'muSE',
	'modu',
	sizeof(module_t),
	(muse_nativefn_t)fn_module_fn,
	NULL,
	module_init,
	module_mark,
	module_destroy,
	NULL
};


/**
 * (module list-of-exported-symbols defn1 defn2 etc)
 *
 * Evaluates to a module object. You can use \ref fn_define "define"
 * to name a module. For example -
 * @code {define name (module ...)} @endcode
 */
static muse_cell fn_module( muse_env *env, void *context, muse_cell args )
{
	return _mk_functional_object( &g_module_type, args );
}

/**
 * {import module1 module2 etc}
 * 
 * Brings the symbols defined by each of the given modules (in
 * the specified order) into the current scope. Definitions provided
 * modules later in the list override any previous definitions.
 */
static muse_cell fn_import( muse_env *env, void *context, muse_cell args )
{
	while ( args )
	{
		muse_cell mcell = _evalnext(&args);
		module_t *m = module_data(env,mcell);
		if ( m != NULL )
		{
			muse_cell symbols = m->exported_symbols;
			int i;
			for ( i = 0; symbols != MUSE_NIL; symbols = _tail(symbols), ++i )
			{
				MUSE_DIAGNOSTICS({
					muse_expect( env, L"{import ... >>module<< ...}", L"s!:", _head(symbols) );
				});

				_define( _head(symbols), m->values[i] );
			}
		}
	}

	return _t();
}

/**
 * (module? m)
 *
 * A predicate to check whether an object is a module or not.
 * Evaluates to \p m if it is a module and to \p () if it isn't.
 */
static muse_cell fn_module_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell m = _evalnext(&args);

	return module_data(env,m) ? m : MUSE_NIL;
}

/**
 * {:: mod1 mod2 etc sym}
 *
 * Scoping operator.
 * 
 * Evaluates to the value of the symbol in the nested module spec mod1 -> mod2 -> ...
 * The symbol itself can be a module in which case the inner module is the
 * result of the scope evaluation and might be used to provide the scope for
 * other code blocks.
 */
static muse_cell fn_scope( muse_env *env, void *context, muse_cell args )
{
	int bsp = _bspos();
	muse_cell result = MUSE_NIL;
	module_t *m = NULL;

	while ( args )
	{
		result = _evalnext(&args);

		_unwind_bindings(bsp);

		m = module_data(env,result);

		if ( args && m )
			bsp = module_enter( env, m );
		else
			break;
	}

	_unwind_bindings(bsp);
	return result;
}

void muse_define_builtin_type_module( muse_env *env )
{
	static const muse_char *k_names[]		= { L"module", L"module?",  L"import", L"::",	 NULL };
	static const muse_nativefn_t k_fns[]	= { fn_module, fn_module_p, fn_import, fn_scope, NULL };

	int sp = _spos();

	const muse_char **names = k_names;
	const muse_nativefn_t *fns = k_fns;
	while ( *names )
	{
		_define( _csymbol(*names), _mk_nativefn( *fns, NULL ) );
		++names;
		++fns;
	}

	_unwind(sp);
}

/*@}*/
/*@}*/