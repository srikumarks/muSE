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
 * A very simple module system for muSE.
 */

#include "muse_builtins.h"
#include "muse_port.h"
#include <stdlib.h>
#include <memory.h>

/** @addtogroup FunctionalObjects */
/*@{*/
/**
 * @defgroup Modules
 *
 * A module is a value that contains a set of variable bindings
 * you can refer to. You define a module using the \ref fn_module "module"
 * syntax which creates a module value. Symbols exported from a module
 * are accessed using the long form notation @code ModuleName.exportSymbol @endcode.
 * If you want to use a module's exports just by their short names,
 * you can import its definitions using \ref fn_import "import".
 */
/*@{*/

enum { MODULE_SEP = '.' };

typedef struct {
	muse_cell name;
	muse_cell value;
} module_binding_t;
	
typedef struct _module_t
{
	muse_functional_object_t base;
	int length;
	module_binding_t *bindings;
	muse_cell main;
} module_t;

static void introduce_module_local( muse_env *env, module_t *m );
muse_cell fn_module( muse_env *env, void *context, muse_cell args );

/**
 * Searches this module's symbols for a symbol defined to the given value.
 * If any of the module's symbols is itself is a module, then it recursively
 * searches the sub modules. The returned value is a list that gives the
 * path through the module hierarchy to the value.
 */
muse_cell module_find_symbol_with_value( muse_env *env, void *obj, muse_cell value )
{
	module_t *m = (module_t*)obj;
	int i = 0, N = m->length;
	for ( ; i < N; ++i ) {
		module_binding_t b = m->bindings[i];
		if ( b.value == value )
			return _cons( b.name, MUSE_NIL );
		else {
			module_t *m2 = (module_t*)muse_functional_object_data( env, b.value, 'mmod' );
			if ( m2 ) {
				muse_cell found = module_find_symbol_with_value( env, m2, value );
				return found ? _cons( b.name, found ) : MUSE_NIL;
			}
		}
	}

	return MUSE_NIL;
}

static void module_init( muse_env *env, void *ptr, muse_cell args )
{
	int bsp = _bspos();
	module_t *m = (module_t*)ptr;
	muse_cell modname, exports;
	muse_cell sym_main = _csymbol(L"main");
	modname = _next(&args);
	if ( _cellt(modname) == MUSE_CONS_CELL ) {
		// This is an anonymous module.
		exports = modname;
		modname = MUSE_NIL;
	} else {
		exports = _next(&args);
	}
	
	m->length = muse_list_length( env, exports );
	m->bindings = (module_binding_t*)calloc( m->length, sizeof(module_binding_t) );
	m->main = _mk_nativefn( syntax_do, NULL );
	
	/* Reset the definitions of exported values. */
	{
		int i = 0;
		muse_cell e = exports;
		
		/* HACK: We mofiy the last NULL character in the text contents temporarily
		to get the prefix string. This saves an allocation. */
		for ( ; i < m->length; ++i ) {
			muse_cell sym = _next(&e);
			module_binding_t *b = m->bindings + i;

			MUSE_DIAGNOSTICS({
				muse_expect( env, L"(module name (.. >>export<< ..) ..)", L"v?", sym, MUSE_SYMBOL_CELL );
			});
			
			b->name = sym;
			b->value = b->name;
			_pushdef( b->name, b->name );
		}
	}

	/* Bring into scope the definition of the module's own symbol. 
	 So it can be captured by functions in the body if necessary. */
	if ( modname ) {
		_pushdef( modname, m->base.self );
	}
	
	/* Also make (the module) evaluate to the current
	 module for the convenience of the body. */
	muse_push_recent_scope( env );
	muse_add_recent_item( env, (muse_int)fn_module, m->base.self );
	
	/* Make the "main" symbol local. */
	_pushdef( sym_main, sym_main );

	/* Evaluate the body of the module. */
	if ( args ) {
		/* This module's body has already been parsed. So evaluate it and gather the
		 definitions. The limitation of this approach is that read-macros defined in
		 the module cannot be used within the module. But you can export functions,
		 objects, etc. .. and even unused macro definitions. */
		_force( muse_do( env, args ) );
	} else {
		/* A empty body means "treat the rest of the current input as the module body.".
		 This approach has the advantage that the module can define, use and export
		 macros as well. */
		muse_port_t p = muse_current_port( env, MUSE_INPUT_PORT, NULL );
		int sp = _spos();
		while ( !port_eof(p) ) {
			_eval(muse_pread(p));
			_unwind(sp);
		}
	}
	
	/* Keep around the definition of the "main" symbol. */
	{
		muse_cell mainval = _symval(sym_main);
		if ( mainval != sym_main )
			m->main = mainval;
		else
		{
			/* It is the same as "do". */
		}
	}
	
	/* Capture new definitions of the exported symbols. */
	{
		int i = 0;
		for ( ; i < m->length; ++i ) {
			module_binding_t *b = m->bindings + i;
			b->value = _symval(b->name);
		}
	}
	
	/* Restore old definitions. */
	_unwind_bindings(bsp);
	muse_pop_recent_scope( env, (muse_int)fn_module, m->base.self );
}

/**
 * Adds new definitions if found and replaces old definitions 
 * with new ones. Note that we don't need to know the module
 * name. So the args starts with the module exports list,
 * unlike module_init().
 */
static muse_cell module_augment( muse_env *env, module_t *m, muse_cell args )
{
	int bsp = _bspos();
	muse_cell exports;
	muse_cell sym_main = _csymbol(L"main");
	int extralen = 0, newlen = m->length;
	exports = _next(&args);
	
	extralen = muse_list_length( env, exports );
	newlen = m->length + extralen;
	m->bindings = (module_binding_t*)realloc( m->bindings, sizeof(module_binding_t) * newlen );
	
	/* Bring the original definitions into scope. */
	introduce_module_local( env, m );
	
	/* Reset the definitions of exported values. */
	{
		int i = 0;
		muse_cell e = exports;
		
		/* HACK: We mofiy the last NULL character in the text contents temporarily
		 to get the prefix string. This saves an allocation. */
		while ( e ) {
			muse_cell sym = _next(&e);
			muse_boolean exists = MUSE_FALSE;

			MUSE_DIAGNOSTICS({
				muse_expect( env, L"(module name (.. >>export<< ..) ..)", L"v?", sym, MUSE_SYMBOL_CELL );
			});
			
			for ( i = 0; i < m->length; ++i ) {
				module_binding_t *b = m->bindings + i;
				if ( b->name == sym ) {
					// Already exists. Ignore this entry.
					exists = MUSE_TRUE;
					break;
				}
			}
			
			if ( exists == MUSE_FALSE ) {
				// Add a new entry. We've already allocated
				// enough storage for m->bindings.
				module_binding_t *b = m->bindings + m->length;
				b->name = sym;
				b->value = b->name;
				_pushdef( b->name, b->name );
				m->length++;
			}
		}
	}
	
	/* Also make (the module) evaluate to the current
	 module for the convenience of the body. */
	muse_push_recent_scope( env );
	muse_add_recent_item( env, (muse_int)fn_module, m->base.self );

	/* Evaluate the body of the module. */
	if ( args ) {
		/* This module's body has already been parsed. So evaluate it and gather the
		 definitions. The limitation of this approach is that read-macros defined in
		 the module cannot be used within the module. But you can export functions,
		 objects, etc. .. and even unused macro definitions. */
		_force( muse_do( env, args ) );
	} else {
		/* A empty body means "treat the rest of the current input as the module body.".
		 This approach has the advantage that the module can define, use and export
		 macros as well. */
		muse_port_t p = muse_current_port( env, MUSE_INPUT_PORT, NULL );
		int sp = _spos();
		while ( !port_eof(p) ) {
			_eval(muse_pread(p));
			_unwind(sp);
		}
	}
	
	/* Keep around the definition of the "main" symbol. */
	{
		muse_cell mainval = _symval(sym_main);
		if ( mainval != sym_main )
			m->main = mainval;
		else
		{
			/* It is the same as "do". */
		}
	}
	
	/* Capture new definitions of the exported symbols. */
	{
		int i = 0;
		for ( ; i < m->length; ++i ) {
			module_binding_t *b = m->bindings + i;
			b->value = _symval(b->name);
		}
	}
	
	/* Restore old definitions. */
	_unwind_bindings(bsp);
	return muse_pop_recent_scope( env, (muse_int)fn_module, m->base.self );
}

static void module_mark( muse_env *env, void *ptr )
{
	module_t *m = (module_t*)ptr;
	int i;
	for ( i = 0; i < m->length; ++i ) {
		// The symbols are automatically marked.
		muse_mark( env, m->bindings[i].value );
	}
	muse_mark( env, m->main );
}

static void module_destroy( muse_env *env, void *ptr )
{
	module_t *m = (module_t*)ptr;
	free( m->bindings );
	m->length = 0;
	m->bindings = NULL;
	m->main = MUSE_NIL;
}

/**
 * Writes out the vector to the given port in such a
 * way that the expression written out is converted
 * to a vector by a trusted read operation.
 */
static void module_write( muse_env *env, void *ptr, void *port )
{
	module_t *m = (module_t*)ptr;
	muse_port_t p = (muse_port_t)port;
	
	pretty_printer_indent(p);
	port_putc( '{', p );
	port_write( "module ", 7, p );	
	
	port_putc( '(', p );
	{
		int i;
		for ( i = 0; i < m->length; ++i )
		{
			if ( i > 0 ) port_putc( ' ', p );
			muse_pwrite( p, m->bindings[i].name );
		}
	}
	port_putc( ')', p );
	port_write( " ...}", 5, p );
	pretty_printer_unindent(p);
}

static void introduce_module_local( muse_env *env, module_t *m )
{
	int i = 0;
	for ( ; i < m->length; ++i ) {
		_pushdef( m->bindings[i].name, m->bindings[i].value );
	}
}


/**
 * The passed expression's head is the object. We convert the
 * expression into a "do" expression with the body bound using the
 * module's symbols.
 */
static muse_cell module_scope_begin( muse_env *env, void *self, muse_cell expr )
{
	introduce_module_local( env, (module_t*)self );
	{
		muse_cell tailpart = muse_bind_copy_expr( env, _tail(expr), MUSE_FALSE );
		_setht( expr, ((module_t*)self)->main, tailpart ); 
		return expr;
	}
}

static void module_scope_end( muse_env *env, void *self, int bsp )
{
	/* Nothing special to do. The lambda capture part
	will do all the necessary stack unwinding. */
	_unwind_bindings(bsp);
}

static muse_scope_view_t g_module_scope_view = {	module_scope_begin,	module_scope_end };

static muse_cell module_get( muse_env *env, void *self, muse_cell key, muse_cell argv )
{
	module_t *m = (module_t*)self;
	int i = 0;
	for ( i = 0; i < m->length; ++i ) {
		module_binding_t *b = m->bindings + i;
		if ( b->name == key ) {
			if ( argv )
				return muse_get( env, b->value, _head(argv), _tail(argv) );
			else
				return b->value;
		}
	}
	
	/* Raise an error and let the exception continue by supplying a value.
	   The value can be obtained in the exception handler by choosing a
	   different key to use in the same object, or by using some other
	   default object. */
	{
		muse_cell value = muse_raise_error( env, _csymbol(L"error:key-not-found"), _cons( m->base.self, _cons( key, MUSE_NIL ) ) );
		if ( argv ) 
			return muse_get( env, value, _head(argv), _tail(argv) );
		else
			return value;
	}
}

static muse_cell module_put( muse_env *env, void *self, muse_cell key, muse_cell argv )
{
	module_t *m = (module_t*)self;
	int i = 0;
	for ( i = 0; i < m->length; ++i ) {
		module_binding_t *b = m->bindings + i;
		if ( b->name == key ) {
			if ( _tail(argv) )
				return muse_put( env, b->value, _head(argv), _tail(argv) );
			else
				return (b->value = _head(argv));
		}
	}
	
	/* If the put operation needs to get something from this module and pass
	   the put on to the value, then we just raise a "key not found" error
	   with the same semantics as module_get. If it is the module's binding
	   that needs to be modified and the key could not be located, we raise
	   an "immutable key" error and let it resume by supplying a put result. */
	if ( _tail(argv) ) 
		return muse_get( env,
						muse_raise_error( env, _csymbol(L"error:key-not-found"), _cons( m->base.self, _cons( key, MUSE_NIL ) ) ),
						_head(argv), 
						_tail(argv) );
	else
		return muse_raise_error( env, _csymbol(L"error:immutable-key"), _cons( m->base.self, _cons( key, MUSE_NIL ) ) );
}

static muse_prop_view_t g_module_prop_view = { module_get, module_put };

static void *module_view( muse_env *env, int id )
{
	switch (id) {
		case 'prop':	return &g_module_prop_view;
		case 'scop':	return &g_module_scope_view;
		default:			return NULL;
	}
}

/**
 * @code(MyMod ...body...)@endcode
 *
 * Behaves exactly like \c do under all circumstances,
 * except that the symbols in the body part are interpreted
 * in the context of the module.
 *
 * When you use such an expression within a closure creating
 * expression, the head is replaced with "do" since the body
 * is already automatically bind-copied in the context of the
 * module and there is no longer any necessity for the module
 * object sitting at the head. So this function is called only
 * when the module object is used outside of a closure capturing
 * expression - i.e. either at the top level or inside any 
 * immediately evaluated expression at the top level.
 */
muse_cell module_syntax( muse_env *env, module_t *m, muse_cell args )
{
	int bsp = _bspos();
	introduce_module_local( env, m );
	{
		muse_cell result = muse_apply( env, m->main, args, MUSE_FALSE, MUSE_FALSE );
		_unwind_bindings(bsp);
		return result;
	}
}

static muse_functional_object_type_t g_module_type =
{
	'muSE',
	'mmod',
	sizeof(module_t),
	(muse_nativefn_t)module_syntax,
	module_view,
	module_init,
	module_mark,
	module_destroy,
	module_write
};

/**
 * @code (module MyMod (exportA exportB ...) ...body... ) @endcode
 *
 * Such a module expression introduces global symbols
 * MyMod.exportA, MyMod.exportB, etc. with the appropriate
 * bindings.
 *
 * Within a module, you use the usual \ref fn_define "define"
 * function to introduce definitions for whatever ie exported
 * from the module. If you introduce definitions of other symbols,
 * they will be visible only to the module block and will disappear
 * from all scope when the module completes loading. Such local 
 * symbols' values can be usefully captured in closures however.
 *
 * The \c module expression defines the value of the \c MyMod symbol
 * to be a function of one argument. This function takes a symbol
 * and evaluates to its value in the context of the module.
 *
 * Another way to define a module is to place 
 * @code (module MyMod (exportA exportB ...)) @endcode
 * at the start of a module file. In this case, the rest of
 * the file will be considered to be the module definition
 * and will be read in. The advantage of this approach over
 * the top approach is that using the top approach you cannot
 * define macros for local use within the module. Whereas
 * in the above approach you have all the facilities normally
 * available via \ref fn_load "load".
 *
 * Modules are values. Therefore you are free to define functions
 * that return modules, etc. Modules may export other modules as well,
 * which can be accessed using the dot notation - for example: m1.m2.m3.val 
 *
 * Module exports are mutable. You can use the generic \ref fn_get "get"
 * and \ref fn_put "put" functions to fetch and modify the exports of
 * a module. You cannot, however, add new exports or remove exports 
 * from a module using "put". To add new exports or to change existing
 * definitions (for example, to reload a module), you can evaluate
 * @code (module M (newexport1 oldexport1 ...) ...) @endcode
 * If your module is defined in a file and you've edited it, you
 * can use this property to redefine the module by simply callind
 * @code (load "mymodule.scm") @endcode to have your module's 
 * definition changes take effect. Note that this refresh mechanism
 * cannot be used to \b remove module exports.
 *
 * Right before the body of a module is evaluated, the new module object
 * itself is made available in the "recent computations" list so that
 * you can get at the "current module" object by evaluating the
 * expression @code (the module) @endcode. This holds even for
 * anonymous modules.
 *
 * @exception error:key-not-found
 * Handler format: @code (fn (resume 'error:key-not-found obj key) ...) @endcode
 * Raised when the given key cannot be found in the given module during a lookup.
 * You can resume by resolving the intended value.
 *
 * @exception error:immutable-key
 * Handler format: @code (fn (resume 'error:immutable-key module key) ...) @endcode
 * Raised if the module doesn't export the given key. Only exported keys can
 * be modified using put. You can resume with the intended result of the put
 * operation.
 *
 * @exception error:invalid-module-name
 * Handler format: @code (fn (resume 'error:invalid-module-name name) ...) @endcode
 * Raised if this module definition is trying to augment an existing module, but
 * the value of the given module name doesn't refer to a module and neither is it
 * an undefined symbol.
 */
muse_cell fn_module( muse_env *env, void *context, muse_cell args )
{
	int bsp = _bspos();
	muse_cell mname = _head(args);
	module_t *m = NULL;
	
	if ( _cellt(mname) == MUSE_SYMBOL_CELL )
	{
		muse_cell mod = muse_symbol_value( env, mname );
		if ( mod != mname ) {
		TRY_MODULE_AGAIN:
			m = (module_t*)muse_functional_object_data( env, mod, 'mmod' );
	
			if ( m != NULL ) {
				// Leave the module name out of the arguments.
				return module_augment( env, m, _tail(args) );
			} else {
				// Raise an error and allow to continue by resuming with
				// a valid module object.
				int sp = _spos();
				mod =  muse_raise_error( env, _csymbol(L"error:invalid-module-name"), _cons( mname, MUSE_NIL ) );
				_unwind(sp);
				_spush(mod);
				goto TRY_MODULE_AGAIN;
			}
		} else {
			// Include the module name in the arguments.
			muse_cell mod = muse_mk_functional_object( env, &g_module_type, args );
			if ( bsp == 0 )
				_define(mname,mod);
			else
				_pushdef(mname,mod);
			return mod;
		}
	}
	else if ( _cellt(mname) == MUSE_CONS_CELL )
	{
		// Create anonymous module.
		return muse_mk_functional_object( env, &g_module_type, args );
	}
	else
	{
		return muse_raise_error( env, _csymbol(L"error:module-syntax"), MUSE_NIL );
	}
}

static muse_cell module_expr_scope_begin( muse_env *env, void *self, muse_cell expr )
{
	/* Introduce the modules and then vanish from the function body. */
	muse_cell module, body, mname, exports, main_body;

	module = _head(expr);
	body = _tail(expr);
	mname = _head(body);
	if ( _cellt(mname) == MUSE_SYMBOL_CELL )
	{
		/* The module has a name. Save the name binding. */
		_pushdef( mname, mname );

		exports = _head(_tail(body));
		main_body = _tail(_tail(body));
	}
	else if ( _cellt(mname) == MUSE_CONS_CELL )
	{
		exports = mname;
		mname = MUSE_NIL;
		main_body = _tail(body);
	}
	else
	{
		return muse_raise_error( env, _csymbol(L"error:module-syntax"), MUSE_NIL );
	}


	{
		int bsp = _bspos();

		/* Save the bindings of all the module-local symbols. */
		{
			muse_cell exports = _head(_tail(body));
			muse_assert( _cellt(exports) == MUSE_CONS_CELL );
			while ( exports ) 
			{
				muse_cell sym = _next(&exports);
				_pushdef(sym,sym);
			}
		}

		/* Bind-copy the rest of the module body. */
		{
			muse_cell rest_of_body = muse_bind_copy_expr( env, main_body, MUSE_FALSE );
			_unwind_bindings(bsp);
			if ( mname )
				return _cons( module, _cons( mname, _cons( exports, rest_of_body ) ) );
			else
				return _cons( module, _cons( exports, rest_of_body ) );
		}
	}
}

static void module_expr_scope_end( muse_env *env, void *self, int bsp )
{
}

static muse_scope_view_t g_module_expr_scope_view = { module_expr_scope_begin, module_expr_scope_end };

static void *module_expr_view( muse_env *env, int id )
{
	switch (id) {
		case 'scop':	return &g_module_expr_scope_view;
		default:			return NULL;
	}
}

/* When module is used within the body of a closure creation expression
using (fn ..), the module can capture values from the environment. */
static muse_functional_object_type_t g_module_expr_scope_type = {
	'muSE',
	'(mod',
	sizeof(muse_functional_object_t),
	fn_module,
	module_expr_view,
	NULL,
	NULL,
	NULL,
	NULL
};


static void introduce_module_global( muse_env *env, module_t *m )
{
	int i = 0;
	for ( ; i < m->length; ++i ) {
		_define( m->bindings[i].name, m->bindings[i].value );
	}
}

muse_cell fn_require( muse_env *env, void *context, muse_cell args );

/**
 * @code (import ModA ModB ...) @endcode
 *
 * Introduces symbols in all the named modules into the current
 * context, in their short forms. Useful in two places -
 *  - Inside a module definition to make the short form of symbols
 *    from aother module available for the scope of the module
 *    being defined.
 *  - To globally introduce short form names of the symbols in
 *    the given modules, when used at the REPL or file level.
 */
muse_cell fn_import( muse_env *env, void *context, muse_cell args )
{
	int bsp = _bspos();

	if ( bsp == 0 ) {
		args = fn_require( env, context, args );
	} else {
		args = muse_eval_list( env, args );
	}

	while ( args ) {
		muse_cell mod = _next(&args);
		module_t *m = (module_t*)_functional_object_data( mod, 'mmod' );
		if ( m ) {
			if ( bsp == 0 )
				introduce_module_global( env, m );
			else
				introduce_module_local( env, m );
		}
	}

	return MUSE_NIL;
}

static muse_cell require_modules( muse_env *env, void *context, int i, muse_boolean *eol )
{
	muse_cell *args = (muse_cell*)context;

	(*eol) = MUSE_FALSE;
	
	if ( *args ) {
		muse_cell mod = _evalnext(args);
		int sp = _spos();

		do {
			if ( mod == MUSE_NIL )
				return MUSE_NIL;

			if ( _cellt(mod) == MUSE_SYMBOL_CELL ) {
				/* Try loading local file first. */
				muse_cell expr = muse_list( env, "S(S(ScT))(SSc)", 
												 L"try", L"load", L"format", mod, L".scm", 
												 L"fn", L"_", MUSE_NIL );
				muse_cell modl = muse_eval( env, expr, MUSE_FALSE );
				if ( _functional_object_data( modl, 'mmod' ) ) {
					/* Load succeeded. */
					return modl;
				} else {
					/* If failed, load from the URL. */
					expr = muse_list( env, "S(S(STcT))", 
										   L"load", L"fetch-uri", L"format", 
										   L"http://muvee-symbolic-expressions.googlecode.com/svn/trunk/lib/", mod, L".scm" );
					modl = muse_eval( env, expr, MUSE_FALSE );
					if ( _functional_object_data( modl, 'mmod' ) ) {
						/* Load succeeded. */
						return modl;
					} else {
						/* If failed, raise exception. You can continue by passing a valid module
						reference. If you continue using NIL, it will be ignored. and the next module
						will be processed.*/
						_unwind(sp);
						mod = muse_raise_error( env, _csymbol(L"error:invalid-module-reference"), _cons( mod, MUSE_NIL ) );
						continue;
					}
				}
			} else if ( _functional_object_data( mod, 'mmod' ) ) {
				/* Module already loaded. Continue. */
				return mod;
			} else {
				_unwind(sp);
				mod = muse_raise_error( env, _csymbol(L"error:invalid-module-reference"), _cons( mod, MUSE_NIL ) );
				continue;
			}
		} while ( MUSE_TRUE );

	} else {
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
}

/**
 * (require ModuleSymbol ...)
 *
 * Will first search the current directory for file named "ModuleSymbol.scm"
 * and load that if found. The module is not "import"ed. If such a file
 * doesn't exist, it will fetch and load 
 *  "http://muvee-symbolic-expressions.googlecode.com/svn/trunk/lib/ModuleSymbol.scm"
 * 
 * Can raise fetch-uri and load related exceptions.
 *
 * @exception error:invalid-module-reference
 * Handler format: @code (fn ('error:invalid-module-reference given-ref) ...) @endcode
 * Raised when the module (if not loaded yet) could neither be resolved as a local
 * file nor as a file on the library site.
 * 
 */
muse_cell fn_require( muse_env *env, void *context, muse_cell args )
{
	return muse_generate_list( env, require_modules, &args );
}

static muse_cell import_scope_begin( muse_env *env, void *self, muse_cell expr )
{
	/* Introduce the modules and then vanish from the function body. */
	return fn_import( env, self, _tail(expr) );
}

static void import_scope_end( muse_env *env, void *self, int bsp )
{
}

static muse_scope_view_t g_import_scope_view = { import_scope_begin, import_scope_end };

static void *import_view( muse_env *env, int id )
{
	switch (id) {
		case 'scop':	return &g_import_scope_view;
		default:			return NULL;
	}
}

/* When import is used within the body of a closure creation expression
using (fn ..), it has the effect of importing the module symbols for
the body and vanishing from the resultant closure so that there is no
runtime overhead when invoking the closure. We do this using the scope
view and by turning the import function into an object. */
static muse_functional_object_type_t g_import_type = {
	'muSE',
	'(imp',
	sizeof(muse_functional_object_t),
	fn_import,
	import_view,
	NULL,
	NULL,
	NULL,
	NULL
};


/*@}*/
/*@}*/

void muse_define_builtin_type_module( muse_env *env )
{
	int sp = _spos();
	_define( _csymbol(L"module"), _mk_functional_object( &g_module_expr_scope_type, MUSE_NIL ) );
	_define( _csymbol(L"import"), _mk_functional_object( &g_import_type, MUSE_NIL ) );
	_define( _csymbol(L"require"), _mk_nativefn( fn_require, NULL ) );
	_unwind(sp);
}