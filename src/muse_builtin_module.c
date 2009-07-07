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
 * you can refer to. You define a module using the \ref syntax_module "module"
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

static void module_init( muse_env *env, void *ptr, muse_cell args )
{
	int bsp = _bspos();
	module_t *m = (module_t*)ptr;
	muse_cell mname, exports;
	muse_cell sym_main = _csymbol(L"main");
	mname = _next(&args);
	exports = _next(&args);
	
	MUSE_DIAGNOSTICS({
		muse_expect( env, L"(module >>name<< ..)", L"v?", mname, MUSE_SYMBOL_CELL );
		if ( _symval(mname) != mname ) {
			/* Module already defined. */
			muse_message( env, L"(module >>name<< ..)", L"Module name %m already in use.", mname ); 
		}
		muse_expect( env, L"(module name >>(exports..)<< ..)", L"v?", exports, MUSE_CONS_CELL );
	});

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
		muse_cell value = muse_raise_error( env, _csymbol(L"error:key-not-found"), _cons( m->base.ref, _cons( key, MUSE_NIL ) ) );
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
	   an "immutable key" error and let it continue by supplying a substitute
	   key instead. */
	if ( _tail(argv) ) 
		return muse_get( env,
						muse_raise_error( env, _csymbol(L"error:key-not-found"), _cons( m->base.ref, _cons( key, MUSE_NIL ) ) ),
						_head(argv), 
						_tail(argv) );
	else
		return module_put( env, self, muse_raise_error( env, _csymbol(L"error:immutable-key"), _cons( m->base.ref, _cons( key, MUSE_NIL ) ) ), argv );
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
 * (MyMod ...)
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
 * @code
 * (module MyMod (exportA exportB ...)
 *   ...definitions...
 * )
 * @endcode
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
 * from a module.
 */
muse_cell fn_module( muse_env *env, void *context, muse_cell args )
{
	muse_cell mname = _head(args);
	muse_cell mod = muse_mk_functional_object( env, &g_module_type, args );
	_define(mname,mod);
	return mod;
}

static void introduce_module_global( muse_env *env, module_t *m )
{
	int i = 0;
	for ( ; i < m->length; ++i ) {
		_define( m->bindings[i].name, m->bindings[i].value );
	}
}

/**
 * (import ModA ModB ..)
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
	while ( args ) {
		muse_cell mod = _evalnext(&args);
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
	_define( _csymbol(L"module"), _mk_nativefn( fn_module, NULL ) );
	_define( _csymbol(L"import"), _mk_functional_object( &g_import_type, MUSE_NIL ) );
	_unwind(sp);
}