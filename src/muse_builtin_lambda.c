/**
 * @file muse_builtin_lambda.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_builtins.h"
#include "muse_port.h"

static void anonymize_formals( muse_env *env, muse_cell syms )
{
	if ( !syms )
		return;
	
	switch ( _cellt(syms) )
	{
		case MUSE_SYMBOL_CELL :
			/* I've temporarily given up on making muSE multi-threaded.
			 * So there is little point in spending cells in anonymizing
			 * local variables. I'm switching back to preserving local
			 * variables instead. This line used to be
			 * @code
			 * mnuse_pushdef( syms, _mk_anon_symbol() );
			 * @endcode
			 * */
			_pushdef( syms, syms );
			break;
		case MUSE_CONS_CELL :
			if ( !_isquote(_head(syms)) )
			{
				anonymize_formals( env, _head(syms) );
				anonymize_formals( env, _tail(syms) );
			}
			break;
		case MUSE_LAMBDA_CELL :
			/* This is the case of a guarded pattern. 
			Anonymize the lambda's argument pattern. */
			anonymize_formals( env, _quq(_head(syms)) );
			break;

		default:;
	}
}

static muse_cell anonymize_copy_letvars( muse_env *env, muse_cell bindings )
{
	if ( bindings )
	{
		muse_cell b = _head(bindings);
		muse_cell bcopy = _cons( _head(b), muse_bind_copy_expr( env, _tail(b), MUSE_FALSE ) );
		anonymize_formals( env, _head(b) );
		return _cons( bcopy, anonymize_copy_letvars( env, _tail(bindings)) );
	}
	else
		return MUSE_NIL;
}

static muse_cell anonymize_copy_case_body( muse_env *env, muse_cell body )
{
	if ( body )
	{
		muse_cell case1 = _head(body);
		muse_cell case1_copy = _cons(_head(case1), MUSE_NIL);
		
		{
			int sp = _spos();
			int bsp = _bspos();
			anonymize_formals( env, _head(case1) );
			_sett( case1_copy, muse_bind_copy_expr( env, _tail(case1), MUSE_FALSE ) );
			_unwind_bindings(bsp);
			_unwind(sp);
		}
		
		return _cons( case1_copy, anonymize_copy_case_body(env,_tail(body)) );
	}
	else
	{
		return MUSE_NIL;
	}
}

/**
 * A key function for implementing portions of closure creation.
 * What it does is top create a copy of the given "body" expression
 * binding all defined symbols to their values along the way. 
 *
 * @param body The expression whose copy needs to be created.
 * @param list_start If the body expression is the head part of a list expression,
 *   pass MUSE_TRUE, otherwise pass MUSE_FALSE. In general, you should pass
 *   MUSE_FALSE.
 */
muse_cell muse_bind_copy_expr( muse_env *env, muse_cell body, muse_boolean list_start )
{
	if ( body <= 0 )
		return body;
	
	switch ( _cellt(body) )
	{
		case MUSE_CONS_CELL :
		{
			muse_cell h, t;
			
			h = muse_bind_copy_expr( env, _head(body), MUSE_TRUE );
			if ( list_start )
			{
				if ( _cellt(h) == MUSE_NATIVEFN_CELL )
				{
					muse_nativefn_cell fn = _ptr(h)->fn;
					if ( fn.fn == fn_quote )
					{
						/* This is a quoted expression. Don't
						do any substitution in the body. */
						return body;
					}
					else if ( fn.fn == syntax_lambda || fn.fn == syntax_block || fn.fn == syntax_generic_lambda || fn.fn == syntax_generic_block )
					{
						/* Need to copy body for subexpressions that are
						both lexically as well as dynamically scoped. The
						dynamic scoping behaviour is limited to the nearest
						lexical scope enclosing the expression. */
						muse_cell c = MUSE_NIL;
						muse_cell formals = _head(_tail(body));
						if ( _head(formals) == env->builtin_symbols[MUSE_QUOTE] )
						{
							/* If the formal parameter list is quoted, it means
							the arguments to be given to this lambda must be passed
							to it unevaluated. */
							formals = _tail(formals);
							{
								int bsp = _bspos();
								anonymize_formals( env, formals );
								c = muse_bind_copy_expr( env, _tail(_tail(body)), MUSE_FALSE );
								_unwind_bindings(bsp);
							}
							return _cons( h, _cons( muse_quote(env,formals), c ) );
						}
						else
						{
							int bsp = _bspos();
							anonymize_formals( env, formals );
							c = muse_bind_copy_expr( env, _tail(_tail(body)), MUSE_FALSE );
							_unwind_bindings(bsp);
							return _cons( h, _cons( formals, c ) );
						}
					}
					else if ( fn.fn == syntax_let )
					{
						muse_cell c = _cons( MUSE_NIL, MUSE_NIL );
						int bsp = _bspos();
						muse_cell vars = anonymize_copy_letvars( env, _head(_tail(body)) );
						_setht( c, h, _cons( vars, muse_bind_copy_expr( env, _tail(_tail(body)), MUSE_FALSE) ) );
						_unwind_bindings(bsp);
						return c;
					}
					else if ( fn.fn == syntax_case )
					{
						muse_cell obj = muse_bind_copy_expr(env,_head(_tail(body)), MUSE_FALSE);
						return _cons( h, _cons( obj, anonymize_copy_case_body(env,_tail(_tail(body))) ) );
					}
					else if ( fn.fn == fn_define )
					{
						/* Note that in the case of fn_define, we don't unwind the bindings
						stack because the new definition should be in effect for the 
						rest of the body that contains the definition. */
						muse_cell c = MUSE_NIL;
						muse_cell name = _head(_tail(body));
						anonymize_formals( env, name );
						return _cons( h, _cons( name, muse_bind_copy_expr( env, _tail(_tail(body)), MUSE_FALSE ) ) );
					}
					else if ( fn.context )
					{
						/* We test for the 'scop' view which lets native objects
						introduce definitions within their scope. */
						muse_functional_object_t *obj = NULL;
						muse_scope_view_t *scope = (muse_scope_view_t*)_fnobjview( h, 'scop', obj );
						if ( scope ) {
							int sp = _spos();
							int bsp = _bspos();
							muse_cell body_subst = scope->begin( env, obj, _cons( h, _tail(body) ) );
							scope->end( env, obj, bsp );
							_unwind(sp);
							_spush(body_subst);
							return body_subst;
						}
					}
				}
			}
			else
			{
				if ( h != _head(body) && _cellt(h) == MUSE_SYMBOL_CELL )
				{
					/* The head evaluated to another symbol. Prevent its evaluation by quoting it. */
					h = _qq(h);
				}
			}

			t = muse_bind_copy_expr( env, _tail(body), MUSE_FALSE );
			return _cons( h, t );
		}
		case MUSE_SYMBOL_CELL :
		{
			/* We're evaluating a symbol. Quick-quote it by negating the
			cell value in order to prevent the result from being evaluated
			as an unquoted value. Closure context symbols will appear
			quoted but formals symbols (which are already quick quoted
			in anonymize_formals() function) will appear unquoted, which is 
			what we need. */
			muse_cell v = _symval(body);
			return _cellt(v) == MUSE_CONS_CELL ? -v : v;
		}
		default:
			return body;
	}
}

/**
 * A "captured recent scope" object is for the purpose of
 * allowing the use of (the ..) expressions and 'it' within
 * closures, to refer to the environment in which the closure
 * was created.
 */
typedef struct 
{
	muse_functional_object_t base;
	recent_entry_t scope[MUSE_MAX_RECENT_ITEMS];
	int count;
	muse_cell it;
} captured_recent_scope_t;

static void crs_init( muse_env *env, void *ptr, muse_cell args )
{
	captured_recent_scope_t *crs = (captured_recent_scope_t*)ptr;
	muse_process_frame_t *p = env->current_process;
	recent_t *r = &(p->recent);
	recent_context_t *c = r->contexts.vec + r->contexts.top;
	int d = c->depth;
	int i = c->base + d % MUSE_MAX_RECENT_ITEMS;
	int n = (c->base - c->prev) + c->depth;
	int k = 0;
	if ( n > MUSE_MAX_RECENT_ITEMS )
		n = MUSE_MAX_RECENT_ITEMS;
	while ( n > 0 && k < MUSE_MAX_RECENT_ITEMS ) {		
		/* i is pointing to the slot into which
		 the next recent item will be placed.
		 So we need to rewind by one item 
		 before we copy. */
		--d;
		if ( d >= 0 ) 
			i = c->base + d % MUSE_MAX_RECENT_ITEMS;
		else
			--i;
		
		crs->scope[k] = r->entries.vec[i];
		--n;
		++k;
	}
	crs->count = k;
		 
	crs->it = _symval(_builtin_symbol(MUSE_IT));
}

static void crs_mark( muse_env *env, void *ptr )
{
	captured_recent_scope_t *crs = (captured_recent_scope_t*)ptr;
	int i = 0;
	for ( i = 0; i < crs->count; ++i ) {
		/* Note that we only need to mark the value part.
		The key part is not necessarily a muSE cell
		and even if it is, it surely has a reference
		elsewhere in the system. If the key doesn't
		have a reference, it is not possible to refer
		to the recent computation using a (the ..)
		expression. */
		muse_mark( env, crs->scope[i].value );
	}
	
	muse_mark( env, crs->it );
}

/**
 * Writes out the vector to the given port in such a
 * way that the expression written out is converted
 * to a vector by a trusted read operation.
 */
static void crs_write( muse_env *env, void *ptr, void *port )
{
	port_write( "<<internal>>", 12, (muse_port_t)port );
}

/**
 * Simply copies the recent list into the current recent scope.
 */
static muse_cell fn_crs( muse_env *env, captured_recent_scope_t *crs, muse_cell args )
{
	int i = crs->count - 1;
	for ( ; i >= 0; --i ) {
		muse_add_recent_item( env, crs->scope[i].key, crs->scope[i].value );
	}
	_define(_builtin_symbol(MUSE_IT), crs->it);
	return MUSE_NIL;
}

static muse_functional_object_type_t g_captured_recent_scope_type =
{
	'muSE',
	'crsc',
	sizeof(captured_recent_scope_t),
	(muse_nativefn_t)fn_crs,
	NULL,
	crs_init,
	crs_mark,
	NULL,
	crs_write
};

/**
 * @code (with-recent (fn (...) ... (the thing1) ... it ...)) @endcode
 * 
 * Converts the function generated by the (fn ...) expression
 * so that its body can close on recent values in the context
 * in which the closure is being created.
 *
 * @todo Infer the use of with-recent depending on the
 * usage pattern of the and it within the body of the 
 * function itself. Its quite complicated to do.
 */
muse_cell fn_with_recent( muse_env *env, void *context, muse_cell args )
{
	/* Capture the recently computed items so that the closure can
	refer to them using (the ..) expressions and 'it'. The _cons
	is to turn the captured function into a function call. */
	muse_cell crs = _cons( muse_mk_functional_object( env, &g_captured_recent_scope_type, MUSE_NIL ), MUSE_NIL );
	muse_cell result = muse_do( env, args );
	if ( _cellt(result) == MUSE_LAMBDA_CELL ) {
		/* Edit the function directly and insert the crs call right after the meta information. */
		_sett( _tail(result), _cons(crs,_tail(_tail(result))) );
		return result;
	} else {
		return result;
	}
}

/**
 * @code (fn formal-args ...body...) @endcode
 * Common syntax -
 * @code
 * (fn (x1 x2 ... xN)
 *   expr1
 *   expr2
 *   ...
 *   result-expr)
 * @endcode
 * 
 * \c fn creates a closure when it is executed.
 * A closure is a copy of the body of the lambda with
 * all non-parameter variables bound to their current
 * values. If an undefined symbol (free variable) is used in the body,
 * it will simply evaluate to itself. Once the symbol is
 * defined after the creation of the closure, the defined
 * value will take the place of the symbol in the closure.
 * 
 * For example -
 * @code
 * (define norm3 (fn (x y z) (sqrt (+ (* x x) (* y y) (* z z)))))
 * @endcode
 * defines the symbol \c norm3 to be a function that computes 
 * the norm (i.e. vector-length) of a 3-vector. You use the function
 * in expressions like the following -
 * @code
 * (norm3 3 4 0)
 * (norm3 10 20 30)
 * @endcode
 * 
 * Binding formals -
 * 
 * Argments to a function are given as a list. Therefore a function
 * may accept a variable number of arguments, computing different (but
 * hopefully related) values in the different cases. The mechanism using which
 * a function accesses its entire argument list is more general than
 * purely for that case, though.
 * 
 * The arguments to the \c norm3 function in the above examples
 * may be considered to be lists like @code (10 20 30) @endcode. In order
 * to obtain the argument list, simply drop the function position. It is
 * now easy to see that the formal argument pattern @code (x y z) @endcode
 * is similar to the given argument list and a one-to-one mapping can be
 * established between the symbols \c x, \c y and \x z in the formals list
 * and the values in the arguments list. This is how the matching and binding
 * is done.
 * 
 * In order to illustrate the matching method, let us write the
 * \c norm3 argument list in its canonical form - as 
 * @code (10 . (20 . (30 . ()))) @endcode. The formals pattern can also
 * be written in such a canonical form - as @code (x . (y . (z . ()))) @endcode.
 * 
 * @note The binder lets a symbol to be bound to either the head or the 
 * tail of any cons cell in the argument list. Anything other than a symbol 
 * in the formals pattern must match the argument position exactly. 
 * 
 * Therefore if you simply use one symbol as the formals specification 
 * (ex: \c args), it will be bound to the entire argument list. If you use
 * a pattern like @code (x . xs) @endcode, \c x will be bound to the first
 * argument \c 10 and \c xs will be bound to the first cons cell's tail,
 * i.e. the list @code (20 30) @endcode. It is valid for a symbol to be 
 * bound to \c (), but it is not valid to deconstruct a \c () value and
 * assign its components to symbols. i.e. \c () will not match against
 * <tt>(x . xs)</tt> but will match against \c xs.
 * 
 * @see muse_bind_formals()
 * @see syntax_let
 * @see syntax_case
 */
muse_cell syntax_lambda( muse_env *env, void *context, muse_cell args )
{
	muse_cell formals = _head(args);
	muse_cell body = _tail(args);
	muse_cell closure = _setcellt( _cons( formals, MUSE_NIL ), MUSE_LAMBDA_CELL );
	
	if ( _head(formals) == env->builtin_symbols[MUSE_QUOTE] )
	{
		/* The formals list is quoted. So we have to create the lambda
		which will process its arguments unevaluated. We indicate this
		by quick-quoting the formals list in the generated closure. 
		Typically, these "macros" should only be used at read-time.

		Using quick-quoting to indicate macros saves us the cost of
		doing _head(_head(fn)) == env->builtin_symbol[MUSE_QUOTE]
		kind of elaborate check.
		
		See also _eval and muse_apply_lambda. */

		int bsp = _bspos();

		/* Skip the quote indicator. */
		formals = _tail(formals);

		anonymize_formals( env, formals );
		anonymize_formals( env, _builtin_symbol(MUSE_IT) );
		
		_sett( closure, muse_bind_copy_expr( env, body, MUSE_FALSE ) );
		
		_unwind_bindings(bsp);

		/* Use quick-quoting instead of muse_quote for
		efficiency at runtime. This minimizes impact on
		ordinary lambda execution. */
		_seth( closure, _qq(formals) );
	}
	else
	{
		int bsp = _bspos();
		anonymize_formals( env, formals );
		anonymize_formals( env, _builtin_symbol(MUSE_IT) );
		
		_sett( closure, muse_bind_copy_expr( env, body, MUSE_FALSE ) );
				
		_unwind_bindings(bsp);

		_seth( closure, formals );
	}

	// As a short cut, if the first entry in a function's body is
	// a constant and there are other items afterwards, 
	// use it as the name of the function. This is useful 
	// for (fn (..) ..) kind of expressions used anonymously.
	{
		muse_cell clbody = _tail(closure);
		if ( clbody )
		{
			muse_cell rest = _tail(clbody);
			if ( rest )
			{
				// Check head to see if it is a constant.
				muse_cell h = _head(clbody);
				switch ( _cellt(h) )
				{
				case MUSE_TEXT_CELL:
				case MUSE_INT_CELL:
				case MUSE_FLOAT_CELL:
					_sett(closure,rest);
					meta_putname( env, closure, h );
				default:;
				}
			}

			{
				int sp = _spos();
				muse_cell argcell = _cons( body, MUSE_NIL );
				muse_put( env, muse_get_meta( env, closure ), _builtin_symbol(MUSE_CODE), argcell );
				_unwind(sp);
				_returncell(argcell);
			}
		}
	}

	return closure;
}

/**
 * @code (fn: (arg1 arg2 ...) ...body...) @endcode
 *
 * Very similar to \ref syntax_lambda, except that it doesn't
 * create a closure. All the free variables in the block
 * (i.e. those except the given arguments) are expected
 * to be bound by the environment in which the block is
 * invoked. Otherwise a block is identical to \ref syntax_lambda
 * and can be used in all places a closure can be used.
 *
 * In particular, a block is effective with call/cc to
 * specify jump out points such as exceptions and loop breaks.
 * A normal closure can be used as well, but the closure will be 
 * repeatedly created every time the call/cc expression is evaluated.
 *
 * Creating a block is extremely cheap compared
 * to creating a closure using \ref syntax_lambda. As a thumb rule,
 * blocks are equivalent to closures when they are consumed within 
 * the scope of their declaration - as in the following example -
 * @code
 *    (map (fn: (x) (* x 2)) '(1 2 3 4 5))
 * @endcode
 * The above code evaluates to the list
 * @code
 *    (2 4 6 8 10)
 * @endcode
 * and \c fn: in this case is equivalent to using \c fn.
 *
 * If you define a symbol to a block, then you need to be careful,
 * because the value of any free variables (non-local) used within
 * the block can be redefined outside the block. For example -
 * @code
 *   (define y 2)
 *   (define double (fn: (x) (* x y)))
 *   (double 3)
 *       -> 6
 *   (double 5)
 *       -> 10
 *   (define y 3)
 *   (double 3)
 *       -> 9
 *   (define y 100)
 *   (double 3)
 *       -> 300
 * @endcode
 * whereas the same sequence with "fn" instead will be -
 * @code
 *   (define y 2)
 *   (define double (fn (x) (* x y)))
 *   (double 3)
 *       -> 6
 *   (double 5)
 *       -> 10
 *   (define y 3)
 *   (double 3)
 *       -> 6
 *   (define y 100)
 *   (double 3)
 *       -> 6
 * @endcode
 * because \c fn "captures" the value of y at the time it is invoked to
 * create a closure. It is the closure thus created which is bound to
 * the symbol "double".
 */
muse_cell syntax_block( muse_env *env, void *context, muse_cell args )
{
	return _setcellt( args, MUSE_LAMBDA_CELL );
}

static muse_cell case_lambda( muse_env *env, muse_cell fn )
{
	muse_cell generic_args = _csymbol(L"{{generic-args}}");
	
	muse_cell qqopt_generic_args = _head(fn) < 0 ? _qq(generic_args) : generic_args;

	muse_cell case_e = _head(fn) < 0 ? _cons( _quq(_head(fn)), _tail(fn) ) : _setcellt( fn, MUSE_CONS_CELL );

	/* Construct a "case-lambda" for the generic function. */
	{
		muse_cell gfn = _setcellt( _cons( qqopt_generic_args,
										  _cons( _cons( _symval(_csymbol(L"case")),
														_cons( generic_args,
															   _cons( case_e, 
																	  MUSE_NIL ) ) ),
											     MUSE_NIL ) ),
								   MUSE_LAMBDA_CELL );

		return gfn;
	}
}

/** @addtogroup GenericFns Generic functions 
 * A generic function is one whose definition can be augmented
 * even after it is first created. Every function presents a
 * "case" to the evaluator - its supplied argument may or
 * may not match the declared argument pattern of the function.
 * The generic function mechanism allows you to define a function
 * in terms of a number of such cases that are incrementally
 * specified - potentially in different source files.
 *
 * For example, the humble factorial can be defined in 
 * two phases -
 * @code
 * (define fact (gfn (1) 1))
 * (define fact (fn (n) (* n (fact (- n 1)))))
 * @endcode
 * The second function's definition is added to the existing 
 * generic function so that you can do the following -
 * @code
 * > (fact 3)
 * 6
 * > (fact 5)
 * 120
 * @endcode
 *
 * You can use \ref fn_define_extension "define-extension" or
 * \ref fn_define_override "define-override" to augment a generic
 * function. When supplied with a generic function, \ref fn_define "define"
 * itself behaves like \c define-extension which is the most common
 * use case for generic functions.
 *
 * It is important to note that if a partially specified generic function
 * is captured in a closure and then augmented with a new case, 
 * the new case \b will influence the closure.
 *
 * Since macros are just functions, you can create "generic macros"
 * the same way you create generic functions. The constraint here is
 * that a function used to extend a generic macro must itself be
 * a macro.
 *
 * There is some overhead to using generic functions
 * versus normal functions - the overhead of dispatching a given
 * set of arguments to the appropriate case. 
 */
/*@{*/

/**
 * (gfn formal-args body)
 *
 * \c gfn creates a generic function that can be extended or overridden using
 * \ref fn_define_extension "define-extension" and \ref fn_define_override "define-override". 
 * You can define generic functions as well as macros.
 *
 * @see \ref syntax_generic_block "gfn:"
 */
muse_cell syntax_generic_lambda( muse_env *env, void *context, muse_cell args ) 
{
	return case_lambda( env, syntax_lambda( env, context, args ) );
}

/**
 * (gfn: formal-args body)
 *
 * \c gfn: creates a generic function that can be extended or overridden using
 * \ref fn_define_extension "define-extension" and \ref fn_define_override "define-override". 
 * This version is similar to \ref syntax_generic_lambda "gfn", but
 * uses dynamic scoping in its body.
 *
 * @see \ref syntax_generic_lambda "gfn"
 */
muse_cell syntax_generic_block( muse_env *env, void *context, muse_cell args ) 
{
	return case_lambda( env, syntax_block( env, context, args ) );
}

/*@}*/

/**
 * (apply fn arglist).
 * Equivalent to @code (eval (cons fn arglist)) @endcode. The \c apply function
 * lets you apply the given function to the given argument list, both of which
 * may be values of other expressions.
 * 
 * For example -
 * @code
 * (define one-to-ten '(1 2 3 4 5 6 7 8 9 10))
 * (print (apply + one-to-ten))
 * @endcode
 * 
 * The above code will print the sum of numbers from 1 to 10, and is equivalent
 * to @code (print (+ 1 2 3 4 5 6 7 8 9 10)) @endcode.
 */
muse_cell fn_apply( muse_env *env, void *context, muse_cell args )
{
	muse_cell fn = _evalnext(&args);
	return _apply( fn, _evalnext(&args), MUSE_TRUE );
}

/**
 * @param kvpairs A list with the 1st, 3rd, 5th etc. entries being symbols
 * and the 2nd, 4th, etc. places being the values that those symbols must be
 * set to.
 */
static int bind_keys( muse_env *env, muse_cell kvpairs, muse_boolean args_already_evaluated )
{
	int bsp = _bspos();
	int sp = _spos();

	while ( kvpairs )
	{
		muse_cell sym = _evalnext(&kvpairs);

		MUSE_DIAGNOSTICS({
			muse_expect( env, L"(call/keywords f ... >>sym<< val ...)", L"v?", sym, MUSE_SYMBOL_CELL );
		});

		{
			muse_cell val = args_already_evaluated ? _next(&kvpairs) : _evalnext(&kvpairs);

			_pushdef( sym, val );
		}
	}

	_unwind(sp);
	return bsp;
}

/**
 * @code (call/keywords f 'key1 val1 'key2 val2 ...) @endcode
 *
 * \p f is a user defined function (non-native) that has a bunch of
 * named arguments. You can supply values to those named arguments
 * in another order compared to what it was declared with.
 * For example -
 * @code
 * (define f (fn (x y) (if (< x y) x y)))
 * @endcode
 * defines the minimum function.
 * You can call this function as follows -
 * @code
 * (call/keywords f 'y 15 'x 3)
 * @endcode
 * to get \c 3 as the answer.
 * Note that all arguments \b must be specified for the result to make sense.
 * That condition is not checked for. Note that the symbol positions are
 *
 *
 * Note that although it makes sense to specify all arguments without exception,
 * the behaviour is such that arguments which are unspecified take on
 * the value of the symbol at invocation time. This means you can introduce
 * "default" values using \ref syntax_let "let" blocks. For example -
 * @code
 * (let ((x 15)) (call/keywords f 'y 30))
 * @endcode
 * will produce \c 15 as the answer. In this sense, 
 * @code (call/keywords f 'x 15 'y 30) @endcode
 * is equivalent to 
 * @code (let ((x 15) (y 30)) (call/keywords f)) @endcode.
 * This gives you yet another kind of dynamic scoping behaviour.
 *
 * If you want to know the function's formal argument list at any time,
 * you can evaluate @code (first f) @endcode.
 *
 * If \p f is a macro function - i.e. it requires its arguments in an
 * unevaluated form - the expressions in place of values are used without
 * being first evaulated. Note that the symbol positions are always evaluated.
 */
muse_cell fn_call_w_keywords( muse_env *env, void *context, muse_cell args )
{
	yield_process(env,1);

	{
		muse_cell f = _evalnext(&args);

		/* Only works with user defined functions. */
		switch ( _cellt(f) )
		{
		case MUSE_LAMBDA_CELL :
			/* Bind all the keyword arguments to their values. */
			{
				int bsp = bind_keys( env, args, (_head(f) < 0) ? MUSE_TRUE : MUSE_FALSE );

				muse_cell result = _do(_tail(f));

				_unwind_bindings(bsp);

				return result;
			}
		case MUSE_NATIVEFN_CELL :
			{
				/* We're calling a native function which will do its own argument
				evaluation. So pass the arguments without evaluating. */
				int bsp = bind_keys( env, args, MUSE_TRUE );

				/* We don't pass any arguments in the keywords case. If 
				the function also takes positional arguments, it can use this
				to determine whether it is being called with keywords or with
				positional arguments. */
				muse_cell result = _ptr(f)->fn.fn( env, _ptr(f)->fn.context, MUSE_NIL );

				_unwind_bindings(bsp);

				return result;

			}
		default:
			MUSE_DIAGNOSTICS({
				muse_message( env, L"(call/keywords >>fn<< ...)", L"Can only call functions!\nYou gave [%m].", f );
			});
			return MUSE_NIL;
		}
	}
}

static int bind_alist( muse_env *env, muse_cell kvpairs )
{
	int bsp = _bspos();
	int sp = _spos();

	while ( kvpairs )
	{
		muse_cell pair = _next(&kvpairs);

		MUSE_DIAGNOSTICS({
			muse_expect( env, L"(apply/keywords f '(... (>>key<< . value) ...))", L"v?", _head(pair), MUSE_SYMBOL_CELL );
		});

		_pushdef( _head(pair), _tail(pair) );
	}

	_unwind(sp);
	return bsp;
}

/**
 * The counterpart of \ref fn_call_w_keywords "call/keywords" that applies a function
 * to an a-list of key-value pairs.
 * @code
 * (define f (fn (x y) (if (< x y) x y)))
 * (define args '((x . 15) (y . 30)))
 * (apply/keywords f args)
 * @endcode
 * will evaluate to \c 15.
 */
muse_cell fn_apply_w_keywords( muse_env *env, void *context, muse_cell args )
{
	yield_process(env,1);

	{
		muse_cell f = _evalnext(&args);

		/* Bind all the keyword arguments to their values. */
		{
			int bsp = bind_alist( env, _evalnext(&args) );

			muse_cell result = MUSE_NIL;
			
			switch ( _cellt(f) )
			{
			case MUSE_LAMBDA_CELL	: result = _do(_tail(f)); break;
			case MUSE_NATIVEFN_CELL : result = _ptr(f)->fn.fn( env, _ptr(f)->fn.context, MUSE_NIL ); break;
			default:
				MUSE_DIAGNOSTICS({
					muse_message( env, L"(apply/keywords >>fn<< ...)", L"Can only apply functions!\nYou gave [%m].", f );
				});
			}

			_unwind_bindings(bsp);

			return result;
		}
	}
}

/**
 * Runs through the bindings and returns MUSE_TRUE if everything bound successfully
 * and MUSE_FALSE if there was a failure. In case of failure, all bindings are reversed.
 */
static muse_boolean bind_letvars( muse_env *env, muse_cell bindings )
{
	if ( bindings )
	{
		muse_cell binding = _head(bindings);
		int bsp = _bspos();
		muse_cell value = _eval(_head(_tail(binding)));
		if ( muse_bind_formals( env, _head(binding), value ) )
		{
			/* This binding succeeded. If everything else also succeeds,
			we keep this binding, otherwise we reverse it. */
						
			if ( bind_letvars( env, _tail(bindings) ) )
			{
				return MUSE_TRUE;
			}
			else
			{
				_unwind_bindings(bsp);
				return MUSE_FALSE;
			}
		}
		else
		{
			MUSE_DIAGNOSTICS({
				muse_message( env,	L"Error in let expression!",
								L"The pattern\n\t%m\n"
								L"doesn't match the result of the expression\n\t%m\n"
								L"which evaluates to\n\t%m.",
								_head(binding),
								_head(_tail(binding)),
								value );
			});

			return MUSE_FALSE;
		}
	}
	else
	{
		/* Nothing to bind always succeeds 'cos there's no reason to fail :) */
		return MUSE_TRUE;
	}
}

/**
 * (let &lt;variable-bindings&gt; &lt;body&gt;).
 * Syntax -
 * @code
 * (let ((pattern1 value1)
 *       (pattern2 value2)
 *       ...
 *       (patternN valueN))
 *   expr1
 *   expr2
 *   ...
 *   result-expr)
 * @endcode
 * 
 * \c let introduces local variables bound to the results of
 * given expressions in the context of a block of code.
 * For example, an expression to compute the distance
 * between two points (x1,y1) and (x2,y2) can be written
 * as -
 * @code
 * (let ((dx (- x2 x1))
 *       (dy (- y2 y1)))
 *   (print "Computing distance between two points ...")
 *   (sqrt (* dx dx) (* dy dy)))
 * @endcode
 * The above code introduces the symbols \c dx and \c dy bound to
 * <tt>x2-x1</tt> and <tt>y2-y1</tt> respectively, in the expression
 * <tt>(sqrt (* dx dx) (* dy dy))</tt>.
 * 
 * The result of the let expression is the result of the last 
 * expression in the body. In the above example, the last statement
 * computes the \c sqrt.
 * 
 * The variable binding scheme in \c let is exactly the same as
 * the argument binding scheme for lambdas. This means, you can 
 * decompose lists using let expressions as follows -
 * @code
 * (let (((x y . xs) things))
 *    (print "x = " x ", y = " y ", and the rest are " xs))
 * @endcode
 * If \c things is the list <tt>(1 2 3 4)</tt>, the above 
 * expression will print
 * @code
 * x = 1, y = 2, and the rest are (3 4)
 * @endcode
 * 
 * If any of the binding operations failed, the let block is not
 * evaluated and the result is \c ().
 * 
 * @see muse_bind_formals()
 * @see syntax_lambda
 * @see syntax_case
 */
muse_cell syntax_let( muse_env *env, void *context, muse_cell args )
{
	int sp = _spos();
	int bsp = _bspos();
	muse_cell result = MUSE_NIL;
	muse_cell bindings = _next(&args);
	
	_spush(bindings);
	if ( bind_letvars( env, bindings ) )
	{
		_spush(args);
		result = _do( args );
		_unwind(sp);
		_spush(result);
	}
	else
	{
		/* Some binding failed. */
		_unwind(sp);
	}
	
	_unwind_bindings(bsp);
	return result;
}

muse_cell guarded_do( muse_env *env, muse_cell expr );

/**
 * (case object &lt;match-cases&gt;).
 * Syntax -
 * @code
 * (case object
 *       (match-expr1 result1)
 *       (match-expr2 result2)
 *       ...
 *       (T else-result))
 * @endcode
 * Like \c switch in C, but way more expressive. The result
 * of a \c case expression is the result corresponding to the
 * \c match-expr that succeeded in a match-bind operation 
 * against the given \c object.
 * 
 * In the simplest case, when \c object is a symbol, you
 * can use \c case like \c switch as follows -
 * @code
 * (case object
 *       ('one "One a penny")
 *       ('two "Two a penny")
 *       ('what "Hot cross buns"))
 * @endcode
 * 
 * The match expressions are not limited to constants and are
 * basically the same as the binding expressions for \c let
 * or \c lambda. For example, here is a \c case expression to
 * print the head of a list -
 * @code
 * (case list-object
 *       ((x . xs) (print "Head of " list-object " is " x "."))
 *       (() (print "Cannot take head of the empty list!")))
 * @endcode
 * In the above code, the first match expression <tt>(x . xs)</tt>
 * is bound against the given \c list-object. If \c list-object
 * is a non-empty list, this bind operation will succeed and
 * \c x will be bound to the head of the list and \c xs will be
 * bound to the tail of the list. The second match expression is
 * the empty list which will succeed in matching against another
 * empty list only. So if \c list-object is the empty list, then
 * the second print statement will be evaluated.
 * 
 * @see muse_bind_formals()
 * @see syntax_lambda
 * @see syntax_let
 */
muse_cell syntax_case( muse_env *env, void *context, muse_cell args )
{
	muse_cell object = _evalnext(&args);
	muse_cell cases = args;
	int bsp = _bspos();
	
	MUSE_DIAGNOSTICS({
		muse_expect( env, L"case expression", L"v?!=", cases, MUSE_CONS_CELL, MUSE_NIL );
	});

	while ( cases )
	{
		muse_cell thiscase = _next(&cases);
		
		MUSE_DIAGNOSTICS({
			if ( !muse_expect( env, L"case expression", L"v!=", _tail(thiscase), MUSE_NIL ) )
				muse_message( env,L"Syntax error in case body",
							  L"%m\n\nis not a valid case body, which must have the form\n"
							  L"(<case-pattern> <body>...)",
							  thiscase );
		});

		if ( muse_bind_formals( env, _head(thiscase), object ) )
		{
			muse_cell result = guarded_do( env, _tail(thiscase) );

			_unwind_bindings(bsp);
			return muse_add_recent_item( env, (muse_int)syntax_case, result );
		}
	}

	MUSE_DIAGNOSTICS({
		muse_message( env,	L"Error in case expression",
						L"The object\n\t%m\nfailed to match any case.",
						object 
						);
	});

	return MUSE_NIL;
}

static muse_cell delay_expr( muse_env *env, muse_cell expr )
{
	muse_cell e = muse_bind_copy_expr(env,expr,MUSE_TRUE);
	if ( _isquote(e) )
		return e;
	else
		return _setcellt( _cons( MUSE_NIL, e ), MUSE_LAZY_CELL );
}

/**
 * @defgroup LazyEvaluation Lazy evaluation
 */
/*@{*/
/**
 * @code (lazy fn arg1 arg2 ...) @endcode
 *
 * Delays the application of the given function to the given
 * arguments. Each argument is itself evaluated right away before
 * encapsulating the function call as a lazy application.
 *
 * Since native function calls are not tail call optimized, you
 * can use \ref fn_lazy "lazy" to make a delayed tail call to
 * the function.
 * 
 * For reading such an expression, think of the "lazy" as a
 * tag that marks the remainder of the expression as lazy,
 * rather than an operator applied to its arguments. The meaning
 * of a (lazy f a1 a2 ..) expression is exactly the same as
 * (f a1 a2 ...) except that the application is not performed
 * immediately. Note that (lazy f) is the same as (f).
 *
 * @see \ref fn_lcons "lcons"
 */
muse_cell fn_lazy( muse_env *env, void *context, muse_cell args )
{
	muse_cell eargs = muse_eval_list( env, args );
	return _setcellt( eargs, MUSE_LAZY_CELL );
}

/**
 * (lcons a b)
 *
 * A "lazy" version of (cons a b) where the a and b are expressions
 * that are not immediately evaluated. They'll be evaluated when their
 * values are required - i.e. when you do a (first ...) or (rest ...)
 * invocation on the resultant cons cell.
 *
 * Any operation that accesses the head or tail of a lazy cons cell
 * will force the evaluation. Such operations include printing out
 * the list, calling nth, take, drop, etc.
 *
 * For example -
 * @code
 * > (define (numsfrom N) (lcons N (numsfrom (+ N 1))))
 * > (take 10 (numsfrom 100))
 * (100 101 102 103 104 105 106 107 108 109)
 * @endcode
 *
 * @see \ref fn_lazy "lazy"
 * @see \ref fn_cons "cons"
 */
muse_cell fn_lcons( muse_env *env, void *context, muse_cell args )
{
	muse_cell h = delay_expr(env,_next(&args));
	muse_cell t = delay_expr(env,_next(&args));

	return _cons( h, t );
}
/*@}*/
