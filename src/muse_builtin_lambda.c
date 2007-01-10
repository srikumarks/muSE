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

static void anonymize_formals( muse_cell syms )
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
			 * mnuse_pushdef( syms, muse_mk_anon_symbol() );
			 * @endcode
			 * */
			muse_pushdef( syms, syms );
			break;
		case MUSE_CONS_CELL :
			if ( !_isquote(_head(syms)) )
			{
				anonymize_formals( _head(syms) );
				anonymize_formals( _tail(syms) );
			}
			break;
		case MUSE_LAMBDA_CELL :
			/* This is the case of a guarded pattern. 
			Anonymize the lambda's argument pattern. */
			anonymize_formals( _quq(_head(syms)) );
			break;

		default:;
	}
}

static void anonymize_letvars( muse_cell bindings )
{
	int sp = _spos();
	while ( bindings )
	{
		anonymize_formals( _head(_next(&bindings)) );
		_unwind(sp);
	}
}

static muse_cell bind_copy_body( muse_cell body, muse_boolean list_start );

static muse_cell anonymize_copy_case_body( muse_cell body )
{
	if ( body )
	{
		muse_cell case1 = _head(body);
		muse_cell case1_copy = muse_cons(_head(case1), MUSE_NIL);
		
		{
			int sp = _spos();
			int bsp = _bspos();
			anonymize_formals( _head(case1) );
			_sett( case1_copy, bind_copy_body( _tail(case1), MUSE_FALSE ) );
			_unwind_bindings(bsp);
			_unwind(sp);
		}
		
		return muse_cons( case1_copy, anonymize_copy_case_body(_tail(body)) );
	}
	else
	{
		return MUSE_NIL;
	}
}

static muse_cell bind_copy_body( muse_cell body, muse_boolean list_start )
{
	if ( body <= 0 )
		return body;
	
	switch ( _cellt(body) )
	{
		case MUSE_CONS_CELL :
		{
			muse_cell h, t;
			
			h = bind_copy_body( _head(body), MUSE_TRUE );
			if ( list_start )
			{
				if ( _cellt(h) == MUSE_NATIVEFN_CELL )
				{
					muse_nativefn_t fn = _ptr(h)->fn.fn;
					if ( fn == fn_quote )
					{
						/* This is a quoted expression. Don't
						do any substitution in the body. */
						return body;
					}
					else if ( fn == syntax_lambda || fn == syntax_block )
					{
						/* Need to copy body for subexpressions that are
						both lexically as well as dynamically scoped. The
						dynamic scoping behaviour is limited to the nearest
						lexical scope enclosing the expression. */
						muse_cell c = MUSE_NIL;
						muse_cell formals = _head(_tail(body));
						if ( _head(formals) == _env()->builtin_symbols[MUSE_QUOTE] )
						{
							/* If the formal parameter list is quoted, it means
							the arguments to be given to this lambda must be passed
							to it unevaluated. */
							formals = _tail(formals);
							{
								int bsp = _bspos();
								anonymize_formals( formals );
								c = bind_copy_body( _tail(_tail(body)), MUSE_FALSE );
								_unwind_bindings(bsp);
							}
							return muse_cons( h, muse_cons( muse_quote(formals), c ) );
						}
						else
						{
							int bsp = _bspos();
							anonymize_formals( formals );
							c = bind_copy_body( _tail(_tail(body)), MUSE_FALSE );
							_unwind_bindings(bsp);
							return muse_cons( h, muse_cons( formals, c ) );
						}
					}
					else if ( fn == syntax_let )
					{
						muse_cell c = muse_cons( MUSE_NIL, MUSE_NIL );
						int bsp = _bspos();
						anonymize_letvars( _head(_tail(body)) );
						_setht( c, h, bind_copy_body(_tail(body),MUSE_FALSE) );
						_unwind_bindings(bsp);
						return c;
					}
					else if ( fn == syntax_case )
					{
						muse_cell obj = bind_copy_body(_head(_tail(body)), MUSE_FALSE);
						return muse_cons( h, muse_cons( obj, anonymize_copy_case_body(_tail(_tail(body))) ) );
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

			t = bind_copy_body( _tail(body), MUSE_FALSE );
			return muse_cons( h, t );
		}
		case MUSE_SYMBOL_CELL :
		{
			/* We're evaluating a symbol. Quick-quote it by negating the
			cell value in order to prevent the result from being evaluated
			as an unquoted value. Closure context symbols will appear
			quoted but formals symbols (which are already quick quoted
			in anonymize_formals() function) will appear unquoted, which is 
			what we need. */
			muse_cell v = muse_symbol_value(body);
			return _cellt(v) == MUSE_CONS_CELL ? -v : v;
		}
		default:
			return body;
	}
}



/**
 * (fn formal-args <body>).
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
	muse_cell closure = _setcellt( muse_cons( formals, MUSE_NIL ), MUSE_LAMBDA_CELL );

	if ( _head(formals) == _env()->builtin_symbols[MUSE_QUOTE] )
	{
		/* The formals list is quoted. So we have to create the lambda
		which will process its arguments unevaluated. We indicate this
		by quick-quoting the formals list in the generated closure. 
		Typically, these "macros" should only be used at read-time.

		Using quick-quoting to indicate macros saves us the cost of
		doing _head(_head(fn)) == _env()->builtin_symbol[MUSE_QUOTE]
		kind of elaborate check.
		
		See also muse_eval and muse_apply_lambda. */

		int bsp = _bspos();

		/* Skip the quote indicator. */
		formals = _tail(formals);

		anonymize_formals( formals );
		
		_sett( closure, bind_copy_body( body, MUSE_FALSE ) );
		
		_unwind_bindings(bsp);

		/* Use quick-quoting instead of muse_quote for
		efficiency at runtime. This minimizes impact on
		ordinary lambda execution. */
		_seth( closure, _qq(formals) );
	}
	else
	{
		int bsp = _bspos();
		anonymize_formals( formals );
		
		_sett( closure, bind_copy_body( body, MUSE_FALSE ) );
		
		_unwind_bindings(bsp);

		_seth( closure, formals );
	}

	return closure;
}

/**
 * (fn: (arg1 arg2 --- argN) ---body---).
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
	muse_cell fn = muse_evalnext(&args);
	return muse_apply( fn, muse_evalnext(&args), MUSE_TRUE );
}

/**
 * @param kvpairs A list with the 1st, 3rd, 5th etc. entries being symbols
 * and the 2nd, 4th, etc. places being the values that those symbols must be
 * set to.
 */
static int bind_keys( muse_cell kvpairs, muse_boolean args_already_evaluated )
{
	int bsp = _bspos();
	int sp = _spos();

	while ( kvpairs )
	{
		muse_cell sym = muse_evalnext(&kvpairs);

		MUSE_DIAGNOSTICS({
			muse_expect( L"(call/keywords f ... >>sym<< val ...)", L"v?", sym, MUSE_SYMBOL_CELL );
		});

		{
			muse_cell val = args_already_evaluated ? _next(&kvpairs) : muse_evalnext(&kvpairs);

			muse_pushdef( sym, val );
		}
	}

	_unwind(sp);
	return bsp;
}

/**
 * (call/keywords f 'key1 val1 'key2 val2 ...)
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
 * "default" values using \ref fn_let "let" blocks. For example -
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
	yield_process(1);

	{
		muse_cell f = muse_evalnext(&args);

		/* Only works with user defined functions. */
		muse_assert( _cellt(f) == MUSE_LAMBDA_CELL );

		/* Bind all the keyword arguments to their values. */
		{
			int bsp = bind_keys(args, (_head(f) < 0) ? MUSE_TRUE : MUSE_FALSE );

			muse_cell result = muse_do(_tail(f));

			_unwind_bindings(bsp);

			return result;
		}
	}
}

static int bind_alist( muse_cell kvpairs )
{
	int bsp = _bspos();
	int sp = _spos();

	while ( kvpairs )
	{
		muse_cell pair = _next(&kvpairs);

		MUSE_DIAGNOSTICS({
			muse_expect( L"(apply/keywords f >>alist<<)", L"v?", _head(pair), MUSE_SYMBOL_CELL );
		});

		muse_pushdef( _head(pair), _tail(pair) );
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
	yield_process(1);

	{
		muse_cell f = muse_evalnext(&args);

		/* Only works with user defined functions. */
		muse_assert( _cellt(f) == MUSE_LAMBDA_CELL );

		/* Bind all the keyword arguments to their values. */
		{
			int bsp = bind_alist( muse_evalnext(&args) );

			muse_cell result = muse_do(_tail(f));

			_unwind_bindings(bsp);

			return result;
		}
	}
}

/**
 * Runs through the bindings and returns MUSE_TRUE if everything bound successfully
 * and MUSE_FALSE if there was a failure. In case of failure, all bindings are reversed.
 */
static muse_boolean bind_letvars( muse_cell bindings )
{
	if ( bindings )
	{
		muse_cell binding = _head(bindings);
		int bsp = _bspos();
		muse_cell value = muse_eval(_head(_tail(binding)));
		if ( muse_bind_formals( _head(binding), value ) )
		{
			/* This binding succeeded. If everything else also succeeds,
			we keep this binding, otherwise we reverse it. */
						
			if ( bind_letvars( _tail(bindings) ) )
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
				muse_message(	L"Error in let expression!",
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
	if ( bind_letvars( bindings ) )
	{
		_spush(args);
		result = muse_do( args );
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
	muse_cell object = muse_evalnext(&args);
	muse_cell cases = args;
	int bsp = _bspos();
	
	MUSE_DIAGNOSTICS({
		muse_expect( L"case expression", L"v?!=", cases, MUSE_CONS_CELL, MUSE_NIL );
	});

	while ( cases )
	{
		muse_cell thiscase = _next(&cases);
		
		MUSE_DIAGNOSTICS({
			if ( !muse_expect( L"case expression", L"v!=", _tail(thiscase), MUSE_NIL ) )
				muse_message( L"Syntax error in case body",
							  L"%m\n\nis not a valid case body, which must have the form\n"
							  L"(<case-pattern> <body>...)",
							  thiscase );
		});

		if ( muse_bind_formals( _head(thiscase), object ) )
		{
			muse_cell result = muse_do( _tail(thiscase) );

			_unwind_bindings(bsp);
			return result;
		}
	}

	MUSE_DIAGNOSTICS({
		muse_message(	L"Error in case expression",
						L"The object\n\t%m\nfailed to match any case.",
						object 
						);
	});

	return MUSE_NIL;
}

