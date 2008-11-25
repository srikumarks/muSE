/**
 * @file _eval.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_opcodes.h"
#include <stdlib.h>

/**
 * Returns the given arguments quoted.
 * The behaviour of quote is that 
 * @code s = _eval( muse_quote(s) ) @endcode
 */
MUSEAPI muse_cell muse_quote( muse_env *env, muse_cell args )
{
	return _cons( env->builtin_symbols[MUSE_QUOTE], args );
}

/**
 * Evaluates the given symbolic expression.
 * 	- Numbers, strings, native functions and lambda
 * 		functions evaluate to themselves.
 * 	- Quoted expressions evaluate to their unquoted forms.
 * 	- A list is evaluated by evaluating the first
 * 		element, interpreting it as a function to be 
 * 		supplied the rest of the list as arguments. If
 * 		the first element is not a function, then the
 * 		list evaluates to itself.
 * 
 * The result of the evaluation is usually protected
 * by the stack if it is a newly allocated cell.
 */
MUSEAPI muse_cell muse_eval( muse_env *env, muse_cell sexpr, muse_boolean lazy )
{	
	/* A -ve cell number is used as an indicator that the 
	object being referred to is "quick-quoted". So in this
	case the evaluation of a quick quoted expression is
	just its -ve value as a cell. */
	if ( sexpr <= 0 )
		return _quq(sexpr);
	
	switch ( _cellt(sexpr) )
	{
		case MUSE_SYMBOL_CELL	:
			/* Symbol evaluation */
			return _symval(sexpr);
		case MUSE_CONS_CELL		:
			/* Function application */
			{
				muse_cell result = muse_apply( env, _eval(_head(sexpr)), _tail(sexpr), MUSE_FALSE, lazy );
				return lazy ? result : _force(result);
			}
		default					:
			/* Self evaluation. */
			return sexpr;
	}
}

/**
 * Evaluates the head of the list referred to by the
 * given sexpr pointer and steps the pointer to the
 * tail of the list, getting ready for the next call.
 * This is useful to evaluate arguments inside native 
 * functions.
 */
MUSEAPI muse_cell muse_evalnext( muse_env *env, muse_cell *sexpr )
{
	return _eval( _next(sexpr) );
}

/**
 * Evaluates each element of the given list,
 * builds a new list out of the results of the
 * evaluations and returns the list.
 */
MUSEAPI muse_cell muse_eval_list( muse_env *env, muse_cell list )
{
	muse_cell h = MUSE_NIL, t = MUSE_NIL, c = MUSE_NIL;
	int sp = -1;
	
	if ( _cellt(list) != MUSE_CONS_CELL )
		return _eval(list);

	if ( list )
	{
		h = t = _cons( MUSE_NIL, MUSE_NIL );
		sp = _spos();
		_seth( h, _eval( _next(&list) ) );
		_unwind(sp);
	}
	
	while ( list )
	{
		if ( _cellt(list) == MUSE_CONS_CELL )
			c = _cons( _eval( _next(&list) ), MUSE_NIL );
		else
		{	/* This isn't a proper list. Set the tail of last cons cell
			to the value of "list" itself and return. */
			c = _eval(list);
			list = MUSE_NIL;
		}

		_sett( t, c );
		t = c;
		_unwind(sp); /* We can unwind because we now hold a reference 
			to the head of the result list on the stack. */
	}
	
	return h;
}

/**
 * Calls the given C-native function with the given
 * sexpr as its argument list.
 */
muse_cell muse_apply_nativefn( muse_env *env, muse_cell fn, muse_cell args )
{
	register muse_nativefn_cell *f = &_ptr(fn)->fn;
	muse_assert( f->fn != NULL );
	return f->fn( env, f->context, args );
}

/**
 * Binds the symbols in the given formals list to
 * the corresponding values in the args list.
 * This recursive definition of muse_(un)bind_formals 
 * can perform arbitrary structure pattern matching between the
 * formals specification and the given arguments.
 * 
 * For ex: A formals specification of @code ( (a b) (c . d) ) @endcode
 * and an argument list with the structure @code ( (1 2) (3 4 5 6) ) @endcode
 * will cause the bindings -
 * 	- a = 1
 * 	- b = 2
 * 	- c = 3
 * 	- d = (4 5 6)
 *
 * You're allowed to use function values in patterns, which you can create
 * using the braces notation to evaluate code at read-time. When such a function
 * value is encountered, its arguments are first pattern tested against the 
 * value at the position of the function. If the pattern testing succeeds and the
 * body of the function evalutes to a non-NIL value, then the bindings are
 * left as is for the remainder of the evaluation context. If either the
 * argument pattern didn't match or the function body evaluated to nil, then
 * the bindings are reversed and the pattern match is taken to have failed.
 * These functions in patterns therefore serve as guards.
 *
 * For example, here is a contrived function of two arguments "a" and "b" which
 * returns "b-a" if b > a and fails otherwise -
 *
 * @code
 * (fn {fn (a b) (< a b)} (- b a))
 * @endcode
 * 
 * @return MUSE_TRUE if the match succeeded and MUSE_FALSE if
 * some element failed. In case of failure, no variables are bound.
 * 
 * @see syntax_lambda
 * @see syntax_case
 * @see syntax_let
 */
MUSEAPI muse_boolean muse_bind_formals( muse_env *env, muse_cell formals, muse_cell args )
{
	if ( formals == args ) 
		return MUSE_TRUE; /* Usually happens when formals == args == MUSE_NIL */
	
	else if ( formals == MUSE_NIL )
		return MUSE_FALSE;
	
	switch ( _cellt(formals) )
	{
		case MUSE_SYMBOL_CELL		:
			_pushdef( formals, args ); 
			return MUSE_TRUE;	/* Can match when args == MUSE_NIL as well. */
			
		case MUSE_CONS_CELL			:
			if ( _isquote(_head(formals)) )
			{
				/* If we're dealing with a quoted expression,
				we have to use eq to compare. */
				return muse_equal( env, _tail(formals), args );
			}

			if ( _cellt(args) == MUSE_LAZY_CELL )
				args = _force(args);

			if ( _cellt(args) != MUSE_CONS_CELL )
			{
				return MUSE_FALSE; /* We cannot match a cons cell against something that's not one. */
			}
			else if ( args )
			{
				/* Do not decompose a () into (().()) when matching
				 * more formals. It is OK to match a symbol to (),
				 * but it is not OK to match a patten like @code (x y z) @endcode
				 * to () simply because () can be infinitely decomposed
				 * as for example, @code (().(().(().()))) @endcode. */
				int bsp = _bspos();
				if ( muse_bind_formals( env, _head(formals), muse_head(env,args) ) )
				{
					if ( muse_bind_formals( env, _tail(formals), muse_tail(env,args) ) )
					{
						return MUSE_TRUE; /* Both head and tail matched. */
					}
					else
					{
						/* Head matched, but tail didn't. We have to unbind whatever
						we've bound so far and wind back up the call stack. This single
						unbind statement guarantees by induction that muse_bind_formals() 
						either succeeds fully or doesn't bind any variable. */
						_unwind_bindings(bsp);
						return MUSE_FALSE;
					}
				}
			}
		
			/* No matches. */
			return MUSE_FALSE;
		case MUSE_LAMBDA_CELL:
			/* In case there is a lambda in the pattern, it is used like a guard.
			That is, its argument pattern is first bound to the value and the body
			evaluated. If the body evaluates to a non-NIL value, then the argument
			pattern is left bound as is. Otherwise the binding is unwound. 
			
			See muse_apply_lambda for details. */
			{
				muse_cell fn = formals;
				muse_cell subformals = _quq(_head(fn));
				int bsp = _bspos();

				/* Bind all formal parameters. If binding failed, return MUSE_NIL. */
				if ( muse_bind_formals( env, subformals, args ) )
				{
					/*	Evaluate the body. If the body evaluates to a non-NIL
					value, then leave the bindings as is and continue. */
					int sp = _spos();
					muse_cell result = _force(_do( _tail(fn) ));
					_unwind(sp);
					if ( result )
					{
						/* Leave the bindings on the bindings stack as is. */
						return MUSE_TRUE;
					}
					else
					{
						/* Condition failed. Unbind the latest bindings. */
						_unwind_bindings(bsp);
						return MUSE_FALSE;
					}
				}
				else
				{
					/* Argument binding failed, which means the given thing
					doesn't match even the guard's pattern. So this binding is
					considered to have failed. Unwind any bindings that 
					the match might have left on the bindings stack. */
					_unwind_bindings(bsp);
					return MUSE_FALSE;
				}
			}
		default : return muse_equal( env, formals, args );
	}
}

muse_cell syntax_lambda( muse_env *env, void *context, muse_cell args );

/**
 * Applies the given function specification to the
 * given argument list and returns whatever the function
 * returns. The function *must* be a lambda function
 * and not a C-native function. The formal argument
 * list of the function is bound to the given
 * argument list and the body of the function is
 * invoked. The formals are unbound after the function
 * completes and the result of evaluating the body
 * is returned.
 * 
 * @see syntax_lambda
 */
muse_cell muse_apply_lambda( muse_env *env, muse_cell fn, muse_cell args )
{
	/* The formals list could be quick quoted to indicate that
	the arguments must not be evaluated - i.e. the function is
	a syntax transformer. */
	muse_cell formals = _quq(_head(fn));
	
	/* Keep a tab on the current state of the binding stack so 
	that we can revert to this point after we're done with the
	sub expressions. */
	int bsp = _bspos();

	muse_trace_push( env, NULL, fn, args );

	/* Bind all formal parameters. If binding failed, return MUSE_NIL. */
	if ( muse_bind_formals( env, formals, args ) )
	{
		/* Create a new scope for the "recent items" list so that
		the \ref fn_the "the" references created within th function
		don't affect the caller's context. */
		muse_push_recent_scope(env);

		/* Undefine the "it" symbol so that \ref fn_the "the"
		expressions within the function can affect "it" locally. */
		_pushdef( _builtin_symbol(MUSE_IT), _builtin_symbol(MUSE_IT) );

		{
			/*	Evaluate the body. 
				Only "result" will remain on the stack. */
			muse_cell result = _do( _tail(fn) );
		
			/* Restore the save bindings. */
			_unwind_bindings(bsp);
			
			muse_trace_pop(env);
			return muse_pop_recent_scope( env, fn, result );
		}
	}
	else
	{
		MUSE_DIAGNOSTICS({
			muse_message(	env, L"Function application",
							L"The given arguments\n\t%m\n"
							L"do not match the function's argument specification\n\t%m",
							args,
							formals );
		});

		muse_trace_pop(env);
		return MUSE_NIL;
	}
}

/**
 * Quick quoting a cell is used as an efficient means
 * to pass expressions that have already been evaluated.
 * The cell's reference is simply negated to indicate
 * that it has been "quick-quoted". If you call _eval()
 * on a quick-quoted objects, the object will be unquoted
 * by just negating its cell reference.
 */
static muse_cell quick_quote_list( muse_env *env, muse_cell list )
{
	muse_cell h = list;
	while ( h )
	{
		_seth( h, _qq(muse_head(env,h)) );
		h = muse_tail(env,h);
	}
	return list;
}

/**
 * After quick quoting a list when passing to a native function,
 * it must be unquoted before doing anything else.
 */
static muse_cell quick_unquote_list( muse_env *env, muse_cell list )
{
	muse_cell h = list;
	while ( h )
	{
		_seth( h, _quq(muse_head(env,h)) );
		h = muse_tail(env,h);
	}

	return list;
}

/**
 * Applies the given native function or lambda to the given
 * list of arguments and returns the result. The arguments
 * are not evaluated when passing them to a native function
 * but are evaluated when passing to a lambda. Not evaluating
 * the arguments for a native function lets us define syntaxes
 * in native code. If you're passing already evaluated
 * arguments in the args list, set the \p args_already_evaluated
 * parameter to MUSE_TRUE.
 * 
 * @see muse_apply_lambda
 */
MUSEAPI muse_cell muse_apply( muse_env *env, muse_cell fn, muse_cell args, muse_boolean args_already_evaluated, muse_boolean lazy )
{
	/* Check whether we've devoted enough attention to this process. */
	yield_process(env,1);

	{
		int sp = _spos();
		muse_cell result = fn;
		_spush(fn);
		_spush(args);
		
		switch ( _cellt(fn) )
		{
			case MUSE_NATIVEFN_CELL		:
				{
					if ( args_already_evaluated )
					{
						result = muse_apply_nativefn( env, fn, quick_quote_list(env, args) );
						quick_unquote_list(env, args);
					}
					else
					{
						result = muse_apply_nativefn( env, fn, args );
					}
				}
				break;
			case MUSE_LAMBDA_CELL		: 
				/* The head of fn gives us the list of formal parameters. If the formals
				list is -ve (i.e. quick-quoted) we must not evaluate the arguments and pass
				the arg list as it was given, unevaluated. Otherwise, we evaluate all the
				arguments in list order. 
				
				See also syntax_lambda and muse_apply_lambda implementation. */
				args = (args_already_evaluated || _head(fn) < 0) ? args : muse_eval_list(env, args);
				result = lazy ? _setcellt(_cons(fn,args), MUSE_LAZY_CELL)
							  : muse_apply_lambda( env, fn, args );
				break;
				
			default						:
				/*	If the first argument is not a function, simply return the sexpr. 
					In this case, apply behaves like list. */

				MUSE_DIAGNOSTICS2({ 
					switch ( _cellt(fn) )
					{
					case MUSE_INT_CELL :
					case MUSE_FLOAT_CELL :
						/* Allow lists beginning with integers and floats. They
						should be interpreted as lists silently and ther eis no ambiguity
						about the coder's intention here. This is quite useful in
						writing out integer n-tuples as lists without having to
						place a quote before the list. */
						break;
					case MUSE_SYMBOL_CELL :
						{
							int dist = 0;
							muse_cell sim = muse_similar_symbol( env, fn, &dist );
							const muse_char *csim = muse_symbol_name( env, sim );
							if ( dist >= 5 ) csim = L"...can't guess...";
							muse_message( env, L"apply", 
													L"You tried to use the undefined symbol [%m] as a function.\n"
													L"in the expression - \n\n%m\n\n"
													L"Maybe you meant [%s]?",
												fn,
												_cons( fn, args ),
													csim );
						}
						break;
					default:
						muse_message( env, L"apply", L"You tried to use the undefined symbol [%m] as a function\n"
												L"in the expression -\n\n%m\n\n"
												L"It will be considered as a list.",
												fn, _cons( fn, args ) );
					}
				});

				result = _cons( fn, args_already_evaluated ? args : muse_eval_list(env, args) );
				break;
		}
		
		_unwind(sp);
		_spush(result);
		return result;
	}
}

/**
 * A block of code is a list of expressions to evaluate
 * in sequence. The result of evaluating a block is the
 * result of evaluating the last expression in the block.
 */
MUSEAPI muse_cell muse_do( muse_env *env, muse_cell block )
{
	muse_cell result = MUSE_NIL;
	int sp = _spos();
	
	while ( block )
	{
		muse_cell term = _next(&block);

		_unwind(sp); /* Discard previous result on stack. */

		result = muse_eval( env, term, block ? MUSE_FALSE : MUSE_TRUE );
	}
	
	return result;
}


/**
 * Forces evaluation of a lazy cell.
 */
MUSEAPI muse_cell muse_force( muse_env *env, muse_cell cell )
{
	int sp = _spos();
	int N = 0;

	/* The cell > 0 condition means quick-quoted cells
	and MUSE_NIL are forced to themselves. */
	if ( cell > 0 && _cellt(cell) == MUSE_LAZY_CELL ) {

		muse_cell value = cell;

		muse_push_recent_scope(env);

		do {
			_unwind(sp);

			{
				muse_cell h = _head(cell);
				
				if ( h )
					/* When the head of a lazy cell is not MUSE_NIL, it should
					be a function that is applied to the tail of the cell. */
					cell = muse_apply( env, h, _tail(cell), MUSE_TRUE, MUSE_FALSE );
				else
					/* When the head of a lazy cell is MUSE_NIL, then
					the forced value of the cell is the value of the tail. */
					cell = muse_eval( env, _tail(cell), MUSE_FALSE );

			}

			++N;

		} while ( cell > 0 && _cellt(cell) == MUSE_LAZY_CELL );

		muse_pop_recent_scope( env, MUSE_NIL, MUSE_NIL );

		{
			recent_entry_t *e = muse_find_recent_lazy_item(env);
			if ( e ) {
				muse_assert( e->value == value );
				e->value = cell;
			} else {
				muse_add_recent_item( env, _head(value), cell );
			}
		}

	}

	return cell;
}
