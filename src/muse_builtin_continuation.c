/**
 * @file muse_builtin_continuation.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#include "muse_builtins.h"
#include "muse_opcodes.h"
#include "muse_port.h"
#include <stdlib.h>
#include <setjmp.h>
#include <memory.h>

typedef struct _continuation_t
{
	muse_functional_object_t base;
	jmp_buf		state;
	muse_process_frame_t *process;
	int			process_atomicity;
	size_t		system_stack_size;
	void		*system_stack_from;
	void		*system_stack_copy;
	int			muse_stack_size;
	int			muse_stack_from;
	muse_cell	*muse_stack_copy;
	int			bindings_stack_size;
	int			bindings_stack_from;
	muse_cell	*bindings_stack_copy;
	int			bindings_size;
	muse_cell	*bindings_copy;
	recent_t	recent;
	muse_cell	this_cont;
	muse_cell	invoke_result;
	int			num_eval_timeouts;
} continuation_t;

static void continuation_init( muse_env *env, void *p, muse_cell args )
{
}

static void mark_array( muse_env *env, muse_cell *begin, muse_cell *end )
{
	while ( begin < end )
	{
		muse_mark( env, *begin++ );
	}
}

static void continuation_mark( muse_env *env, void *p )
{
	continuation_t *c = (continuation_t*)p;
	
	muse_assert( c->process->state_bits != MUSE_PROCESS_DEAD );

	mark_array( env, c->muse_stack_copy, c->muse_stack_copy + c->muse_stack_size );
	mark_array( env, c->bindings_stack_copy, c->bindings_stack_copy + c->bindings_stack_size );
	mark_array( env, c->bindings_copy, c->bindings_copy + c->bindings_size );

	muse_mark_recent( env, &(c->recent) );
}

static void continuation_destroy( muse_env *env, void *p )
{
	continuation_t *c = (continuation_t*)p;

	free( c->system_stack_copy );	
	free( c->muse_stack_copy );
	free( c->bindings_stack_copy );
	free( c->bindings_copy );
	muse_clear_recent( &(c->recent) );
	
	{
		muse_functional_object_t base = c->base;
		memset( p, 0, sizeof(continuation_t) );
		c->base = base;
	}
}

static muse_cell *copy_current_bindings( muse_env *env, int *size )
{
	muse_stack *s = &(env->current_process->locals);
	muse_cell *copy = NULL;
	int i = 0;

	(*size) = env->num_symbols;
	copy = (muse_cell*)malloc( sizeof(muse_cell) * (*size) );
	memcpy( copy, s->bottom, sizeof(muse_cell) * (*size) );

	return copy;
}


static void restore_bindings( muse_env *env, muse_cell *bindings, int size )
{
	muse_cell *end = bindings + size;
	
	muse_assert( size >= 0 );

	memcpy( env->current_process->locals.bottom, bindings, sizeof(muse_cell) * size );
}

static void *min3( void *p1, void *p2, void *p3 )
{
	char *c1 = (char*)p1, *c2 = (char*)p2, *c3 = (char*)p3;
	char *c = c1;
	
	if ( c2 < c ) c = c2;
	if ( c3 < c ) c = c3;
	return c;
}

static void *max3( void *p1, void *p2, void *p3 )
{
	char *c1 = (char*)p1, *c2 = (char*)p2, *c3 = (char*)p3;
	char *c = c1;
	
	if ( c2 > c ) c = c2;
	if ( c3 > c ) c = c3;
	return c;	
}

static muse_cell capture_continuation( muse_env *env, muse_cell cont )
{
	continuation_t *c = (continuation_t*)_functional_object_data(cont,'cont');

	muse_cell result = setjmp( c->state );
	
	if ( result == MUSE_NIL )
	{
		/* We're capturing the continuation. Save all state. */
		
		/* First determine if the stack grows up or down. */
		muse_boolean stack_grows_down = ((char *)env->current_process->cstack.top > (char *)&cont) ? MUSE_TRUE : MUSE_FALSE;
		
		if ( stack_grows_down )
		{
			/* Save system state up to the c variable. Note that c's address is 
			less than result's, therefore result will also get saved. */
			SAVE_STACK_POINTER( saved_sp );
			
			c->system_stack_from = saved_sp;
			c->system_stack_size = (char*)env->current_process->cstack.top - (char*)c->system_stack_from;
			c->system_stack_copy = malloc( c->system_stack_size );
			muse_assert( is_main_process(env) || c->system_stack_size < env->current_process->cstack.size * sizeof(muse_cell) );

			memcpy( c->system_stack_copy, c->system_stack_from, c->system_stack_size );
		}
		else
		{
			muse_assert( MUSE_FALSE && "Unsupported stack growth direction!" );

			/* Save system state up to the result variable. Note that result's address is 
			greater than c's, therefore c will also get saved. */
			c->system_stack_from = env->current_process->cstack.top;
			c->system_stack_size = (char*)max3(&c, &result, &stack_grows_down) - (char*)env->stack_base;
			c->system_stack_copy = malloc( c->system_stack_size );
			memcpy( c->system_stack_copy, c->system_stack_from, c->system_stack_size );
		}

		/* Save the muse stack. */
		c->muse_stack_from = 0;
		c->muse_stack_size = _spos();
		c->muse_stack_copy = malloc( sizeof(muse_cell) * c->muse_stack_size );
		memcpy( c->muse_stack_copy, _stack()->bottom, sizeof(muse_cell) * c->muse_stack_size );
		
		/* Save the bindings stack. */
		c->bindings_stack_from = 0;
		c->bindings_stack_size = _bspos();
		c->bindings_stack_copy = malloc( sizeof(muse_cell) * c->bindings_stack_size );
		memcpy( c->bindings_stack_copy, env->current_process->bindings_stack.bottom, sizeof(muse_cell) * c->bindings_stack_size );

		/* Save all bindings. */
		c->bindings_copy = copy_current_bindings( env, &c->bindings_size );

		/* Save recent items. */
		c->recent = muse_copy_recent( &(env->current_process->recent) );

		/* Save a pointer to the current process. */
		c->process = env->current_process;
		c->process_atomicity = env->current_process->atomicity;
		c->num_eval_timeouts = env->current_process->num_eval_timeouts;

		c->this_cont = cont;
		
		/* A -ve return value indicates a capture return. */
		return -1;
	}
	else
	{
		/* A pointer to the continuation is passed in "result". 
		The invoke_result field is set to the argument supplied to the invocation. */
		/* TODO: Passing a pointer in "result" is not 64-bit clean, because
		result is 32-bit and the pointer can be 64-bit. But until longjmp interface
		has 64-bit int, this simply has to be worked around. */
		c = (continuation_t*)(size_t)result;

		/* Restore the system stack. The function parameters become valid after the
		stack is restored and we can refer to "env" and others. */
		memcpy( c->system_stack_from, c->system_stack_copy, c->system_stack_size );
		muse_assert( c && c->base.type_info->type_word == 'cont' );
		
		/* Restore the process atomicity that was at capture time. 
		Also, continuation invocations cannot cross process boundaries, so
		the current process must be the one in which the continuation
		was captured. */
		muse_assert( env->current_process == c->process );
		c->process->atomicity = c->process_atomicity;
		c->process->num_eval_timeouts = c->num_eval_timeouts;

		/* Restore the evaluation stack. */
		memcpy( _stack()->bottom + c->muse_stack_from, c->muse_stack_copy, sizeof(muse_cell) * c->muse_stack_size );
		_unwind( c->muse_stack_from + c->muse_stack_size );

		/* Restore the bindings stack. */
		memcpy( c->process->bindings_stack.bottom + c->bindings_stack_from, c->bindings_stack_copy, sizeof(muse_cell) * c->bindings_stack_size );
		c->process->bindings_stack.top = c->process->bindings_stack.bottom + c->bindings_stack_from + c->bindings_stack_size;

		/* Restore the saved symbol values. */
		restore_bindings( env, c->bindings_copy, c->bindings_size );

		/* Restore the recent values. */
		muse_restore_recent( &(c->recent), &(c->process->recent) );

		muse_assert( c->invoke_result >= 0 );
		muse_assert( (c->process->state_bits & MUSE_PROCESS_DEAD) == 0 );

		/* We return to fn_callcc after this. So to ensure we get into the
		"continuation invoked" branch, we have to make sure that the result
		value is non-zero. fn_callcc knows about this +1 and will decrement 
		the result and use it as a cell reference. */
		return c->invoke_result+1;
	}
}

static muse_cell fn_continuation( muse_env *env, continuation_t *c, muse_cell args )
{
	/* Continuation invocation cannot cross process boundaries. */
	muse_assert( c->process == env->current_process );

	c->invoke_result = _evalnext(&args);
	
	longjmp( c->state, (int)(size_t)c );

	return MUSE_NIL;
}

static muse_functional_object_type_t g_continuation_type =
{
	'muSE',
	'cont',
	sizeof(continuation_t),
	(muse_nativefn_t)fn_continuation,
	NULL,
	continuation_init,
	continuation_mark,
	continuation_destroy
};

/**
 * (call/cc (fn (k) --- (k result) ---)).
 *
 * Abbreviation for "call with current continuation", call/cc is an
 * implementation of the scheme recommendation for continuation support.
 * The first and only argument to call/cc is expected to be a function
 * that takes a single argument called the continuation. call/cc will
 * then call this function, supplying the current continuation as the
 * argument.
 *
 * A brief intro to continuations follows. When evaluating 
 * a sub-expression of any expression, the remainder of the computation may
 * be thought of as a function that expects the result of the sub-expression
 * evaluation. This "remainder of the computation" function w.r.t. the
 * specific sub-expression is called the "continuation" at that point
 * in the evaluation process. 
 *
 * The whole expression may be rewritten as a call to this continuation
 * function with the argument as the result of the sub-expression under
 * consideration. Note that the continuation function does
 * not return a value to the context in which it is called. Instead,
 * it "breaks out" of the context and pretends as though the result
 * of the sub-expression is the argument supplied to the 
 * continuation function at invocation time. 
 *
 * Its time for an example - what'll the following code print?
 * .. and then, what'll it print when \c bomb is defined to \c T instead?
 * @code
 * (define bomb ())
 * (print (+ 1 2 (call/cc (fn (k)
 *						     (print "before\n")
 *						     (if bomb (k 0))
 *						     (print "after\n")
 *							 3))
 *		     4 5))
 * @endcode
 *
 * When \c bomb is \c (), the @code (k 0) @endcode part is not
 * evaluated due to the if condition failing. Therefore the output 
 * will be -
 * @code
 * before
 * after
 * 15
 * @endcode
 *
 * When \c bomb is changed to \c T, the if block will kick in
 * and @code (k 3) @endcode will be evaluated. But since continuation
 * functions do not return to te point of invocation, but to the point
 * of the \c call/cc which captured them, the @code (print "after\n") @endcode
 * expression never gets evaluated and the result of the \c call/cc block
 * is not \c 3 as one would expect, but \c 0, because that's the argument
 * given to the continuation function when it is invoked! So you'll get
 * @code
 * before
 * 12
 * @endcode
 * as the result printed to the screen. Note that we know that the
 * continuation invocation did not return to its invocation point because
 * the expression @code (print "after\n") @endcode did not get evaluated.
 *
 * Continuations are rather powerful. The can be used to implement 
 * language constructs such as -
 *	-# try-catch style exception handling,
 *	-# breaking out of loops.
 *	-# suspend and resume mechanism.
 *
 * In general, it should be possible to store away the continuation
 * function for future invocation. Early on in muSE's development, only
 * a limited implementation of call/cc was put in in order to support
 * breaking out of loops. Later on a full implementation of continuations
 * was added. Now, you can store away the continuation function in a variable
 * and invoke it as many times as you need to, because the continuation
 * captures a complete snapshot of the execution environment at the time
 * it is created. 
 *
 * @todo The current implementation of call/cc seems to be working properly 
 * on Windows + Intel, but doesn't work correctly on PowerPC. 
 * Needs investigation.
 */
muse_cell fn_callcc( muse_env *env, void *context, muse_cell args )
{
	muse_cell proc = _evalnext(&args);
	
	muse_cell cont = _mk_functional_object( &g_continuation_type, MUSE_NIL );
	
	muse_cell result = capture_continuation(env,cont);
	
	if ( result < 0 )
	{
		/* We just captured the continuation. Invoke the proc with the 
		continuation as the argument. */
		
		return _apply( proc, _cons( cont, MUSE_NIL ), MUSE_TRUE );
	}
	else
	{
		/* The continuation was just invoked. Return the result
		without evaluating the proc. The actual result is not "result",
		but "result-1" because the fn_continuation function makes sure
		to invoke this branch by giving a result that is > 0. This is
		does by adding 1 to the actual result. */
		return result-1;
	}
}

/**
 * @addtogroup Exceptions
 */
/*@{*/
/**
 * @defgroup Exceptions_impl Internals
 *
 * muSE implements an exception mechanism that allows you to normally
 * resume computation from the point where an exception is raised.
 * An expression that might \ref fn_raise "raise" an exception is wrapped by a 
 * \ref syntax_try "try" expression and a list of handlers to try.
 *
 * When a \c try expression is evaluated, the handlers are all evaluated
 * first and placed on a stack of handlers to try when an exception is
 * raised. (The handler stack is maintained as the value of the internal symbol
 * \c "{{trap}}".) The \c try protected expression is then evaluated. When evaluation is
 * complete, the handler stack is unwound and the set of handlers defined by the
 * enclosing \ref syntax_try "try" block take effect.
 *
 * If you want to try an alternative code path instead of raising a resumable
 * exception, use \ref fn_retry "retry" instead of \ref fn_raise "raise".
 * Alternative code paths are to be considered part of the "interface" of 
 * a function and must be documented along with the function.
 */
/*@{*/

/**
 * Captures all the information required to jump
 * back to an execution point within stack scope.
 * Its essentially a cheaper continuation that cannot
 * be retained beyond its marked scope.
 */
typedef struct _resume_point_t
{
	jmp_buf state;		/**< Captures the point of execution to return to. */
	int spos;			/**< Captures the state of the muSE stack. */
	int bspos;			/**< Captures the state of the muSE bindings stack. */
	int atomicity;		/**< Captures the atomicity to return to. */
	muse_cell trapval;	/**< Captures the state of the trap stack that we should restore to. */
	muse_cell resumingtrap; /**< The trap one of whose handlers resumed the exception. */
	muse_cell result;	/**< Holds the result of the resume invocation. */
	recent_t recent;		/**< The top index of the recent list at capture time. */
	int num_eval_timeouts;	/**< The depth of the timeout stack when the capture is made. */
} resume_point_t;

/**
 * @code
 * Usage: if ( resume_capture( env, rp, setjmp(rp->state) ) == 0 )
 *            ...
 *        else
 *            ...
 *            return rp->result;
 * @endcode
 */
static int resume_capture( muse_env *env, resume_point_t *rp, int setjmp_result )
{
	if ( setjmp_result == 0 )
	{
		rp->spos = _spos();
		rp->bspos = _bspos();
		rp->atomicity = env->current_process->atomicity;
		rp->trapval = _symval( _builtin_symbol( MUSE_TRAP_POINT ) );
		rp->resumingtrap = MUSE_NIL;
		rp->result = 0;
		rp->recent = env->current_process->recent;
		rp->num_eval_timeouts = env->current_process->num_eval_timeouts;
	}
	else
	{
		env->current_process->num_eval_timeouts = rp->num_eval_timeouts;
		env->current_process->atomicity = rp->atomicity;
		_unwind( rp->spos );
		_unwind_bindings( rp->bspos );
		_define( _builtin_symbol( MUSE_TRAP_POINT ), rp->trapval );
		rp->result = (setjmp_result >= 0) ? (setjmp_result-1) : setjmp_result;
		env->current_process->recent = rp->recent;
	}

	return setjmp_result;
}

/**
 * Invokes an already captured resume point with the given result.
 * The longjmp call is made with result+1 and rp->result will
 * be set to the longjmp return value minus 1.
 */
static void resume_invoke( muse_env *env, resume_point_t *p, muse_cell result )
{
	longjmp( p->state, (result >= 0) ? (result+1) : result );
}

/**
 * A trap point is a marker for the beginning of a
 * (try...) block. When you return to a trap point,
 * you return with a value that is supposed to be the
 * value of the try block. Trap points are maintained as
 * a stack of values of the built-in symbol "{{trap}}",
 * which is available via MUSE_TRAP_POINT.
 */
typedef struct _trap_point_t
{
	muse_functional_object_t base;
	resume_point_t escape;	/**< The resume point to invoke to return from the try block. */
	muse_cell handlers;		/**< A list of unevaluated handlers. */
	muse_cell prev;			/**< The previous shallower trap point. */
	muse_cell next;			/**< The next deeper trap point. */
	muse_cell tried_handlers; /**< The list of handlers already tried. 
									 Used to prevent re-entry into the same handler
									 that might result in an infinite loop and stack blow up. */
	muse_cell finalizers;  /**< Often, you want some resource cleanup operation to
								happen when the try block exits. For this purpose,
								we keep a "finalizers" list to which you can add
								thunks by invoking (finally (fn () ...) ...).
								Finalizers are evaluated in the reverse order in which
								they are created. */
} trap_point_t;

#define _tpdata(trap) trap_point_data(env,trap)
static trap_point_t *trap_point_data( muse_env *env, muse_cell trap )
{
	return (trap_point_t*)_functional_object_data(trap, 'trap');
}

/**
* The function that gets called to resume a particular exception.
 * At exception raise time, a resume point is captured and passed
 * on to the handlers. A handler may choose to resume the computation
 * by calling the resume function with a particular result value.
 */
static muse_cell fn_resume( muse_env *env, void *context, muse_cell args )
{
	if ( !muse_doing_gc(env) )
	{
		resume_point_t *rp = (resume_point_t*)context;
		
		if ( rp->resumingtrap ) {
			trap_point_t *tp = _tpdata(rp->resumingtrap);
			if ( tp ) {
				_step(&(tp->tried_handlers));
			}
		}
		
		resume_invoke( env, rp, _evalnext(&args) );
	}
	
	return MUSE_NIL;
}

static void trap_point_init( muse_env *env, void *p, muse_cell args )
{
	trap_point_t *trap = (trap_point_t*)p;

	/* We're evaluating the list of handlers here. This is
	fairly expensive to simply enter a try block. We either accept
	this overhead or accept the overhead of capturing a full 
	continuation at the point at which the exception is raised
	in order to get resumable exceptions. */
	trap->handlers = muse_eval_list(env, args);

	trap->prev = _symval( _builtin_symbol( MUSE_TRAP_POINT ) );

	{
		trap_point_t *trap_prev = _tpdata(trap->prev);
		trap->tried_handlers = trap_prev ? trap_prev->tried_handlers : MUSE_NIL;
	}
}

static void trap_point_mark( muse_env *env, void *p )
{
	trap_point_t *trap = (trap_point_t*)p;
	muse_mark( env, trap->handlers );
	muse_mark( env, trap->tried_handlers );
	muse_mark( env, trap->prev );
	muse_mark( env, trap->next );
	muse_mark( env, trap->finalizers );
}

/**
 * Starts from the deepest trap point, runs its finalizers,
 * then all the way up to the try block that is exiting.
 */
static void trap_point_finalize( muse_env *env, trap_point_t *trap )
{
	/* Deepest first. */
	if ( trap->next )
		trap_point_finalize( env, _tpdata(trap->next) );
		
	/* Disconnect the finished trap point. */
	trap->next = MUSE_NIL;
	
	while ( trap->finalizers )
	{
		muse_cell f = trap->finalizers;
		muse_cell thunk = _next(&(trap->finalizers));
		_apply(thunk,MUSE_NIL,MUSE_TRUE);
		_returncell(f); /* Cell used by runtime and not accessible to system. */
	}
}

static muse_cell fn_trap_point( muse_env *env, void *trap_point, muse_cell args )
{
	muse_assert( !"fn_trap_point should never be called!" );
	return MUSE_NIL;
}

static muse_functional_object_type_t g_trap_point_type =
{
	'muSE',
	'trap',
	sizeof(trap_point_t),
	fn_trap_point,
	NULL,
	trap_point_init,
	trap_point_mark,
	NULL
};

/**
 * Examines the handlers of the given scope first, then followed by
 * the handlers of the enclosing try scope, and so on until a successful
 * handler was invoked or execution reached the top level without a handler.
 * In the latter case, the process is terminated with an "unhandled exception"
 * error.
 */
static muse_cell try_handlers( muse_env *env, muse_cell handler_args )
{
	muse_cell sym_trap_point = _builtin_symbol( MUSE_TRAP_POINT );

	muse_cell trapval = _symval( sym_trap_point );

	trap_point_t *trap = _tpdata(trapval);
	
	/* Note that the value of the "{{trap}}" symbol, which holds the
	list of trap points, is not modified in the loop below. This has the
	consequence that if a handler itself raises an exception, the exception
	pattern is searched for again starting from the handlers in the 
	inner-most try block from which the original exception was raised. 
	Using the "tried_handlers" list, we prevent the exception raising handler
	from calling itself again, but every other handler gets a second chance.
	This facility gives us Common-Lisp like exception handling and
	recovery without the syntactic clutter. */

	if ( trap )
	{
		muse_cell handlers = trap->handlers;

		do
		{
			/* Ignore all the handlers that are being tried now. */
			while ( handlers && muse_find_list_element( env, &(trap->tried_handlers), _head(handlers) ) )
				_step(&handlers);

			if ( handlers )
			{
				muse_cell h = _next(&handlers);

				if ( _cellt(h) == MUSE_LAMBDA_CELL )
				{
					/* A handler needs to be examined. */
					muse_cell formals = _quq(_head(h));
					int bsp = _bspos();

					if ( muse_bind_formals( env, formals, handler_args ) )
					{
						muse_cell result;
						trap->tried_handlers = _cons(h,trap->tried_handlers);
						
						/* If the first value in the handlers list is a resume point,
							then save the trap that might invoke the resume point in it. */
						if ( handler_args && _cellt(handler_args) == MUSE_CONS_CELL ) 
						{
							muse_cell rpc = _head(handler_args);
							if ( _cellt(rpc) == MUSE_NATIVEFN_CELL && _ptr(rpc)->fn.fn == fn_resume ) {
								resume_point_t *rp = (resume_point_t*)_ptr(rpc)->fn.context;
								rp->resumingtrap = trapval;
							}
						}
						
						result = _force(_do(_tail(h)));

						/* The cons cell taken for the handlers list is a 
						scaffolding cell taken by the runtime and not accessible
						to the running code. So we can immediately return it to
						the free list. This will save some unnecessary garbage 
						build up.*/ 
						_returncell( _step( &(trap->tried_handlers) ) );

						_unwind_bindings(bsp);
						resume_invoke( env, &(trap->escape), result );
					}
				}
				else
				{
					/* The "handler" itself is the result of the try block. */
					resume_invoke( env, &(trap->escape), h );
				}
			}

			/* Switch to handlers of shallower scopes if necessary. */
			while ( !handlers )
			{
				trapval = trap->prev;
				trap = _tpdata(trapval);

				if ( trap == NULL )
				{
					handlers = MUSE_NIL;
					break;
				}

				/* When we are switching to handlers of shallower traps,
				it shouldn't matter if a handler has already been tried.
				We should try all shallower handlers again. */
				trap->tried_handlers = MUSE_NIL;
				handlers = trap->handlers;
			}
		}
		while (handlers);
	}

	/* No handler succeeded in handling the exception. */
	{
		muse_cell sym_deh = muse_builtin_symbol( env, MUSE_DEFAULT_EXCEPTION_HANDLER );
		muse_cell deh = _symval(sym_deh);
		if ( sym_deh == deh || _isfn(deh) == MUSE_FALSE )
		{
			/* No default exception handler defined. */
			muse_message( env,L"Unhandled exception!", L"%m\n\nin process %m", _tail(handler_args), process_id(env->current_process) );
		}
		else
		{
			/* Invoke the default exception handler. */
			muse_apply( env, deh, handler_args, MUSE_TRUE, MUSE_FALSE );
		}
	
		remove_process( env->current_process );
	}

	/* Issue#30 - Build warning about not all control paths returning value.
	   This function doesn't return after the above remove_process() call. */
	return MUSE_NIL;
}
/*@}*/
/*@}*/

/**
 * Marks an expression that needs to be protected by
 * exception handlers. A try block has the following syntax -
 * @code
 *  (try
 *       expr
 *       handler1
 *       handler2
 *       ....
 *  )
 * @endcode
 *
 * First it tries to evaluate the given \c expr. If the expression
 * raised an exception using (raise...), then each of the handlers
 * is tried in turn until one matches. The handlers are evaluated
 * at the time the try block is entered, not when an exception is 
 * raised, so for efficiency reasons you should use
 * in-place handlers (using the macro brace facility) which do not
 * refer to the lexical context of the try block if possible and
 * use closures for handlers only when you absolutely need them.
 * If your handlers are predetermined before entering the try block
 * and you only need to evaluate a symbol to get at the handlers,
 * then entering a try block is efficient.
 * 
 * A handler can be a function expression - like (fn args expr) or
 * (fn: args expr). If it is such an expression, each handler is
 * tried in turn until the argument pattern of one of the handlers
 * matches the raised exception. The body of the handler whose
 * arguments matched the exception is evaluated and the result
 * is returned as the result of the try block. 
 *
 * The first argument to the handler is an exception object whose
 * sole purpose is to enable the handler to resume the computation
 * with a given value as the result of the (raise...) expression that
 * raised the exception. The rest of the arguments to a handler
 * are the same arguments that were passed to \c raise.
 *
 * Handlers may themselves raise exceptions. The raised exceptions
 * start \b again from the inner most set of handlers and
 * try to find another handler to match it. This gives you the
 * facility to decide exception handling policies at a level higher
 * than the level that implements the policy. Note that if you raise 
 * the same exception pattern captured by the handler, you'll
 * \b not end up in the same handler again in an infinite loop and blow the
 * stack. The handler invocation is protected from such re-entry.
 * This lets you delegate the exception to an even higher level.
 *
 * If a handler is not a function object, then its value is
 * used as the value of the try block without further checks.
 * Such a "handler" always succeeds.
 *
 * If none of the handlers match, then the handlers of
 * the previous enclosing try block are examined.
 *
 * A handler may choose to resume the computation by invoking
 * the exception object with the desired value that should be
 * returned by the (raise ...) expression.
 *
 * If continuations are captured in the middle of try blocks,
 * they will automatically include the correct state of the
 * try block nesting because they will capture the correct
 * value of the "{{trap}}" symbol.
 *
 * It is convenient to use read-time evaluated dynamically
 * scoped function objects as handlers since they cause the
 * least overhead and are usually sufficiently general.
 * For example -
 * @code
 * (try 
 *   (do (write "Difference = "
 *         (if (< a b)
 * 	           (- b a)
 *             (raise 'NotInOrder a b)))
 *       (write "Product = " (* a b)))
 *       
 *   {fn: (ex 'NotInOrder x y)
 * 	      (ex (- x y))})
 * @endcode
 */
muse_cell syntax_try( muse_env *env, void *context, muse_cell args ) 
{
	muse_cell trapval = _mk_functional_object( &g_trap_point_type, _tail(args) );

	trap_point_t *tp = _tpdata(trapval);

	muse_cell result = MUSE_NIL;

	/* The next pointer isn't linked yet. Do that. */
	{
		trap_point_t *tpprev = _tpdata(tp->prev);
		if ( tpprev )
			tpprev->next = trapval;
	}

	_define( _builtin_symbol( MUSE_TRAP_POINT ), trapval );

	if ( resume_capture( env, &(tp->escape), setjmp(tp->escape.state) ) == 0 )
	{
		/* Evaluate the body of the try block. */
		result = _evalnext(&args);
	}
	else
	{
		if ( tp->escape.result < 0 )
		{
			/* Something invoked "retry". Try all handlers with the given
			argument list - which is given by -ve of the result code. */
			try_handlers( env, _quq(tp->escape.result) );
		}
		else
		{
			/* Exception raised and a handler was invoked which 
			gave us this value. Return from the try block with the
			given result. */
			result = tp->escape.result;
		}
	}

	/* We need to invoke the finalizers of all the trap points
	that led us up to this point, starting from the deepest one. */
	trap_point_finalize( env, tp );
	
	tp->tried_handlers = MUSE_NIL;
	_define( _builtin_symbol( MUSE_TRAP_POINT ), tp->prev );
	return result;
}

static muse_cell raise_error( muse_env *env, muse_cell args )
{
	resume_point_t *rp = (resume_point_t*)calloc( 1, sizeof(resume_point_t) );
	muse_cell resume_pt = _mk_destructor( fn_resume, rp );
	muse_cell handler_args = _cons( resume_pt, args );

	if ( resume_capture( env, rp, setjmp(rp->state) ) == 0 )
	{
		return try_handlers( env, handler_args );
	}
	else
	{
		return rp->result;
	}
}

/**
 * Raises an error which can be resumed just like an exception raised
 * using the @ref fn_raise "raise" operator. 
 *
 * The handler pattern that will match the raised error is given
 * by - @code (ex error . info) @endcode
 *	- ex - The resume continuation.
 *	- error - A symbol identifying the error.
 *	- info - Arbitrary info to be passed on to handlers.
 */
MUSEAPI muse_cell muse_raise_error( muse_env *env, muse_cell error, muse_cell info ) 
{
	return raise_error( env, _cons( error, info ) );
}

/**
 * (raise [exception-details])
 *
 * Raises an exception described by the given arguments. Handlers are
 * matched against the pattern of arguments to determine which handler
 * to use to handle the exception. It is useful to use a quoted symbol
 * as the first argument which describes the exception. A handler can
 * then specify the same quoted symbol as its second argument in order
 * to get to handle the exception.
 *
 * (raise..) evaluates the matching handler without unwinding
 * to the enclosing try block. This means any raised exception can be
 * resumed by invoking the exception object (passed as the first argument
 * to the handler) with the resume value as the argument.
 *
 * @see syntax_try
 */ 
muse_cell fn_raise( muse_env *env, void *context, muse_cell args ) 
{
	return raise_error( env, muse_eval_list(env,args) );
}

/**
 * Similar to \ref fn_raise "raise" but does not prepend a resume
 * continuation to the trial condition. This is useful to provide
 * alternative inner code paths that can be invoked from 
 * an outer level. The argument pattern of an alternative should
 * not expect a resume continuation as the first argument.
 *
 * While a "raise" expression may evaluate to some value, a "retry" 
 * exception never completes evaluation, so it is not meaningful to 
 * use its result value for anything.
 */
muse_cell fn_retry( muse_env *env, void *context, muse_cell args )
{
	muse_cell trapval = _symval( _builtin_symbol( MUSE_TRAP_POINT ) );
	muse_cell handler_args = muse_eval_list( env, args );
	trap_point_t *trap = _tpdata(trapval);

	if ( trap )
	{
		resume_invoke( env, &(trap->escape), _qq(handler_args) );
	}
	else
	{
		muse_message( env, L"(retry ...)", L"No alternatives to try!\nEnclose in (try ...) block and provide alternatives." );
		remove_process( env->current_process );
	}

	return MUSE_NIL; /* Never returns! */
}

/**
 * (finally ...block...)
 *
 * Installs a finalizer for the current trap state.
 * A finally block's body will be captured in a closure
 * and installed as a finalizer thunk. This thunk will be
 * invoked at the point when the immediately enclosing
 * \ref syntax_try "try" block completes - normally or
 * abnormally. \c finally thus provides a cleanup mechanism
 * for resources that might otherwise be left lingering
 * around.
 *
 * Note that \c finally is not a clause <em>outside</em>
 * the expression protected by a \c try, but actually
 * occurs and is evaluated within the protected code
 * block. Therefore it can occur not merely within the
 * syntactic scope of a try expression, but actually
 * anywhere (and any number of times) within the 
 * execution scope of the try expression - including
 * any functions called within the expression. 
 */
muse_cell syntax_finally( muse_env *env, void *context, muse_cell args )
{
	/* Get the current trap point. */
	muse_cell trapval = _symval( _builtin_symbol( MUSE_TRAP_POINT ) );
	trap_point_t *trap = _tpdata(trapval);
	
	if ( trap )
	{
		muse_cell finalizer = _force(_eval( syntax_lambda(env,NULL,_cons(MUSE_NIL,args)) ));
		trap->finalizers = _cons( finalizer, trap->finalizers );	
		return finalizer;
	}
	else
	{
		MUSE_DIAGNOSTICS2({ muse_message( env, L"(finally ...)", L"No enclosing (try ...) block!" ); });
		return MUSE_NIL;
	}
}

/**
 * Displays a list of handlers you can try.
 */
int print_handler_choices( muse_env *env, trap_point_t *tp, muse_port_t p, int nchoices )
{
	muse_cell handlers = tp->handlers;
	while ( handlers )
	{
		muse_cell h = _next(&handlers);
		muse_cell tried = tp->tried_handlers;
		muse_cell *triedp = muse_find_list_element(env, &tried, h);
		if ( triedp == NULL )
		{
			char buf[8];
			int len = sprintf(buf,"\n%d:\t", ++nchoices);
			port_write( buf, len, p );
			if ( _cellt(h) == MUSE_LAMBDA_CELL )
				muse_pwrite( p, _head(h) );
			else
				muse_pwrite( p, h );
		}
	}
	
	if ( tp->next )
		return print_handler_choices( env, _tpdata(tp->next), p, nchoices );
	else
		return nchoices;
}

/**
 * Accepts any exception and asks the user to make a choice about what to do
 * by dropping into a nested REPL.
 */
muse_cell fn_top_level_handler( muse_env *env, void *context, muse_cell args )
{
	char msg[256];
	int len;

	muse_port_t mstderr = muse_stdport( env, MUSE_STDERR_PORT );
	muse_port_t mstdin = muse_stdport( env, MUSE_STDIN_PORT );
	
	muse_cell exinfo = _evalnext(&args);
	
	muse_assert( exinfo != MUSE_NIL && "Impossible for exceptions to be ()!" );
	
	{
		len = sprintf( msg, ">>>>>>>>>\nUnhandled exception: " );
		port_write( msg, len, mstderr );
		muse_pwrite(mstderr,exinfo);
		len = sprintf( msg, "\nOptions:" );
		port_write( msg, len, mstderr );
	}
	
	{
		muse_cell trap = _symval( _builtin_symbol( MUSE_TRAP_POINT ) );
		trap_point_t *tp = _tpdata(trap);
		
		int choices = print_handler_choices( env, tp, mstderr, 0 );
		
		if ( choices == 0 )
		{
			len = sprintf( msg, "  <<none>>" );
			port_write( msg, len, mstderr );
		}
	}
	
	len = sprintf( msg, "\nException info available in symbol '_'.\n" );
	port_write( msg, len, mstderr );
	port_write( "ex> ", 4, mstderr );
	port_flush( mstderr );
	
	{
		muse_cell expr = muse_pread(mstdin);
		len = sprintf( msg, "<<<<<<<<<<<\n" );
		port_write( msg, len, mstderr );
		port_flush( mstderr );
		
		return _force(_eval(expr));
	}
}

/**
 * Wraps an apply in a try block with the default top-level handler.
 */
muse_cell try_apply( muse_env *env, muse_cell fn, muse_cell args )
{
	muse_port_t p = muse_stdport(env,MUSE_STDERR_PORT);
	muse_cell wrapped_expr = muse_list( env, "Sc(SS(cS))", 
										L"try",
										_cons( fn, args ),
										L"fn",
										L"_",
										_mk_nativefn(fn_top_level_handler,NULL),
										L"_"
										);
	port_putc('\n',p);
	return _force(_eval(wrapped_expr));
}