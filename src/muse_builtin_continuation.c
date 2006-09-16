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
#include <stdlib.h>
#include <setjmp.h>
#include <memory.h>

typedef struct _continuation_t
{
	muse_functional_object_t base;
	jmp_buf		state;
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
	muse_cell	this_cont;
	muse_cell	invoke_result;
} continuation_t;

static void continuation_init( void *p, muse_cell args )
{
}

static void mark_array( muse_cell *begin, muse_cell *end )
{
	while ( begin < end )
	{
		muse_mark( *begin++ );
	}
}

static void continuation_mark( void *p )
{
	continuation_t *c = (continuation_t*)p;
	
	mark_array( c->muse_stack_copy, c->muse_stack_copy + c->muse_stack_size );
	mark_array( c->bindings_stack_copy, c->bindings_stack_copy + c->bindings_stack_size );
	mark_array( c->bindings_copy, c->bindings_copy + c->bindings_size );
}

static void continuation_destroy( void *p )
{
	continuation_t *c = (continuation_t*)p;

	free( c->system_stack_copy );	
	free( c->muse_stack_copy );
	free( c->bindings_stack_copy );
	free( c->bindings_copy );
	
	{
		muse_functional_object_t base = c->base;
		memset( p, 0, sizeof(continuation_t) );
		c->base = base;
	}
}

static muse_cell *copy_current_bindings( int *size )
{
	muse_stack *s = _symstack();
	muse_cell *begin = s->bottom;
	muse_cell *end = s->bottom + s->size;
	muse_cell *copy = NULL;
	int i = 0;

	(*size) = _env()->num_symbols * 2;
	copy = (muse_cell*)malloc( sizeof(muse_cell) * (*size) );

	while ( begin < end )
	{
		muse_cell b = *begin++;

		while ( b )
		{
			muse_cell sym = _next(&b);
			copy[i++] = sym;
			copy[i++] = _symval(sym);
		}
	}

	return copy;
}


static void restore_bindings( muse_cell *bindings, int size )
{
	muse_cell *end = bindings + size;
	
	muse_assert( size >= 0 && (size % 2 == 0) );

	while ( bindings < end )
	{
		_def( bindings[0], bindings[1] );
		bindings += 2;
	}
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

static muse_cell capture_continuation( muse_cell cont )
{
	continuation_t *c = (continuation_t*)muse_functional_object_data(cont,'cont');

	muse_cell result = setjmp( c->state );
	
	if ( result == MUSE_NIL )
	{
		/* We're capturing the continuation. Save all state. */
		
		/* First determine if the stack grows up or down. */
		muse_boolean stack_grows_down = ((char *)_env()->stack_base > (char *)&cont) ? MUSE_TRUE : MUSE_FALSE;
		
		if ( stack_grows_down )
		{
			/* Save system state up to the c variable. Note that c's address is 
			less than result's, therefore result will also get saved. */
			c->system_stack_from = min3(&c, &result, &stack_grows_down);
			c->system_stack_size = (char*)_env()->stack_base - (char*)c->system_stack_from;
			c->system_stack_copy = malloc( c->system_stack_size );
			memcpy( c->system_stack_copy, c->system_stack_from, c->system_stack_size );
		}
		else
		{
			/* Save system state up to the result variable. Note that result's address is 
			greater than c's, therefore c will also get saved. */
			c->system_stack_from = _env()->stack_base;
			c->system_stack_size = (char*)max3(&c, &result, &stack_grows_down) - (char*)_env()->stack_base;
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
		memcpy( c->bindings_stack_copy, _env()->bindings_stack.bottom, sizeof(muse_cell) * c->bindings_stack_size );

		/* Save all bindings. */
		c->bindings_copy = copy_current_bindings( &c->bindings_size );

		c->this_cont = cont;
		
		/* A -ve return value indicates a capture return. */
		return -1;
	}
	else
	{
		/* result-1 is the continuation object that was invoked, with
		the invoke_result field set to the argument supplied to the invocation. */
		c = (continuation_t*)muse_functional_object_data(result-1,'cont');
		muse_assert( c && c->base.type_info->type_word == 'cont' );
		
		/* Restore the evaluation stack. */
		memcpy( _stack()->bottom + c->muse_stack_from, c->muse_stack_copy, sizeof(muse_cell) * c->muse_stack_size );
		_unwind( c->muse_stack_from + c->muse_stack_size );

		/* Restore the bindings stack. */
		memcpy( _env()->bindings_stack.bottom + c->bindings_stack_from, c->bindings_stack_copy, sizeof(muse_cell) * c->bindings_stack_size );
		_env()->bindings_stack.top = _env()->bindings_stack.bottom + c->bindings_stack_from + c->bindings_stack_size;

		/* Restore the saved symbol values. */
		restore_bindings( c->bindings_copy, c->bindings_size );

		/* Restore the system stack. */
		memcpy( c->system_stack_from, c->system_stack_copy, c->system_stack_size );

		muse_assert( c->invoke_result >= 0 );

		/* We return to fn_callcc after this. So to ensure we get into the
		"continuation invoked" branch, we have to make sure that the result
		value is non-zero. fn_callcc knows about this +1 and will decrement 
		the result and use it as a cell reference. */
		return c->invoke_result+1;
	}
}

static muse_cell fn_continuation( muse_env *env, continuation_t *c, muse_cell args )
{
	c->invoke_result = muse_evalnext(&args);
	
	longjmp( c->state, c->this_cont + 1 );
	
	return MUSE_NIL;
}

static muse_functional_object_type_t g_continuation_type =
{
	'muSE',
	'cont',
	sizeof(continuation_t),
	(muse_nativefn_t)fn_continuation,
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
	muse_cell proc = muse_evalnext(&args);
	
	muse_cell cont = muse_mk_functional_object( &g_continuation_type, MUSE_NIL );
	
	muse_cell result = capture_continuation(cont);
	
	if ( result < 0 )
	{
		/* We just captured the continuation. Invoke the proc with the 
		continuation as the argument. */
		
		return muse_apply( proc, muse_cons( cont, MUSE_NIL ), MUSE_TRUE );
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
