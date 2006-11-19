/**
 * @file muse_builtins.c
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
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

static const struct _builtins 
	{
		const muse_char *name;
		muse_nativefn_t fn;
	} k_builtins[] =
{
{		L"quote",		fn_quote			},
{		L"cons",		fn_cons				},
{		L"eval",		fn_eval				},
{		L"fn",			syntax_lambda		},
{		L"fn:",			syntax_block		},
{		L"let",			syntax_let			},
{		L"apply",		fn_apply			},
{		L"call/cc",		fn_callcc			},

/************** Property list and alist functions ***************/
{		L"get",			fn_get				},
{		L"put",			fn_put				},
{		L"assoc",		fn_assoc			},
{		L"plist",		fn_plist			},
{		L"symbol",		fn_symbol			},
{		L"name",		fn_name				},
	
/************** Cell and symbol manipulation ***************/
{		L"define",		fn_define			},
{		L"set!",		fn_set_M			},
{		L"setf!",		fn_setf_M			},
{		L"setr!",		fn_setr_M			},
{		L"first",		fn_first			},
{		L"rest",		fn_rest				},
{		L"next",		fn_next				},
{		L"nth",			fn_nth				},
{		L"take",		fn_take				},
{		L"drop",		fn_drop				},
{		L"dup",			fn_dup				},
{		L"list",		fn_list				},
{		L"length",		fn_length			},
{		L"append!",		fn_append_M			},

/************** Higher Order Functions ***************/
{		L"size",			fn_length			},
{		L"map",			fn_map				},
{		L"join",			fn_join				},
{		L"collect",		fn_collect			},
{		L"reduce",		fn_reduce			},
{		L"find",		fn_find				},
{		L"andmap",		fn_andmap			},
{		L"ormap",		fn_ormap			},
{		L"for-each",	fn_for_each			},
{		L"transpose",	fn_transpose		},

/************** Math ***************/
{		L"+",			fn_add				},
{		L"-",			fn_sub				},
{		L"*",			fn_mul				},
{		L"/",			fn_div				},
{		L"%",			fn_mod				},
{		L"i/",			fn_idiv				},
{		L"++",			fn_inc				},
{		L"--",			fn_dec				},
{		L"trunc",		fn_trunc			},
{		L"rand",		fn_rand				},
{		L"pow",			fn_pow				},
	
/************** Comparisons ***************/
{		L"eq?",			fn_eq				},
{		L"=",			fn_equal			},
{		L"!=",			fn_ne				},
{		L"<",			fn_lt				},
{		L">",			fn_gt				},
{		L"<=",			fn_le				},
{		L">=",			fn_ge				},
{		L"and",			fn_and				},
{		L"or",			fn_or				},
{		L"not",			fn_not				},
	
/************** Constructs ***************/
{		L"if",			syntax_if			},
{		L"cond",		syntax_cond			},
{		L"do",			syntax_do			},
{		L"while",		syntax_while		},
{		L"for",			syntax_for			},
{		L"case",		syntax_case			},
{		L"stats",		fn_stats			},
	
/************** Type checks ***************/
{		L"int?",		fn_int_p			},
{		L"float?",		fn_float_p			},
{		L"cons?",		fn_cons_p			},
{		L"fn?",			fn_fn_p				},
{		L"symbol?",		fn_symbol_p			},
{		L"text?",		fn_string_p			},
	
/************** Algorithms ***************/
{		L"sort!",		fn_sort_inplace		},
{		L"sort",		fn_sort				},
	
/************** Classes ***************/
{		L"class",		fn_class			},
{		L"new",			fn_new				},
{		L"->",			fn_obj_pty			},
{		L"send",		fn_send				},
{		L"send-super",	fn_send_super		},
{		L"<-",			fn_send				},
{		L"<<-",			fn_send_super		},
	
/************** Ports ***************/
{		L"port?",		fn_port_p			},
{		L"close",		fn_close			},
{		L"eof?",		fn_eof_p			},
{		L"print",		fn_print			},
{		L"write",		fn_write			},
{		L"read",		fn_read				},
{		L"flush",		fn_flush			},
{		L"load",		fn_load				},
{		L"write-xml",	fn_write_xml		},
{		L"exit",		fn_exit				},

/************** Ports ***************/
{		L"spawn",		fn_spawn			},
{		L"atomic",		fn_atomic			},
{		L"receive",		fn_receive			},
{		L"run",			fn_run				},

/************** Miscellaneous **************/
{		L"format",					fn_format					},
{		L"string-length",			fn_string_length			},
{		L"time-taken-us",			fn_time_taken_us			},
{		L"generate-documentation",	fn_generate_documentation	},
{		L"load-plugin",				fn_load_plugin				},
{		L"list-files",				fn_list_files				},
{		L"list-folders",			fn_list_folders				},
	
{		NULL,			NULL				}
};

void muse_load_builtin_fns()
{
	const struct _builtins *b = k_builtins;
	int sp = _spos();
	
	while ( b->name )
	{
		muse_define( muse_csymbol(b->name), muse_mk_nativefn( b->fn, NULL ) );
		_unwind(sp);
		
		++b;
	}
	
	muse_math_load_common_unary_functions();
	muse_define_builtin_type_vector();
	muse_define_builtin_type_hashtable();
	muse_define_builtin_fileport();
	muse_define_builtin_networking();
}

/**
 * Quotes the given arguments without evaluating them. For example,
 * @code (quote . hello) @endcode
 * is identical to @code 'hello @endcode
 */
muse_cell fn_quote( muse_env *env, void *context, muse_cell args )
{
	return args;
}

/**
 * (cons head tail).
 * Creates a new cons cell with the given head and tail.
 * If no free cells are available, invokes the garbage collector
 * and grows the heap if necessary.
 * @see muse_cons()
 */
muse_cell fn_cons( muse_env *env, void *context, muse_cell args )
{
	muse_cell h, t;
	
	h = muse_evalnext(&args);
	t = muse_evalnext(&args);
	return muse_cons( h, t );
}

/**
 * (eval s-expr).
 * Evaluates the given single s-expression and returns the result.
 * For example, @code (eval '(+ 2 3)) @endcode will result in
 * \c 5. In this sense, \c eval is the counter part of \c quote.
 */
muse_cell fn_eval( muse_env *env, void *context, muse_cell args )
{
	return muse_eval( muse_evalnext(&args) );
}

/**
 * (if cond-expr then-expr [else-expr]).
 * Evaluate the \c cond-expr first. If the \c cond-expr evaluates
 * to something that's not \c (), the \c if expression evaluates
 * to the result of the \c then-expr. If an \c else-expr is
 * supplied and the \c cond-expr evaluated to \c (), the result
 * will be the evaluation of the \c else-expr. If no \c else-expr
 * is supplied and the condition failed to test, then \c () is the result.
 * 
 * For example - 
 * @code
 * (if (= 1 2)
 *     (print "muSE doesn't know numbers!")
 *     (print "muSE knows numbers, alright"))
 * @endcode
 */
muse_cell syntax_if( muse_env *env, void *context, muse_cell args )
{
	muse_cell expr = muse_evalnext(&args);
	
	if ( expr )
		return muse_evalnext(&args); /* then */
	
	args = _tail(args); /* Skip then portion. */
	
	if ( args )
		return muse_evalnext(&args); /* else */
	
	return MUSE_NIL;
}

/**
 * \c cond is the generalization of if-then-else nested blocks.
 * @code
 * (cond
 *	(test1 result1)
 *	(test2 result2)
 *	...
 *	(T else))
 * @endcode
 * The above form will first check whether \c test1 is satisfied. If it is,
 * then the result will be the evaluation of \c result1. If not, then
 * \c test2 is checked, and so on.
 * For example -
 * @code
 * (cond
 * 	((< a b) (print "a < b"))
 * 	((< a c) (print "a < c, but not b"))
 * 	(T (print "a is >= b and c")))
 * @endcode
 */
muse_cell syntax_cond( muse_env *env, void *context, muse_cell args )
{
	int sp = _spos();
	
	while ( args )
	{
		muse_cell clause = _head(args);
		
		if ( muse_eval( _head(clause) ) )
		{
			_unwind(sp);
			return muse_do( _tail(clause) );
		}
		
		args = _tail(args);
	}
	
	_unwind(sp);
	return MUSE_NIL;
}

/**
 * (do <expressions>).
 * Defines an expression block. The result of the do block is the
 * result of the last expression in the block, after evaluating all
 * the expressions in the block in sequence.
 */
muse_cell syntax_do( muse_env *env, void *context, muse_cell args )
{
	return muse_do( args );
}

/**
 * (while bool-expr <body>).
 * Repeatedly evaluates the body statements as long as
 * the \c bool-expr expression evaluates to something non-nil.
 */
muse_cell syntax_while( muse_env *env, void *context, muse_cell args )
{
	muse_cell bool_expr		= _next(&args);
	muse_cell body			= args;
	muse_cell result		= MUSE_NIL;
	int sp = _spos();
	
	while ( muse_eval(bool_expr) )
	{
		_unwind(sp);
		result = muse_do(body);
	}
	
	_unwind(sp);
	_spush(result);
	return result;
}

/**
 * (for init-expr cond-expr step-expr body [result-expr]).
 * For-loop must be obvious to all ye C-fans.
 * ex:
 * @code
		(for (set! i (dup 0)) (< i 1000) (++ i)
		 (do (set! j (+ j (* i i)))
			 (print j))
		 j)
	@endcode
 */
muse_cell syntax_for( muse_env *env, void *context, muse_cell args )
{
	muse_cell init_expr		= _next(&args);
	muse_cell cond_expr		= _next(&args);
	muse_cell update_expr	= _next(&args);
	muse_cell body			= _next(&args);
	muse_cell result_expr	= MUSE_NIL;
	muse_cell result		= MUSE_NIL;
	muse_boolean result_expr_given = MUSE_FALSE;
	int sp = _spos();

	/* A result expression after the loop body is optional. */
	if ( args )
	{
		result_expr_given = MUSE_TRUE;
		result_expr = _next(&args);
	}

	/* Perform the initialization. */
	muse_eval( init_expr );
	
	/* Evaluate the body as long as the condition holds. */
	while ( muse_eval(cond_expr) )
	{
		_unwind(sp);
		result = muse_eval(body);
		muse_eval(update_expr);
	}
	
	/* If result expression is given, use it. Otherwise
		use the the last returned value of the body. */
	if ( result_expr_given )
		result = muse_eval(result_expr);
	
	_unwind(sp);
	_spush(result);
	return result;
}

/**
 * (stats).
 *
 * Evaluates to a triplet @code (free-cells stack-size num-symbols) @endcode.
 */
muse_cell fn_stats( muse_env *env, void *context, muse_cell args )
{
	int free_cell_count		= _env()->heap.free_cell_count;
	int stack_size			= (int)(_stack()->top - _stack()->bottom);
	return muse_cons( 
					 muse_mk_int(free_cell_count),
					 muse_cons(
							   muse_mk_int( stack_size ),
							   muse_cons(
										 muse_mk_int( _env()->num_symbols ),
										 MUSE_NIL)));
}

/************************ Type checks ***********************/

/**
 * (int? x).
 * Evaluates to x if x is an integer cell and to () if its not.
 */
muse_cell fn_int_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = muse_evalnext(&args);
	
	return _cellt(arg) == MUSE_INT_CELL ? arg : MUSE_NIL;
}

/**
 * (float? x).
 * Evaluates to x if x is a float cell and to () if it is not.
 */
muse_cell fn_float_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = muse_evalnext(&args);
	
	return _cellt(arg) == MUSE_FLOAT_CELL ? arg : MUSE_NIL;
}

/**
 * (number? x).
 * Evaluates to x if x is either an integer or a float cell. 
 * Otherwise evaluates to ().
 */
muse_cell fn_number_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = muse_evalnext(&args);
	
	return (_cellt(arg) == MUSE_INT_CELL || _cellt(arg) == MUSE_FLOAT_CELL) ? arg : MUSE_NIL;
}

/**
 * (cons? x).
 * Evaluates to x if x is a cons cell - i.e. a portion of a list
 * or a pair created by \c cons. If not, it evaluates to ().
 * 
 * @see muse_cons
 * @see fn_cons
 */
muse_cell fn_cons_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = muse_evalnext(&args);
	
	return _cellt(arg) == MUSE_CONS_CELL ? arg : MUSE_NIL;
}

/**
 * (fn? x).
 * Evaluates to x if x is a lambda function or a c-native function.
 * Evaluates to () if it is not.
 * @see syntax_lambda
 * @see muse_mk_nativefn()
 */
muse_cell fn_fn_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = muse_evalnext(&args);
	
	return _isfn(arg) ? arg : MUSE_NIL;
}

/**
 * (symbol? x).
 * Evaluates to x if x is a symbol (named or anonymous) and evaluates
 * to () if it isn't.
 */
muse_cell fn_symbol_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = muse_evalnext(&args);
	
	return _cellt(arg) == MUSE_SYMBOL_CELL ? arg : MUSE_NIL;
}

/**
 * (string? x).
 * Evaluates to x if x is a text cell and to () if it isn't.
 */
muse_cell fn_string_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = muse_evalnext(&args);
	
	return _cellt(arg) == MUSE_TEXT_CELL ? arg : MUSE_NIL;
}

/**
 * (time-taken-us <block>).
 * Returns the time taken to execute the block,
 * in microseconds.
 */
muse_cell fn_time_taken_us( muse_env *env, void *context, muse_cell args )
{
	void *timing = muse_tick();
	muse_do( args );
	return muse_mk_int( muse_tock(timing) );
}

/**
 * (exit).
 * Exits the process.
 */
muse_cell fn_exit( muse_env *env, void *context, muse_cell args )
{
	exit(0);
	return MUSE_NIL;
}

static void gendoc_for_symbol( muse_cell symbol, muse_port_t p )
{
	muse_cell plist = muse_symbol_plist(symbol);
	muse_cell pty = MUSE_NIL;

	if ( plist )
	{
		char buffer[1024];
		int size;
		MUSE_DIAGNOSTICS3({ fprintf( stderr, "symbol: %S\n", muse_symbol_name(symbol) ); });

		/* Generate documentation only if there is a plist for the symbol. */
		size = sprintf(buffer, "/**\n@defgroup g%d %S\n", symbol, muse_symbol_name(symbol) );
		port_write( buffer, size, p );
		
		pty = muse_assoc( plist, muse_builtin_symbol(MUSE_SIGNATURE) );
		if ( pty )
		{
			size = sprintf(buffer, "@section Signature\n@code\n");
			port_write( buffer, size, p );
			muse_pwrite( p, muse_tail(pty) );
			size = sprintf( buffer, "\n@endcode\n\n" );
			port_write( buffer, size, p );
		}

		pty = muse_assoc( plist, muse_builtin_symbol(MUSE_USAGE) );
		if ( pty )
		{
			size = sprintf( buffer, "@section Usage\n@code" );
			port_write( buffer, size, p );
			muse_pprint( p, muse_head(muse_tail(pty)) );
			size = sprintf( buffer, "\n@endcode\n\n" );
			port_write( buffer, size, p );
		}

		pty = muse_assoc( plist, muse_builtin_symbol(MUSE_DESCR) );
		if ( pty )
		{
			muse_cell text = muse_tail(pty);

			size = sprintf( buffer, "@section Description\n" );
			port_write( buffer, size, p );
			while ( text )
			{
				muse_pprint( p, muse_head(text) );
				port_putc( '\n', p );
				text = muse_tail(text);
			}
		}

		pty = muse_assoc( plist, muse_builtin_symbol(MUSE_CODE) );
		if ( pty )
		{
			size = sprintf( buffer, "@section Code\n<pre>\n" );
			port_write( buffer, size, p );
			muse_pwrite(p, muse_tail(pty));
			size = sprintf( buffer, "\n</pre>\n\n");
			port_write( buffer, size, p );
		}

		size = sprintf( buffer, "\n */\n\n" );
		port_write( buffer, size, p );
	}
}

/**
 * (generate-documentation output-file-name).
 */
muse_cell fn_generate_documentation( muse_env *env, void *context, muse_cell args )
{
	/* Walk the symbols stack and generate documentation for
	each symbol with a property list. */

	wchar_t buffer[4096];
	int size;
	muse_cell filename;
	FILE *f;
	muse_port_t p;

	filename = muse_evalnext(&args);

	swprintf( buffer, 4096, L"%S.txt", muse_text_contents( filename, NULL ) );

	f = muse_fopen(buffer,L"wb");
	if ( !f )
		return MUSE_NIL;
	p = muse_assign_port(f, MUSE_PORT_WRITE);

	/* The symbol table is a hash table where each bucket has a list
	of symbols. */
	size = sprintf( (char*)buffer, "/** @defgroup %S */\n/*@{*/\n", muse_text_contents( filename, NULL ) );
	port_write( buffer, size, p );
	{
		muse_cell *syms = _env()->symbol_stack.bottom;
		muse_cell *syms_end = syms + _env()->symbol_stack.size;

		while ( syms < syms_end )
		{
			muse_cell s = *syms++;
			while ( s )
			{
				gendoc_for_symbol(_next(&s), p);
			}
		}
	}
	size = sprintf( (char*)buffer, "\n/*@}*/\n" );
	port_write( buffer, size, p );

	port_close(p);
	muse_unassign_port(p);
	return _t();
}

/**
 * (format --args-- ).
 * Converts each arg to a string, concatenates all the strings
 * and returns the result as a single string.
 */
muse_cell fn_format( muse_env *env, void *context, muse_cell args )
{
	int sp = _spos();
	muse_cell values = muse_eval_list(args);

	int total_length = 0;
	muse_cell result = MUSE_NIL;

	/* Replace each value with its string representation. */
	{
		muse_cell v = values;
		while ( v )
		{
			char buffer[128];
			muse_cell c = _head(v);
			muse_cell replacement = MUSE_NIL;

			switch ( _cellt(c) )
			{
			case MUSE_INT_CELL : 
				sprintf( buffer, MUSE_FMT_INT, muse_int_value(c) );
				replacement = muse_mk_ctext_utf8(buffer);
				break;
			case MUSE_FLOAT_CELL :
				sprintf( buffer, MUSE_FMT_FLOAT, muse_float_value(c) );
				replacement = muse_mk_ctext_utf8(buffer);
				break;
			case MUSE_TEXT_CELL :
				replacement = c;
				break;
			case MUSE_SYMBOL_CELL :
				replacement = _tail(_head(_tail(c)));
				break;
			default:;
			}

			_seth( v, replacement );

			if ( replacement )
			{
				int length = 0;
				muse_text_contents( replacement, &length );
				total_length += length;
			}

			v = _tail(v);
		}
	}

	/* Create the result buffer. */
	result = muse_mk_text( 0, ((muse_char*)0) + total_length );

	/* Copy the string pieces into the result buffer. */
	{
		muse_char *buffer = (muse_char*)muse_text_contents( result, NULL );

		muse_cell v = values;

		while ( v )
		{
			muse_cell c = _next(&v);

			if ( c )
			{
				int length = 0;
				const muse_char *text = muse_text_contents( c, &length );
				memcpy( buffer, text, sizeof(muse_char) * length );
				buffer += length;
			}
		}
	}

	_unwind(sp);
	_spush(result);
	return result;
}

/**
 * (string-length s).
 * Returns the number of characters in the string or () if s is not a string.
 */
muse_cell fn_string_length( muse_env *env, void *context, muse_cell args )
{
	muse_cell s = muse_evalnext(&args);
	if ( s && _cellt(s) == MUSE_TEXT_CELL )
	{
		muse_text_cell t = _ptr(s)->text;
		return muse_mk_int( t.end - t.start );
	}
	else
	{
		return MUSE_NIL;
	}
}

/**
 * (load-plugin dll-filename).
 * Loads and dynamically links the given plugin. 
 * Returns the result of invoking the plugin's entry point function.
 */
muse_cell fn_load_plugin( muse_env *env, void *context, muse_cell args )
{
	muse_cell filename = muse_evalnext(&args);
	return muse_link_plugin( muse_text_contents( filename, NULL ), args );
}

/**
 * (spawn (fn (pid . _) ...) [attention]) -> pid
 * Spawns a new process which will evaluate the given thunk until
 * it returns T.
 */
muse_cell fn_spawn( muse_env *env, void *context, muse_cell args )
{
	muse_cell thunk = muse_evalnext(&args);
	int attention = args ? (int)muse_int_value( muse_evalnext(&args) ) : env->parameters[MUSE_DEFAULT_ATTENTION];

	muse_process_frame_t *p = init_process_mailbox( create_process( env, attention, thunk, NULL ) );
	prime_process( env, p );
	return process_id(p);
}

/**
 * (atomic ...)
 * Behaves like do, but executes the entire body atomically,
 * without switching to another process.
 */
muse_cell fn_atomic( muse_env *env, void *context, muse_cell args )
{
	muse_cell result = MUSE_NIL;
	enter_atomic();
	result = muse_do(args);
	leave_atomic();
	return result;
}

/**
 * (receive)
 * (receive timeout_us)
 * 
 * Waits for and returns the next message
 */
muse_cell fn_receive( muse_env *env, void *context, muse_cell args )
{
	muse_int timeout_us = args ? muse_int_value( muse_evalnext(&args) ) : -1;

	muse_process_frame_t *p = env->current_process;

	muse_cell msgs = muse_tail( p->mailbox );

	if ( !msgs )
	{
		/* Wait for timeout value if specified. */
		p->state_bits = MUSE_PROCESS_WAITING;

		if ( timeout_us > 0 )
		{
			p->state_bits |= MUSE_PROCESS_HAS_TIMEOUT;
			p->timeout_us = muse_elapsed_us(env->timer) + timeout_us;
		}

		switch_to_process( env, p->next );
	}

	/* Check for message again. If there's still no message, return with MUSE_NIL. 
	An actual message will never be MUSE_NIL because it will contain the PID of the
	sending process at the head. */
	msgs = muse_tail( p->mailbox );

	if ( msgs )
	{
		/* Yes! We've received a message. Remove it from the queue and return it. */
		muse_set_tail( p->mailbox, muse_tail( msgs ) );

		/* After removing, check if we've reached the end of the message queue. */
		if ( msgs == p->mailbox_end )
			p->mailbox_end = p->mailbox;

		return muse_head( msgs );
	}
	else
		return MUSE_NIL;
}


/**
 * (run)
 * (run duration-us)
 *
 * The first version never returns and keeps running all processes.
 * The second version runs for the given duration (in microseconds).
 */
muse_cell fn_run( muse_env *env, void *context, muse_cell args )
{
	muse_int timeout_us = args ? muse_int_value( muse_evalnext(&args) ) : -1;
	muse_int endtime_us = timeout_us + muse_elapsed_us(env->timer);

	do
	{
		switch_to_process( env, env->current_process->next );
	}
	while ( timeout_us < 0 || muse_elapsed_us(env->timer) < endtime_us );

	return MUSE_NIL;
}