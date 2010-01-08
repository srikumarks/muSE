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
#if MUSE_PLATFORM_WINDOWS
#include <windows.h>
#endif

static const struct _builtins 
	{
		const muse_char *name;
		muse_nativefn_t fn;
	} k_builtins[] =
{
{		L"quote",		fn_quote			},
{		L"lazy",		fn_lazy				},
{		L"cons",		fn_cons				},
{		L"lcons",		fn_lcons			},
{		L"eval",		fn_eval				},
{		L"fn",			syntax_lambda		},
{		L"lambda",		syntax_lambda		},
{		L"fn:",			syntax_block		},
{		L"gfn",			syntax_generic_lambda	},
{		L"gfn:",		syntax_generic_block	},
{		L"let",			syntax_let			},
{		L"apply",		fn_apply			},
{		L"apply/keywords",	fn_apply_w_keywords	},
{		L"call/keywords",	fn_call_w_keywords	},
{		L"the",			fn_the				},
{		L"meta",		fn_meta				},
{		L"trace",		fn_trace			},
{		L"with-recent",	fn_with_recent		},
{		L"symbol-whose-value-is",	fn_symbol_whose_value_is	},

/************** Continuations and exception mechanism ***************/
{		L"call/cc",		fn_callcc			},
{		L"try",			syntax_try				},
{		L"raise",		fn_raise			},
{		L"retry",		fn_retry			},
{		L"finally",		syntax_finally		},

/************** Property list and alist functions ***************/
{		L"get",			fn_get				},
{		L"prim:put",	fn_put				},
{		L"put*",		fn_put_many			},
{		L"assoc",		fn_assoc			},
{		L"plist",		fn_plist			},
{		L"symbol",		fn_symbol			},
{		L"name",		fn_name				},
{		L"gensym",		fn_gensym			},
	
/************** Cell and symbol manipulation ***************/
{		L"define",		fn_define			},
{		L":=",			fn_define			},
{		L"define-extension",	fn_define_extension		},
{		L"define-override",		fn_define_override		},
{		L"set!",		fn_set_M			},
{		L"setf!",		fn_setf_M			},
{		L"setr!",		fn_setr_M			},
{		L"first",		fn_first			},
{		L"rest",		fn_rest				},
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
{		L"datafn",		fn_datafn			},

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
{		L"min",			fn_min				},
{		L"max",			fn_max				},
	
/************** Constructs ***************/
{		L"if",			syntax_if			},
{       L"when",        syntax_when         },
{       L"unless",      syntax_unless       },
{		L"cond",		syntax_cond			},
{		L"do",			syntax_do			},
{		L"while",		syntax_while		},
{		L"for",			syntax_for			},
{		L"case",		syntax_case			},
{		L"stats",		fn_stats			},
	
/************** Type checks ***************/
{		L"int?",		fn_int_p			},
{		L"float?",		fn_float_p			},
{		L"number?",		fn_number_p			},
{		L"cons?",		fn_cons_p			},
{		L"fn?",			fn_fn_p				},
{		L"lambda?",		fn_lambda_p				},
{		L"symbol?",		fn_symbol_p			},
{		L"text?",		fn_string_p			},
{		L"whatis",		fn_whatis			},
	
/************** Type conversions ***************/
{		L"int",			fn_int				},
{		L"float",		fn_float			},
{		L"number",		fn_number			},

/************** Algorithms ***************/
{		L"sort!",		fn_sort_inplace		},
{		L"sort",		fn_sort				},
{		L"reverse",		fn_reverse			},
{		L"reverse!",	fn_reverse_inplace	},
	
/************** Classes ***************/
{		L"new",			fn_new				},
{		L"object",		fn_new				},
{		L"supers",		fn_supers			},
{		L"super-invoke",	fn_super_invoke	},
{		L"super-invoke*",	fn_super_invoke_explicit	},
{		L"isa?",		fn_isa_p			},
{		L"object?",		fn_object_p			},
	
/************** Ports ***************/
{		L"port?",		fn_port_p			},
{		L"close",		fn_close			},
{		L"eof?",		fn_eof_p			},
{		L"print",		fn_print			},
{		L"write",		fn_write			},
{		L"read",		fn_read				},
{		L"read-line",	fn_read_line		},
{		L"flush",		fn_flush			},
{		L"mickey",		fn_mickey			},
{		L"load",		fn_load				},
{		L"write-xml",	fn_write_xml		},
{		L"read-xml",	fn_read_xml			},
{		L"xml",			fn_xml				},
{		L"write-json",	fn_write_json		},
{		L"read-json",	fn_read_json		},
{		L"json",		fn_json				},
{		L"exit",		fn_exit				},
{		L"tab-syntax",		fn_tab_syntax		},
{		L"scheme-syntax",	fn_scheme_syntax	},

/************** Ports ***************/
{		L"spawn",		fn_spawn			},
{		L"this-process", fn_this_process	},
{		L"atomic",		syntax_atomic		},
{		L"receive",		fn_receive			},
{		L"run",			fn_run				},
{		L"post",		fn_post				},
{		L"process?",	fn_process_p		},
{		L"with-timeout-us",	fn_with_timeout_us	},

/************** Miscellaneous **************/
{		L"format",					fn_format					},
{		L"string-length",			fn_string_length			},
{		L"substring",				fn_substring				},
{		L"time-taken-us",			fn_time_taken_us			},
{		L"generate-documentation",	fn_generate_documentation	},
{		L"load-plugin",				fn_load_plugin				},
{		L"list-files",				fn_list_files				},
{		L"list-folders",			fn_list_folders				},
{		L"split",					fn_split					},
{		L"system",					fn_system					},
{		L"alert",					fn_alert					},
{		L"urlencode",				fn_urlencode				},
{		L"urldecode",				fn_urldecode				},
{		L"launch",					fn_launch					},
{		L"windows-registry-entry",	fn_windows_registry_entry	},
{		L"temp-folder",				fn_temp_folder				},
{		L"temp-file",				fn_temp_file				},
{		L"to-lower",				fn_to_lower					},
{		L"to-upper",				fn_to_upper					},
	
{		NULL,			NULL				}
};

void muse_define_builtin_memport(muse_env *env);
void muse_define_image_properties( muse_env *env );
void muse_define_crypto( muse_env *env );

void muse_load_builtin_fns(muse_env *env)
{
	const struct _builtins *b = k_builtins;
	int sp = _spos();
	
	while ( b->name )
	{
		_define( _csymbol(b->name), _mk_nativefn( b->fn, NULL ) );
		_unwind(sp);
		
		++b;
	}
		
	muse_define_put_macro(env);
	muse_define_builtin_local(env);
	muse_math_load_common_unary_functions(env);
	muse_define_builtin_type_vector(env);
	muse_define_builtin_type_hashtable(env);
	muse_define_builtin_type_bytes(env);
	muse_define_builtin_type_module(env);
	muse_define_builtin_type_box(env);
	muse_define_builtin_fileport(env);
	muse_define_builtin_memport(env);
	muse_define_builtin_networking(env);
	muse_register_com_support(env);
	muse_define_image_properties(env);
	muse_define_xml_codes(env);
	muse_define_crypto(env);
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
 * @see _cons()
 */
muse_cell fn_cons( muse_env *env, void *context, muse_cell args )
{
	muse_cell h, t;
	
	h = _evalnext(&args);
	t = _evalnext(&args);
	return _cons( h, t );
}

/**
 * @code (eval s-expr) @endcode
 * Evaluates the given single s-expression and returns the result.
 * For example, @code (eval '(+ 2 3)) @endcode will result in
 * \c 5. In this sense, \c eval is the counter part of \c quote.
 */
muse_cell fn_eval( muse_env *env, void *context, muse_cell args )
{
	return muse_eval( env, _evalnext(&args), MUSE_TRUE );
}

/**
 * @code (if cond-expr then-expr else-expr) @endcode
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
	MUSE_DIAGNOSTICS({
		if ( !args )
			muse_message( env, L"(if >>cond<< then else)", L"Missing condition in 'if' construct.\n%m", args );
		if ( !_tail(args) )
			muse_message( env, L"(if cond >>then<< else)", L"Missing 'then' part of 'if' construct.\n%m", args );
		if ( !_tail(_tail(args)) )
			muse_message( env, L"(if cond then >>else<<)", L"Missing 'else' part of 'if' construct.\n%m", args );
	});

	{
		muse_cell expr = _evalnext(&args);
		muse_push_copy_recent_scope(env);
		{
			int bp = _bspos();
			_push_binding(_builtin_symbol(MUSE_IT));
			{
				muse_cell result = muse_eval( env, _head( expr ? args : _tail(args) ), MUSE_TRUE );
				_unwind_bindings(bp);
				muse_pop_recent_scope( env, (muse_int)syntax_if, result );
				return result;
			}
		}
	}
}

muse_cell guarded_do( muse_env *env, muse_cell expr )
{
	muse_push_copy_recent_scope(env);
	{
		int bp = _bspos();
		_push_binding(_builtin_symbol(MUSE_IT));
		{
			muse_cell result = _do(expr);
			_unwind_bindings(bp);
			muse_pop_recent_scope( env, 0, MUSE_NIL );
			return result;
		}
	}
}

/**
 * @code (when cond ...body...) @endcode
 *
 * Evaluates \p cond first. If condition evaluated to non-NIL value, then
 * the body is evaluated just like \p do and the value of the last expression
 * is the value of the \c when construct. Otherwise the value is \c ().
 */
muse_cell syntax_when( muse_env *env, void *context, muse_cell args )
{
	muse_cell expr = _evalnext(&args);
	if ( expr ) {
		return guarded_do( env, args );
	} else
		return MUSE_NIL;
}

/**
 * @code (unless cond ...body...) @endcode
 *
 * Evaluates \p cond first. If condition evaluated to \c (), then the body 
 * is evaluated just like \c do and  the value of the last expression is
 * the value of the \c unless construct. Otherwise the value is \c ().
 */
muse_cell syntax_unless( muse_env *env, void *context, muse_cell args )
{
	muse_cell expr = _evalnext(&args);
	if ( !expr ) {
		return guarded_do( env, args );
	} else
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
		
		muse_push_copy_recent_scope(env);
		{
			int bp = _bspos();
			_push_binding(_builtin_symbol(MUSE_IT));

			if ( _eval( _head(clause) ) )
			{
				muse_cell result;
				_unwind(sp);
				result = _do( _tail(clause) );
				_unwind_bindings(bp);
				muse_pop_recent_scope( env, (muse_int)syntax_cond, result );
				return result;
			}
			else
			{
				_unwind_bindings(bp);
				muse_pop_recent_scope( env, 0, MUSE_NIL );
			}
		}
		
		args = _tail(args);
	}
	
	_unwind(sp);
	return MUSE_NIL;
}

/**
 * @code (do ...expressions...) @endcode
 * Defines an expression block. The result of the do block is the
 * result of the last expression in the block, after evaluating all
 * the expressions in the block in sequence.
 */
muse_cell syntax_do( muse_env *env, void *context, muse_cell args )
{
	return guarded_do( env, args );
}

/**
 * @code (while bool-expr <body>) @endcode
 * Repeatedly evaluates the body statements as long as
 * the \c bool-expr expression evaluates to something non-nil.
 */
muse_cell syntax_while( muse_env *env, void *context, muse_cell args )
{
	muse_cell bool_expr		= _next(&args);
	muse_cell body			= args;
	muse_cell result		= MUSE_NIL;
	int sp = _spos();
	
	MUSE_DIAGNOSTICS({
		muse_message( env, L"(while ...)", L"The 'while' loop construct is now deprecated.\nUse tail recursion instead." );
	});
	
	while ( _eval(bool_expr) )
	{
		_unwind(sp);
		result = _force(_do(body));
	}
	
	_unwind(sp);
	_spush(result);
	return result;
}

/**
 * @code (for init-expr cond-expr step-expr body [result-expr]) @endcode
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

	MUSE_DIAGNOSTICS({
		muse_message( env, L"(for ...)", L"The 'for' loop construct is now deprecated.\nUse tail recursion instead." );
	});
	
	/* A result expression after the loop body is optional. */
	if ( args )
	{
		result_expr_given = MUSE_TRUE;
		result_expr = _next(&args);
	}

	/* Perform the initialization. */
	_eval( init_expr );
	
	/* Evaluate the body as long as the condition holds. */
	while ( _eval(cond_expr) )
	{
		_unwind(sp);
		result = _eval(body);
		_eval(update_expr);
	}
	
	/* If result expression is given, use it. Otherwise
		use the the last returned value of the body. */
	if ( result_expr_given )
		result = _eval(result_expr);
	
	_unwind(sp);
	_spush(result);
	return result;
}

/**
 * @code (stats) @endcode
 *
 * Evaluates to an object whose properties describe the
 * current internal state of the muSE interpreter.
 */
muse_cell fn_stats( muse_env *env, void *context, muse_cell args )
{
	int free_cell_count		= env->heap.free_cell_count;
	int sp					= _spos();
	
	muse_cell obj = fn_new(env,NULL,MUSE_NIL);
	return muse_put_many( env, obj, 
						 muse_list( env, "SiSiSiSi",
								   L"free-cells", free_cell_count,
								   L"stack-size", sp,
								   L"symbol-count", env->num_symbols,
								   L"bindings-depth", _bspos()/2 ) );
}

/************************ Type checks ***********************/

/**
 * @code (int? x) @endcode
 * Evaluates to x if x is an integer cell and to () if its not.
 */
muse_cell fn_int_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = _evalnext(&args);
	
	return _cellt(arg) == MUSE_INT_CELL ? arg : MUSE_NIL;
}

/**
 * @code (float? x) @endcode
 * Evaluates to x if x is a float cell and to () if it is not.
 */
muse_cell fn_float_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = _evalnext(&args);
	
	return _cellt(arg) == MUSE_FLOAT_CELL ? arg : MUSE_NIL;
}

/**
 * @code (number? x) @endcode
 * Evaluates to x if x is either an integer or a float cell. 
 * Otherwise evaluates to ().
 */
muse_cell fn_number_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = _evalnext(&args);
	
	return (_cellt(arg) == MUSE_INT_CELL || _cellt(arg) == MUSE_FLOAT_CELL) ? arg : MUSE_NIL;
}

/**
 * @code (cons? x) @endcode
 * Evaluates to x if x is a cons cell - i.e. a portion of a list
 * or a pair created by \c cons. If not, it evaluates to ().
 * 
 * @see _cons
 * @see fn_cons
 */
muse_cell fn_cons_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = _evalnext(&args);
	
	return _cellt(arg) == MUSE_CONS_CELL ? arg : MUSE_NIL;
}

/**
 * @code (fn? x) @endcode
 * Evaluates to x if x is a lambda function or a c-native function.
 * Evaluates to () if it is not.
 * @see syntax_lambda
 * @see muse_mk_nativefn()
 */
muse_cell fn_fn_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = _evalnext(&args);
	
	return _isfn(arg) ? arg : MUSE_NIL;
}

/**
 * @code (lambda? x) @endcode
 * Evaluates to x if x is a lambda function.
 * Evaluates to () if it is not.
 * @see fn_fn_p
 */
muse_cell fn_lambda_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = _evalnext(&args);
	
	return (_cellt(arg) == MUSE_LAMBDA_CELL) ? arg : MUSE_NIL;
}

/**
 * @code (symbol? x) @endcode
 * Evaluates to x if x is a symbol (named or anonymous) and evaluates
 * to () if it isn't.
 */
muse_cell fn_symbol_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = _evalnext(&args);
	
	return _cellt(arg) == MUSE_SYMBOL_CELL ? arg : MUSE_NIL;
}

/**
 * @code (string? x) @endcode
 * Evaluates to x if x is a text cell and to () if it isn't.
 */
muse_cell fn_string_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = _evalnext(&args);
	
	return _cellt(arg) == MUSE_TEXT_CELL ? arg : MUSE_NIL;
}

/**
 * @code (int thing) @endcode
 *
 * Converts the given thing to an integer or returns () if
 * it cannot. Integers, floats and strings containing integers
 * can be converted to integers using this function.
 */
muse_cell fn_int( muse_env *env, void *context, muse_cell args )
{
	muse_cell thing = _evalnext(&args);
	switch ( _cellt(thing) )
	{
	case MUSE_INT_CELL : return thing;
	case MUSE_FLOAT_CELL : return _mk_int((muse_int)_floatvalue(thing));
	case MUSE_TEXT_CELL :
		{
			int len = 0;
			const muse_char *text = _text_contents( thing, &len );
			muse_int i = 0;
			int n = swscanf( text, L"%lld", &i );
			if ( n == 1 )
				return _mk_int(i);
			else
				return MUSE_NIL;
		}
	default: return MUSE_NIL;
	}
}

/**
 * @code (float thing) @endcode
 *
 * Converts the given thing to a floating point number or returns () if
 * it cannot. Integers, floats and strings containing floats
 * can be converted to floats using this function.
 */
muse_cell fn_float( muse_env *env, void *context, muse_cell args )
{
	muse_cell thing = _evalnext(&args);
	switch ( _cellt(thing) )
	{
	case MUSE_FLOAT_CELL : return thing;
	case MUSE_INT_CELL : return _mk_float((muse_float)_intvalue(thing));
	case MUSE_TEXT_CELL :
		{
			int len = 0;
			const muse_char *text = _text_contents( thing, &len );
			muse_float f = 0;
			int n = swscanf( text, L"%f", &f );
			if ( n == 1 )
				return _mk_float(f);
			else
				return MUSE_NIL;
		}
	default: return MUSE_NIL;
	}
}

/**
 * @code (number thing) @endcode
 *
 * Returns a number - either int or float. If thing is a string,
 * it is parsed to determine whether it is an int or float.
 * Returns () if thing cannot be converted to a number.
 */
muse_cell fn_number( muse_env *env, void *context, muse_cell args )
{
	muse_cell thing = _evalnext(&args);
	switch ( _cellt(thing) )
	{
	case MUSE_FLOAT_CELL : 
	case MUSE_INT_CELL : 
		return thing;
	case MUSE_TEXT_CELL :
		{
			int len = 0;
			const muse_char *text = _text_contents( thing, &len );
			muse_float f = 0;
			muse_int i = 0;
			int n = swscanf( text, L"%lf", &f );
			n += swscanf( text, L"%lld", &i );
			if ( n > 0 )
			{
				if ( (muse_float)i == f )
					return _mk_int(i);
				else
					return _mk_float(f);
			}
			else
				return MUSE_NIL;
		}
	default: return MUSE_NIL;
	}
}

/**
 * @code (time-taken-us ...block...) @endcode
 * Returns the time taken to execute the block,
 * in microseconds.
 */
muse_cell fn_time_taken_us( muse_env *env, void *context, muse_cell args )
{
	void *timing = muse_tick();
	_force(_do( args ));
	return _mk_int( muse_tock(timing) );
}

/**
 * @code (exit) @endcode
 * Exits the process.
 */
muse_cell fn_exit( muse_env *env, void *context, muse_cell args )
{
	exit(0);
	return MUSE_NIL;
}

static void gendoc_for_symbol( muse_env *env, muse_cell symbol, muse_port_t p )
{
	muse_cell plist = muse_symbol_plist(env,symbol);
	muse_cell pty = MUSE_NIL;

	if ( plist )
	{
		char buffer[1024];
		int size;
		MUSE_DIAGNOSTICS3({ fprintf( stderr, "symbol: %S\n", muse_symbol_name(env,symbol) ); });

		/* Generate documentation only if there is a plist for the symbol. */
		size = sprintf(buffer, "/**\n@defgroup g%d %S\n", symbol, muse_symbol_name(env,symbol) );
		port_write( buffer, size, p );
		
		pty = muse_assoc( env, plist, _builtin_symbol(MUSE_SIGNATURE) );
		if ( pty )
		{
			size = sprintf(buffer, "@section Signature\n@code\n");
			port_write( buffer, size, p );
			muse_pwrite( p, _tail(pty) );
			size = sprintf( buffer, "\n@endcode\n\n" );
			port_write( buffer, size, p );
		}

		pty = muse_assoc( env, plist, _builtin_symbol(MUSE_USAGE) );
		if ( pty )
		{
			size = sprintf( buffer, "@section Usage\n@code" );
			port_write( buffer, size, p );
			muse_pprint( p, _head(_tail(pty)) );
			size = sprintf( buffer, "\n@endcode\n\n" );
			port_write( buffer, size, p );
		}

		pty = muse_assoc( env, plist, _builtin_symbol(MUSE_DESCR) );
		if ( pty )
		{
			muse_cell text = _tail(pty);

			size = sprintf( buffer, "@section Description\n" );
			port_write( buffer, size, p );
			while ( text )
			{
				muse_pprint( p, _head(text) );
				port_putc( '\n', p );
				text = _tail(text);
			}
		}

		pty = muse_assoc( env, plist, _builtin_symbol(MUSE_CODE) );
		if ( pty )
		{
			size = sprintf( buffer, "@section Code\n<pre>\n" );
			port_write( buffer, size, p );
			muse_pwrite(p, _tail(pty));
			size = sprintf( buffer, "\n</pre>\n\n");
			port_write( buffer, size, p );
		}

		size = sprintf( buffer, "\n */\n\n" );
		port_write( buffer, size, p );
	}
}

/**
 * @code (generate-documentation output-file-name) @endcode
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

	filename = _evalnext(&args);

	swprintf( buffer, 4096, L"%S.txt", _text_contents( filename, NULL ) );

	f = muse_fopen(buffer,L"wb");
	if ( !f )
		return MUSE_NIL;
	p = muse_assign_port(env, f, MUSE_PORT_WRITE);

	/* The symbol table is a hash table where each bucket has a list
	of symbols. */
	size = sprintf( (char*)buffer, "/** @defgroup %S */\n/*@{*/\n", _text_contents( filename, NULL ) );
	port_write( buffer, size, p );
	{
		muse_cell *syms = env->symbol_stack.bottom;
		muse_cell *syms_end = syms + env->symbol_stack.size;

		while ( syms < syms_end )
		{
			muse_cell s = *syms++;
			while ( s )
			{
				gendoc_for_symbol( env,_next(&s), p);
			}
		}
	}
	size = sprintf( (char*)buffer, "\n/*@}*/\n" );
	port_write( buffer, size, p );

	port_close(p);
	muse_unassign_port(p);
	return _t();
}

static muse_cell as_string( muse_env *env, muse_cell c, int sp )
{
	char buffer[128];

	switch ( _cellt(c) )
	{
	case MUSE_INT_CELL : 
		sprintf( buffer, MUSE_FMT_INT, _intvalue(c) );
		return muse_mk_ctext_utf8(env,buffer);
	case MUSE_FLOAT_CELL :
		sprintf( buffer, MUSE_FMT_FLOAT, _floatvalue(c) );
		return muse_mk_ctext_utf8(env,buffer);
	case MUSE_TEXT_CELL :
		return c;
	case MUSE_SYMBOL_CELL :
		return _tail(_head(_tail(c)));
	case MUSE_NATIVEFN_CELL :
		{
			muse_functional_object_t *f = NULL;
			muse_format_view_t *fv = _fnobjview( c, 'frmt', f );
			if ( fv ) {
				muse_cell replacement = fv->format( env, f );
				muse_assert( replacement != c ); /*< Potential for infinite loop if this assert fails. */
				_unwind(sp);
				_spush(replacement);
				return as_string( env, replacement, sp );
			}
		}

	default:
		return MUSE_NIL;
	}
}

/**
 * @code (format ...args...) @endcode
 * Converts each arg to a string, concatenates all the strings
 * and returns the result as a single string. Things that
 * support the 'frmt' view such as \ref fn_bytes_p "bytes?"
 * and \ref fn_object_p "object?" convert themselves to strings.
 *
 * Objects, in particular, get their "as-string" methods 
 * invoked to get a string representation. The "as-string"
 * can also be a field. If it isn't a method, its value
 * is used as the string representation of the object.
 * In particular, the "as-string" implementation can return
 * or be any object which can be converted to a string, such
 * as int, float or even another object.
 *
 * For example -
 * @code
 * > (define ex 
 *     (object ()
 *       'name		"telepathic-robot"
 *       'id		3
 *       'as-string (fn (self) (format self.name "-" self.id))))
 * > (format "object=" ex)
 * object=telepathic-robot-3
 * @endcode
 */
muse_cell fn_format( muse_env *env, void *context, muse_cell args )
{
	int sp = _spos();
	muse_cell values = muse_eval_list(env,args);

	int total_length = 0;
	muse_cell result = MUSE_NIL;

	/* Replace each value with its string representation. */
	{
		muse_cell v = values;
		while ( v )
		{
			muse_cell replacement = as_string( env, _head(v), _spos() );

			if ( replacement ) {
				int length = 0;
				_text_contents( replacement, &length );
				total_length += length;
			}

			_seth(v,replacement);
			v = _tail(v);
		}
	}

	/* Create the result buffer. */
	result = muse_mk_text( env, 0, ((muse_char*)0) + total_length );

	/* Copy the string pieces into the result buffer. */
	{
		muse_char *buffer = (muse_char*)_text_contents( result, NULL );

		muse_cell v = values;

		while ( v )
		{
			muse_cell c = _next(&v);

			if ( c )
			{
				int length = 0;
				const muse_char *text = _text_contents( c, &length );
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
 * @code (string-length s) @endcode
 * Returns the number of characters in the string or () if s is not a string.
 */
muse_cell fn_string_length( muse_env *env, void *context, muse_cell args )
{
	muse_cell s = _evalnext(&args);
	if ( s && _cellt(s) == MUSE_TEXT_CELL )
	{
		muse_text_cell t = _ptr(s)->text;
		return _mk_int( t.end - t.start );
	}
	else
	{
		return MUSE_NIL;
	}
}

/**
 * @code (substring str start-index [count]) @endcode
 *
 * Gives the substring of length \p count characters
 * of the given string starting from \p start-index.
 * If the \p start-index is considered to be modulo
 * [0,length) ... which means you can get the last 
 * 3 characters of a string like this -
 * @code (substring str -3 3) @endcode
 *
 * If you request more characters than is available in
 * the string, you get as much of the string as is avaiable.
 *
 * The count is optional. If omitted, the substring is
 * considered up to the end of the string. You can use this
 * to shorten the above example to -
 * @code (substring str -3) @endcode
 *
 * @exception error:integer-expected
 * Handler format: @code (fn (resume 'error:integer-expected value) ...) @endcode
 * Raised when an integer was expected and a different value was given.
 * No point resuming since this is a grave programming error.
 *
 * @exception error:string-expected
 * Handler format: @code (fn (resume 'error:string-expected value) ...) @endcode
 * Raised when a string was expected and a different value was given.
 * No point resuming since this is a grave programming error.
 */
muse_cell fn_substring( muse_env *env, void *context, muse_cell args )
{
	muse_cell str = _evalnext(&args);
	if ( _cellt(str) == MUSE_TEXT_CELL ) {
		muse_cell start_index_c = _evalnext(&args);
		if ( _cellt(start_index_c) == MUSE_INT_CELL ) {
			int start_index = (int)_intvalue(start_index_c);
			int count = 0, str_length = 0;
			const muse_char *str_text = muse_text_contents( env, str, &str_length );

			/* If count is not specified, get as many characters as available. */
			if ( args ) {
				muse_cell c = _evalnext(&args);
				if ( _cellt(c) == MUSE_INT_CELL )
					count = (int)_intvalue(c);
				else {
					muse_message( env, L"(substring str start-index >>count<<)", L"Expected an integer, got [%m]", c );
					return muse_raise_error( env, _csymbol(L"error:integer-expected"), _cons(c,MUSE_NIL) );
				}

				/* If count is "-3", for instance, treat it as "str_length - 3". */
				count = (count < 0) ? (str_length + (count % str_length)) : count;
			} else {
				count = str_length;
			}

			if ( str_length <= 0 )
				return str;

			/* Take modulo [0,length) */
			start_index = ((start_index % str_length) + str_length) % str_length;

			/* Limit count to number of available characters. */
			if ( start_index + count > str_length )
				count = str_length - start_index;

			return muse_add_recent_item( env, (muse_int)fn_substring, muse_mk_text( env, str_text + start_index, str_text + start_index + count ) );
		} else {
			muse_message( env, L"(substring str >>start-index<< [count])", L"Expected an integer, got [%m]", start_index_c );
			return muse_raise_error( env, _csymbol(L"error:integer-expected"), _cons(start_index_c,MUSE_NIL) );
		}
	} else {
		muse_message( env, L"(substring >>str<< start-index [count])", L"Expected a string, got [%m]", str );
		return muse_raise_error( env, _csymbol(L"error:string-expected"), _cons(str,MUSE_NIL) );
	}
}

/**
 * @code (load-plugin dll-filename) @endcode
 * Loads and dynamically links the given plugin. 
 * Returns the result of invoking the plugin's entry point function.
 */
muse_cell fn_load_plugin( muse_env *env, void *context, muse_cell args )
{
	muse_cell filename = _evalnext(&args);
	return muse_link_plugin( env, _text_contents( filename, NULL ), args );
}

/**
 * @code (this-process) @endcode
 *
 * Evaluates to the pid of the process in which it is evaluated.
 */
muse_cell fn_this_process( muse_env *env, void *context, muse_cell args )
{
	return process_id( env->current_process );
}

/**
 * @code (spawn (fn () [body]) [attention]) -> pid @endcode
 *
 * Spawns a new process which will evaluate the given thunk.
 * The (optional) attention value is a positive integer
 * giving the number of reductions to perform in the created process
 * before yielding to other processes. The default value is 10.
 *
 * The result of the spawn expression is a pid using which you can
 * identify the created process and send messages to it by using the
 * pid as a normal function.
 *
 * The process will end when the thunk completes evaluation.
 * So if you want to create a server process, you can express
 * it as an infinitely tail recursive function. (Tail recursive
 * calls are stack optimized.)
 */
muse_cell fn_spawn( muse_env *env, void *context, muse_cell args )
{
	muse_cell thunk = _evalnext(&args);
	int attention = args ? (int)_intvalue( _evalnext(&args) ) : env->parameters[MUSE_DEFAULT_ATTENTION];

	muse_process_frame_t *p = init_process_mailbox( create_process( env, attention, thunk, NULL ) );
	prime_process( p );
	return process_id( p );
}

/**
 * @code (atomic ...body...) @endcode
 *
 * Behaves like \ref syntax_do "do", but evaluates the entire body atomically,
 * i.e. without switching to another process, as long as you don't use the
 * pausing functions \ref fn_receive "receive" and \ref fn_run "run".
 */
muse_cell syntax_atomic( muse_env *env, void *context, muse_cell args )
{
	muse_cell result = MUSE_NIL;
	enter_atomic(env);
	result = _force(_do(args));
	leave_atomic(env);
	return result;
}

/**
 * @code (receive [pid] [timeout-us]) @endcode
 *
 * Waits for and returns the next message in the process' mailbox.
 * It has four forms -
 *	- (receive)
 *	- (receive pid)
 *	- (receive timeout-us)
 *	- (receive pid timeout-us)
 * 
 * If a process-id is given as the argument, it waits until a message is 
 * received from that specific process. If a timeout value (in microseconds) 
 * is given, it waits until either a message is received or the timeout expires. 
 * If the timeout expired, the receive expression evaluates to MUSE_NIL - i.e. to ().
 */
muse_cell fn_receive( muse_env *env, void *context, muse_cell args )
{
	muse_process_frame_t *p = env->current_process;
	muse_cell pid = MUSE_NIL;
	muse_int timeout_us = -1;
	muse_cell msgs = MUSE_NIL;

	if ( args )
	{
		/* Check whether the first arg is a pid that we have to wait for. */
		pid = _evalnext(&args);

		switch ( _cellt(pid) )
		{
		case MUSE_NATIVEFN_CELL :
			/* Yes it is. Check whether the next argument is a timeout value. */
			timeout_us = args ? _intvalue( _evalnext(&args) ) : -1;
			break;
		case MUSE_INT_CELL :
		case MUSE_FLOAT_CELL :
			/* Its not. It is a timeout value. */
			timeout_us = _intvalue( pid );
			pid = MUSE_NIL;
			break;
		default:
			muse_assert( !"Invalid argument to (receive...)" );
		}
	}

	/* Set the pid wwe're waiting for. */
	p->waiting_for_pid = pid;
	msgs = p->mailbox;

	if ( pid )
	{
		/* Search for the message. */
		while ( _tail(msgs) && _head(_head(_tail(msgs))) != pid )
			msgs = _tail(msgs);
	}

	if ( !_tail(msgs) )
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
	msgs = p->mailbox;

	if ( pid )
	{
		/* Search for the message. */
		while ( _tail(msgs) && _head(_head(_tail(msgs))) != pid )
			msgs = _tail(msgs);
	}

	if ( _tail(msgs) )
	{
		/* Yes! We've received a message. Remove it from the queue and return it. */
		muse_cell msg = _tail(msgs);
		p->waiting_for_pid = MUSE_NIL; /**< No longer waiting for a pid. */
		_sett( msgs, _tail( msg ) );

		/* After removing, check if we've reached the end of the message queue. */
		if ( msg == p->mailbox_end )
			p->mailbox_end = msgs;

		return _head( msg );
	}
	else
		return MUSE_NIL; /**< We've timed out. */
}


/**
 * @code (run [duration-us]) @endcode
 *
 * If the duration argument is omitted, the expression never terminates
 * and the process becomes blocked. If a duration is given (in microseconds),
 * the process yields time to other processes for at least that much
 * time. There is no accuracy guarantee for the timing.
 */
muse_cell fn_run( muse_env *env, void *context, muse_cell args )
{
	muse_process_frame_t *p = env->current_process;
	muse_int timeout_us = args ? _intvalue( _evalnext(&args) ) : -1;
	muse_int endtime_us = timeout_us + muse_elapsed_us(env->timer);

	do
	{
		p->state_bits = MUSE_PROCESS_WAITING;

		if ( timeout_us >= 0 )
		{
			p->state_bits |= MUSE_PROCESS_HAS_TIMEOUT;
			p->timeout_us = endtime_us;
		}

		switch_to_process( env, env->current_process->next );
	}
	while ( timeout_us < 0 );

	return MUSE_NIL;
}

/**
 * Checks whether the given thing is a process id. 
 */
#define _is_pid(pid) is_pid(env,pid)
static muse_cell is_pid( muse_env *env, muse_cell pid )
{
	if ( pid && _cellt(pid) == MUSE_NATIVEFN_CELL && _ptr(pid)->fn.fn == (muse_nativefn_t)fn_pid )
		return pid;
	else
		return MUSE_NIL;
}

/**
 * @code (post msg [pid]) @endcode
 *
 * Allows you to post an arbitrary sexpr as a message to the current process.
 * The difference between using post and \ref fn_pid "pid" (as a function)
 * is that when posting a message using \ref fn_pid "pid", the message consists
 * of the argument list and the pid of the sending process is prepended to
 * this list. If you use "post", the message is the first argument - as is.
 * This means "post" is the only way to post, for example, a symbol or a number
 * as the message itself.
 *
 * An important use for this function is to postpone the processing of
 * a message in situations where how to process it cannot be determined at
 * the time the message is received. If you supply an optional pid, a message can
 * be diverted to another process without modification. In this case, the 
 * message is not posted to the current process.
 *
 * Ex: Postponing the processing of a message -
 * @code
 * (case (receive)
 *    (pattern-1 ....)
 *    (pattern-2 ....)
 *    ...
 *    (any (post any)))
 * @endcode
 */
muse_cell fn_post( muse_env *env, void *context, muse_cell args )
{
	muse_cell msg = _evalnext(&args);

	if ( args )
	{
		/* We've been given a pid to post to. */
		muse_cell pid = _evalnext(&args);

		MUSE_DIAGNOSTICS({
			if ( !_is_pid(pid) )
				muse_message( env,L"(post msg >>[pid]<<)", L"Expected a process id as the second argument.\nGot\n\t%m\ninstead.", pid );
		});

		post_message( (muse_process_frame_t*)(_ptr(pid)->fn.context), msg );
	}
	else
	{
		/* Post to the current process. */
		post_message( env->current_process, msg );
	}

	return MUSE_NIL;
}

/**
 * @code (process? pid) @endcode
 *
 * Evaluates to \p pid if it is a valid process id and to <tt>()</tt> if it isn't.
 */
muse_cell fn_process_p( muse_env *env, void *context, muse_cell args )
{
	return _is_pid( _evalnext(&args) );
}

/**
 * @code (with-timeout-us 'id us ...body...) @endcode
 *
 * Evaluates body like a \c do statement does, but imposes the
 * given timeout for it. If the timeout expires during the
 * evaluation of the body, it raises the ('timeout 'id given-us elapsed-us)
 * exception.
 */
muse_cell fn_with_timeout_us( muse_env *env, void *context, muse_cell args )
{
	muse_cell id = _evalnext(&args);
	muse_int timeout_us = _intvalue(_evalnext(&args));
	muse_cell result = MUSE_NIL;

	muse_assert( _cellt(id) == MUSE_SYMBOL_CELL );

	{
		int bsp = _bspos();
		push_timeout( env, id, timeout_us );
		result = muse_force( env, muse_do( env, args ) );
		_unwind_bindings(bsp);
		env->current_process->num_eval_timeouts--;
	}

	return result;
}

/**
 * @code (the recent-key) @endcode
 *
 * Looks up the given recent-key to access the result of a recently
 * done computation. Some builtin functions add their results to the
 * recent list as do all user-defined functions. The recent list length
 * is 8. This roughly matches the level at which readability begin to
 * suffer instead of improving.
 *
 * Here is a trivial example for illustration - a function that 
 * computes (a+b)/(a-b).
 * @code
 * > (define (add a b) (+ a b))
 * > (define (sub a b) (- a b))
 * > (define (h a b)
 *    (add a b)
 *    (sub a b)
 *    (/ (the add) (the sub)))
 * > (h 5 2)
 * 2.333333
 * > (the h)
 * 2.333333
 * @endcode
 *
 * @note The \c add and \c sub recent results are not accessible to the
 * caller of \c h. Evaluating @code (the add) @endcode after evaluating
 * @code (h 5 2) @endcode will result in a "recent item not found" message
 * box. This is a very important property of \c the - its ability to
 * restrict the scope of the recent computations to the scope of user
 * defined functions so that callers can access the results of <em>their</em>
 * function evaluations rather than those made by the functions they 
 * evaluate.
 * 
 * Here is another contrived one -
 * @code
 * (if (find x some-list)
 *    (length (the find))
 *    (raise 'FindFailed x))
 * @endcode
 * The above example is clearer using \c the and without it, you might
 * have to write -
 * @code
 * (let ((result (find x some-list)))
 *    (if result 
 *       (length result)
 *       (raise 'FindFailed x)))
 * @endcode
 * which introduces a new name \c result into the code, besides
 * adding a level of nesting for the purpose. The \c the primitive
 * therefore reduces the need to introduce new temporary names into
 * the code by allowing you to refer to recent computation results
 * using the function that computed it as the key. As in the trivial
 * @code (h a b) @endcode example above, it allows you to reduce
 * the level of nesting as well.
 *
 * @par \c the and \c it
 * Within a given scope, the reserved symbol "it" can be used to
 * refer to the most recently evaluated "the" expression. "it"
 * will refer to the most recent "the" expression even if that
 * result has been forgotten in the recent list and can no longer
 * be obtained by re-evaluating the "the" expression. Here's an example -
 * @code
 * (if (find x some-list)
 *    (if (>= (length (the find)) 3)
 *       (print it)
 *       (print (the length) "is too short because" it "is less than 3."))
 *    (raise 'FindFailed x))
 * @endcode
 *
 * @par Builtins that support \c the -
 *	- length, map, join, collect, reduce,
 *	- find, andmap, ormap
 *	- transpose
 *	- vector, hashtable, list
 *	- read, read-bytes, read-xml
 *	- open-file
 */
muse_cell fn_the( muse_env *env, void *context, muse_cell args )
{
	muse_cell key = _evalnext(&args);
	muse_int ikey = key;

	if ( _cellt(key) == MUSE_NATIVEFN_CELL ) {
		ikey = (muse_int)(size_t)(_ptr(key)->fn.fn);
	}

	{
		muse_cell val = MUSE_NIL;
		if ( muse_find_recent_item( env, ikey, &val ) ) {
			_define( _builtin_symbol(MUSE_IT), val );
			return val;
		} else {
			MUSE_DIAGNOSTICS({
				muse_message( env, L"(the >>fn<<)", L"No such recent computation by function %m.", key );
			});
			return key;
		}
	}
}

/**
 * @code (trace on) or (trace off) @endcode
 */
muse_cell fn_trace( muse_env *env, void *context, muse_cell args )
{
	muse_cell on = _csymbol(L"on");
	muse_cell off = _csymbol(L"off");
	if ( args ) {
		muse_cell arg = _next(&args);
		if ( arg == on ) { env->parameters[MUSE_ENABLE_TRACE] = MUSE_TRUE; return on; }
		if ( arg == off ) { env->parameters[MUSE_ENABLE_TRACE] = MUSE_FALSE; return off; }
		return MUSE_NIL;
	} else {
		return env->parameters[MUSE_ENABLE_TRACE] ? on : off;
	}
}

#if MUSE_PLATFORM_WINDOWS
/**
 * Special code for Win32 to do roughly the same thing 
 * as system() except that no console window should be opened.
 * If you want to execute a command like "dir" or "copy",
 * you need to do -
 *   (system "cmd /c" "copy a.txt b.txt")
 *
 * Thanks to http://www.codeproject.com/KB/winsdk/runsilent.aspx
 */
static int muse_shell_execute( char *command_line )
{
	STARTUPINFOA si;
	PROCESS_INFORMATION pi;
	int exitCode = 0;

	memset(&si, 0, sizeof(si));
	si.cb = sizeof(si);
	si.dwFlags = STARTF_USESHOWWINDOW;
	si.wShowWindow = SW_HIDE;

	if ( !CreateProcessA( NULL, command_line, NULL, NULL, FALSE, CREATE_NEW_CONSOLE, NULL, NULL, &si, &pi ) )
		return -1;

	WaitForSingleObject( pi.hProcess, INFINITE );
	if ( !GetExitCodeProcess( pi.hProcess, &exitCode ) )
		exitCode = 0;

	CloseHandle( pi.hThread );
	CloseHandle( pi.hProcess );
	return exitCode;
}
#else
/**
 * For unix-like systems, we just use system().
 */
#define muse_shell_execute system
#endif

/**
 * (system cmd arg1 arg2 ...)
 * Executes the system command passing the given args.
 */
muse_cell fn_system( muse_env *env, void *context, muse_cell args )
{
	muse_cell formatfn = muse_mk_nativefn( env, fn_format, NULL );
	muse_cell cmd_and_args = muse_eval_list( env, args );
	muse_cell cmd = _head(cmd_and_args);
	muse_cell eargs = _tail(cmd_and_args);
	muse_cell val = muse_apply( env, formatfn, _cons( cmd, MUSE_NIL ), MUSE_TRUE, MUSE_FALSE );
	int nargs = muse_list_length( env, eargs );
	
#ifdef MUSE_WIDECHAR_VERSION
	wchar_t command_line[1024];

	int N = swprintf( command_line, 1024, L"%ls", muse_text_contents( env, val, NULL ) );
	
	int i = 0;
	for ( i = 0; i < nargs; ++i, eargs = _tail(eargs) )
	{
		muse_cell val = muse_apply( env, formatfn, _cons( _head(eargs), MUSE_NIL ), MUSE_TRUE, MUSE_FALSE );
		N += swprintf( command_line + N, 1024 - N, L" \"%ls\"", muse_text_contents( env, val, NULL ) );
	}

	return muse_mk_int( env, _wsystem( command_line ) );
#else
	char command_line[1024];
	
	int N = snprintf( command_line, 1024, "%ls", muse_text_contents( env, val, NULL ) );
	
	int i = 0;
	for ( i = 0; i < nargs; ++i, eargs = _tail(eargs) )
	{
		muse_cell val = muse_apply( env, formatfn, _cons( _head(eargs), MUSE_NIL ), MUSE_TRUE, MUSE_FALSE );
		N += snprintf( command_line + N, 1024 - N, " \"%ls\"", muse_text_contents( env, val, NULL ) );
	}
	
	return muse_mk_int( env, muse_shell_execute( command_line ) );
#endif
	
}

static muse_boolean symbol_on_stack( muse_env *env, muse_cell sym )
{
	muse_stack *s = &env->current_process->bindings_stack;
	muse_cell *from = s->bottom, *top = s->top;
	while ( from < top ) {
		if ( (*from) == sym )
			return MUSE_TRUE;
		from += 2;
	}
	return MUSE_FALSE;
}

muse_cell module_find_symbol_with_value( muse_env *env, void *obj, muse_cell value );

/**
 * @code (symbol-whose-value-is value) @endcode
 *
 * Looks up the symbol table and gives the symbol which is currently 
 * (in dynamic scope) defined to the given value.
 */
muse_cell fn_symbol_whose_value_is( muse_env *env, void *context, muse_cell args )
{
	muse_cell value = _evalnext(&args);

	/* Traverse the symbol table and find the symbol that
	has the given cell as its value. */

	muse_stack symbols = env->symbol_stack;
	int i;
	for ( i = 0; i < symbols.size; ++i )
	{
		muse_cell symlist = symbols.bottom[i];
		
		while ( symlist )
		{
			muse_cell sym = _next(&symlist);

			if ( _symval(sym) == value ) {
				if ( !symbol_on_stack(env,sym) ) /* Value found. */
					return sym;
			} else {
				/* Recursively search any modules for such a value. */
				void *data = muse_functional_object_data( env, _symval(sym), 'mmod' );
				if ( data ) {
					muse_cell found = module_find_symbol_with_value( env, data, value );
					if ( found )
						return _cons( sym, found );
				}
			}
		}
	}

	return MUSE_NIL;
}
