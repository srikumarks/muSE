/**
 * @file muse.h
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#ifndef __MUSE_H__
#define __MUSE_H__

#include <stdio.h>
#include <wchar.h>

#include "muse_platform.h"

BEGIN_MUSE_C_FUNCTIONS

/**
 * @mainpage muSE - muvee Symbolic Expressions
 * 
 * muSE is a portable compact lisp dialect that is flexible enough to be
 * used as a data format as well as a scripting language. Here is a list
 * of muSE features -
 * 	- Very simple, general, structured data format based on lists
 * 	- Supports symbols, integers, floats, strings and lists
 * 	- Main abstraction mechanism is the closure
 * 	- A simple object system for OOP
 * 	- Small code foot-print
 * 	- Simple garbage collector (< 1ms for 60000 cells)
 * 	- Simple C integration API for compute-intensive algorithms
 *  - Ability to add C/C++ based native functionality in the form of plugin DLLs.
 *	- A simple native object system for adding new native object types.
 *
 * Language features include -
 *	- Lexically scoped closures as well as dynamically scoped blocks.
 *	- Expressive reader macro system where macros are first class citizens 
 *		(i.e. can be passed around as arguments to functions, assigned to variables, etc.)
 *	- Uniform use of pattern matching for variable binding. 
 *	- Erlang style message passing processes.
 *	- Networking support that's process aware.
 *	- Resumable exceptions with exception handler dispatch using pattern matching bind.
 *	- Generic functions.
 *  
 * @section Objects Basic objects
 * 
 * Everything in muSE is built from the following set of objects -
 * - Decimal numbers - @code 10, 23, -57, etc.@endcode 
 * - Hexadecimal numbers - @code 0xffee, 0xABcd, etc. @endcode upto 64-bits.
 * - Fractional numbers - @code 0.1, 3.1415, -2.71828, .1e-5, -.53e10 etc. @endcode. 
 *		- Constant fractions such as 4/3 and -15/36 are supported for reading as well.
 * - Strings - @code "Hello world!", "Hello ""Kumar""!", etc. @endcode. Use
 *		a pair of double quotes to embed a single double quote character
 *		into a string. (There are no other escape codes.)
 * - Symbols - @code hello, WORLD, <DouglasAdams>, Piglet*, etc. @endcode (i.e. any
 * 		contiguous set of characters that doesn't start with a number.)
 * - Lists - @code (1 2 3), (hello world 42), (numbers ("PI = " 3.1415) ("e = " 2.718)) @endcode 
 * 
 * @section Notations Notations
 * 
 * Apart from the notation for objects in the previous section, some speial 
 * cases follow -
 * 
 * 	- Falsehood is expressed using the empty list <tt>()</tt>. Anything else goes
 * 		for truth.
 * 	- You can "quote" an expression if you do not want it to be evaluated.
 * 		You do that by placing a single quote mark before the expression.
 * 		For example : @code '(hello "muSE") @endcode is the quoted list 
 * 		@code (hello "muSE") @endcode. If you'd used the second unquoted form
 * 		instead of the first, muSE will try to lookup the function represented by the
 * 		symbol \c hello and evaluate it, supplying the rest of the list as arguments.
 * 	- The list notation is an abbreviation of the fundamental list construction
 * 		operation called "cons". "cons" takes two objects and constrcuts
 * 		a pair out of them. Such a pair is notated as (a . b). A list is
 * 		made out of such pairs like this - @code (a . (b . (c . (d . ())))) @endcode 
 * 		where <tt>()</tt> is the NIL or empty list. This list is abbreviated
 * 		as @code (a b c d) @endcode 
 * 	- It is possible to have something else instead of the <tt>()</tt> at the 
 * 		end of the list. This is notated as @code (a b c . d) @endcode which stands for
 * 		@code (a . (b . (c . d))) @endcode, and is not a proper list.
 *
 * @section MuseLanguage muSE language
 * 
 * @subsection ML_StructuringCode Structuring code
 *	- \ref fn_define "define", \ref syntax_let "let", \ref syntax_do "do"
 * 	- \ref syntax_lambda "fn", \ref syntax_block "fn:"
 *	- \ref syntax_while "while", \ref syntax_for "for"
 * 	- \ref syntax_case "case"
 *	- \ref syntax_if "if", \ref syntax_cond "cond"
 *	- \ref syntax_try "try", \ref fn_raise "raise", \ref fn_retry "retry", \ref syntax_finally "finally"
 * 
 * @subsection ML_MathOps Mathematical operators
 *	- Binary operators 
 *		- \ref fn_add "+", \ref fn_sub "-", \ref fn_mul "*", \ref fn_div "/"
 *		- \ref fn_mod "%", \ref fn_idiv "i/", \ref fn_pow "pow"
 *	- Unary operators
 *		- \ref fn_trunc "trunc"
 *		- sqrt, log, log10, exp
 *		- sin, cos, tab, asin, acos, atan
 *		- sinh, cosh, tanh,
 *		- fabs, floor, ceil
 *	- Other functions
 *		- \ref fn_rand "rand"
 *
 * @subsection ML_DataStructures Data structures
 *	- \ref fn_cons "cons", \ref fn_first "first", \ref fn_rest "rest"
 *	- \ref fn_lcons "lcons", \ref fn_lazy "lazy"
 *	- \ref Vectors "vectors"
 *	- \ref Hashtables "hashtables"
 *	- \ref ByteArray "byte arrays"
 *	- \ref Boxes "boxes"
 *
 * @subsection ML_ObjectSystem Object system
 * 	- \ref fn_class "class"
 * 	- \ref fn_new "new"
 * 	- \ref fn_send "<-" (send message to object)
 * 	- \ref fn_obj_pty "->" (get object property)
 * 
 * @subsection ML_IO Input and output
 *	- \ref PortIO
 *	- \ref fn_open_file "open-file", \ref fn_memport "memport"
 * 	- \ref fn_print "print", \ref fn_write "write"
 * 	- \ref fn_read "read"
 *	- \ref fn_close "close"
 *
 * @subsection ML_Processes Processes
 *	- \ref fn_spawn "spawn", \ref fn_receive "receive", \ref syntax_atomic "atomic", \ref fn_post "post"
 *	- \ref fn_run "run", \ref fn_this_process "this-process", \ref fn_process_p "process?"
 */
 
/**
 * @defgroup MuseAPI Embedding API for the Muse language.
 * 
 * The muSE embedding API is a C wrapper around the core muSE runtime with
 * additional support functions. muSE - which stands for "muvee Symbolic Expressions" - 
 * is an environment for manipulating list-based data structures. It consists
 * of functions to create and parse such data structures as well as functions
 * to read and write them from/to files.
 * 
 * @section MuseEnv The muSE environment
 * 
 * The muSE environment holds all the data constructed using the muSE API
 * calls and keeps track of object references. There is a single global
 * muse environment \ref muse_env* active at any given time. One creates
 * this global muse environment using \ref muse_init_env and destroys it
 * using \ref muse_destroy_env.
 * 
 * @section MuseDataTypes The basic data types in muSE
 * 	- 64-bit integers - see \ref MUSE_INT_CELL, \ref muse_mk_int
 * 	- 64-bit floats - see \ref MUSE_FLOAT_CELL, \ref muse_mk_float
 * 	- and unicode strings - see \ref MUSE_TEXT_CELL, \ref muse_mk_text and family
 * 	- symbols - see \ref MUSE_SYMBOL_CELL, \ref muse_symbol and family
 * 
 * @section ConsCell The cons cell
 * 
 * The object that glues the above fundamental types into complex
 * structures is the "cons cell", aka a "pair" (\ref MUSE_CONS_CELL, \ref _cons).
 * A cons (short for "constructor") cell is a pair of cells which
 * are referred to as the \b head and \b tail of the cons cell.
 * A cons cell may have another cons cell as its head or tail.
 * 
 * If one uses the notation @code (cons h t) @endcode to denote a
 * cons cell whose head is \c h and tail is \c t, then we can
 * encode a list of items @code (h1 h2 h3 ... hN) @endcode using cons 
 * cells like this -
 * @code
 * (cons h1 (cons h2 (cons h3 ... (cons hN ()) ... )))
 * @endcode
 * where () is another name for the empty list and is used to mark the end of 
 * a list.
 * 
 * @section Symbols Symbols
 * 
 * Symbols are objects that are uniquely identified by giving their
 * names (see \ref muse_symbol, \ref muse_csymbol, and family). A symbol may 
 * have a value (see \ref muse_symbol_value and \ref muse_define) and a list 
 * of properties (see \ref muse_symbol_plist, \ref muse_get_prop and \ref muse_put_prop).
 * 
 * The property list of a symbol is in the form of what's called an
 * "association list" - a list of property-value pairs. Each property-value pair
 * is stored in a cons cell with the property in the head and the value
 * in the tail. (see \ref muse_assoc)
 * 
 * It is also possible to have anonymous symbols which differ from
 * named symbols in only one aspect - they have no name and are not preserved
 * eternally. Anonymous symbols form the foundation of muSE's simple, yet
 * powerful object system.
 * 
 * @section SExpressions S-Expressions
 * 
 * Compound structures built out of cons cells, and values such as ints, 
 * floats, symbols, strings, are called s-expressions - short for
 * "symbolic expressions". This term includes functions as well.
 * 
 * @section IO Reading and writing s-expressions
 * 
 * The functions \ref muse_pread and \ref muse_pwrite read and write complete
 * s-expressions from a given IO stream. You can use this feature to load
 * and save data to files, or to build a simple interpreter using a 
 * read-eval-print-loop (REPL for short). For a brief description of the syntax
 * of the data structure representation, see \ref mainpage.
 * 
 */
/*@{*/

/** 
 * Type codes for the various cell types.
 */
typedef enum 
{
	MUSE_CONS_CELL,			/**< A CONS cell contains a pair of cells - the "head" and "tail". */
	MUSE_LAMBDA_CELL,		/**< A LAMBDA cell contains a definition of a function. */
	MUSE_SYMBOL_CELL,		/**< Holds a SYMBOL that may have been given a value or have properties. */
	MUSE_NATIVEFN_CELL,		/**< A NATIVEFN holds the address of C-function to execute with an arbitrary argument. */
	MUSE_INT_CELL,			/**< Holds a single 64-bit signed integer. */
	MUSE_FLOAT_CELL,		/**< Holds a single 64-bit floating point number - i.e. a "double". */
	MUSE_TEXT_CELL,			/**< Holds an immutable copy of a wide character string - as ptrs to beginning and end. */
	MUSE_LAZY_CELL			/**< A cell whose head is a function and whose tail is its argument list. */
} muse_cell_t;

typedef struct _muse_env		muse_env;	/**< Identifies a particular muse instance. */
typedef wchar_t				muse_char;	/**< Unicode character type used throughout muse. */
typedef longlong_t			muse_int;	/**< 64-bit signed integer type. */
typedef double				muse_float;	/**< 64-bit double precision floating point type. */
typedef int					muse_cell;	/**< A cell is referred using a single 32-bit signed integer. */
typedef enum { MUSE_FALSE, MUSE_TRUE } muse_boolean; /**< Ask George Boole. */

/**
 * You can include native C-function calls within Muse procedure execution. 
 * This is how built-in functionality is provided. A Muse nativefn can be passed
 * a single pointer to arbitrary data that is not managed by the muse environment.
 * This is called its "context" and serves to supply any data and state the function
 * may need to maintain. For example, \p context can be the pointer to a C++
 * object instance and the nativefn can in turn call some specific method of that
 * object.
 * 
 * @param env The muse environment that's calling the native function.
 * @param context The function's closure argument.
 * @param args A list of arguments that was passed to the function when it was
 * 			called within the muse environment.
 */
typedef muse_cell (*muse_nativefn_t)( muse_env *env, void *context, muse_cell args );

/**
 * Some builtin-symbols are provided for general use.
 * @see muse_builtin_symbol()
 */
typedef enum
{
	MUSE_NIL,
	MUSE_T,
	MUSE_QUOTE,
	MUSE_CLASS,
	MUSE_SELF,
	MUSE_SUPER,
	MUSE_DOC,
	MUSE_CODE,
	MUSE_SIGNATURE,
	MUSE_USAGE,
	MUSE_BRIEF,
	MUSE_DESCR,
	MUSE_TIMEOUT,
	MUSE_DEFINE,
	MUSE_TRAP_POINT,
	MUSE_DEFAULT_EXCEPTION_HANDLER,
	MUSE_CLOSURE,
	MUSE_NAME,
	MUSE_IT,
	MUSE_THE,
	MUSE_TIMEOUTVAR,
	
	MUSE_NUM_BUILTIN_SYMBOLS /**< Not a symbol. */
} muse_builtin_symbol_t;

/** @name Managing the muse environment. */
/*@{*/
/**
 * When creating a new muse environment, you can configure it using
 * a set of parameters. Parameters are passed as an array of 32-bit
 * integers where indices 0, 2, 4, etc. hold parameter names and indices
 * 1, 3, 5, etc. hold parameter values. This enumeration defines the 
 * set of configurable parameters.
 */
typedef enum
{
	MUSE_END_OF_LIST,			/**< Used to signal the end of the parameter list.	No value needed. */
	MUSE_HEAP_SIZE,				/**< Integer parameter giving required heap size.	*/
	MUSE_GROW_HEAP_THRESHOLD,	/**< Percentage of heap size usage above which to grow the heap. Default = 80. */
	MUSE_STACK_SIZE,			/**< Integer parameter giving required stack size.	*/
	MUSE_MAX_SYMBOLS,			/**< Integer parameter giving max symbols used.		*/
	MUSE_DISCARD_DOC,			/**< Boolean parameter indicating that documentation should not be kept. Default = MUSE_FALSE. */
	MUSE_PRETTY_PRINT,			/**< Boolean parameter indicating whether write and print should indent their output. Default = MUSE_TRUE */
	MUSE_TAB_SIZE,				/**< Defaults to 4. Controls pretty printed output. */
	MUSE_DEFAULT_ATTENTION,		/**< The default attention with which a process is spawned. Defaults to 1. */
	MUSE_ENABLE_OBJC,			/**< Sets up objective C support in muSE using [] expressions. Also allocates 
								 *   its own auto-release pool. Default is MUSE_FALSE. */
	MUSE_OWN_OBJC_AUTORELEASE_POOL, /**< Creates a keeps a reference to an independent auto-release pool, which is 
									 * released when the muSE environment is destroyed. Default is MUSE_TRUE. */
	
	MUSE_NUM_PARAMETER_NAMES	/**< Not a parameter. */
} muse_env_parameter_name_t;

MUSEAPI muse_env	*muse_init_env( const int *parameters );
MUSEAPI void		muse_destroy_env( muse_env *env );
/*@}*/

/** @name Basic memory management */
/*@{*/
MUSEAPI muse_cell	muse_cons( muse_env *env, muse_cell head, muse_cell tail );
MUSEAPI muse_cell	muse_mk_int( muse_env *env, muse_int i );
MUSEAPI muse_cell	muse_mk_float( muse_env *env, muse_float f );
MUSEAPI muse_cell	muse_mk_text( muse_env *env, const muse_char *start, const muse_char *end );
MUSEAPI muse_cell	muse_mk_text_utf8( muse_env *env, const char *start, const char *end );
MUSEAPI muse_cell	muse_mk_ctext( muse_env *env, const muse_char *start );
MUSEAPI muse_cell	muse_mk_ctext_utf8( muse_env *env, const char *start );
MUSEAPI muse_cell	muse_mk_nativefn( muse_env *env, muse_nativefn_t fn, void *context );
MUSEAPI muse_cell	muse_mk_destructor( muse_env *env, muse_nativefn_t fn, void *context );
MUSEAPI muse_cell	muse_mk_anon_symbol(muse_env *env);
MUSEAPI muse_cell	muse_list( muse_env *env, const char *format, ... ); // c, i, I, f, T, t, S, s
MUSEAPI muse_cell	muse_symbol( muse_env *env, const muse_char *start, const muse_char *end );
MUSEAPI muse_cell	muse_csymbol( muse_env *env, const muse_char *sym );
MUSEAPI muse_cell	muse_symbol_utf8( muse_env *env, const char *start, const char *end );
MUSEAPI muse_cell	muse_csymbol_utf8( muse_env *env, const char *sym );
MUSEAPI muse_cell	muse_builtin_symbol( muse_env *env, muse_builtin_symbol_t s );
MUSEAPI int			muse_stack_pos(muse_env *env);
MUSEAPI void		muse_stack_unwind( muse_env *env, int stack_pos );
MUSEAPI muse_cell	muse_stack_push( muse_env *env, muse_cell obj );
MUSEAPI void		muse_trace_push( muse_env *env, const muse_char *label, muse_cell fn, muse_cell arglist );
MUSEAPI void		muse_trace_pop( muse_env *env );
MUSEAPI size_t		muse_trace_report( muse_env *env, size_t numchars, muse_char *buffer );
MUSEAPI void		muse_gc( muse_env *env, int free_cells_needed );
MUSEAPI void		muse_mark( muse_env *env, muse_cell cell );
MUSEAPI muse_boolean muse_doing_gc( muse_env *env );
/*@}*/

/** @name Cell access */
/*@{*/
MUSEAPI muse_cell_t	muse_cell_type( muse_cell cell );
MUSEAPI muse_boolean muse_isfn( muse_cell cell );
MUSEAPI muse_cell	muse_head( muse_env *env, muse_cell cell );
MUSEAPI muse_cell	muse_tail( muse_env *env, muse_cell cell );
MUSEAPI muse_cell	muse_tail_n( muse_env *env, muse_cell cell, int n );
MUSEAPI muse_int	muse_int_value( muse_env *env, muse_cell cell );
MUSEAPI muse_float	muse_float_value( muse_env *env, muse_cell cell );
MUSEAPI const muse_char *muse_text_contents( muse_env *env, muse_cell cell, int *length );
MUSEAPI void *		muse_nativefn_context( muse_env *env, muse_cell cell, muse_nativefn_t *fn );
MUSEAPI const muse_char *muse_symbol_name( muse_env *env, muse_cell sym );
MUSEAPI muse_cell	muse_symbol_value( muse_env *env, muse_cell sym );
MUSEAPI int			muse_list_length( muse_env *env, muse_cell list );
MUSEAPI muse_cell	muse_list_last( muse_env *env, muse_cell list );
MUSEAPI muse_cell	muse_list_append( muse_env *env, muse_cell head, muse_cell tail );
MUSEAPI muse_cell	muse_array_to_list( muse_env *env, int count, const muse_cell *array, int astep );
MUSEAPI muse_cell *	muse_list_to_array( muse_env *env, muse_cell list, int *lengthptr );
MUSEAPI void		muse_list_extract( muse_env *env, int count, muse_cell list, int lstep, muse_cell *array, int astep );

/**
 * A function that is called to generate elements which are collected
 * into a list by \c muse_generate_list(). 
 * 
 * @param context Arbitrary data that is passed by \c muse_generate_list to the
 * generator function, without being touched.
 * 
 * @param i An integer indicating the zero-based index of the list element that 
 * the generator function is being asked to generate.
 * 
 * @param eol If the generator function wants to end the list at the given
 * index \p i, then it can simply set \c (*eol) to \c MUSE_TRUE and return.
 * If \c (*eol) is found to be \c MUSE_FALSE, then the element returned by
 * the generator function is appended to the list.
 */ 
typedef muse_cell (*muse_list_generator_t)( muse_env *env, void *context, int i, muse_boolean *eol );
MUSEAPI muse_cell	muse_generate_list( muse_env *env, muse_list_generator_t generator, void *context );
/*@}*/

/** @name Cell editing */
/*@{*/
MUSEAPI muse_cell	muse_set_cell( muse_env *env, muse_cell cell, muse_cell head, muse_cell tail );
MUSEAPI muse_cell	muse_set_head( muse_env *env, muse_cell cell, muse_cell head );
MUSEAPI muse_cell	muse_set_tail( muse_env *env, muse_cell cell, muse_cell tail );
MUSEAPI muse_cell	muse_set_int( muse_env *env, muse_cell int_cell, muse_int value );
MUSEAPI muse_cell	muse_set_float( muse_env *env, muse_cell float_cell, muse_float value );
MUSEAPI muse_cell	muse_set_text( muse_env *env, muse_cell text, const muse_char *start, const muse_char *end );
MUSEAPI muse_cell	muse_set_ctext( muse_env *env, muse_cell text, const muse_char *start );
MUSEAPI muse_cell	muse_define( muse_env *env, muse_cell symbol, muse_cell value );
MUSEAPI muse_cell	muse_pushdef( muse_env *env, muse_cell symbol, muse_cell value );
MUSEAPI muse_cell	muse_popdef( muse_env *env, muse_cell symbol );
MUSEAPI int			muse_bindings_stack_pos( muse_env *env );
MUSEAPI void		muse_bindings_stack_unwind( muse_env *env, int pos );
MUSEAPI muse_cell	muse_dup( muse_env *env, muse_cell obj );
MUSEAPI muse_cell	*muse_find_list_element( muse_env *env, muse_cell *listptr, muse_cell element );
/*@}*/

/** @name Property lists */
/*@{*/
MUSEAPI int			muse_eq( muse_env *env, muse_cell a, muse_cell b );
MUSEAPI int			muse_equal( muse_env *env, muse_cell a, muse_cell b );
MUSEAPI int			muse_compare( muse_env *env, muse_cell a, muse_cell b );
MUSEAPI muse_cell	muse_symbol_plist( muse_env *env, muse_cell sym );
MUSEAPI muse_cell	muse_assoc( muse_env *env, muse_cell alist, muse_cell prop );
MUSEAPI muse_cell	*muse_assoc_iter( muse_env *env, muse_cell *alist, muse_cell prop );
MUSEAPI muse_cell	muse_get_prop( muse_env *env, muse_cell sym, muse_cell prop );
MUSEAPI	muse_cell	muse_get( muse_env *env, muse_cell obj, muse_cell key, muse_cell argv );
MUSEAPI muse_cell	muse_put_prop( muse_env *env, muse_cell sym, muse_cell prop, muse_cell value );
MUSEAPI	muse_cell	muse_put( muse_env *env, muse_cell obj, muse_cell prop, muse_cell argv );
MUSEAPI muse_cell	muse_search_object( muse_env *env, muse_cell obj, muse_cell member );
muse_cell 	muse_set_object_property( muse_env *env, muse_cell object, muse_cell property, muse_cell value );
MUSEAPI muse_cell	muse_get_meta( muse_env *env, muse_cell fn );
/*@}*/

/** @name I/O */
/*@{*/
MUSEAPI muse_cell	muse_load( muse_env *env, FILE *f );
/*@}*/

/** @name Evaluation */
/*@{*/
MUSEAPI muse_cell	muse_eval( muse_env *env, muse_cell sexpr, muse_boolean lazy );
MUSEAPI muse_cell	muse_evalnext( muse_env *env, muse_cell *sexpr );
MUSEAPI muse_cell	muse_eval_list( muse_env *env, muse_cell list );
MUSEAPI muse_cell	muse_apply( muse_env *env, muse_cell fn, muse_cell args, muse_boolean args_already_evaluated, muse_boolean lazy );
MUSEAPI muse_cell	muse_do( muse_env *env, muse_cell block );
MUSEAPI muse_cell	muse_quote( muse_env *env, muse_cell args );
MUSEAPI muse_boolean muse_bind_formals( muse_env *env, muse_cell formals, muse_cell values );
MUSEAPI muse_cell	muse_callcc( muse_env *env, muse_cell proc );
MUSEAPI muse_cell	muse_force( muse_env *env, muse_cell cell );
MUSEAPI muse_cell	muse_raise_error( muse_env *env, muse_cell error, muse_cell info );
MUSEAPI muse_cell	muse_bind_copy_expr( muse_env *env, muse_cell body, muse_boolean list_start );
/*@}*/

/** @name Misc */
/*@{*/
MUSEAPI muse_int	muse_hash( muse_env *env, muse_cell obj );
MUSEAPI muse_int	muse_hash_text( const muse_char *start, const muse_char *end, muse_int initial );
MUSEAPI muse_int	muse_hash_data( const unsigned char *start, const unsigned char *end, muse_int initial );
MUSEAPI void*		muse_tick();
MUSEAPI muse_int	muse_elapsed_us( void *timer );
MUSEAPI muse_int	muse_tock( void *timer );
MUSEAPI void		muse_sleep( muse_int time_us );
MUSEAPI FILE*		muse_fopen( const muse_char *filename, const muse_char *options );
/*@}*/

/** @name Diagnostics */
/*@{*/
MUSEAPI const muse_char *muse_typename( muse_cell thing );
MUSEAPI size_t		muse_sprintf( muse_env *env, muse_char *buffer, size_t maxlen, const muse_char *format, ... );
MUSEAPI void		muse_message( muse_env *env, const muse_char *context, const muse_char *format, ... );
MUSEAPI muse_boolean muse_expect( muse_env *env, const muse_char *context, const muse_char *spec, ... );
MUSEAPI muse_cell	muse_similar_symbol( muse_env *env, muse_cell symbol, int *distance );
MUSEAPI muse_cell	muse_symbol_with_value( muse_env *env, muse_cell value );
/*@}*/

/** @name Multilingual stuff */
/*@{*/
	MUSEAPI size_t	muse_unicode_to_utf8( char *out, size_t out_maxlen, const muse_char *win, size_t win_len );
	MUSEAPI size_t	muse_utf8_to_unicode( muse_char *wout, size_t wout_maxlen, const char *in, size_t in_len );
	MUSEAPI size_t	muse_utf8_size( const muse_char *wstr, size_t length );
	MUSEAPI size_t	muse_unicode_size( const char *utf8, size_t nbytes );
/*@}*/

/** @name Dynamically loading plugins */
/*@{*/
/**
 * A muSE plugin is expected to export only one function - the entry point -
 * of this type. When linking the plugin using \c muse_link_plugin, this
 * entry point function is invoked and the result returned by this function
 * is returned by \c muse_link_plugin.
 * 
 * @param module The system specific handle of the loaded dynamically linked module.
 * In Windows, this is the HMODULE returned by the \c LoadLibrary call. In Unix, its
 * the handle returned by \c dlopen.
 * 
 * @param env The muse environment which is linking the plugin. The plugin entry
 * function is expected to set its current environment to this environment pointer
 * before calling any muSE API functions.
 * 
 * @param arglist An arbitrary list of arguments for use by the plugin entry
 * point function.
 * 
 * @return Any s-expression. This return value will be returned by \c muse_link_plugin.
 */
typedef muse_cell (*muse_plugin_entry_t)( void *module, muse_env *env, muse_cell arglist );
MUSEAPI muse_cell muse_link_plugin( muse_env *env, const muse_char *path, muse_cell arglist );
/*@}*/

/** @name REPL - Read Eval Print Loop */
/*@{*/
MUSEAPI void		muse_repl(muse_env *env);
/*@}*/

/** @name Extending muSE with functional objects */
/*@{*/

/**
 * Type information for a functional object.
 */
typedef struct
{
	int magic_word; /**< Should always be 'muSE'. */
	int type_word;  /**< Should be some type specific four character word. */

	int size;
	/**<
	 * The size of the object in bytes - total.
	 */

	muse_nativefn_t fn;
	/**<
	 * The function that will be invoked when the object
	 * is used in the function position.
	 */
	
	void *(*view)( muse_env *env, int id );
	/**<
	 * A "view" is some arbitrary piece of information descriptive
	 * of this object's type, what the object can do, or what
	 * you can do with it. A particular object type can choose
	 * to provide any "view" it sees as appropriate. Currently
	 * the only view is as a 'mnad' or a "Monad view" that
	 * allows the standard map, join, filter and reduce operations
	 * on it. Its technically not a monad, but I couldn't find any
	 * other data structure term that can summarize the fact that 
	 * these functions can be used on lists, vectors, hashtables.
	 */

	void (*init)( muse_env *env, void *obj, muse_cell args );
	/**<
	 * An initializer function that's called after object creation
	 * to initialize its contents according to the given arguments.
	 */

	void (*mark)( muse_env *env, void *obj );
	/**<
	 * A type specific marker function that is expected to mark
	 * all objects referenced by this functional object using
	 * \ref muse_mark function.
	 *
	 * @param obj A pointer to this object, starting at the base
	 * muse_functional_object_t address.
	 */

	void (*destroy)( muse_env *env, void *obj );
	/**<
	 * The functional obnject will be kept on the specials stack.
	 * When no references to the object are detected, the destroy
	 * function of the object will be called and it will be
	 * removed from the environment.
	 */
	
	void (*write)( muse_env *env, void *obj, void *port );
	/**<
	 * The object should be written out in a textual format such that
	 * read (with macros and braces enabled) will be able to automatically
	 * create the object when it encounters the expression.
	 *
	 * If this function is NULL, the standard "<prim:blah>" kind of 
	 * unreadable stuff will be written out.
	 */
} muse_functional_object_type_t;

/**
 * Any muSE functional object must always begin with
 * this structure.
 */
typedef struct
{
	int magic_word; /**< Will always be 'muSE'. */
	muse_functional_object_type_t *type_info; /**< Type information that's constant for all instances. */
} muse_functional_object_t;

MUSEAPI muse_functional_object_t *muse_create_object( muse_env *env, muse_functional_object_type_t *type_info, muse_cell init_args );
MUSEAPI void muse_destroy_object( muse_env *env, muse_functional_object_t *obj );
MUSEAPI muse_cell muse_mk_functional_object( muse_env *env, muse_functional_object_type_t *type_info, muse_cell init_args );
MUSEAPI muse_functional_object_t *muse_functional_object_data( muse_env *env, muse_cell fobj, int type_word );
/*@}*/

/**
 * @name Ports API
 *
 * A port is an I/O abstraction that you can use to
 * work with objects like files and network connections.
 * Anything that satisfies a simple protocol can be used
 * as a port. All standard muSE functions such as \ref muse_pwrite
 * and \ref muse_pread will work with any port that satisfies
 * such the protocol.
 *
 * @see Ports
 */
/*@{*/
typedef struct _muse_port_base_t *muse_port_t;
typedef enum 
{
	MUSE_STDIN_PORT,
	MUSE_STDOUT_PORT,
	MUSE_STDERR_PORT
} muse_stdport_t;


/**
 * These bits are used to identify various port features,
 * whether it is for reading, writing, etc.
 */
typedef enum
{
	MUSE_PORT_READ					= 1,
		/**<
		 * Indicates that the port is to be used for input.
		 */

	MUSE_PORT_WRITE					= 2,
		/**<
		 * Indicates that the port is to be used for output.
		 */

	MUSE_PORT_READ_WRITE			= MUSE_PORT_READ | MUSE_PORT_WRITE,
		/**<
		 * Convenient definition for bi-directional ports.
		 */

	MUSE_PORT_EZSCHEME				= 4,
		/**<
		 * Says that the input from the given port is uses
		 * the EZSCHEME syntax.
		 */

	MUSE_PORT_READ_EXPAND_BRACES	= 0x10,
		/**<
		 * Allows muse_pread() function to expand s-expressions
		 * that use braces instead of parentheses. This is enabled
		 * by default for standard input and for files from which
		 * code is loaded using muse_load().
		 */

	MUSE_PORT_READ_DETECT_MACROS	= 0x20,
		/**<
		 * Allows muse_pread() function to detect and expand macro
		 * expressions. Macro expressions are those parentheses 
		 * delimited list expressions with a macro symbol as the first
		 * list element. A macro symbol is a symbol that is defined to
		 * a macro function at the point at which the macro expression
		 * is encountered.
		 *
		 * Macro expression detection is always disabled when reading quoted
		 * expressions, though brace expansion may be active.
		 */

	MUSE_PORT_TRUSTED_INPUT			= MUSE_PORT_READ | MUSE_PORT_READ_EXPAND_BRACES | MUSE_PORT_READ_DETECT_MACROS
		/**<
		 * Convenience definition indicating that input from the given
		 * port can be trusted, therefore enabling macro expansion.
		 * Standard input and file ports using by muse_load() are
		 * considered to be trusted input sources. 
		 */

} muse_port_mode_bits_t;

MUSEAPI muse_port_t muse_port( muse_env *env, muse_cell p );
MUSEAPI muse_port_t muse_stdport( muse_env *env, muse_stdport_t descriptor );
MUSEAPI muse_port_t muse_assign_port( muse_env *env, FILE *f, int mode );
MUSEAPI void		muse_unassign_port( muse_port_t p );
MUSEAPI muse_cell	muse_pread( muse_port_t port );
MUSEAPI void		muse_pwrite( muse_port_t port, muse_cell sexpr );
MUSEAPI void		muse_pprint( muse_port_t port, muse_cell sexpr );
MUSEAPI void		muse_mickey( muse_port_t in, muse_port_t out );
MUSEAPI muse_port_t muse_create_memport( muse_env *env );
MUSEAPI muse_cell	muse_read_xml_node( muse_port_t in );
MUSEAPI muse_port_t muse_current_port( muse_env *env, muse_stdport_t which, muse_port_t port );
/*@}*/

/**
 * @name Data structure API
 *
 * Provides functions to create and use vectors and hash tables via the API.
 * Its available at the language level even without this API.
 */
/*@{*/
/** @name Vectors */
/*@{*/
MUSEAPI muse_cell	muse_mk_vector( muse_env *env, int length );
MUSEAPI int			muse_vector_length( muse_env *env, muse_cell vec );
MUSEAPI muse_cell	muse_vector_get( muse_env *env, muse_cell vec, int index );
MUSEAPI muse_cell	muse_vector_put( muse_env *env, muse_cell vec, int index, muse_cell value );
/*@}*/
/** @name Hashtables */
/*@{*/
MUSEAPI muse_cell	muse_mk_hashtable( muse_env *env, int length );
MUSEAPI int			muse_hashtable_length( muse_env *env, muse_cell ht );
MUSEAPI muse_cell	muse_hashtable_get( muse_env *env, muse_cell ht, muse_cell key );
MUSEAPI muse_cell	muse_hashtable_put( muse_env *env, muse_cell ht, muse_cell key, muse_cell value );
/*@}*/
/** @name Boxes */
/*@{*/
MUSEAPI muse_cell	muse_mk_box( muse_env *env, muse_cell contents );
MUSEAPI muse_cell	muse_box_get( muse_env *env, muse_cell box );
MUSEAPI muse_cell	muse_box_set( muse_env *env, muse_cell box, muse_cell contents );
/*@}*/
/*@}*/

/*@}*/

/**
 * @name Views
 */
/*@{*/
/**
 * A monad view (id = 'mnad') provides higher order functional
 * operations over collections.
 */
typedef struct
{
	muse_cell (*size)( muse_env *env, void *self );
	/**<
	 * Should return the number of elements in the data structure.
	 */
	
	muse_cell (*map)( muse_env *env, void *self, muse_cell fn );
	/**<
	 * Iterates the given function over all the "elements" of this object
	 * and constructs a new object of the same type with the results.
	 */
	
	muse_cell (*join)( muse_env *env, void *self, muse_cell obj, muse_cell reduction_fn );
	/**<
	 * Joins two objects (must be of the same type) using the given reduction_fn
	 * to resolve conflicts. Two vectors are joined by concatenation. Two lists are
	 * are joined by concatenation. Two hashtables are joined by merging their
	 * key-value pairs into a single table (you can do set union using this).
	 */
	
	muse_cell (*collect)( muse_env *env, void *self, muse_cell predicate, muse_cell mapper, muse_cell reduction_fn );
	/**<
	 * Iterates over the elements of this object, applying the \p predicate.
	 * All the elements satisfying the predicate are collected into a new
	 * object of the same type. If the \p mapper parameter is not MUSE_NIL,
	 * it is expected to be a function that will be used to transform the
	 * element before collecting it into the new object. If the \p mapper is
	 * MUSE_NIL, it is equivalent to an identity function.
	 */
	
	muse_cell (*reduce)( muse_env *env, void *self, muse_cell reduction_fn, muse_cell initial );	
	/**<
	 * Applies the given reduction function, which is expected to be
	 * commutative and associative for simplicity, and folds the
	 * results into successive reduction calls, returning the final
	 * outcome as the result.
	 */
} muse_monad_view_t;
/*@}*/
 
typedef muse_boolean (*muse_iterator_callback_t)( muse_env *env, void *self, void *context, muse_cell entry );
/**<
 * An iterator function that is invoked by the "iterator" monad operator on all elements
 * of a collection.
 *
 * @param self This is the object itself.
 * @param context The arbitrary context data pointer passed to the iterator function.
 * @param entry The next object to be iterated over.
 * @return Returning MUSE_TRUE will continue the iteration. Returning MUSE_FALSE will stop it.
 */

typedef muse_cell (*muse_iterator_t)( muse_env *env, void *self, muse_iterator_callback_t callback, void *context );
/**<
 * An 'iter' view will provide this function to call that can be used to iterate over collections.
 */

typedef muse_cell (*muse_datafn_t)( muse_env *env, void *self, muse_cell datafn );
/**<
 * A 'dtfn' view will return this function which you can call to set the behaviour
 * of a data structure to be that of a function. Whenever a value is not found in the
 * data structure (such as vector or hashtable), the function \p datafn will be invoked
 * to determine the value. The result of the function should be cached for future accesses.
 *
 * The return value will be the value of the data function before changing it to \p datafn.
 */

/**
 * A property view (id = 'prop') says that an object support get and set
 * methods for properties. Ay object supporting the 'prop' view can be used
 * with \ref fn_get "get" and \ref fn_put "put" functions.
 */
typedef struct {

	muse_cell (*get_prop)( muse_env *env, void *self, muse_cell key, muse_cell argv );
	/**<
	 * Gets the value of the property associated with the given key.
	 */

	muse_cell (*put_prop)( muse_env *env, void *self, muse_cell key, muse_cell argv );
	/**<
	 * Modifies the value of the property identified by \p key to the
	 * given \p value. The return value can be any value that the
	 * object deems relevant. It is generally the value itself. The
	 * return value should be documented in the specific object. Usually
	 * one doesn't need the return value, but occasionally it might be
	 * useful in an object specific way.
	 */
} muse_prop_view_t;

/**
 * An object can expose a scope view (id = 'scop') if it defines some
 * special bindings within the scope of the expression 
 * with the object in the function position.
 */
typedef struct {
	muse_cell (*begin)( muse_env *env, void *self, muse_cell expr );
		/**<
		 * Called when an object is found at the head of an s-expression
		 * during \ref muse_bind_copy_expr in order to do any special
		 * handling of the tail part. The \p expr passed in already
		 * has the head modified to the object whose \c begin is
		 * being invoked, but the rest of the expression is passed
		 * untouched. 
		 *
		 * Thre return value is expected to be an expression that will
		 * be used in place of the original expression. The tail
		 * part of the expression usualy needs to be bind-copied
		 * using \ref muse_bind_copy_expr.
		 */

	void (*end)( muse_env *env, void *self, int bsp );
		/**<
		 * Signals the end of the scope of the object.
		 * Any special bindings created in begin must be
		 * undone in end.
		 *
		 * The bindings stack position is passed in \p bsp. 
		 * This is the stack position before the begin.
		 * It is up to the object to unwind the stack
		 * to the given bsp.
		 */
} muse_scope_view_t;

END_MUSE_C_FUNCTIONS

#endif /* __MUSE_H__ */
