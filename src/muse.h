/**
 * @file muse.h
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-scheme.googlecode.com/svn/trunk/LICENSE.txt
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
 * 	- Small code foot-print (45kB for full interpreter)
 * 	- Simple garbage collector (< 1ms for 60000 cells)
 * 	- Simple C integration API for compute-intensive algorithms
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
 * 	- \ref syntax_case "case"
 * 	- \ref syntax_while "while", \ref syntax_for "for"
 *	- \ref syntax_if "if", \ref syntax_cond "cond"
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
 * @subsection ML_ObjectSystem Object system
 * 	- \ref fn_class "class"
 * 	- \ref fn_new "new"
 * 	- \ref fn_send "<-"
 * 	- \ref fn_obj_pty "->"
 * 
 * @subsection ML_IO Input and output
 * 	- \ref fn_print "print"
 * 	- \ref fn_write "write"
 * 	- \ref fn_read "read"
 *	- \ref fn_open_file "open-file"
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
 * structures is the "cons cell", aka a "pair" (\ref MUSE_CONS_CELL, \ref muse_cons).
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
	MUSE_TEXT_CELL			/**< Holds an immutable copy of a wide character string - as ptrs to beginning and end. */
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
	MUSE_RETURN,
	MUSE_BREAK,
	MUSE_CLASS,
	MUSE_SUPER,
	MUSE_DOC,
	MUSE_CODE,
	MUSE_SIGNATURE,
	MUSE_USAGE,
	MUSE_BRIEF,
	MUSE_DESCR,
	MUSE_TIMEOUT,
	MUSE_DEFINE,
	
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
	
	MUSE_NUM_PARAMETER_NAMES	/**< Not a parameter. */
} muse_env_parameter_name_t;

muse_env	*muse_init_env( const int *parameters );
void		muse_destroy_env( muse_env *env );
muse_env	*muse_get_current_env();
muse_env	*muse_set_current_env( muse_env *env );
/*@}*/

/** @name Basic memory management */
/*@{*/
muse_cell	muse_cons( muse_cell head, muse_cell tail );
muse_cell	muse_mk_int( muse_int i );
muse_cell	muse_mk_float( muse_float f );
muse_cell	muse_mk_text( const muse_char *start, const muse_char *end );
muse_cell	muse_mk_text_utf8( const char *start, const char *end );
muse_cell	muse_mk_ctext( const muse_char *start );
muse_cell	muse_mk_ctext_utf8( const char *start );
muse_cell	muse_mk_nativefn( muse_nativefn_t fn, void *context );
muse_cell	muse_mk_destructor( muse_nativefn_t fn, void *context );
muse_cell	muse_mk_anon_symbol();
muse_cell	muse_list( const char *format, ... ); // c, i, I, f, T, t, S, s
muse_cell	muse_symbol( const muse_char *start, const muse_char *end );
muse_cell	muse_csymbol( const muse_char *sym );
muse_cell	muse_symbol_utf8( const char *start, const char *end );
muse_cell	muse_csymbol_utf8( const char *sym );
muse_cell	muse_builtin_symbol( muse_builtin_symbol_t s );
int			muse_stack_pos();
void		muse_stack_unwind( int stack_pos );
muse_cell	muse_stack_push( muse_cell obj );
void		muse_gc( int free_cells_needed );
void		muse_mark( muse_cell cell );
/*@}*/

/** @name Cell access */
/*@{*/
muse_cell_t	muse_cell_type( muse_cell cell );
muse_boolean muse_isfn( muse_cell cell );
muse_cell	muse_head( muse_cell cell );
muse_cell	muse_tail( muse_cell cell );
muse_cell	muse_tail_n( muse_cell cell, int n );
muse_cell	muse_next( muse_cell cell );
muse_int		muse_int_value( muse_cell cell );
muse_float	muse_float_value( muse_cell cell );
const muse_char *muse_text_contents( muse_cell cell, int *length );
const muse_char *muse_symbol_name( muse_cell sym );
muse_cell	muse_symbol_value( muse_cell sym );
int			muse_list_length( muse_cell list );
muse_cell	muse_list_last( muse_cell list );
muse_cell	muse_list_append( muse_cell head, muse_cell tail );
muse_cell	muse_array_to_list( int count, const muse_cell *array, int astep );
muse_cell *	muse_list_to_array( muse_cell list, int *lengthptr );
void		muse_list_extract( int count, muse_cell list, int lstep, muse_cell *array, int astep );

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
typedef muse_cell (*muse_list_generator_t)( void *context, int i, muse_boolean *eol );
muse_cell	muse_generate_list( muse_list_generator_t generator, void *context );
/*@}*/

/** @name Cell editing */
/*@{*/
muse_cell	muse_set_cell( muse_cell cell, muse_cell head, muse_cell tail );
muse_cell	muse_set_head( muse_cell cell, muse_cell head );
muse_cell	muse_set_tail( muse_cell cell, muse_cell tail );
muse_cell	muse_set_int( muse_cell int_cell, muse_int value );
muse_cell	muse_set_float( muse_cell float_cell, muse_float value );
muse_cell	muse_set_text( muse_cell text, const muse_char *start, const muse_char *end );
muse_cell	muse_set_ctext( muse_cell text, const muse_char *start );
muse_cell	muse_define( muse_cell symbol, muse_cell value );
muse_cell	muse_pushdef( muse_cell symbol, muse_cell value );
muse_cell	muse_popdef( muse_cell symbol );
muse_cell	muse_dup( muse_cell obj );
muse_cell	*muse_find_list_element( muse_cell *listptr, muse_cell element );
/*@}*/

/** @name Property lists */
/*@{*/
int			muse_eq( muse_cell a, muse_cell b );
int			muse_equal( muse_cell a, muse_cell b );
int			muse_compare( muse_cell a, muse_cell b );
muse_cell	muse_symbol_plist( muse_cell sym );
muse_cell	muse_assoc( muse_cell alist, muse_cell prop );
muse_cell	*muse_assoc_iter( muse_cell *alist, muse_cell prop );
muse_cell	muse_get_prop( muse_cell sym, muse_cell prop );
muse_cell	muse_put_prop( muse_cell sym, muse_cell prop, muse_cell value );
/*@}*/

/** @name I/O */
/*@{*/
muse_cell	muse_load( FILE *f );
/*@}*/

/** @name Evaluation */
/*@{*/
muse_cell	muse_eval( muse_cell sexpr );
muse_cell	muse_evalnext( muse_cell *sexpr );
muse_cell	muse_eval_list( muse_cell list );
muse_cell	muse_apply( muse_cell fn, muse_cell args, muse_boolean args_already_evaluated );
muse_cell	muse_do( muse_cell block );
muse_cell	muse_quote( muse_cell args );
muse_boolean muse_bind_formals( muse_cell formals, muse_cell values );
muse_cell	muse_callcc( muse_cell proc );
/*@}*/

/** @name Misc */
/*@{*/
muse_int	muse_hash( muse_cell obj );
muse_int	muse_hash_text( const muse_char *start, const muse_char *end, muse_int initial );
muse_int	muse_hash_data( const unsigned char *start, const unsigned char *end, muse_int initial );
void*		muse_tick();
muse_int	muse_tock(void*);
void		muse_sleep( muse_int time_us );
FILE*		muse_fopen( const muse_char *filename, const muse_char *options );
/*@}*/

/** @name Diagnostics */
/*@{*/
const muse_char *muse_typename( muse_cell thing );
int			muse_sprintf( muse_char *buffer, int maxlen, const muse_char *format, ... );
void		muse_message( const muse_char *context, const muse_char *format, ... );
muse_boolean muse_expect( const muse_char *context, const muse_char *spec, ... );
muse_cell	muse_similar_symbol( muse_cell symbol, int *distance );
muse_cell	muse_symbol_with_value( muse_cell value );
/*@}*/

/** @name Multilingual stuff */
/*@{*/
	int		muse_unicode_to_utf8( char *out, int out_maxlen, const muse_char *win, int win_len );
	int		muse_utf8_to_unicode( muse_char *wout, int wout_maxlen, const char *in, int in_len );
	int		muse_utf8_size( const muse_char *wstr, int length );
	int		muse_unicode_size( const char *utf8, int nbytes );
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
muse_cell muse_link_plugin( const muse_char *path, muse_cell arglist );
/*@}*/

/** @name REPL - Read Eval Print Loop */
/*@{*/
void		muse_repl();
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

	void (*init)( void *obj, muse_cell args );
	/**<
	 * An initializer function that's called after object creation
	 * to initialize its contents according to the given arguments.
	 */

	void (*mark)( void *obj );
	/**<
	 * A type specific marker function that is expected to mark
	 * all objects referenced by this functional object using
	 * \ref muse_mark function.
	 *
	 * @param obj A pointer to this object, starting at the base
	 * muse_functional_object_t address.
	 */

	void (*destroy)( void *obj );
	/**<
	 * The functional obnject will be kept on the specials stack.
	 * When no references to the object are detected, the destroy
	 * function of the object will be called and it will be
	 * removed from the environment.
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

muse_cell muse_mk_functional_object( muse_functional_object_type_t *type_info, muse_cell init_args );
muse_functional_object_t *muse_functional_object_data( muse_cell fobj, int type_word );
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

muse_port_t muse_port( muse_cell p );
muse_port_t muse_stdport( muse_stdport_t descriptor );
muse_port_t muse_assign_port( FILE *f, int mode );
void		muse_unassign_port( muse_port_t p );
muse_cell	muse_pread( muse_port_t port );
void		muse_pwrite( muse_port_t port, muse_cell sexpr );
void		muse_pprint( muse_port_t port, muse_cell sexpr );
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
muse_cell	muse_mk_vector( int length );
int			muse_vector_length( muse_cell vec );
muse_cell	muse_vector_get( muse_cell vec, int index );
muse_cell	muse_vector_put( muse_cell vec, int index, muse_cell value );
/*@}*/
/** @name Hashtables */
muse_cell	muse_mk_hashtable( int length );
int			muse_hashtable_length( muse_cell ht );
muse_cell	muse_hashtable_get( muse_cell ht, muse_cell key );
muse_cell	muse_hashtable_put( muse_cell ht, muse_cell key, muse_cell value );
/*@}*/
/*@}*/

/*@}*/

END_MUSE_C_FUNCTIONS

#endif /* __MUSE_H__ */
