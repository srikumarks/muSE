/**
 * @file muse_builtins.h
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#ifndef __MUSE_BUILTINS_H__
#define __MUSE_BUILTINS_H__

#include "muse_opcodes.h"

#ifndef __MUSE_BUILTIN_MATH_H__
#include "muse_builtin_math.h"
#endif

/** @addtogroup CoreLanguage Core language */
/*@{*/
muse_cell fn_quote( muse_env *env, void *context, muse_cell args );
muse_cell fn_lazy( muse_env *env, void *context, muse_cell args );
muse_cell fn_cons( muse_env *env, void *context, muse_cell args );
muse_cell fn_lcons( muse_env *env, void *context, muse_cell args );
muse_cell syntax_lambda( muse_env *env, void *context, muse_cell args );
muse_cell syntax_block( muse_env *env, void *context, muse_cell args );
muse_cell syntax_generic_lambda( muse_env *env, void *context, muse_cell args );
muse_cell syntax_generic_block( muse_env *env, void *context, muse_cell args );
muse_cell syntax_let( muse_env *env, void *context, muse_cell args );
muse_cell syntax_case( muse_env *env, void *context, muse_cell args );
muse_cell fn_apply( muse_env *env, void *context, muse_cell args );
muse_cell fn_apply_w_keywords( muse_env *env, void *context, muse_cell args );
muse_cell fn_eval( muse_env *env, void *context, muse_cell args );
muse_cell fn_call_w_keywords( muse_env *env, void *context, muse_cell args );
muse_cell fn_callcc( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup Exceptions Raising and handling exceptions */
/*@{*/
muse_cell syntax_try( muse_env *env, void *context, muse_cell args );
muse_cell fn_raise( muse_env *env, void *context, muse_cell args );
muse_cell fn_retry( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup PropertyLists Property lists */
/*@{*/
muse_cell fn_get( muse_env *env, void *context, muse_cell args);
muse_cell fn_put( muse_env *env, void *context, muse_cell args);
muse_cell fn_assoc( muse_env *env, void *context, muse_cell args);
muse_cell fn_plist( muse_env *env, void *context, muse_cell args);
muse_cell fn_symbol( muse_env *env, void *context, muse_cell args );
muse_cell fn_name( muse_env *env, void *context, muse_cell args ); 
muse_cell fn_gensym( muse_env *env, void *context, muse_cell args ); 
/*@}*/

/** @addtogroup CellManipulation Basic cell, list and symbol manipulation */
/*@{*/
muse_cell fn_define( muse_env *env, void *context, muse_cell args );
muse_cell fn_define_extension( muse_env *env, void *context, muse_cell args );
muse_cell fn_define_override( muse_env *env, void *context, muse_cell args );
muse_cell fn_set_M( muse_env *env, void *context, muse_cell args );
muse_cell fn_setf_M( muse_env *env, void *context, muse_cell args );
muse_cell fn_setr_M( muse_env *env, void *context, muse_cell args );
muse_cell fn_first( muse_env *env, void *context, muse_cell args );
muse_cell fn_rest( muse_env *env, void *context, muse_cell args );
muse_cell fn_nth( muse_env *env, void *context, muse_cell args );
muse_cell fn_take( muse_env *env, void *context, muse_cell args );
muse_cell fn_drop( muse_env *env, void *context, muse_cell args );
muse_cell fn_dup( muse_env *env, void *context, muse_cell args );
muse_cell fn_list( muse_env *env, void *context, muse_cell args );
muse_cell fn_append_M( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup HOFs Higher order functions */
/*@{*/
muse_cell fn_length( muse_env *env, void *context, muse_cell args );
muse_cell fn_map( muse_env *env, void *context, muse_cell args );
muse_cell fn_join( muse_env *env, void *context, muse_cell args );
muse_cell fn_collect( muse_env *env, void *context, muse_cell args );
muse_cell fn_reduce( muse_env *env, void *context, muse_cell args );
muse_cell fn_find( muse_env *env, void *context, muse_cell args );
muse_cell fn_andmap( muse_env *env, void *context, muse_cell args );
muse_cell fn_ormap( muse_env *env, void *context, muse_cell args );
muse_cell fn_for_each( muse_env *env, void *context, muse_cell args );
muse_cell fn_transpose( muse_env *env, void *context, muse_cell args );
muse_cell fn_datafn( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup Comparisons Comparisons */
/*@{*/
muse_cell fn_eq( muse_env *env, void *context, muse_cell args );
muse_cell fn_equal( muse_env *env, void *context, muse_cell args );
muse_cell fn_lt( muse_env *env, void *context, muse_cell args );
muse_cell fn_gt( muse_env *env, void *context, muse_cell args );
muse_cell fn_le( muse_env *env, void *context, muse_cell args );
muse_cell fn_ge( muse_env *env, void *context, muse_cell args );
muse_cell fn_ne( muse_env *env, void *context, muse_cell args );
muse_cell fn_and( muse_env *env, void *context, muse_cell args );
muse_cell fn_or( muse_env *env, void *context, muse_cell args );
muse_cell fn_not( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup LanguageConstructs Language constructs */
/*@{*/
muse_cell syntax_if( muse_env *env, void *context, muse_cell args );
muse_cell syntax_when( muse_env *env, void *context, muse_cell args ); 
muse_cell syntax_unless( muse_env *env, void *context, muse_cell args ); 
muse_cell syntax_cond( muse_env *env, void *context, muse_cell args );
muse_cell syntax_do( muse_env *env, void *context, muse_cell args );
muse_cell syntax_while( muse_env *env, void *context, muse_cell args );
muse_cell syntax_for( muse_env *env, void *context, muse_cell args );
muse_cell fn_stats( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup TypeChecking Type checking */
/*@{*/
muse_cell fn_int_p( muse_env *env, void *context, muse_cell args );
muse_cell fn_float_p( muse_env *env, void *context, muse_cell args );
muse_cell fn_number_p( muse_env *env, void *context, muse_cell args );
muse_cell fn_cons_p( muse_env *env, void *context, muse_cell args );
muse_cell fn_fn_p( muse_env *env, void *context, muse_cell args );
muse_cell fn_symbol_p( muse_env *env, void *context, muse_cell args );
muse_cell fn_string_p( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup TypeCasting Type casting */
/*@{*/
muse_cell fn_int( muse_env *env, void *context, muse_cell args );
muse_cell fn_float( muse_env *env, void *context, muse_cell args );
muse_cell fn_number( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup ObjectSystem Object system */
/*@{*/
muse_cell fn_class( muse_env *env, void *context, muse_cell args );
muse_cell fn_new( muse_env *env, void *context, muse_cell args );
muse_cell fn_obj_pty( muse_env *env, void *context, muse_cell args );
muse_cell fn_send( muse_env *env, void *context, muse_cell args );
muse_cell fn_send_super( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup Algorithms Algorithms */
/*@{*/
muse_cell fn_sort_inplace( muse_env *env, void *context, muse_cell args );
muse_cell fn_sort( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup PortIO Port I/O */
/*@{*/
muse_cell fn_port_p( muse_env *env, void *context, muse_cell args );
muse_cell fn_close( muse_env *env, void *context, muse_cell args );
muse_cell fn_eof_p( muse_env *env, void *context, muse_cell args );
muse_cell fn_print( muse_env *env, void *context, muse_cell args );
muse_cell fn_write( muse_env *env, void *context, muse_cell args );
muse_cell fn_read( muse_env *env, void *context, muse_cell args );
muse_cell fn_flush( muse_env *env, void *context, muse_cell args );
muse_cell fn_load( muse_env *env, void *context, muse_cell args );
muse_cell fn_write_xml( muse_env *env, void *context, muse_cell args );
muse_cell fn_read_xml( muse_env *env, void *context, muse_cell args );
muse_cell fn_exit( muse_env *env, void *context, muse_cell args );
/*@}*/

/** 
 * @addtogroup Processes Processes 
 *
 * The "processes" branch of muSE has an implementation of a notion of
 * processes as an abstraction that helps model concurrently running
 * tasks. Every muSE environment is initialized with a <em>main process</em>
 * through which all API calls enter and exit. A muSE expression may 
 * spawn off additional processes using the \ref fn_spawn "spawn" 
 * function. The evaluator may switch to another process upon any
 * invocation of muse_apply(). Therefore, process switching appears
 * to be pre-emptive at the Scheme code level, but evaluation is
 * atomic at the C code level unless explicitly pre-empted in a muse_apply(). 
 * You can force the evaluation of any expression to occur atomically 
 * using the \ref syntax_atomic "atomic" construct.
 *
 * The basic mechanism by which processes communicate is by sending
 * messages to each other. A process's \ref fn_pid "pid" serves as
 * the function to use to send it messages, which can be arbitrary
 * s-expressions. A process receives messages sent to its mailbox 
 * using the \ref fn_receive "receive" function, which might block
 * until a message is available or a timeout is reached.
 *
 * It is possible for any process to yield an arbitrary chunk of time
 * to other processes using the \ref fn_run "run" expression.
 *
 * Each process is given its own namespace of symbol values. This means
 * a process cannot change the value of a symbol that is being used by
 * another process. When one process spawns another, the created process
 * inherits the creating process's symbol values. If it subsequently 
 * changes the symbol values, the changes are not visible to the process
 * that created it. However, if two processes share an object such as a 
 * list, both processes can modify the contents of the object. The only 
 * constructs available to coordinate such modifications are
 * message passing and atomic blocks.
 */
/*@{*/
muse_cell fn_spawn( muse_env *env, void *context, muse_cell args );
muse_cell fn_this_process( muse_env *env, void *context, muse_cell args );
muse_cell syntax_atomic( muse_env *env, void *context, muse_cell args );
muse_cell fn_receive( muse_env *env, void *context, muse_cell args );
muse_cell fn_run( muse_env *env, void *context, muse_cell args );
muse_cell fn_post( muse_env *env, void *context, muse_cell args );
muse_cell fn_process_p( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @name Misc */
/*@{*/
muse_cell fn_format( muse_env *env, void *context, muse_cell args ); 
muse_cell fn_string_length( muse_env *env, void *context, muse_cell args );
muse_cell fn_time_taken_us( muse_env *env, void *context, muse_cell args );
muse_cell fn_generate_documentation( muse_env *env, void *context, muse_cell args );
muse_cell fn_load_plugin( muse_env *env, void *context, muse_cell args );
muse_cell fn_list_files( muse_env *env, void *context, muse_cell args );
muse_cell fn_list_folders( muse_env *env, void *context, muse_cell args );
void muse_load_builtin_fns( muse_env *env );
/*@}*/

/** 
 * @addtogroup FunctionalObjects Functional objects 
 *
 * A functional object is a general way to add natively implemented
 * objects to muSE. For each such new type, you define a global
 * type info structure \ref muse_functional_object_type_t
 * that holds the type information of the object, including
 * methods to keep track of cell references held by the object,
 * how to destroy it, etc.
 *
 * muSE ports are also implemented as functional objects, though
 * they don't have an associated function and have to be used
 * through the generic "read", "write" and such functions.
 */
/*@{*/
void muse_define_builtin_type_vector(muse_env *env);
void muse_define_builtin_type_hashtable(muse_env *env);
void muse_define_builtin_type_bytes( muse_env *env );
/*@}*/

void muse_define_builtin_networking(muse_env *env);

#endif /* __MUSE_BUILTINS_H__ */
