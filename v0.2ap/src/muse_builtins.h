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
muse_cell fn_cons( muse_env *env, void *context, muse_cell args );
muse_cell syntax_lambda( muse_env *env, void *context, muse_cell args );
muse_cell syntax_block( muse_env *env, void *context, muse_cell args );
muse_cell syntax_let( muse_env *env, void *context, muse_cell args );
muse_cell syntax_case( muse_env *env, void *context, muse_cell args );
muse_cell fn_apply( muse_env *env, void *context, muse_cell args );
muse_cell fn_eval( muse_env *env, void *context, muse_cell args );
muse_cell fn_callcc( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup PropertyLists Property lists */
/*@{*/
muse_cell fn_get( muse_env *env, void *context, muse_cell args);
muse_cell fn_put( muse_env *env, void *context, muse_cell args);
muse_cell fn_assoc( muse_env *env, void *context, muse_cell args);
muse_cell fn_plist( muse_env *env, void *context, muse_cell args);
muse_cell fn_symbol( muse_env *env, void *context, muse_cell args );
muse_cell fn_name( muse_env *env, void *context, muse_cell args ); 
/*@}*/

/** @addtogroup CellManipulation Basic cell, list and symbol manipulation */
/*@{*/
muse_cell fn_define( muse_env *env, void *context, muse_cell args );
muse_cell fn_set_M( muse_env *env, void *context, muse_cell args );
muse_cell fn_setf_M( muse_env *env, void *context, muse_cell args );
muse_cell fn_setr_M( muse_env *env, void *context, muse_cell args );
muse_cell fn_first( muse_env *env, void *context, muse_cell args );
muse_cell fn_rest( muse_env *env, void *context, muse_cell args );
muse_cell fn_next( muse_env *env, void *context, muse_cell args );
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
muse_cell fn_exit( muse_env *env, void *context, muse_cell args );
/*@}*/

/** @addtogroup Processes Processes */
/*@{*/
muse_cell fn_spawn( muse_env *env, void *context, muse_cell args );
muse_cell fn_atomic( muse_env *env, void *context, muse_cell args );
muse_cell fn_receive( muse_env *env, void *context, muse_cell args );
muse_cell fn_run( muse_env *env, void *context, muse_cell args );
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
void muse_load_builtin_fns();
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
void muse_define_builtin_type_vector();
void muse_define_builtin_type_hashtable();
/*@}*/

void muse_define_builtin_networking();

#endif /* __MUSE_BUILTINS_H__ */
