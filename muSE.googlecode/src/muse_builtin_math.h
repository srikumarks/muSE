/**
 * @file muse_builtin_math.h
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#ifndef __MUSE_BUILTIN_MATH_H__
#define __MUSE_BUILTIN_MATH_H__

#ifndef __MUSE_OPCODES_H__
#include "muse_opcodes.h"
#endif

BEGIN_MUSE_C_FUNCTIONS

muse_cell fn_add( muse_env *env, void *context, muse_cell args );
muse_cell fn_sub( muse_env *env, void *context, muse_cell args );
muse_cell fn_mul( muse_env *env, void *context, muse_cell args );
muse_cell fn_div( muse_env *env, void *context, muse_cell args );
muse_cell fn_idiv( muse_env *env, void *context, muse_cell args );
muse_cell fn_mod( muse_env *env, void *context, muse_cell args );

muse_cell fn_inc( muse_env *env, void *context, muse_cell args );
muse_cell fn_dec( muse_env *env, void *context, muse_cell args );
muse_cell fn_trunc( muse_env *env, void *context, muse_cell args );
muse_cell fn_rand( muse_env *env, void *context, muse_cell args );
muse_cell fn_pow( muse_env *env, void *context, muse_cell args );

void muse_math_load_common_unary_functions( muse_env *env );

END_MUSE_C_FUNCTIONS

#endif /* __MUSE_BUILTIN_MATH_H__ */