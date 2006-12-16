/**
 * @file muse_platform.h
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#ifndef __MUSE_PLATFORM_H__
#define __MUSE_PLATFORM_H__

#include "muse_config.h"

#ifdef _WIN32
#	include "muse_win32.h"
#else
#	define MUSE_FMT_INT		"%lld"
#	define MUSE_FMT_FLOAT	"%.10lg"
#	define MUSE_FMT_STRING	"%ls"
#	define MUSE_FMT_QSTRING	"\"%ls\""

typedef long long longlong_t;

#ifdef __DARWIN_PPC__
#	define SAVE_STACK_POINTER( var ) void *var = NULL; asm { stw r1, var; }

#	define CHANGE_STACK_POINTER(sp_value) \
        	                        do\
                	                {\
									void *new_sp = (void*)(sp_value);\
									asm { lwz r1, new_sp; };\
                                	}\
                                	while(0)
#endif

#define MUSE_PLATFORM_POSIX 1

#endif

#ifdef __cplusplus
#	define BEGIN_MUSE_C_FUNCTIONS  extern "C" {
#	define END_MUSE_C_FUNCTIONS }
#else
#	define BEGIN_MUSE_C_FUNCTIONS 
#	define END_MUSE_C_FUNCTIONS
#endif

#include <setjmp.h>

#ifdef MUSE_DEBUG_BUILD
	void muse_assert_failed( const char *file, int line, const char *condtext );
#	define muse_assert( cond ) do { if ( !(cond) ) muse_assert_failed( __FILE__, __LINE__, #cond ); } while(0)
#else
#	define muse_assert( cond )
#endif

#if MUSE_DIAGNOSTICS_LEVEL > 0
	#define MUSE_DIAGNOSTICS(statement) do { statement; } while (0)
#else
	#define MUSE_DIAGNOSTICS(statement)
#endif

#if MUSE_DIAGNOSTICS_LEVEL > 1
	#define MUSE_DIAGNOSTICS2(statement) do { statement; } while (0)
#else
	#define MUSE_DIAGNOSTICS2(statement)
#endif	

#if MUSE_DIAGNOSTICS_LEVEL > 2
	#define MUSE_DIAGNOSTICS3(statement) do { statement; } while (0)
#else
	#define MUSE_DIAGNOSTICS3(statement)
#endif

#endif /* __MUSE_PLATFORM_H__ */
