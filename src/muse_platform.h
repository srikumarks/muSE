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

#if __ppc__
#	define SAVE_STACK_POINTER( var ) \
			void *var = NULL; \
			asm { stw r1, var; };

#	define CHANGE_STACK_POINTER(sp_value) \
        	                        do\
                	                {\
									void *new_sp = (void*)(sp_value);\
									asm { lwz r1, new_sp; };\
                                	}\
                                	while(0)
#endif

#if __i386
#	define SAVE_STACK_POINTER( var ) \
			void *var = NULL; \
			asm("movl %%esp, %0;" : "=r"(var));

#	define CHANGE_STACK_POINTER(sp_value) \
			do\
			{\
				void *new_sp = (void*)(sp_value);\
				asm("push %0; pop %%esp;" : : "r"(new_sp));\
			}\
			while(0)
#endif

// Thanks to contribution by cjames... here -
// http://code.google.com/p/muvee-symbolic-expressions/issues/detail?id=36
//
// See here - http://stackoverflow.com/questions/1753602/registers-for-x86-64-processors
// for a summary of the 64-bit registers. The stack pointer "esp"
// becomes "rsp" in the 64-bit asm. Also, the "q" asms "movq" and "pushq"
// work with 64-bit values ("q" for "quad words", I think).
#if __x86_64__
#	define SAVE_STACK_POINTER( var ) \
            void *var = NULL; \
            asm("movq %%rsp, %0;" : "=r"(var));

#	define CHANGE_STACK_POINTER(sp_value) \
            do\
            {\
                void *new_sp = (void*)(sp_value);\
                asm("pushq %0; popq %%rsp;" : : "r"(new_sp));\
            }\
            while(0)
#endif

#define MUSE_PLATFORM_POSIX 1

#  if defined(BSD) || __APPLE__ & __MACH__
#      define MUSE_PLATFORM_BSD 1
#include <sys/ioctl.h>
#include <unistd.h>
#  endif

#endif

#ifdef __cplusplus
#	define BEGIN_MUSE_C_FUNCTIONS  extern "C" {
#	define END_MUSE_C_FUNCTIONS }
#else
#	define BEGIN_MUSE_C_FUNCTIONS 
#	define END_MUSE_C_FUNCTIONS
#endif

#include <setjmp.h>

#ifndef MUSEAPI
#define MUSEAPI
#endif

#ifdef MUSE_DEBUG_BUILD
BEGIN_MUSE_C_FUNCTIONS
	MUSEAPI void muse_assert_failed( void *env, const char *file, int line, const char *condtext );
END_MUSE_C_FUNCTIONS
#	define muse_assert( cond ) { if ( !(cond) ) muse_assert_failed( env, __FILE__, __LINE__, #cond ); }
#	define muse_debug_only(expr) expr
#else
#	define muse_assert( cond )
#	define muse_debug_only(expr)
#endif

#if MUSE_DIAGNOSTICS_LEVEL > 0
	#define MUSE_DIAGNOSTICS(statement) { statement; }
#else
	#define MUSE_DIAGNOSTICS(statement)
#endif

#if MUSE_DIAGNOSTICS_LEVEL > 1
	#define MUSE_DIAGNOSTICS2(statement) { statement; }
#else
	#define MUSE_DIAGNOSTICS2(statement)
#endif	

#if MUSE_DIAGNOSTICS_LEVEL > 2
	#define MUSE_DIAGNOSTICS3(statement) { statement; }
#else
	#define MUSE_DIAGNOSTICS3(statement)
#endif

#endif /* __MUSE_PLATFORM_H__ */
