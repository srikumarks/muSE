/**
 * @file muse_win32.h
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#ifndef __MUSE_WIN32_H__
#define __MUSE_WIN32_H__

#include <ctype.h>
#include <io.h>

#ifndef __cplusplus
#define inline __inline
#endif

#define snprintf _snprintf
#define alloca _alloca
#define read _read
#define write _write
#define fileno _fileno
#define tell _tell

#define MUSE_FMT_INT		"%I64d"
#define MUSE_FMT_FLOAT		"%.10lg"
#define MUSE_FMT_STRING		"%S"
#define MUSE_FMT_QSTRING	"\"%S\""

typedef __int64 longlong_t;

#define SAVE_STACK_POINTER( var ) void *var = NULL; __asm { mov var, esp }

#define CHANGE_STACK_POINTER(sp_value) \
				do\
				{\
					void *new_sp = (void*)(sp_value);\
					__asm push new_sp\
					__asm pop esp\
				}\
				while(0)

#define MUSE_PLATFORM_WINDOWS 1

#endif /* __MUSE_WIN32_H__ */