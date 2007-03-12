/**
 * @file muse_plugin.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#include "muse_opcodes.h"

#ifdef MUSE_PLATFORM_WINDOWS
#	include <windows.h>
#else
#	include <dlfcn.h>
#	include <stdlib.h>
#endif

/**
 * Loads a muse plugin DLL and calls its entry point function
 * which is to have the name "muse_plugin_entry" and be of the type
 * muse_plugin_entry_t. The function must be declared extern "C"
 * if defined in a CPP file and must be exported from the DLL.
 *
 * The entry point function receives 3 arguments -
 *	- The first argument is the HMODULE of the loaded library.
 *	- The second argument is the muse environment pointer.
 *	- The third argument is an arbitrary argument list that can
 *		used for whatever purpose the plugin sees fit.
 * The entry point function must call muse_set_current_env() with
 * the given env pointer before calling any other muse functions.
 *
 * @param path Path to the DLL file.
 * @param arglist Arg list passed to the entry point function.
 * 
 * @see muse_plugin_entry_t
 */
#ifdef MUSE_PLATFORM_WINDOWS
muse_cell muse_link_plugin( muse_env *env, const muse_char *path, muse_cell arglist )
{
	HMODULE dll = LoadLibraryW(path);

	if ( !dll )
		return MUSE_NIL; /* Could not load library. */

	{
		muse_plugin_entry_t entry = (muse_plugin_entry_t)GetProcAddress( dll, "muse_plugin_entry" );
		if ( !entry )
		{
			FreeLibrary(dll);
			return MUSE_NIL; /* Could not get entry point. */
		}

		return entry( dll, env, arglist );
	}
}
#else
muse_cell muse_link_plugin( muse_env *env, const muse_char *path, muse_cell arglist )
{
	char buffer[4096];
	void *dll = NULL;
	
	muse_unicode_to_utf8( buffer, 4096, path, wcslen(path) );
	
	dll = dlopen( buffer, RTLD_LAZY | RTLD_LOCAL );
	if ( !dll )
		return MUSE_NIL; /* Could not load library. */
	
	{
		muse_plugin_entry_t entry = (muse_plugin_entry_t)dlsym( dll, "muse_plugin_entry" );
		if ( !entry )
		{
			dlclose(dll);
			return MUSE_NIL; /* Entry point not found. */
		}
		
		return entry( dll, env, arglist );
	}
}
#endif