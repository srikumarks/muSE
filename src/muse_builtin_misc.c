/**
 * @file muse_builtin_misc.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * A collection of OS-specific functions exposed in the language.
 */

#include "muse_builtins.h"
#include <string.h>

#ifdef MUSE_PLATFORM_WINDOWS
#include <windows.h>

typedef struct 
{
	muse_cell path;
	DWORD attrmask, attrcomp;
	HANDLE search;
	WIN32_FIND_DATAW data;
} filesearchinfo_t;

static muse_cell generate_files( muse_env *env, filesearchinfo_t *info, int i, muse_boolean *eol )
{
	if ( i == 0 )
	{
		info->search = FindFirstFileW( _text_contents( info->path, NULL ), &info->data );
		if ( info->search == INVALID_HANDLE_VALUE )
		{
			(*eol) = MUSE_TRUE;
			return MUSE_NIL;
		}
	}
	else
	{
		if ( !FindNextFileW( info->search, &info->data ) )
		{
			FindClose(info->search);
			(*eol) = MUSE_TRUE;
			return MUSE_NIL;
		}
	}

	while ( (info->data.dwFileAttributes & info->attrmask) != info->attrcomp 
				|| wcscmp(info->data.cFileName, L".") == 0
				|| wcscmp(info->data.cFileName, L"..") == 0 )
	{
		if ( !FindNextFileW( info->search, &info->data ) )
		{
			FindClose(info->search);
			(*eol) = MUSE_TRUE;
			return MUSE_NIL;
		}
	}

	(*eol) = MUSE_FALSE;
	return muse_mk_ctext( env, info->data.cFileName );
}

/**
 * (list-files [pattern]).
 * Returns a list of files that patch the given pattern. 
 * For example:
 * @code
 * (list-files "*.jpg")
 * @endcode
 * will list the JPG files in the current folder.
 * Note that the returned list only has the file names
 * and not the full paths to the files.
 */
muse_cell fn_list_files( muse_env *env, void *context, muse_cell args )
{
	filesearchinfo_t info;
	info.path		= _evalnext(&args);
	info.attrmask	= FILE_ATTRIBUTE_DIRECTORY;
	info.attrcomp	= 0;

	return muse_generate_list( env, (muse_list_generator_t)generate_files, &info );
}

/**
 * (list-folders parent-folder).
 * Returns a list of folder names for all the folders that exist under
 * the given parent folder.
 * For example:
 * @code
 * (list-folders "../*")
 * @endcode
 * will list the folders above the current folder.
 * Note that the returned list only has the folder names and not
 * the full paths to the folders. Also, the folder names don't end
 * with '/' or any such path separator character.
 */
muse_cell fn_list_folders( muse_env *env, void *context, muse_cell args )
{
	filesearchinfo_t info;
	info.path		= _evalnext(&args);
	info.attrmask	= FILE_ATTRIBUTE_DIRECTORY;
	info.attrcomp	= FILE_ATTRIBUTE_DIRECTORY;

	return muse_generate_list( env, (muse_list_generator_t)generate_files, &info );
}

#else // POSIX


static muse_cell generate_files( muse_env *env, FILE *info, int i, muse_boolean *eol )
{
	char line[4096];
	if ( fgets( line, 4096, info ) )
	{
		int len = strlen(line);

		// Erase the newline character at the end.
		if ( line[len-1] == '\n' || line[len-1] == '\r' )
		{
			line[--len] = '\0';
		}

		// Erase any "/" ending present for directory names.
		if ( line[len-1] == '/' )
		{
			line[--len] = '\0';
		}

		(*eol) == MUSE_FALSE;
		return muse_mk_text_utf8(env, line, line+len);
	}
	else
	{
		(*eol) = MUSE_TRUE;
		pclose(info);
		return MUSE_NIL;
	}
}

/**
 * (list-files [pattern]).
 * Returns a list of files that patch the given pattern. 
 * For example:
 * @code
 * (list-files "*.jpg")
 * @endcode
 * will list the JPG files in the current folder.
 * Note that the returned list only has the file names
 * and not the full paths to the files.
 */
muse_cell fn_list_files( muse_env *env, void *context, muse_cell args )
{
	FILE *f;
	char cmd[4096];
	snprintf( cmd, 4096, "ls -p %S | grep -v /", _text_contents( _evalnext(&args), NULL ) );

	f = popen( cmd, "r" );
	if ( f )
		return muse_generate_list( env, (muse_list_generator_t)generate_files, f );

	return MUSE_NIL;
}

/**
 * (list-folders parent-folder).
 * Returns a list of folder names for all the folders that exist under
 * the given parent folder.
 * For example:
 * @code
 * (list-folders "../*")
 * @endcode
 * will list the folders above the current folder.
 * Note that the returned list only has the folder names and not
 * the full paths to the folders. Also, the folder names don't end
 * with '/' or any such path separator character.
 */
muse_cell fn_list_folders( muse_env *env, void *context, muse_cell args )
{
	FILE *f;
	char cmd[4096];
	snprintf( cmd, 4096, "ls -p %S | grep /", _text_contents( _evalnext(&args), NULL ) );

	f = popen( cmd, "r" );
	if ( f )
		return muse_generate_list( env, (muse_list_generator_t)generate_files, f );

	return MUSE_NIL;
}

#endif