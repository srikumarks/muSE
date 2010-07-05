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
#include <stdlib.h>
#include "muse_port.h"
#include "muse_utils.h"

#ifdef MUSE_PLATFORM_WINDOWS
#include <windows.h>
#include <shellapi.h>
#include <shlwapi.h>
#include <winreg.h>

#pragma comment(lib,"shell32")
#pragma comment(lib,"advapi32")

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
 * @code (list-files [pattern]) @endcode
 * Returns a list of files that patch the given pattern. 
 * For example:
 * @code
 * (list-files "*.jpg")
 * @endcode
 * will list the JPG files in the current folder.
 * Note that the returned list only has the file names
 * and not the full paths to the files.
 *
 * Another use for list-files is to check whether a particular
 * file exists. If \p p is the full path to the file, say,
 * then @code (list-files p) @endcode will be () if the
 * file doesn't exist and will be a single-entry list of 
 * the file's name (stripped of path) if the file exists.
 */
muse_cell fn_list_files( muse_env *env, void *context, muse_cell args )
{
	filesearchinfo_t info;
	info.path		= _evalnext(&args);
	info.attrmask	= FILE_ATTRIBUTE_DIRECTORY;
	info.attrcomp	= 0;

	return muse_add_recent_item( env, (muse_int)fn_list_files, muse_generate_list( env, (muse_list_generator_t)generate_files, &info ) );
}

/**
 * (list-folders [pattern]).
 * Returns a list of folder names for all the folders that satisfy the 
 * given pattern.
 * For example:
 * @code
 * (list-folders "../*")
 * @endcode
 * will list the folders above the current folder.
 * Note that the returned list only has the folder names and not
 * the full paths to the folders. Also, the folder names don't end
 * with '/' or any such path separator character.
 *
 * Another use for list-folders is to check whether a particular
 * folder exists. If \p p is the full path to the folder without
 * any trailing slashes/backslashes,
 * then @code (list-folders p) @endcode will be () if the
 * folder doesn't exist and will be a single-entry list of 
 * the folder's name (stripped of path) if the folder exists.
 */
muse_cell fn_list_folders( muse_env *env, void *context, muse_cell args )
{
	filesearchinfo_t info;
	info.path		= _evalnext(&args);
	info.attrmask	= FILE_ATTRIBUTE_DIRECTORY;
	info.attrcomp	= FILE_ATTRIBUTE_DIRECTORY;

	return muse_add_recent_item( env, (muse_int)fn_list_folders, muse_generate_list( env, (muse_list_generator_t)generate_files, &info ) );
}

#else // POSIX
#include <sys/time.h>

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

static int copy_with_space_escapes( int N, char *buffer, const muse_char *str )
{
	int count = 0;
	
	while ( count < N && (*str) ) {
		if ( isspace(*str) ) {
			buffer[count++] = '\\';
		}
		buffer[count++] = (char)(*str++);
	}
	
	buffer[count] = '\0';
	return count;
}

/**
 * @code (list-files [pattern]) @endcode
 * Returns a list of files that patch the given pattern. 
 * For example:
 * @code
 * (list-files "*.jpg")
 * @endcode
 * will list the JPG files in the current folder.
 * Note that the returned list only has the file names
 * and not the full paths to the files.
 *
 * Another use for list-files is to check whether a particular
 * file exists. If \p p is the full path to the file, say,
 * then @code (list-files p) @endcode will be () if the
 * file doesn't exist and will be a single-entry list of 
 * the file's name (stripped of path) if the file exists.
 */
muse_cell fn_list_files( muse_env *env, void *context, muse_cell args )
{
	FILE *f;
	char cmd[4096];
	int len = 0;
	
	const muse_char *path = _text_contents( _evalnext(&args), NULL );
	
	const muse_char *wild = wcschr( path, '*' );
	
	if ( wild )
	{
		len += snprintf( cmd, 4096, "ls -p " );
		len += copy_with_space_escapes( wild-path, cmd + len, path );
		len += snprintf( cmd+len, 4096-len, " | grep '%ls$'", wild+1 );
	}
	else
	{
		/* No wild card. We can directly get contents. */
		len = snprintf( cmd, 4096, "ls -p " );
		len += copy_with_space_escapes( 4096-len, cmd + len, path );
		len += snprintf( cmd+len, 4096-len, " | grep -v /" );
	}

	printf( "command = [%s]\n", cmd );
	
	f = popen( cmd, "r" );
	if ( f )
		return muse_add_recent_item( env, (muse_int)fn_list_files, muse_generate_list( env, (muse_list_generator_t)generate_files, f ) );

	return MUSE_NIL;
}

/**
 * (list-folders [pattern]).
 * Returns a list of folder names for all the folders that satisfy the 
 * given pattern.
 * For example:
 * @code
 * (list-folders "../*")
 * @endcode
 * will list the folders above the current folder.
 * Note that the returned list only has the folder names and not
 * the full paths to the folders. Also, the folder names don't end
 * with '/' or any such path separator character.
 *
 * Another use for list-folders is to check whether a particular
 * folder exists. If \p p is the full path to the folder without
 * any trailing slashes/backslashes,
 * then @code (list-folders p) @endcode will be () if the
 * folder doesn't exist and will be a single-entry list of 
 * the folder's name (stripped of path) if the folder exists.
 */
muse_cell fn_list_folders( muse_env *env, void *context, muse_cell args )
{
	FILE *f;
	char cmd[4096];
	int len = 0;
	const muse_char *path = _text_contents( _evalnext(&args), NULL );

	len = snprintf( cmd, 4096, "ls -p " );
	len += copy_with_space_escapes( 4096-len, cmd + len, path );
	len += snprintf( cmd+len, 4096-len, " | grep /" );

	f = popen( cmd, "r" );
	if ( f )
		return muse_add_recent_item( env, (muse_int)fn_list_folders, muse_generate_list( env, (muse_list_generator_t)generate_files, f ) );

	return MUSE_NIL;
}

#endif


/**
 * Finds the first position of the given character in the given string.
 * the return value will either point to the character or be 0 if the
 * end of string was reached without encountering the character.
 */
static const muse_char *findnext( const muse_char *str, muse_char c )
{
	while ( str[0] && str[0] != c ) ++str;
	return str;
}

struct separator_state_t {
	const muse_char *str;
	const muse_char *sep;
	const muse_char *currsep;
};

muse_cell fieldgen( muse_env *env, void *context, int i, muse_boolean *eol )
{
	struct separator_state_t *s = (struct separator_state_t*)context;
	
	if ( s->str[0] == 0 )
	{
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
	
	(*eol) = MUSE_FALSE;
	
	{
		const muse_char *end = findnext( s->str, s->currsep[0] );
		muse_cell field = muse_mk_text(env, s->str, end );
		if ( end[0] ) s->str = end + 1; else s->str = end;
		
		/* Cycle through the seequence of separators given. */
		s->currsep++;
		if ( s->currsep[0] == 0 ) s->currsep = s->sep;
		
		return field;
	}
}


/**
 * @code (split "one;two;;three;" ";") -> ("one" "two" "" "three" "") @endcode
 * @code (split "one=1;two=2;three=3" "=;") -> ("one" "1" "two" "2" "three" "3") @endcode
 *
 * The first argument is the string to split and the second 
 * argument is a character separator. 
 *
 * You can define a recursive splitter like this -
 * @code
(define (split-rec str (sep . seps))
	(if seps
		(map (fn (s) (split-rec s seps)) (split str sep))
		(split str sep)))  
 * @endcode
 * so that @code (split-rec "a=1&b=2&c=3" '("&" "=")) @endcode
 * will give you @code (("a" "1") ("b" "2") ("c" "3")) @endcode.
 */
muse_cell fn_split( muse_env *env, void *context, muse_cell args )
{
	muse_cell strc = _evalnext(&args);
	muse_cell sep = _evalnext(&args);

	struct separator_state_t state;
	state.str = _text_contents(strc,NULL);
	state.sep = _text_contents(sep,NULL);
	state.currsep = state.sep;

	return muse_add_recent_item( env, (muse_int)fn_split, muse_generate_list( env, fieldgen, &state ) );
}

/**
 * @code (alert arg1 arg2 ...) @endcode
 *
 * Puts up an "abort-retry-ignore" message box with the given text.
 * The arguments are "format"ed into a single string and presented
 * in the dialog box.
 */
muse_cell fn_alert( muse_env *env, void *context, muse_cell args )
{
	muse_cell msg = fn_format( env, context, args );

	muse_message( env, NULL, L"%s", _text_contents( msg, NULL ) );

	return MUSE_NIL;
}

/**
 * Returns a non-zero integer if the character doesn't need to
 * be encoded for use in a URL. Otherwise it returns 0.
 */
static int urlclearchar( muse_char c )
{
	static int g_chartable_prepared = 0;
	static unsigned char g_chartable[32];

	if ( !g_chartable_prepared ) {
		static const char *clearchars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.:/";
		const char *ci = clearchars;
		memset( g_chartable, 0, sizeof(g_chartable) );
		for ( ; *ci; ++ci ) {
			g_chartable[(*ci)>>3] |= (1 << ((*ci) & 7));
		}
		g_chartable_prepared = 1;
	}

	if ( (c & 0x7f) == c ) {
		return g_chartable[c >> 3] & (1 << (c & 7));
	} else {
		return 0;
	}
}

/**
 * @code (urlencode "string") @endcode
 *
 * Takes an arbitrary string and returns a version that can be used as a part of a URL.
 * The string is not expected to contain unicode characters.
 */
muse_cell fn_urlencode( muse_env *env, void *context, muse_cell args )
{
	muse_cell str = _evalnext(&args);
	int len = 0;
	const muse_char *cstr = muse_text_contents( env, str, &len );

	int olen = len;
	muse_char *ocstr = (muse_char*)calloc( olen+1, sizeof(muse_char) );

	int in = 0, out = 0;
	for ( in = 0; in < len; ++in ) {
		if ( out + 4 >= olen ) {
			ocstr = (muse_char*)realloc( ocstr, sizeof(muse_char) * (olen * 2 + 1) );
			olen *= 2;
		}

		/* Process one character. */
		{
			muse_char c = cstr[in];
			if ( (c & 0x7F) != c ) {
				free(ocstr);
				return muse_raise_error( env, _csymbol(L"error:bad-url-component"), _cons(str, MUSE_NIL) );
			}

			if ( c == ' ' ) {
				ocstr[out++] = '+';
			} else if ( urlclearchar(c) ) {
				ocstr[out++] = c;
			} else {
				/* Encode the character. */
				ocstr[out++] = '%';
				ocstr[out++] = num2hex((c >> 4) & 0xF);
				ocstr[out++] = num2hex(c & 0xF);
			}
		}
	}

	ocstr[out] = '\0';

	{
		muse_cell result = muse_mk_text( env, ocstr, ocstr + out );
		free(ocstr);
		return result;
	}
}

/**
 * @code (urldecode "encoded%20string") @endcode
 *
 * Takes a url-encoded string and returns the regular string form.
 */
muse_cell fn_urldecode( muse_env *env, void *context, muse_cell args )
{
	muse_cell encstr = _evalnext(&args);
	int len = 0;
	const muse_char *cstr = muse_text_contents( env, encstr, &len );
	int olen = len;
	muse_char *ocstr = (muse_char*)calloc( olen+1, sizeof(muse_char) );

	int in = 0, out = 0;
	for ( in = 0; in < len; ) {
		muse_char c = cstr[in++];

		if ( (c & 0x7F) != c ) {
			free(ocstr);
			return muse_raise_error( env, _csymbol(L"error:bad-encoded-url-component"), _cons( encstr, MUSE_NIL ) );
		} else {
			switch ( c ) {
				case '+' : ocstr[out++] = ' '; break;
				case '%' : 
					if ( in+1 >= len ) {
						free(ocstr);
						return muse_raise_error( env, _csymbol(L"error:bad-encoded-url-component"), _cons( encstr, MUSE_NIL ) );
					} else {
						ocstr[out++] = (muse_char)((hex2num(cstr[in]) << 4) + hex2num(cstr[in+1]));
						in += 2;
						break;
					}
				default :
					ocstr[out++] = c;
			}
		}
	}

	{
		muse_cell result = muse_mk_text( env, ocstr, ocstr + out );
		free(ocstr);
		return result;
	}
}

/**
 * @code (launch "file.ext") @endcode
 *
 * Opens the given filesystem object or URL in the default application.
 */
muse_cell fn_launch( muse_env *env, void *context, muse_cell args )
{
#if MUSE_PLATFORM_WINDOWS
	ShellExecute( NULL, L"open", muse_text_contents( env, _evalnext(&args), NULL ), NULL, NULL, SW_SHOW );
#else
	fn_system( env, NULL, muse_list( env, "Tc", L"open", _evalnext(&args) ) );
#endif
	return MUSE_NIL;
}

static muse_cell whatis_0( muse_env *env, muse_cell thing )
{
	switch (_cellt(thing)) {
		case MUSE_TEXT_CELL		: return _csymbol(L"text");
		case MUSE_INT_CELL		:
		case MUSE_FLOAT_CELL	: return _csymbol(L"number");
		case MUSE_SYMBOL_CELL	: return _csymbol(L"symbol");
		case MUSE_CONS_CELL		: return _csymbol(L"cons");
		case MUSE_NATIVEFN_CELL	:
			{
				muse_functional_object_t *fobj = muse_functional_object_data( env, thing, 0 );
				if ( fobj ) {
					switch ( fobj->type_info->type_word ) {
						case 'mobj' : return _csymbol(L"object");
						case 'vect' : return _csymbol(L"vector");
						case 'hash' : return _csymbol(L"hashtable");
						case 'boxx' : return _csymbol(L"box");
						case 'barr' : return _csymbol(L"bytes");
						case 'mmod' : return _csymbol(L"module");
						default		: return MUSE_NIL;
					}
				} else {
					/* Fall through to the next case. */
				}
			}
		case MUSE_LAMBDA_CELL	:
			return _csymbol(L"fn");
		default:
			return MUSE_NIL;
	}
}

static const muse_char *guess_mime_type( const muse_char *path )
{
	const muse_char *ext = wcsrchr( path, '.' );
	if ( ext && ext[0] == '.' ) {
		static const muse_char *s_mime_type_map[] = 
		{
			L".jpg",	L"image/jpeg",
			L".jpeg",	L"image/jpeg",
			L".png",	L"image/png",
			L".bmp",	L"image/bmp",
			L".tiff",	L"image/tiff",
			L".gif",	L"image/gif",
			L".ico",	L"image/vnd.microsoft.icon",
			L".htm",	L"text/html",
			L".html",	L"text/html",
			L".css",	L"text/css",
			L".txt",	L"text/plain",
			L".c",		L"text/plain",
			L".cpp",	L"text/plain",
			L".scm",	L"text/plain",
			L".h",		L"text/plain",
			L".js",		L"text/javascript",
			L".xml",	L"text/xml",
			NULL
		};

		const muse_char **mt = s_mime_type_map;
		for ( ; mt[0]; mt += 2 ) {
#if MUSE_PLATFORM_WINDOWS
			if ( _wcsicmp( ext, mt[0] ) == 0 )
				return mt[1];
#else
			if ( wcscmp( ext, mt[0] ) == 0 )
				return mt[1];
#endif
		}

		/* No extension matched. Treat as generic binary. */
		return L"application/octet-stream";
	} else {
		/* No extension. Cannot guess mime type. */
	}

	return NULL;
}

#if MUSE_PLATFORM_WINDOWS

muse_cell fn_new( muse_env *env, void *context, muse_cell args );

static muse_cell whatis_1( muse_env *env, muse_cell thing )
{
	if ( _cellt(thing) == MUSE_TEXT_CELL ) {
		const muse_char *text = muse_text_contents( env, thing, NULL );
		if ( PathFileExistsW(text) ) {
			/* File or folder exists. */
			muse_cell file = fn_new( env, NULL, MUSE_NIL );
			BOOL dir = PathIsDirectoryW(text);
			muse_put( env, file, _csymbol(L"type"), _cons(_csymbol( dir ? L"folder" : L"file" ), MUSE_NIL) );

			if ( !dir ) {
				const muse_char *mime = guess_mime_type(text);
				if ( mime )
					muse_put( env, file, _csymbol(L"mime-type"), _cons(_csymbol(mime),MUSE_NIL) );
			}

			return file;
		} else if ( UrlIsW( text, URLIS_URL ) ) {
			muse_cell url = fn_new( env, NULL, MUSE_NIL );
			muse_put( env, url, _csymbol(L"type"), _cons(_csymbol(L"url"), MUSE_NIL) );

			{
				muse_char scheme[16];
				DWORD N = 16;
				UrlGetPartW( text, scheme, &N, URL_PART_SCHEME, URL_PARTFLAG_KEEPSCHEME );
				muse_put( env, url, _csymbol(L"scheme"), _cons(_csymbol(scheme), MUSE_NIL) );
				if ( wcscmp(scheme,L"file") == 0 ) {
					muse_char path[MAX_PATH];
					DWORD N = MAX_PATH;
					PathCreateFromUrlW( text, path, &N, 0 );
					muse_put( env, url, _csymbol(L"path"), _cons(muse_mk_text(env,path,path+N),MUSE_NIL) );
				}

				/* Set the mime type if you can guess it. */
				{
					const muse_char *mime = guess_mime_type(text);
					if ( mime ) 
						muse_put( env, url, _csymbol(L"mime-type"), _cons(_csymbol(mime),MUSE_NIL) );
				}
			}

			return url;
		}
	}

	return whatis_0( env, thing );
}

static muse_cell whatis_2( muse_env *env, muse_cell thing, muse_cell level1 )
{
	return level1;
}
#else

static muse_cell whatis_1( muse_env *env, muse_cell thing )
{
	return whatis_0( env, thing );
}

static muse_cell whatis_2( muse_env *env, muse_cell thing, muse_cell level1 )
{
	return level1;
}
#endif

/**
 * @code (whatis thing level) @endcode
 * For \p level 0, returns a type checking function for the thing.
 * For \p level 1, return an object containing properties that are 
 *   extracted without looking into details like file contents.
 * For \p level 2, returns an object containing deep properties
 *   of the thing.
 */
muse_cell fn_whatis( muse_env *env, void *context, muse_cell args )
{
	muse_cell thing = _evalnext(&args);
	int level = args ? (int)_intvalue(_evalnext(&args)) : 0;

	switch ( level ) {
		case 1 : return whatis_1( env, thing );
		case 2 : return whatis_2( env, thing, whatis_1( env, thing ) );
		default: /* 0 and all other values. */
			return whatis_0( env, thing );
	}
}


#if MUSE_PLATFORM_WINDOWS

static HKEY standard_key( const muse_char *name )
{
	if ( wcscmp( name, L"HKEY_CLASSES_ROOT" ) == 0 )
		return HKEY_CLASSES_ROOT;
	else if ( wcscmp( name, L"HKEY_CURRENT_CONFIG" ) == 0 )
		return HKEY_CURRENT_CONFIG;
	else if ( wcscmp( name, L"HKEY_CURRENT_USER" ) == 0 )
		return HKEY_CURRENT_USER;
	else if ( wcscmp( name, L"HKEY_LOCAL_MACHINE" ) == 0 )
		return HKEY_LOCAL_MACHINE;
	else if ( wcscmp( name, L"HKEY_PERFORMANCE_DATA" ) == 0 )
		return HKEY_PERFORMANCE_DATA;
	else if ( wcscmp( name, L"HKEY_PERFORMANCE_NLSTEXT" ) == 0 )
		return HKEY_PERFORMANCE_NLSTEXT;
	else if ( wcscmp( name, L"HKEY_PERFORMANCE_TEXT" ) == 0 )
		return HKEY_PERFORMANCE_TEXT;
	else if ( wcscmp( name, L"HKEY_USERS" ) == 0 )
		return HKEY_USERS;
	else
		return 0;
}

static muse_cell multi_string_generator( muse_env *env, const muse_char **context, int i, muse_boolean *eol )
{
	if ( (*context)[0] )
	{
		size_t len = wcslen( *context );
		muse_cell result = muse_mk_ctext( env, *context );
		(*eol) = MUSE_FALSE;
		(*context) += len + 1;
		return result;
	}
	else
	{
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
}

#endif

/**
 * @code (windows-registry-entry section entry) @endcode
 *
 * Looks up the given entry path in the given section. The entry's
 * components are to be separated by backslashes. Integers become
 * muSE integers, strings become muSE strings, a multi-string becomes
 * a list of strings and a binary blob becomes a byte array.
 *
 * @param section
 * Can be one of "HKEY_CLASSES_ROOT", "HKEY_CURRENT_CONFIG", "HKEY_CURRENT_USER",
 * "HKEY_LOCAL_MACHINE", "HKEY_PERFORMANCE_DATA", "HKEY_PERFORMANCE_NLSTEXT",
 * "HKEY_PERFORMANCE_TEXT" or "HKEY_USERS"
 *
 * @param entry A backslash separated sequence of subkeys.
 *
 * @par Example
 * To get the installed location of muvee Reveal's MVRT.dll, you can
 * query the following registry entry -
 * @code
   (windows-registry-entry 
      "HKEY_LOCAL_MACHINE"
     "SOFTWARE\\Classes\\CLSID\\{30A8E5D8-A4BD-4038-981E-AD2B7AD781EA}\\InprocServer32\\")
   @endcode
 */
muse_cell fn_windows_registry_entry( muse_env *env, void *context, muse_cell args )
{
#if MUSE_PLATFORM_WINDOWS
	muse_cell section_cell = _evalnext(&args);
	muse_cell entry_cell = _evalnext(&args);
	const muse_char *section_name = muse_text_contents( env, section_cell, NULL );
	const muse_char *entry = muse_text_contents( env, entry_cell, NULL );

	HKEY section = standard_key(section_name);
	if ( section )
	{
		HKEY subkey = 0;
		if ( ERROR_SUCCESS == RegOpenKeyW( section, entry, &subkey ) )
		{
			DWORD type = 0;
			DWORD entry_size = 0;
			muse_cell result = MUSE_NIL;
			if ( ERROR_SUCCESS == SHRegGetValueW( subkey, NULL, NULL, SRRF_RT_ANY, &type, NULL, &entry_size ) )
			{
				switch ( type )
				{
				case REG_BINARY:
					{
						muse_cell b = muse_mk_bytes( env, entry_size );
						SHRegGetValueW( subkey, NULL, NULL, SRRF_RT_ANY, &type, muse_bytes_data( env, b, 0 ), &entry_size );
						result = b;
						break;
					}
				case REG_DWORD:
					{
						DWORD dw = 0;
						SHRegGetValueW( subkey, NULL, NULL, SRRF_RT_ANY, &type, &dw, &entry_size );
						result = muse_mk_int( env, dw );
						break;
					}

				case REG_DWORD_BIG_ENDIAN:
					{
						unsigned char dw[4] = "";
						unsigned char temp = 0;
						SHRegGetValueW( subkey, NULL, NULL, SRRF_RT_ANY, &type, &dw, &entry_size );
						temp = dw[0]; dw[0] = dw[3]; dw[3] = temp;
						temp = dw[1]; dw[1] = dw[2]; dw[2] = temp;
						result = muse_mk_int( env, *(DWORD*)dw );
						break;
					}

				case REG_EXPAND_SZ:
					{
						muse_char *buffer = (muse_char*)calloc( 1, entry_size );
						SHRegGetValueW( subkey, NULL, NULL, SRRF_RT_ANY, &type, buffer, &entry_size ); 

						{
							DWORD sz = ExpandEnvironmentStringsW( buffer, NULL, 0 );
							muse_char *obuffer = (muse_char*)calloc( 1, (sz+1) * sizeof(muse_char) );
							sz = ExpandEnvironmentStringsW( buffer, obuffer, sz );
							result = muse_mk_ctext( env, obuffer );
							free(obuffer);
							free(buffer);
							break;
						}
					}

				case REG_SZ:
					{
						muse_char *buffer = (muse_char*)calloc( 1, entry_size );
						SHRegGetValueW( subkey, NULL, NULL, SRRF_RT_ANY, &type, buffer, &entry_size ); 
						result = muse_mk_ctext( env, buffer );
						free(buffer);
						break;
					}

				case REG_MULTI_SZ:
					{
						muse_char *buffer = (muse_char*)calloc( 1, entry_size );
						SHRegGetValueW( subkey, NULL, NULL, SRRF_RT_ANY, &type, buffer, &entry_size ); 
						result = muse_generate_list( env, (muse_list_generator_t)multi_string_generator, &buffer );
						break;
					}

				case REG_QWORD:
					{
						muse_int qw = 0;
						SHRegGetValueW( subkey, NULL, NULL, SRRF_RT_ANY, &type, &qw, &entry_size );
						result = muse_mk_int( env, qw );
						break;
					}

				case REG_LINK:
				case REG_NONE:
				default:
					break;

				}

				if ( subkey != section ) RegCloseKey(subkey);

				if ( !result )
					return muse_raise_error( env, _csymbol(L"error:unsupported-registry-key"), _cons( section_cell, _cons( entry_cell, MUSE_NIL ) ) );

				return muse_add_recent_item( env, (muse_int)fn_windows_registry_entry, result );
			}
			else
			{
				return muse_raise_error( env, _csymbol(L"error:registry-value-not-found"), _cons( section_cell, _cons( entry_cell, MUSE_NIL ) ) );
			}
		}
		else
		{
			return muse_raise_error( env, _csymbol(L"error:invalid-registry-entry"), _cons( section_cell, _cons( entry_cell, MUSE_NIL ) ) );
		}
	}
	else
	{
		return muse_raise_error( env, _csymbol(L"error:invalid-registry-section"), _cons( section_cell, _cons( entry_cell, MUSE_NIL ) ) );
	}
#else
	return muse_raise_error( env, _csymbol(L"error:not-implemented"), MUSE_NIL );
#endif
}

/**
 * @code (temp-folder) @endcode
 *
 * Returns a path into which you can store temporary files.
 * The path has a trailing path separator so you can append a
 * file name to it directly. You're also assured that the
 * folder exists and can be written to. If not, the return value
 * is ().
 */
muse_cell fn_temp_folder( muse_env *env, void *context, muse_cell args )
{
#if MUSE_PLATFORM_WINDOWS
	muse_char buffer[1024];
	int len = 0;
	if ( (len = GetTempPathW( 1024, buffer )) > 0 )
	{
		/* Ensure that the temp folder exists and can be written to. */
		if ( _waccess( buffer, 02 ) == 0 )
			return muse_add_recent_item( env, (muse_int)fn_temp_folder, muse_mk_text( env, buffer, buffer + len ) );
		else
			return MUSE_NIL;
	}
	else
	{
		return MUSE_NIL;
	}
#else
	return muse_add_recent_item( env, (muse_int)fn_temp_folder, muse_mk_ctext( env, L"/tmp/" ) );
#endif
}

/**
 * @code (temp-file folder [prefix]) @endcode
 *
 * Returns the full path to a unique temporary file in the
 * given folder. If no folder is given, then \c temp-folder is
 * used to get a folder for storing temporary files.
 */
muse_cell fn_temp_file( muse_env *env, void *context, muse_cell args )
{
	const muse_char *folder = muse_text_contents( env, _evalnext(&args), NULL );
	const muse_char *prefix = args ? muse_text_contents( env, _evalnext(&args), NULL ) : NULL;

#if MUSE_PLATFORM_WINDOWS
	muse_char buffer[MAX_PATH];

	if ( GetTempFileNameW( folder, prefix, 0, buffer ) != 0 )
	{
		return muse_add_recent_item( env, (muse_int)fn_temp_file, muse_mk_ctext( env, buffer ) );
	}
	else
	{
		return MUSE_NIL;
	}
#else
	char buffer[1024];
	struct timeval t;
	if ( 0 == gettimeofday( &t, NULL ) )
	{
		FILE *f = NULL;
		do
		{
			snprintf( buffer, 1024, "%ls%ls%d.tmp", folder, prefix, t.tv_sec );
			f = fopen( buffer, "rb" );
		}
		while ( f == NULL );
		fclose(f);

		return muse_add_recent_item( env, (muse_int)fn_temp_file, muse_mk_ctext_utf8( env, buffer ) );
	}
	else
	{
		return MUSE_NIL;
	}
#endif
}

/**
 * @code (to-lower string) @endcode
 *
 * Converts the given string to lower case.
 */
muse_cell fn_to_lower( muse_env *env, void *context, muse_cell args )
{
	int len = 0;
	const muse_char *str = muse_text_contents( env, _evalnext(&args), &len );
	muse_cell out = muse_mk_text( env, (const muse_char *)0, ((const muse_char *)0) + len );
	muse_char *strout = (muse_char*)muse_text_contents( env, out, NULL );
	int i = 0;
	while ( i < len )
	{
		strout[i] = (muse_char)towlower( str[i] );
		++i;
	}

	return out;
}

/**
 * @code (to-upper string) @endcode
 *
 * Converts the given string to upper case.
 */
muse_cell fn_to_upper( muse_env *env, void *context, muse_cell args )
{
	int len = 0;
	const muse_char *str = muse_text_contents( env, _evalnext(&args), &len );
	muse_cell out = muse_mk_text( env, (const muse_char *)0, ((const muse_char *)0) + len );
	muse_char *strout = (muse_char*)muse_text_contents( env, out, NULL );
	int i = 0;
	while ( i < len )
	{
		strout[i] = (muse_char)towupper( str[i] );
		++i;
	}

	return out;
}
