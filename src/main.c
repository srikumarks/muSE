/**
 * @file muse.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#include "muse.h"
#include "muse_opcodes.h"
#include "muse_utils.h"
#include <stdlib.h>
#include <string.h>

static const char *k_args_exec_switch				= "--exec";
static const muse_char *k_main_function_name		= L"main";
static const muse_char *k_program_string_name		= L"*program*";

/**
 * Creates an executable from the given set of source files.
 * The usage is like this -
 * @param execfile The full path to the executable file to duplicate.
 * @param argc The number of arguments.
 * @param argv The arguments as strings, which must all be file paths.
 *  The first argument should be the path to the output file.
 */
static int create_exec( muse_env *env, const char *execfile, int argc, char **argv )
{
	const char *outputfile = argv[0];
	void *buffer = NULL;
	int e_size = 0, s_totalsize = 0;
	FILE *e, *o;
	
	e = fopen( execfile, "rb" );
	if ( e == NULL )
	{
		fprintf( stderr, 
				 "If you want to create an executable, you need to invoke muse\n"
				 "with the full path name. If you simply invoke it as 'muse',\n"
				 "the execcutable to use to embed the source into cannot be located.\n\n"
				 );
		return 0;
	}
	
	o = fopen( outputfile, "wb" );
	if ( o == NULL )
	{
		fprintf( stderr, "Invalid output file path '%s'.\n", outputfile );
		fclose(e);
		return 0;
	}

	e_size = muse_fsize(e);
	
	/* Copy executable to output first. The executable itself may have 
	source code at the end, so check for that. */
	{
		int s_pos = 0;
		int s_size = 0;
		int s_footer_size = 0;
		muse_boolean new_exec = MUSE_TRUE;
		
		fseek( e, 0, SEEK_SET );
		buffer = malloc( e_size );
		fread( buffer, 1, e_size, e );

		if ( muSEexec_check( e, &s_pos, &s_size, &s_footer_size ) )
		{
			/* Skip the ending 20 bytes, which will be ";<source-size> muSEexec",
			but tack on the source size to the total, so that the new source
			will simply be appended to the old source. */
			s_totalsize += s_size;
			e_size -= s_footer_size;
			new_exec = MUSE_FALSE;
		}
		
		fwrite( buffer, 1, e_size, o );
		free( buffer );

		fclose(e);
	}
	
	/* Copy the source files to the end. Each file is put into a (load #nnn[...]) expression. */
	{
		int ix = 1;
		for ( ; ix < argc; ++ix )
		{
			FILE *s = fopen( argv[ix], "rb" );
			
			if ( s != NULL )
			{
				int source_pos = 0, source_size = 0;

				if ( muSEexec_check( s, &source_pos, &source_size, NULL ) )
					fseek( s, source_pos, SEEK_SET );
				else
					source_size = muse_fsize(s);

				s_totalsize += fprintf( o, "(load #%d[", source_size );
				buffer = malloc( source_size );
				fread( buffer, 1, source_size, s );
				fwrite( buffer, 1, source_size, o );
				free( buffer );
				fclose( s );
				s_totalsize += fprintf( o, "])\n" );
				s_totalsize += source_size;
			}
		}
	}
	
	/* The end of the muSE executable has to have
	the 20-byte sequence - 
	  ;<decimal-size> muSEexec
	The last 8 characters being muSEexec.
	The decimal number must be 0 padded to the left.
	in order to make the ending sequence exactly 20 bytes.
	When loading source, this sequence will be ignored
	because it will appear as a comment. */
	muSEexec_finish( o, s_totalsize );
	
	return 1;
}


/**
 * Checks if the given file is a muSE executable with sourcee code
 * appended. If it is, then the sourcee code appended is loaded
 * using muse_load().
 */
static int load_exec( muse_env *env, const char *execfile )
{
	FILE *e = fopen( execfile, "rb" );
	int s_pos = 0;
	int s_size = 0;
	
	if ( e == NULL )
	{
		fprintf( stderr,
				 "Warning: If you want muSE to load any library code appended\n"
				 "to the executable, you must invoke the executable with the full\n"
				 "path. Invoking like 'muse' will cause the executable file location\n"
				 "to be unknown and therefore library code cannot be loaded.\n\n"
				 );
		return 0;		
	}
	
	/* Check that this is a muSE executable. */
	if ( muSEexec_check( e, &s_pos, &s_size, NULL ) )
	{
		/* Load the source portion of the executable. */
		fseek( e, s_pos, SEEK_SET );
		muse_load( env, e );
		fclose( e );
		return 1;
	}
	
	fclose( e );
	return 0;
}

typedef struct
{
	int argc;
	char **argv;
} args_list_generator_data_t;

/**
 * Converts each path argument into a muSE text object.
 */
static muse_cell args_list_generator( muse_env *env, void *context, int i, muse_boolean *eol )
{
	args_list_generator_data_t *data = (args_list_generator_data_t*)context;
	
	if ( i < data->argc )
	{
		(*eol) = MUSE_FALSE;
		return muse_mk_ctext_utf8( env, data->argv[i] );
	}
	else
	{
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
}

#if MUSE_PLATFORM_WINDOWS
	unsigned int __stdcall GetModuleFileNameA( void *, char *, unsigned int );
	static void get_execpath( muse_env *env, const char *suggestion, char *result, int size )
	{
		/* Under Windows, GetModuleFileName always works and will 
		always return the full executable path, whereas the argv[0] 
		can be a truncated path. */
		int len = (int)GetModuleFileNameA( NULL, result, size );
		muse_assert( len > 0 );
	}
#else
	static void get_execpath( muse_env *env, const char *suggestion, char *result, int size )
	{
		strcpy( result, suggestion );
		
		{
			FILE *e = fopen( suggestion, "rb" );
			if ( e == NULL )
			{
				/* Get the full path name using the "which" command. */
				char cmd[256];
				FILE *p;
				sprintf( cmd, "which \"%s\"", suggestion );
				p = popen( cmd, "r" );
				if ( p != NULL )
				{
					if ( fgets( result, size, p ) )
					{
						/* Trim ending spaces, tabs and newlines. */
						int len;
						result[size-1] = '\0';
						len = strlen(result);
						while ( isspace( result[len-1] ) ) 
							result[--len] = '\0';
						
						/* Check whether which returned a valid file path. */
						{
							FILE *wf = fopen( result, "rb" );
							if ( wf == NULL )
								strcpy( result, suggestion ); /* which gave us an invalid answer. */
							else
								fclose(wf);
						}
					}
					
					pclose(p);
				}
			}
			else
			{
				/* Succeeded. The suggestion is valid. */
				fclose(e);
			}
		}
	}
#endif

typedef struct {
	int argc;
	char **argv;
} args_t;

static muse_cell fn_repl( muse_env *env, args_t *context, muse_cell args ) 
{ 
	/* Load all files given in the argument. If the "--run" flag is encountered,
	don't start the repl. Just load the files. */
	int enable_repl = 1;
	if ( context && context->argc > 0 ) {
		int sp = _spos();
		int i = 0;
		muse_cell loader = _symval(_csymbol(L"load"));
		for ( i = 0; i < context->argc; ++i ) {
			if ( strcmp( context->argv[i], "--run" ) == 0 ) { 
				enable_repl = 0; 
			} else {
				muse_apply( env, loader, _cons( muse_mk_ctext_utf8( env, context->argv[i] ), MUSE_NIL ), MUSE_TRUE, MUSE_FALSE );
				_unwind(sp);
			}
		}
	}

	if ( enable_repl )
		muse_repl(env); 
	else {
		muse_cell sym_main = _csymbol(L"main");
		muse_cell main = _symval(sym_main);

		if ( (main != sym_main) && _isfn(main) ) {
			_apply( main, MUSE_NIL, MUSE_TRUE );
		}
	}

	return MUSE_NIL; 
}

#if defined(MUSE_PLATFORM_WINDOWS) && defined(MUSE_WINAPP)
#pragma comment(linker, "/SUBSYSTEM:windows /ENTRY:mainCRTStartup")
#endif

/**
 * Usage:
 * A)	fullpath-to-muse/muse --exec execfile source1.scm source2.scm ...
 *
 *		Creates an executable "execfile" of the given source files.
 *		When this executable is run, the source files attached to
 *		it will automatically be loaded.
 *		@see muSEexec_check() for details about identifying such source code.
 *
 * B)	fullpath-to-muse/muse
 *		
 *		Checks whether thhe given muSE executable has source code appended
 *		to it and loads it if there is any such appendix. If no such appendix
 *		is found, it starts the REPL as normal.
 *
 * C)	muse
 *		
 *		Starts the REPL but it may not be able to load any appended source code.
 *
 * If the appended source code has a function called "main", it is invoked with
 * a list of command line argument strings supplied to the executable. If no such
 * main function is defined, it simply starts the REPL.
 */
int main( int argc, char **argv )
{
	char execpath[1024];
	muse_env *env = muse_init_env(NULL);
	
	get_execpath( env, argv[0], execpath, 1024 );

	/* If we've been asked to create an executable, do so. */
	if ( argc > 1 && strcmp( argv[1], k_args_exec_switch ) == 0 )
	{
		/* When passing arguments, skip the exec path as well as the
		   "--exec" switch. */
		create_exec( env, execpath, argc-2, argv+2 );
	}
	
	/* Check if this is a muSE executable that has
	source code at the end of the file. */
	else if ( load_exec( env, execpath ) )
	{
		/* Evaluate the main function, creating a list
		of strings out of the remaining arguments. */
		muse_cell fn_main = _eval( _csymbol(k_main_function_name) );
		
		if ( muse_isfn(fn_main) )
		{
			muse_cell args = MUSE_NIL;
			args_list_generator_data_t data;
			
			/* Set the "*program*" symbol to the string of the program name - i.e. argv[0]. */
			_define( _csymbol(k_program_string_name), muse_mk_ctext_utf8( env, argv[0] ) );

			data.argc = argc-1; /* Skip the first entry, which is the executable's path. */
			data.argv = argv+1;
			
			args = muse_generate_list( env, args_list_generator, &data );
			muse_apply_top_level( env, fn_main, args );
		}
		else
		{
			/* Drop into the REPL if there is no main function defined.
			This way, we can load some library functions that we
			always need and are defined at the scheme level. */
			args_t args;
			args.argc = argc-1;
			args.argv = argv+1;
			muse_apply_top_level( env, _mk_nativefn((muse_nativefn_t)fn_repl,&args), MUSE_NIL );
		}
	}
	else
	{
		/* Its not an executable. Start the REPL. */
		args_t args;

		/* Set the "*program*" symbol to the string of the program name - i.e. argv[0]. */
		_define( _csymbol(k_program_string_name), muse_mk_ctext_utf8( env, argv[0] ) );

		args.argc = argc-1; /* Skip the first entry, which is the executable's path. */
		args.argv = argv+1;

		muse_apply_top_level( env, _mk_nativefn((muse_nativefn_t)fn_repl,&args), MUSE_NIL );
	}

	muse_destroy_env(env);
	return 0;
}
