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
#include <stdlib.h>
#include <string.h>

/**
 * Computes the size of the given file using fseek.
 */
static int fsize( FILE *f )
{
	int pos = 0;
	int size = 0;
	
	pos = ftell( f );
	fseek( f, 0, SEEK_END );
	size = ftell( f );
	fseek( f, pos, SEEK_SET );
	
	return size;
}

enum {
	MUSE_FOOTER_SIZE = 20,
	MUSE_FOOTER_SIGNATURE_SIZE = 8
};

static const char *k_args_exec_switch				= "--exec";
static const char *k_footer_signature				= "muSEexec";
static const char *k_footer_print_format			= ";%010d muSEexec";
static const char *k_footer_scan_format			= ";%d muSEexec";
static const muse_char *k_main_function_name		= L"main";

/**
 * A muSE executable is a binary file to the end of which souce code is
 * appended. This source code is expected to be loaded before any other
 * operation is done. The way we determine whether such attached source
 * code is available or not is to check the last 20 bytes which must have
 * the following format -
 *     ;<10-digit-decimal-number> muSEexec
 * The decimal number gives the size in bytes of the source code stream
 * that precedes this end code. The rationale for the above format for
 * the ending code is that when the source code is loaded, this sequence
 * will be ignored because it looks like a scheme comment.
 *
 * is_muSEexec checks whether such an end code is present in the given
 * executable file and is so, returns the source start position and source
 * size in the locations supplied as arguments.
 */
static int is_muSEexec( FILE *e, int *source_pos, int *source_size )
{
	int e_size = fsize(e);
	char signature[MUSE_FOOTER_SIGNATURE_SIZE+1];
	memset( signature, 0, MUSE_FOOTER_SIGNATURE_SIZE+1 );
	
	fseek( e, e_size - MUSE_FOOTER_SIGNATURE_SIZE, SEEK_SET );	
	fread( signature, 1, MUSE_FOOTER_SIGNATURE_SIZE, e );
	if ( strcmp( signature, k_footer_signature ) != 0 )
	{
		fclose(e);
		return 0;
	}
	
	/* The last 8 characters are muSEexec indeed.
	Parse for ";%d muSEexec" from the last 20 bytes. */
	{
		char ending[MUSE_FOOTER_SIZE+1];
		memset( ending, 0, MUSE_FOOTER_SIZE+1 );
		fseek( e, e_size - MUSE_FOOTER_SIZE, SEEK_SET );
		fread( ending, 1, MUSE_FOOTER_SIZE, e );
		sscanf( ending, k_footer_scan_format, source_size );
	}
	
	/* The source size encoded does not include the 20-byte ending sequence. */
	(*source_pos) = e_size - MUSE_FOOTER_SIZE - (*source_size);
	return 1;
}

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

	e_size = fsize(e);
	
	/* Copy executable to output first. The executable itself may have 
	source code at the end, so check for that. */
	{
		int s_pos = 0;
		int s_size = 0;
		
		fseek( e, 0, SEEK_SET );
		buffer = malloc( e_size );
		fread( buffer, 1, e_size, e );

		if ( is_muSEexec( e, &s_pos, &s_size ) )
		{
			/* Skip the ending 20 bytes, which will be ";<source-size> muSEexec",
			but tack on the source size to the total, so that the new source
			will simply be appended to the old source. */
			s_totalsize += s_size;
			e_size -= MUSE_FOOTER_SIZE;
		}
		
		fwrite( buffer, 1, e_size, o );
		free( buffer );
		fclose(e);
	}
	
	/* Copy the source files to the end. */
	{
		int ix = 1;
		for ( ; ix < argc; ++ix )
		{
			FILE *s = fopen( argv[ix], "rb" );
			
			if ( s != NULL )
			{
				int s_size = fsize(s);
				buffer = malloc( s_size );
				fread( buffer, 1, s_size, s );
				fwrite( buffer, 1, s_size, o );
				free( buffer );
				fclose(s);		
				s_totalsize += s_size;
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
	{
		int n = fprintf( o, k_footer_print_format, s_totalsize );
		muse_assert( n == MUSE_FOOTER_SIZE );
		fclose(o);
	}
	
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
	if ( is_muSEexec( e, &s_pos, &s_size ) )
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

/**
 * Usage:
 * A)	fullpath-to-muse/muse --exec execfile source1.scm source2.scm ...
 *
 *		Creates an executable "execfile" of the given source files.
 *		When this executable is run, the source files attached to
 *		it will automatically be loaded.
 *		@see is_muSEexec() for details about identifying such source code.
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
			data.argc = argc-1; /* Skip the first entry, which is the executable's path. */
			data.argv = argv+1;
			
			args = muse_generate_list( env, args_list_generator, &data );
			_apply( fn_main, args, MUSE_TRUE );
		}
		else
		{
			/* Drop into the REPL if there is no main function defined.
			This way, we can load some library functions that we
			always need and are defined at the scheme level. */
			muse_repl(env);
		}
	}
	else
	{
		/* Its not an executable. Start the REPL. */
		muse_repl(env);
	}

	muse_destroy_env(env);
	return 0;
}
