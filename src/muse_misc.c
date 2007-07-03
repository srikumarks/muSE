/**
 * @file muse_misc.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_opcodes.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#ifdef MUSE_PLATFORM_WINDOWS
#include <windows.h>
#else
#include <sys/time.h>
#endif

#include "muse_port.h"

/**
 * Creates a simple hash of the given data. This hash is sufficient
 * for strings, but not robust enough for arbitrary data. Use
 * some other hash scheme if you want greater strength.
 */
muse_int muse_hash_data( const unsigned char *start, const unsigned char *end, muse_int initial )
{
	muse_int hash = initial;
	
	for ( ; start < end; ++start )
	{
		hash = hash * 65599 + (*start);
	}
	
	return hash;
}

/**
 * A hash similar to muse_hash_data, but tailored for text.
 */
muse_int muse_hash_text( const muse_char *start, const muse_char *end, muse_int initial )
{
	muse_int hash = initial;
	
	for ( ; start < end; ++start )
	{
		hash = hash * 65599 + (*start);
	}
	
	return hash;
}

/**
 * Hashes a given cell reference. The hash has the
 * property that if two cells have different hashes,
 * they are not eq.
 */
muse_int muse_hash( muse_env *env, muse_cell obj )
{
	int t = _cellt(obj);
	
	switch ( t )
	{
		case MUSE_INT_CELL:
		case MUSE_FLOAT_CELL:
			{
				const unsigned char *p = (const unsigned char *)_ptr(obj);
				return muse_hash_data( p, p + sizeof(muse_cell_data), t );
			}
		case MUSE_TEXT_CELL:
			return muse_hash_text( 
								   _ptr(obj)->text.start, 
								   _ptr(obj)->text.end, 
								   MUSE_TEXT_CELL );
		case MUSE_SYMBOL_CELL:
			return _ptr(_head(_head(_tail(obj))))->i;
		default:
			{
				const unsigned char *p = (const unsigned char *)&obj;
				return muse_hash_data( p, p + sizeof(obj), t ); 
			}
	}
}

#ifdef MUSE_PLATFORM_WINDOWS
typedef struct 
{
	LARGE_INTEGER t1, t2, freq;
} ticktock_t;
#else
typedef struct
{
	struct timeval t1, t2;
	struct timezone tz;
} ticktock_t;
#endif

/**
 * Used for performance timing. \c muse_tick()
 * captures the current time and returns a handle
 * containing the time information. Use it with
 * \c muse_tock() to get the time elapsed between
 * the two calls in microseconds.
 */
void* muse_tick()
{
	ticktock_t *timing = (ticktock_t*)malloc(sizeof(ticktock_t));

#ifdef MUSE_PLATFORM_WINDOWS
	QueryPerformanceFrequency( &timing->freq );
	QueryPerformanceCounter( &timing->t1 );
#else
	gettimeofday(&timing->t1, &timing->tz);
#endif

	return timing;
}

/**
 * Returns the time elapsed since the start of the given timer.
 * The timer is the object returned by muse_tick().
 */
muse_int muse_elapsed_us(void *arg)
{
	ticktock_t *timing = (ticktock_t*)arg;
	muse_int diff = 0;

#ifdef MUSE_PLATFORM_WINDOWS
	QueryPerformanceCounter( &timing->t2 );
	diff = (timing->t2.QuadPart - timing->t1.QuadPart);
	diff *= 1000000;
	diff /= timing->freq.QuadPart;

#else
	gettimeofday(&timing->t2, &timing->tz);
	diff = timing->t2.tv_sec - timing->t1.tv_sec;
	diff *= 1000000;
	diff += timing->t2.tv_usec - timing->t1.tv_usec;
#endif

	return diff;
}

/**
 * When passed the timing handle returned by \c muse_tick(),
 * it returns the time elapsed between the \c muse_tick()
 * call and this \c muse_tock() call in microseconds. The
 * handle becomes unusable after it is passed to \c muse_tock().
 */
muse_int muse_tock(void* arg)
{
	muse_int diff = muse_elapsed_us(arg);
	free(arg);
	return diff;
}

/**
 * Sleeps the muse process for the given time 
 * in microseconds.
 */
void muse_sleep( muse_int time_us )
{
#ifdef MUSE_PLATFORM_WINDOWS
	Sleep( (DWORD)(time_us / 1000) );
#else
	struct timeval tv = { (time_t)(time_us / 1000000), (suseconds_t)(time_us % 1000000) };
	select( 1, NULL, NULL, NULL, &tv );
#endif
}

/**
 * A wrapper to the common fopen function so that we can call _wfopen
 * if available, or the narrow version if the wide version isn't available.
 */
FILE* muse_fopen( const muse_char *filename, const muse_char *options )
{
#ifdef MUSE_PLATFORM_WINDOWS
	return _wfopen( filename, options );
#else
	int length						= wcslen(filename);
	int utf8_size					= muse_utf8_size(filename,length);
	char *narrow_filename			= (char*)alloca( utf8_size );
	char *narrow_options			= (char*)alloca(8);

	utf8_size = muse_unicode_to_utf8( narrow_filename, utf8_size, filename, length );
	muse_unicode_to_utf8( narrow_options, 8, options, wcslen(options) );

	return fopen( narrow_filename, narrow_options );
#endif
}

/**
 * Converts the given double-byte unicode string to multibyte UTF8 string.
 * Note that it'll place the terminating null character only if the output
 * buffer can hold it as well.
 *
 * @param out The output buffer to store the UTF8 string.
 * @param out_maxlen The maximum number of characters (bytes) that the output buffer can hold.
 * @param win The input wide character string.
 * @param win_len The number of input wide characters to convert.
 * @return The number of bytes written to the output UTF8 buffer, excluding any
 * terminating null character that we written.
 */
size_t muse_unicode_to_utf8( char *out, size_t out_maxlen, const muse_char *win, size_t win_len )
{
#ifdef MUSE_PLATFORM_WINDOWS
	int result = WideCharToMultiByte( CP_UTF8, 0, win, (int)win_len, out, (int)out_maxlen, NULL, NULL );
#else
	int result = (int)wcstombs( out, win, out_maxlen );
#endif

	assert( win_len == 0 || result > 0 );

	if ( result < (int)out_maxlen )
		out[result] = '\0';

	return result;
}

/**
 * Converts the given UTF8 string to double-byte unicode string.
 * Note that it'll place the terminating null character only if the output
 * buffer can hold it as well.
 *
 * @param in The input UTF8 encoded string.
 * @param in_len The number of bytes in the UTF8 string.
 * @param wout The output buffer to store the double byte unicode string.
 * @param wout_maxlen The maximum number of double-byte *characters* that can 
 *   be written to the output buffer.
 * @return The number of output *characters* written the buffer, excluding any
 * terminating null character that might have been written.
 */
size_t muse_utf8_to_unicode( muse_char *wout, size_t wout_maxlen, const char *in, size_t in_len )
{
#ifdef MUSE_PLATFORM_WINDOWS
	int result = MultiByteToWideChar( CP_UTF8, 0, in, (int)in_len, wout, (int)(wout_maxlen * sizeof(muse_char)) );
#else
	int result = (int)mbstowcs( wout, in, wout_maxlen );
#endif

	assert( in_len == 0 || result > 0 );

	if ( result < (int)wout_maxlen )
		wout[result] = 0;

	return result;
}

/**
 * Returns an estimate of the number of bytes (conservative upper bound)
 * that might be needed to represent the given number of unicode characters
 * using UTF8 encoding.
 */
size_t muse_utf8_size( const muse_char *wstr, size_t length )
{
	assert( length >= 0 );
	return (length + 1) * 2 * sizeof(muse_char);
}


/**
 * Returns an estimate (conservative upper bound) of the number of
 * bytes that will be required to store the converted unicode 
 * version of the given utf8 string.
 */
size_t	muse_unicode_size( const char *utf8, size_t nbytes )
{
	return (nbytes+1) * sizeof(muse_char);
}

#ifdef MUSE_DEBUG_BUILD
/**
 * Prints out an muse_assertion failure message in debug builds.
 */
void muse_assert_failed( void *_env, const char *file, int line, const char *condtext )
{
	muse_env *env = _env;
	static const char *k_heading = "muSE:\tASSERT failed!\n";

	muse_port_t out = _stdport( MUSE_STDERR_PORT );
	port_write( (void*)k_heading, strlen(k_heading), out );
	port_write( "Cond:\t", 6, out );
	port_write( (void*)condtext, strlen(condtext), out );
	port_putc( '\n', out );
	port_write( "File:\t", 6, out );
	port_write( (void*)file, strlen(file), out );
	port_putc( '\n', out );
	port_write( "Line:\t", 6, out );

	{
		char buffer[32];
		int n = sprintf( buffer, "%d", line );
		port_write( buffer, n, out );
	}
	port_putc( '\n', out );
	port_putc( '\n', out );

	assert(0);
}
#endif

/**
 * Returns a non-mutable string describing the type of
 * the \p thing passed as the parameter.
 */
const muse_char *muse_typename( muse_cell thing )
{
	static const muse_char *k_type_names[] =
	{
		L"list",
		L"function",
		L"symbol",
		L"primitive",
		L"integer",
		L"fractional",
		L"string",
		L"INTERNAL"
	};

	return k_type_names[_cellt(thing)];
}

static void muse_exception(muse_env *env, muse_cell args)
{
	muse_cell ehsym = _csymbol(L"{exception-handler}");
	muse_cell eh = _symval(ehsym);
	if ( eh != ehsym )
		_apply( eh, args, MUSE_TRUE );
}

#define _sprintf_object(buffer,maxlen,thing) muse_sprintf_object(env,buffer,maxlen,thing)
static size_t muse_sprintf_object( muse_env *env, muse_char *buffer, size_t maxlen, muse_cell thing );
#define _sprintf_list(buffer,maxlen,thing) muse_sprintf_list(env,buffer,maxlen,thing)
static size_t muse_sprintf_list( muse_env *env, muse_char *buffer, size_t maxlen, muse_cell list );
#define _sprintf_text(buffer,maxlen,thing) muse_sprintf_text(env,buffer,maxlen,thing)
static size_t muse_sprintf_text( muse_env *env, muse_char *buffer, size_t maxlen, muse_cell thing );

static size_t muse_sprintf_list( muse_env *env, muse_char *buffer, size_t maxlen, muse_cell thing )
{
	size_t len = 0;
	muse_cell thing_iter = thing;
	buffer[len++] = '(';
	buffer[len] = '\0';

	while ( thing_iter && maxlen - len > 0 )
	{
		if ( _cellt(thing_iter) == MUSE_CONS_CELL )
		{
			len += _sprintf_object( buffer + len, maxlen - len, _head(thing_iter) );
			if ( maxlen > len && _tail(thing_iter) )
			{
				buffer[len++] = ' ';
				buffer[len] = '\0';
			}

			thing_iter = _tail(thing_iter);
		}
		else
		{
			len += swprintf( buffer + len, maxlen - len, L" . " );
			len += _sprintf_object( buffer + len, maxlen - len, thing_iter );
			break;
		}
	}

	if ( maxlen - len > 1 )
	{
		buffer[len++] = ')';
		buffer[len] = '\0';
	}

	return len;
}

static size_t muse_sprintf_text( muse_env *env, muse_char *buffer, size_t maxlen, muse_cell thing )
{
	size_t len = 0;
	int textlen = 0;
	const muse_char *text = _text_contents( thing, &textlen );
	if ( (size_t)textlen > maxlen - len - 2 )
	{
		/* Too big for buffer. Shorten it. */
		if ( maxlen - len > 5 )
		{
			/* Can show with ellipsis. */
			size_t end = maxlen - 4;
			buffer[len++] = '"';
			while ( len < end )
			{
				buffer[len++] = (*text++);
			}
			buffer[len++] = '.';
			buffer[len++] = '.';
			buffer[len++] = '.';
			buffer[len++] = '"';
		}
		else if ( maxlen - len > 2 )
		{
			/* Can only show 2 characters. */
			buffer[len++] = '%';
			buffer[len++] = 'T';
		}
		else
		{
			/* No space to show any thing. Indicate that using a \ character. */
			buffer[len++] = '\\';
		}
	}
	else
	{
		/* String can be shown in its entirety. */
		buffer[len++] = '"';
		memcpy( buffer + len, text, textlen * sizeof(muse_char) );
		len += textlen;
		buffer[len++] = '"';
	}

	return len;
}

static size_t muse_vsprintf_object( muse_env *env, muse_char *buffer, size_t maxlen, va_list *argv )
{
	return _sprintf_object( buffer, maxlen, va_arg( *argv, muse_cell ) );
}

static size_t muse_sprintf_object( muse_env *env, muse_char *buffer, size_t maxlen, muse_cell thing )
{
	size_t len = 0;

	switch ( _cellt(thing) )
	{
	case MUSE_SYMBOL_CELL	: 
		{
			const muse_char *name = muse_symbol_name(env,thing);
			if ( name ) {
				size_t name_len = wcslen(name);
				if ( name_len > maxlen - len )
					name_len = maxlen - len;
				if ( name_len >= 1 )
				{
					memcpy( buffer + len, name, sizeof(muse_char) * name_len );
					len += name_len;
				}
			} else {
				/* Anonymous object. */
				char obj[64];
				obj[63] = '\0';
				snprintf( obj, 64, "<obj:%x>", thing );
				len += muse_utf8_to_unicode( buffer + len, (maxlen - len), obj, strlen(obj) );
			}
		}
		break;
	case MUSE_TEXT_CELL		: 
		len += _sprintf_text( buffer + len, maxlen - len, thing );
		break;
	case MUSE_INT_CELL		: 
		if ( maxlen - len > 16 )
		{
			char number[64];
			snprintf( number, 64, MUSE_FMT_INT, _intvalue(thing) );
			len += muse_utf8_to_unicode( buffer + len, maxlen - len, number, strlen(number) );
		}
		break;
	case MUSE_FLOAT_CELL	: 
		if ( maxlen - len > 16 )
		{
			char number[64];
			snprintf( number, 64, MUSE_FMT_FLOAT, _floatvalue(thing) );
			len += muse_utf8_to_unicode( buffer + len, maxlen - len, number, strlen(number) );
		}
		break;
	case MUSE_CONS_CELL		: 
		len += _sprintf_list( buffer + len, maxlen - len, thing );
		break;
	case MUSE_LAMBDA_CELL	: 
		len += swprintf( buffer + len, maxlen - len, L"<fn:%x>", thing );
		break;
	case MUSE_NATIVEFN_CELL : 
		len += swprintf( buffer + len, maxlen - len, L"<prim:%x>", thing );
		break;
	default					: 
		buffer[0] = '\0';
	}

	return len;
}

static size_t muse_vsprintf_type( muse_env *env, muse_char *buffer, size_t maxlen, va_list *argv )
{
	muse_cell thing = va_arg( *argv, muse_cell );
	const muse_char *tname = muse_typename( thing );
	size_t tlen = wcslen( tname );
	if ( tlen > maxlen )
		tlen = maxlen;
	memcpy( buffer, tname, tlen * sizeof(muse_char) );
	return tlen;
}

static size_t muse_vsprintf( muse_env *env, muse_char *buffer, size_t maxlen, const muse_char *format, va_list *argv )
{
	size_t len = 0;

	while ( len < maxlen && format[0] )
	{
		if ( format[0] == '%' )
		{
			switch ( format[1] )
			{
			case 'm' :
				/* We treat "%m" as an indicator to place a muse object. */
				len += muse_vsprintf_object( env, buffer + len, maxlen - len, argv );
				break;
			case 't' :
				/* "%t" shows the type of the muse object passed. */
				len += muse_vsprintf_type( env, buffer + len, maxlen - len, argv );
				break;
			case 'i' :
				{
					/* %i shows the cell index of the object. */
					len += swprintf( buffer + len, maxlen - len, L"%d", _celli(va_arg( *argv, muse_cell )) );
				}
				break;
			case 's' :
				{
					/* %s accepts a literal string (unicode). */
					const muse_char *lit = (const muse_char *)va_arg( *argv, muse_char * );
					size_t litlen = wcslen(lit);
					if ( litlen > maxlen - len )
						litlen = maxlen - len;
					memcpy( buffer + len, lit, litlen * sizeof(muse_char) );
					len += litlen;
				}
				break;
			case 'f' :
				{
					char number[64];
					muse_float f = va_arg( *argv, muse_float );
					snprintf( number, 64, MUSE_FMT_FLOAT, f );
					len += muse_utf8_to_unicode( buffer + len, maxlen - len, number, strlen(number) );
				}
				break;
			case 'd' :
				{
					char number[64];
					muse_int d = va_arg( *argv, muse_int );
					snprintf( number, 64, MUSE_FMT_INT, d );
					len += muse_utf8_to_unicode( buffer + len, maxlen - len, number, strlen(number) );
				}
				break;
			default:
				{
					/* Some other character. Use it literally. */
					buffer[len++] = format[1];
				}
			}

			format += 2;
		}
		else
		{
			/* Its not %. Use it as is. */
			buffer[len++] = (*format++);
		}
	}

	buffer[len] = '\0';
	return len;
}

/**
 * Formats a string consisting of the given format string with
 * the other argument objects inserted at specified positions.
 *
 *	- \%m prints the next argument given as a muse_cell.
 *	- \%t prints the type of the argument given as a muse_cell.
 *	- \%i prints the cell index of the argument. This is
 *		like a pointer to the cell.
 *	- \%s means the next argument is a unicode string and 
 *		has to be included literally in the message.
 *	- \%f means the next argument is a muse_float.
 *	- \%d means the next argument is a muse_int.
 *	- \%\% prints the \% character itself.
 *
 * @return The number of characters used in the buffer.
 *
 * @todo The maxlen is not strictly adhered to and its good to
 * give an extra 20 characters more than what you actually need.
 * Needs to be fixed to be strict.
 *
 * @see muse_vsprintf
 */
size_t muse_sprintf( muse_env *env, muse_char *buffer, size_t maxlen, const muse_char *format, ... )
{
	size_t len = 0;
	va_list args;

	va_start( args, format );
	len = muse_vsprintf( env, buffer, maxlen, format, &args );
	va_end( args );
	return len;
}

/**
 * Displays a message string formatted according to the given
 * message string with format codes. The format codes are the
 * same as for muse_sprintf().
 *
 * @param message A format string possibly containing codes
 * indicating where and how to insert a string representation
 * of further arguments.
 *
 * @see muse_sprintf
 * @see muse_vsprintf
 */
void muse_message( muse_env *env, const muse_char *context, const muse_char *message, ... )
{
	enum { MAXLEN = 512 };
	muse_char text[MAXLEN];
	va_list args;
	size_t len = 0;

	muse_assert( wcslen(context) < MAXLEN-60);
	#if MUSE_PLATFORM_WINDOWS
		len += swprintf( text, MAXLEN, L"Context:    %s\n\n", context );
	#else
		len += swprintf( text, MAXLEN, L"%ls\n\n", context );
	#endif

	va_start( args, message );
	len += muse_vsprintf( env, text + len, MAXLEN - len, message, &args );
	va_end( args );

	#if MUSE_PLATFORM_WINDOWS
		switch ( MessageBoxW( NULL, text, L"muSE", MB_ABORTRETRYIGNORE | MB_SETFOREGROUND | MB_TOPMOST | MB_ICONWARNING ) )
		{
		case IDABORT: exit(0);
		case IDRETRY: 
			#if defined(_DEBUG)
				DebugBreak();
			#endif
			muse_exception(env,MUSE_NIL); 
		default:;
		}
	#else
		fprintf( stderr, "===========================\nmuSE diagnostics:\n%ls\n===========================\n", text );
	#endif
}


static muse_boolean muse_test( muse_env *env, const muse_char *context, const muse_char *spec, va_list *argv );
static muse_boolean muse_test_one( muse_env *env, const muse_char *context, muse_boolean force, muse_boolean invert, 
								  muse_cell symbol, muse_cell value, 
								  const muse_char *spec, va_list *argv,
								  muse_char *failure_descr);
static size_t levenshtein_distance( const muse_char *s1, const muse_char *s2 );

/**
 * Checks whether a set of expectations are satisfied by some values.
 * The \p spec parameter is a string of condition characters that consume
 * arguments and perform the appropriate tests. This function, which is itself
 * a sort of mini language, is motivated by the fact that I've often coded
 * ASSERT statements encoding multiple conditions that must be jointly
 * satisfied in some way, having the assert fail, and not immediately knowing 
 * which of those conditions was responsible for the failure and the
 * vales involved. If the entire set of expectations fails in some way,
 * the generated error report (message box or stderr print out) is rather 
 * informative about which exact expectation was violated.
 *
 * - 's' -> The next argument is a symbol (given as muse_cell) which will 
 *			be used as the subject of subsequent tests. Its value will be
 *			used in any tests that may require it.
 *
 * - 'v' -> The next argument is a value (given as a muse_cell) which will 
 *			be used as the subject of subsequent tests. You can use this
 *			if you want to check some properties of a value which is not the
 *			value of any symbol you know. If you know the symbol, then use 's'
 *			in order to get better diagnostic messages.
 *
 * - ':' ->	The symbol must be defined to some value. The argument
 *			is expected to be a unicode string that describes what
 *			the symbol must be defined as. Ex: "a function", 
 *			"a number", etc. This label is used in the message text.
 *
 * - '?' ->	Next argument is int and the value of the symbol
 *			must be of that type - given by enum muse_cell_t.
 *
 * - '=' ->	The symbol's value must be "eq" to the next argument,
 *			which is given as muse_cell.
 *
 * - '[' ->	The next argument must be a muse_float and the symbol's
 *			value will be checked for >= the given limit.
 *
 * - ']' ->	The next argument must be a muse_float and the symbol's
 *			value will be checked for <= the given limit.
 *
 * - '(' ->	The next argument must be a muse_float and the symbol's
 *			value will be checked for > the given limit.
 *
 * - ')' ->	The next argument must be a muse_float and the symbol's
 *			value will be checked for < the given limit.
 * 
 * - "|blah|" ->	The symbol must satisfy at least one of the conditions 
 *				within the || pair. i.e. the conditions are "or"ed.
 *
 * - '!' ->	Placing this character causes the result of the following test
 *			to be inverted. For example, a "not-defined" check can be encoded
 *			as "!:". The following test can be an or-block too.
 */
muse_boolean muse_expect( muse_env *env, const muse_char *context, const muse_char *spec, ... )
{
	va_list args;
	muse_boolean result = MUSE_FALSE;

	va_start( args, spec );
	result = muse_test( env, context, spec, &args );
	va_end( args );

	return result;
}

/**
 * Sometimes, it is useful to know whether another symbol 
 * that's very similar in textual representation from a
 * known symbol. For example, the user may make a spelling
 * mistake in keying in a symbol and it'll be useful to know
 * the alternative. You can use this function in such
 * circumstances to determine the symbol that is very similar
 * to the known symbol, yet different, so that you can warn the
 * user about the closeness. 
 *
 * You can obtain the levenshtein distance measure between the 
 * two symbols if you supply an int pointer in \p outDistance. 
 * You can then use the distance measure to decide whether the 
 * similar symbol is worth bothering about.
 */
muse_cell muse_similar_symbol( muse_env *env, muse_cell symbol, int *outDistance )
{
	int distance = 0x7fffffff;
	int result = MUSE_NIL;

	const muse_char *s1 = muse_symbol_name(env,symbol);

	/* Traverse the symbol table and find the symbol that is
	not the given symbol, but is the least edit distance from it. */

	muse_stack symbols = env->symbol_stack;

	int i;
	for ( i = 0; i < symbols.size; ++i )
	{
		muse_cell symlist = symbols.bottom[i];

		/* Each entry in the symbols hash table is a list of symbols.
		So traverse the list and examine each symbol. */

		while ( symlist )
		{
			muse_cell s2 = _head(symlist);
			const muse_char *s2name = muse_symbol_name(env,s2);

			/* Don't consider comparing the symbol with itself.
			Ignore operators and special symbols. */
			if ( s2 != symbol && isalpha(s2name[0]) )
			{
				int d = (int)levenshtein_distance( s1, s2name );
				if ( d < distance )
				{
					result = s2;
					distance = d;
				}
			}

			symlist = _tail(symlist);
		}
	}

	if ( outDistance )
		(*outDistance) = distance;

	return result;
}

/**
 * Searches the symbol table to find a symbol whose current value is
 * the given cell. The value's contents are not used in the comparison,
 * but the symbol must actually contain this particular cell as its
 * value. This function is useful to lookup a function name given a
 * native function object. If a symbol exists with the given value,
 * it is returned. Otherwise it returns MUSE_NIL.
 *
 * Beware that the complexity of this operation is linear w.r.t.
 * the number of symbols in the environment - defined or undefined.
 */
muse_cell muse_symbol_with_value( muse_env *env, muse_cell value )
{
	/* Traverse the symbol table and find the symbol that
	has the given cell as its value. */

	muse_stack symbols = env->symbol_stack;
	int i;
	for ( i = 0; i < symbols.size; ++i )
	{
		muse_cell symlist = symbols.bottom[i];
		
		while ( symlist )
		{
			muse_cell sym = _next(&symlist);

			if ( _symval(sym) == value ) /* Value found. */
				return sym;
		}
	}

	return MUSE_NIL;
}

static muse_boolean muse_test( muse_env *env, const muse_char *context, const muse_char *spec, va_list *argv )
{
	muse_boolean or_mode = MUSE_FALSE;

	muse_boolean and_result = MUSE_TRUE;
	muse_boolean or_result = MUSE_FALSE;
	muse_boolean invert = MUSE_FALSE, or_invert = MUSE_FALSE;
	const muse_char *or_start = NULL;
	muse_char *failure_descr = (muse_char*)alloca( sizeof(muse_char) * 1024 );
	muse_char *failure_descr_append = failure_descr;
	muse_cell symbol = _csymbol(L"(parameter)");
	muse_cell value = MUSE_NIL;
	failure_descr_append[0] = '\0';

	while ( spec[0] )
	{
		switch ( spec[0] )
		{
		case 's' :
			{
				/* Take the test symbol from the argument list. */
				symbol = va_arg( *argv, muse_cell );
				muse_assert( _cellt(symbol) == MUSE_SYMBOL_CELL );
				value = _symval(symbol);
			}
			break;
		case 'v' :
			{
				/* Take the test value from the argument list. */
				value = va_arg( *argv, muse_cell );
			}
			break;
		case '!' :
			{
				/* The next condition needs to be inverted. */
				invert = !invert;
			}
			break;
		default :
			{
				if ( or_mode )
				{
					if ( spec[0] == '|' )
					{
						or_mode = MUSE_FALSE;
						or_start = NULL;

						if ( !or_result )
						{
							if ( symbol == value )
								muse_message( env,context, or_invert 
															? L"[%m] must not satisfy any of the conditions - \n%s"
															: L"[%m] doesn't satisfy at least one of the conditions -\n%s",
														symbol, failure_descr );
							else
								muse_message( env,context, or_invert 
															? L"[%m], which is [%m], must not satisfy any of the conditions - \n%s"
															: L"[%m], which is [%m], doesn't satisfy at least one of the conditions -\n%s",
														symbol, value, failure_descr );
							return MUSE_FALSE;
						}

						invert = or_invert = MUSE_FALSE;
					}
					else
					{
						muse_boolean test_result = muse_test_one( env, context, MUSE_FALSE, or_invert ^ invert, symbol, value, spec, argv, failure_descr_append );
						failure_descr_append += wcslen(failure_descr_append);
						if ( or_invert ) 
						{
							or_result &= test_result;
							if ( !or_result )
								break;
						}
						else
							or_result |= test_result;
					}
				}
				else
				{
					if ( spec[0] == '|' )
					{
						or_mode = MUSE_TRUE;
						or_start = spec;

						/* The following or-block needs to be inverted, in 
						which case the or_result holds the & of the inverses of
						all the conditions in the or-block. */
						or_result = or_invert = invert;
						invert = MUSE_FALSE;
					}
					else
					{
						and_result &= muse_test_one( env, context, MUSE_TRUE, invert, symbol, value, spec, argv, failure_descr_append );
						failure_descr_append += wcslen(failure_descr_append);
						invert = MUSE_FALSE;
						if ( !and_result )
							break;
					}
				}
			}
		}

		++spec;
	}

	return and_result;
}

static muse_boolean muse_test_limit( muse_env *env, const muse_char *context, muse_boolean force, 
									muse_cell symbol, muse_cell value, muse_float limit, 
									int comparison, int invert, const muse_char *descr,
									muse_char *failure_descr)
{
	/* This function clubs all 4 kinds of numeric comparisons into a single body.

	comparison should either be 1 or -1 depending on whether you want to compare for
	value > limit or value < limit. 

	invert should be 0 or 1 depending on whether you're checking for the inverse of the
	comparison meant by the value of comparison.

	Therefore you check for >= by turning it into "not less than".

	descr is a string describing the comparison operator itself. */

	if ( _cellt(value) == MUSE_INT_CELL )
	{
		muse_int c = comparison * (_intvalue(value) - (muse_int)limit);
		if ( invert ? (c > 0) : (c <= 0) )
		{
			if ( force )
				muse_message( env,context, L"[%m] must have an integer value %s %d.\nIt is [%m] instead.", 
								symbol, descr, (muse_int)limit, value );
			else
				muse_sprintf( env, failure_descr, 256, L"\t- be an integer %s %d\n", descr, (muse_int)limit );
			return MUSE_FALSE;
		}
		else
			return MUSE_TRUE;
	}
	else if ( _cellt(value) == MUSE_FLOAT_CELL )
	{
		muse_float c = comparison * (_floatvalue(value) - limit); 
		if ( invert ? (c > 0) : (c <= 0) )
		{
			if ( force )
				muse_message( env,context, L"[%m] must have a value %s %f.\nIt is [%m] instead.", 
								symbol, descr, (muse_float)limit, value );
			else
				muse_sprintf( env, failure_descr, 256, L"\t- be a number %s %f\n", descr, (muse_float)limit );
			return MUSE_FALSE;
		}
		else
			return MUSE_TRUE;
	}
	else
	{
		if ( force )
			muse_message( env,context, L"[%m] must have a numeric value.\nIt is [%m] instead.", symbol, value );
		else
			swprintf( failure_descr, 64, L"\t- must be a number\n" );

		return MUSE_FALSE;
	}
}


static muse_boolean muse_test_one( muse_env *env, const muse_char *context, muse_boolean force, muse_boolean invert, 
								  muse_cell symbol, muse_cell value, 
								  const muse_char *spec, va_list *argv,
								  muse_char *failure_descr)
{
	switch ( spec[0] )
	{
		case '?' :
			{
				muse_cell_t type = (muse_cell_t)va_arg( *argv, int );
				if ( invert ? _cellt(value) == type : _cellt(value) != type )
				{
					if ( force )
					{
						muse_message(	env, context, 
										L"%m %sexpected to have a value of type %t.\nInstead it is %m.", 
										symbol, invert ? L"not " : L"", type, value );
					}
					else
					{
						muse_sprintf( env, failure_descr, 32, L"\t- be of type %t\n", type );
					}

					return MUSE_FALSE;
				}
				else
					return MUSE_TRUE;
			}
			break;
		case ':' :
			{
				const muse_char *label = (const muse_char *)va_arg( *argv, muse_char* );
				if ( invert ? symbol != value : symbol == value )
				{
					if ( force )
					{
						if ( invert )
							muse_message( env,context, L"[%m] expected to be undefined.\n"
												   L"It is currently [%m].",
											symbol, value );
						else
							muse_message( env,context, L"[%m] is expected to be %s.\n"
												   L"Maybe you meant [%m] instead?",
												   symbol, label, muse_similar_symbol(env,symbol,NULL) );
					}
					else
					{
						swprintf( failure_descr, 128, L"\t- be %s\n", label );
					}
					return MUSE_FALSE;
				}
				else
					return MUSE_TRUE;
			}
			break;
		case '=' :
			{
				muse_cell test = va_arg( *argv, muse_cell );
				muse_boolean eq_p = muse_equal( env,value, test );
				if ( invert ? eq_p : !eq_p )
				{
					if ( force )
					{
						muse_message( env,context, L"[%m] %sexpected to have [%m] as value.\nIt is [%m] instead!", 
							symbol, invert ? L"not " : L"", test, value );
					}
					else
					{
						muse_sprintf( env, failure_descr, 256, L"\t- have value %m\n", test );
					}
					return MUSE_FALSE;
				}
				else
					return MUSE_TRUE;
			}
			break;
		case '(' :
			{
				muse_float limit = va_arg( *argv, muse_float );
				return muse_test_limit( env, context, force, symbol, value, limit, 1, invert, invert ? L"<=" : L">", failure_descr );
			}
			break;
		case ')' :
			{
				muse_float limit = va_arg( *argv, muse_float );
				return muse_test_limit( env, context, force, symbol, value, limit, -1, invert, invert ? L">=" : L"<", failure_descr );
			}
			break;
		case '[' :
			{
				muse_float limit = va_arg( *argv, muse_float );
				return muse_test_limit( env, context, force, symbol, value, limit, -1, !invert, invert ? L"<" : L">=", failure_descr );
			}
			break;
		case ']' :
			{
				muse_float limit = va_arg( *argv, muse_float );
				return muse_test_limit( env, context, force, symbol, value, limit, 1, !invert, invert ? L">" : L"<=", failure_descr );
			}
			break;
		default :
//			muse_assert( !"Invalid character in test spec for symbol! Must be one of \"?!=()[]\"." );
			return MUSE_FALSE;
	}
}

static size_t min3 ( size_t x, size_t y, size_t z )
{
	size_t m = x;
	m = (y < m) ? y : m;
	m = (z < m) ? z : m;
	return m;
}

static size_t levenshtein_distance( const muse_char *s1, const muse_char *s2 )
{
	size_t l1 = wcslen(s1);
	size_t l2 = wcslen(s2);
	size_t rowlen = l1 + 1;
	size_t size_bytes = rowlen * (l2 + 1) * sizeof(size_t);
	size_t *d = (size_t*)alloca( size_bytes );

	assert( l1 < 256 && l2 < 256 && "Warning: Don't use too long strings for distance measure!" );
	memset( d, 0, size_bytes );

	{
		size_t i,j;

		for ( i = 0; i <= l1; ++i )
			d[i] = i;
		for ( j = 0; j <= l2; ++j )
			d[j*rowlen] = j;

		for ( i = 1; i <= l1; ++i )
		{
			for ( j = 1; j <= l2; ++j )
			{
				size_t cost = (toupper(s1[i-1]) == toupper(s2[j-1])) ? 0 : 1;
				d[i + j * rowlen] = min3(
										d[ i - 1 + j * rowlen ] + 1, // deletion
										d[i + (j-1)*rowlen] + 1, // insertion
										d[ i - 1 + (j-1) * rowlen ] + cost // substitution
										);
			}
		}

		return d[l1 + rowlen * l2];
	}
}
