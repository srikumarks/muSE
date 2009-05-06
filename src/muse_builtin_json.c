/**
 * @file muse_builtin_json.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * Support for reading and writing JSON structures.
 * See http://www.json.org/ for JSON syntax.
 */

#include "muse_builtins.h"
#include "muse_port.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>

enum { MAXFRAGLEN = 128 };

typedef struct
{
	int len;
	muse_char chars[MAXFRAGLEN];
} fragment_t;

typedef struct
{
	int N;
	fragment_t **frags;
} buffer_t;

static buffer_t *buffer_alloc();
static void buffer_free( buffer_t *b );
static void buffer_putc( buffer_t *b, muse_char c );
static muse_cell buffer_to_string( buffer_t *b, muse_env *env );
static muse_cell buffer_to_symbol( buffer_t *b, muse_env *env );

static muse_cell json_read( muse_port_t p );
static muse_cell json_read_expr( muse_port_t p );
static void json_skip_whitespace( muse_port_t p );
static muse_cell json_read_number( muse_port_t p );
static muse_cell json_read_key( muse_port_t p );
static muse_cell json_read_string( muse_port_t p );
static muse_cell json_read_keyword( muse_port_t p );
static muse_cell json_read_array( muse_port_t p );
static muse_cell json_read_array_expr( muse_port_t p );
static muse_cell json_read_object( muse_port_t p );
static muse_cell json_read_object_expr( muse_port_t p );

static void json_write( muse_port_t p, muse_cell thing );
static void json_write_number( muse_port_t p, muse_cell num );
static void json_write_string( muse_port_t p, muse_cell str );
static void json_write_array( muse_port_t p, muse_cell arr );
static void json_write_object( muse_port_t p, muse_cell obj );

/**
 * {json}
 *
 * Switches the reader temporarily to the JSON reader
 * so that the next expression is treated as a JSON
 * object. After one JSON item is read in, it returns
 * the reader to the original Scheme reader. So you
 * can use {json} and then follow with an inline
 * definition of a complex nested data structure 
 * you need as a value in Scheme.
 *
 * The difference between (read-json) and {json}
 * is that (read-json) reads a constant JSON expression
 * which is therefore useful to parse query results
 * from servers. OTOH, {json} treats the following
 * JSON expression as a template from which it synthesizes
 * a muSE expression which *when evaluated* will yield
 * the JSON object. This means you can use {json}
 * within function bodies as well. When using {json},
 * you can insert arbitrary muSE expressions in the value
 * positions of objects and arrays. The JSON keywords 'true' and
 * 'false' will end up as quoted symbols and the keyword 'null' will
 * end up being translated as (). Other than that, value
 * positions can use full Scheme expressions. Here is an example -
 * @begincode
 * >  (define (info name)
 *      {json}
 *      { "name" : name,
 *        "kind" : "nice guy",
 *        "email" : (format name "@muvee.com"),
 *        "age"  : [94] } )
 * @endcode
 * The onus is on the coder to make sure that muSE sub-expressions
 * evaluate to valid JSON objects. Otherwise it will not be
 * possible to write the object using write-json.
 *
 * Note that if any part of the object expression doesn't contain
 * variables or muSE sub-expressions, then it is treated as a
 * constant and pre-evaluated at read-time so that it won't have
 * to be evaluated repeatedly. This also means that constant parts 
 * of a JSON object will end up being shared with other JSON objects. 
 * You need to be aware of this only if you want to modify the JSON objects 
 * you create.
 */
muse_cell fn_json( muse_env *env, void *context, muse_cell args )
{
	return json_read_expr( muse_current_port( env, MUSE_STDIN_PORT, NULL ) );
}

/**
 * (write-json port thing)
 * (write-json thing)
 *
 * thing is either a number, string, vector or a hashtable.
 * Writes the object in JSON format to the given port, or to
 * the current output port (usually stdout) if the port is 
 * omitted.
 */
muse_cell fn_write_json( muse_env *env, void *context, muse_cell args )
{
	muse_port_t p = NULL;
	muse_cell arg = _evalnext(&args);
	if ( args ) {
		p = muse_port( env, arg );
		arg = _evalnext(&args);
	} else {
		p = muse_current_port( env, MUSE_STDOUT_PORT, NULL );
	}

	json_write(p,arg);
	return _builtin_symbol(MUSE_T);
}

/**
 * (read-json port)
 * (read-json)
 *
 * Reads and returns a json compatible object - which is
 * either a number, a string, a vector or a hashtable.
 * If you have nested containers expressed in JSON, you
 * can use the (get ...) function to index deeply into
 * the structure. For example,
 * @begincode
 *   > (define j (read-json))
 *   {"kind":"prime-numbers","message":[2,3,5,7,11,13,17]}
 *   {hashtable '(("message" . {vector 2 3 5 7 11 13 17}) ("kind" . "prime-numbers"))}
 *   > (get j "message" 4)
 *   11
 * @endcode
 */
muse_cell fn_read_json( muse_env *env, void *context, muse_cell args )
{
	muse_port_t p = NULL;
	if ( args ) {
		p = muse_port( env, _evalnext(&args) );
	} else {
		p = muse_current_port( env, MUSE_STDIN_PORT, NULL );
	}

	return json_read(p);
}

static buffer_t *buffer_alloc()
{
	buffer_t *b = (buffer_t*)calloc( 1, sizeof(buffer_t) );
	b->N = 1;
	b->frags = (fragment_t**)calloc( 1, sizeof(fragment_t*) );
	b->frags[0] = (fragment_t*)calloc( 1, sizeof(fragment_t) );
	b->frags[0]->len = 0;
	return b;
}

static void buffer_free( buffer_t *b )
{
	int i = 0;
	for ( i = 0; i < b->N; ++i ) {
		free( b->frags[i] );
	}

	free( b->frags );
	free( b );
}

static void buffer_putc( buffer_t *b, muse_char c )
{
	if ( b->frags[b->N-1]->len >= MAXFRAGLEN ) {
		b->frags = (fragment_t**)realloc( b->frags, sizeof(fragment_t*) * (b->N + 1) );
		b->frags[b->N]->len = 0;
		b->N++;
	}

	// Add character.
	{
		fragment_t *f = b->frags[b->N-1];
		f->chars[f->len++] = c;
	}
}

static muse_cell buffer_to_string( buffer_t *b, muse_env *env )
{
	int total_size = 0, i = 0;
	for ( i = 0; i < b->N; ++i )
		total_size += b->frags[i]->len;

	{
		muse_cell txt = muse_mk_text( env, NULL, ((const muse_char *)NULL) + total_size );
		muse_char *txtptr = (muse_char*)muse_text_contents( env, txt, NULL );	

		for ( i = 0; i < b->N; ++i ) {
			fragment_t *f = b->frags[i];
			memcpy( txtptr, f->chars, f->len * sizeof(muse_char) );
			txtptr += f->len;
		}
		return txt;
	}
}

static muse_cell buffer_to_symbol( buffer_t *b, muse_env *env )
{
	muse_cell txt = buffer_to_string( b, env );
	int len = 0;
	const muse_char *txtptr = muse_text_contents( env, txt, &len );
	return muse_symbol( env, txtptr, txtptr + len );
}

static muse_cell json_read( muse_port_t p )
{
	json_skip_whitespace(p);

	if ( !port_eof(p) ) {
		muse_char c = port_getc(p);
		port_ungetc(c,p);
		switch ( c ) {
			case '"': return json_read_string(p);
			case '-': 
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9': return json_read_number(p);
			case '[': return json_read_array(p);
			case '{': return json_read_object(p);
			default:
				if ( c >= 'a' && c <= 'z' )
					return json_read_keyword(p);
				else {
					return muse_raise_error( p->env, muse_csymbol( p->env, L"json:syntax-error" ), MUSE_NIL );
				}
		}
	} else {
		return muse_raise_error( p->env, muse_csymbol( p->env, L"json:unexpected-end-of-stream" ), MUSE_NIL );
	}
}

static muse_cell json_read_expr( muse_port_t p )
{
	json_skip_whitespace(p);

	if ( !port_eof(p) ) {
		muse_char c = port_getc(p);
		port_ungetc(c,p);
		switch ( c ) {
			case '"': return json_read_string(p);
			case '-': 
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9': return json_read_number(p);
			case '[': return json_read_array_expr(p);
			case '{': return json_read_object_expr(p);
			case '(': return muse_pread(p);
			default:
				{
					muse_cell e = muse_pread(p);
					if ( _cellt(e) == MUSE_SYMBOL_CELL ) {
						const muse_char *name = muse_symbol_name(p->env,e);
						if ( wcscmp( name, L"null" ) == 0 )
							return MUSE_NIL;
						else if ( wcscmp( name, L"true" ) == 0 || wcscmp( name, L"false" ) == 0 )
							return muse_quote( p->env, e );
						else
							return e;
					}
					else
						return e;
				}
		}
	} else {
		return muse_raise_error( p->env, muse_csymbol( p->env, L"json:unexpected-end-of-stream" ), MUSE_NIL );
	}
}

static void json_skip_whitespace( muse_port_t p )
{
	if ( port_eof(p) )
		return;
	else {
		muse_char c = port_getc(p);
		if ( c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\f' )
			json_skip_whitespace(p);
		else
			port_ungetc( c, p );
	}
}

static muse_cell json_read_number( muse_port_t p )
{
	return muse_pread(p);
}

static int hexdigit( muse_char c )
{
	if ( c >= '0' && c <= '9' )
		return (int)(c - '0');
	else if ( c >= 'a' && c <= 'f' )
		return 10 + (int)(c - 'a');
	else if (c >= 'A' && c <= 'F' )
		return 10 + (int)(c - 'A');
	else {
		assert( 0 && "invalid hex digit" );
		return -1;
	}
}

static int hexchar( muse_char hex4[4] )
{
	return (hexdigit(hex4[0]) << 24) +
			(hexdigit(hex4[1]) << 16) +
			(hexdigit(hex4[2]) << 8) +
			hexdigit(hex4[3]);
}

static muse_cell json_read_key( muse_port_t p )
{
	muse_env *env = p->env;
	int sp = _spos();
	muse_cell str = json_read_string(p);

	int len = 0;
	const muse_char *strptr = muse_text_contents( env, str, &len );
	str = muse_symbol( env, strptr, strptr + len );
	_unwind(sp);
	return str;
}

static muse_cell json_read_string( muse_port_t p )
{
	muse_char c = port_getc(p);
	assert( c == '"' );

	{
		buffer_t *b = buffer_alloc();
	
		while ( !port_eof(p) ) {
			c = port_getc(p);
			if ( c == '"' ) 
				break;
			else if ( c == '\\' ) {
				c = port_getc(p);
				switch( c ) {
					case '"':	buffer_putc( b, c ); break;
					case '\\':	buffer_putc( b, c ); break;
					case '/':	buffer_putc( b, c ); break;
					case 'b':	buffer_putc( b, '\b' ); break;
					case 'f':	buffer_putc( b, '\f' ); break;
					case 'n':	buffer_putc( b, '\n' ); break;
					case 'r':	buffer_putc( b, '\r' ); break;
					case 't':	buffer_putc( b, '\t' ); break;
					case 'u':
						{
							muse_char d[4];
							d[0] = port_getc(p);
							d[1] = port_getc(p);
							d[2] = port_getc(p);
							d[3] = port_getc(p);
							buffer_putc( b, hexchar(d) );
							break;								
						}
					default:
						muse_raise_error( p->env, muse_csymbol(p->env,L"json:invalid-escape-code"), MUSE_NIL );
						buffer_putc( b, c );
						break;
				}
			} else {
				buffer_putc( b, c );
			}
		}

		{
			muse_cell str = buffer_to_string( b, p->env );
			buffer_free(b);
			return str;
		}
	}
}

static muse_cell json_read_keyword( muse_port_t p )
{
	muse_cell sym = muse_pread(p);
	const muse_char *symname = muse_symbol_name( p->env, sym );
	if ( wcscmp( symname, L"true" ) == 0 || wcscmp( symname, L"false" ) == 0 )
		return sym;
	if ( wcscmp( symname, L"null" ) == 0 )
		return MUSE_NIL;
	return muse_raise_error( p->env, muse_csymbol( p->env, L"json:invalid-symbol" ), sym );
}

static muse_cell json_read_array_items( muse_env *env, muse_port_t p, muse_cell h, muse_cell t, int N );
static muse_cell json_read_array( muse_port_t p )
{
	muse_env *env = p->env;
	muse_char c = port_getc(p);
	assert( c == '[' );
	json_skip_whitespace(p);
	return json_read_array_items( env, p, MUSE_NIL, MUSE_NIL, 0 );
}

static muse_cell json_read_array_items( muse_env *env, muse_port_t p, muse_cell h, muse_cell t, int N )
{
	int i;
	if ( port_eof(p) ) {
		return muse_raise_error( env, _csymbol(L"json:end-of-file-in-array"), MUSE_NIL );
	} else {
		muse_char c = port_getc(p);
		if ( c == ']' ) {
			muse_cell v = muse_mk_vector( env, N );
			for ( i = 0; i < N; ++i ) {
				muse_vector_put( env, v, i, _next(&h) );
			}
			return v;
		} else {
			port_ungetc( c, p );
		}
	}

	if ( h ) {
		int sp = _spos();
		muse_cell n = _cons( json_read(p), MUSE_NIL );
		_sett( t, n );
		t = n;
		_unwind(sp);
	} else { 
		h = t = _cons( json_read(p), MUSE_NIL );
	}

	json_skip_whitespace(p);

	if ( port_eof(p) ) {
		return muse_raise_error( env, _csymbol(L"json:end-of-file-in-array"), h );
	} else {
		muse_char c = port_getc(p);
		if ( c == ',' ) {
			return json_read_array_items( env, p, h, t, N+1 );
		} else if ( c == ']' ) {
			port_ungetc( c, p );
			return json_read_array_items( env, p, h, t, N+1 );
		} else {
			return muse_raise_error( env, _csymbol(L"json:array-syntax-error"), h );
		}
	}
}

static muse_cell json_read_array_expr_items( muse_env *env, muse_port_t p, muse_cell h, muse_cell t, int N );
static muse_cell json_share_array_expr( muse_env *env, muse_cell arr );
static muse_cell json_read_array_expr( muse_port_t p )
{
	muse_env *env = p->env;
	muse_char c = port_getc(p);
	assert( c == '[' );
	json_skip_whitespace(p);
	return json_share_array_expr( 
				env, 
				_cons( 
					_symval(_csymbol(L"vector")), 
					json_read_array_expr_items( env, p, MUSE_NIL, MUSE_NIL, 0 ) ) );
}

static muse_cell json_read_array_expr_items( muse_env *env, muse_port_t p, muse_cell h, muse_cell t, int N )
{
	if ( port_eof(p) ) {
		return muse_raise_error( env, _csymbol(L"json:end-of-file-in-array"), MUSE_NIL );
	} else {
		muse_char c = port_getc(p);
		if ( c == ']' ) {
			return h;
		} else {
			port_ungetc( c, p );
		}
	}

	if ( h ) {
		int sp = _spos();
		muse_cell n = _cons( json_read_expr(p), MUSE_NIL );
		_sett( t, n );
		t = n;
		_unwind(sp);
	} else { 
		h = t = _cons( json_read_expr(p), MUSE_NIL );
	}

	json_skip_whitespace(p);

	if ( port_eof(p) ) {
		return muse_raise_error( env, _csymbol(L"json:end-of-file-in-array"), h );
	} else {
		muse_char c = port_getc(p);
		if ( c == ',' ) {
			return json_read_array_expr_items( env, p, h, t, N+1 );
		} else if ( c == ']' ) {
			port_ungetc( c, p );
			return json_read_array_expr_items( env, p, h, t, N+1 );
		} else {
			return muse_raise_error( env, _csymbol(L"json:array-syntax-error"), h );
		}
	}
}

/**
 * Checks whether the given item is a JSON constant.
 */
static muse_boolean json_is_constant( muse_env *env, muse_cell item )
{
	switch ( _cellt(item) ) {
		case MUSE_TEXT_CELL:
		case MUSE_INT_CELL:
		case MUSE_FLOAT_CELL:
		case MUSE_NATIVEFN_CELL:
			return MUSE_TRUE;
		case MUSE_CONS_CELL:
			return _isquote(_head(item));
		default:
			return MUSE_FALSE;
	}
}

/**
 * Returns arr if sub-expresions are not constant.
 * Otherwise creates a constant vector.
 */
static muse_cell json_share_array_expr( muse_env *env, muse_cell arr )
{
	muse_cell items = _tail(arr);

	while ( items ) {
		if ( !json_is_constant( env, _head(items) ) )
			return arr;

		items = _tail(items);
	}

	return muse_eval( env, arr, MUSE_FALSE );
}

static muse_cell json_read_object_items( muse_env *env, muse_port_t p, muse_cell table );
static muse_cell json_read_object( muse_port_t p )
{
	muse_env *env = p->env;
	muse_char c = port_getc(p);
	assert( c == '{' );
	json_skip_whitespace(p);
	return json_read_object_items( env, p, muse_mk_hashtable( env, 8 ) );
}

static muse_cell json_read_object_items( muse_env *env, muse_port_t p, muse_cell table )
{
	json_skip_whitespace(p);

	if ( port_eof(p) ) 
		return muse_raise_error( env, _csymbol(L"json:end-of-file-in-object"), MUSE_NIL );
	else {
		muse_char c = port_getc(p);
		if ( c == '}' ) {
			return table;
		} else {
			int sp = _spos();
			muse_cell key, value;
			port_ungetc(c,p);

			key = json_read_key(p);
			json_skip_whitespace(p);
			if ( port_eof(p) ) {
				return muse_raise_error( env, _csymbol(L"json:end-of-file-in-object"), MUSE_NIL );
			} else {
				muse_char c = port_getc(p);
				if ( c == ':' ) {
					value = json_read(p);
					muse_hashtable_put( env, table, key, value );
					_unwind(sp);

					{
						muse_char c = port_getc(p);
						if ( c == ',' ) {
							return json_read_object_items( env, p, table );
						} else if ( c == '}' ) {
							return table;
						} else {
							return muse_raise_error( env, _csymbol(L"json:object-syntax-error"), table );
						}
					}
				} else {
					return muse_raise_error( env, _csymbol(L"json:object-syntax-error"), table );
				}
			}
		}
	}
}

static muse_cell json_read_object_expr_items( muse_env *env, muse_port_t p, muse_cell h, muse_cell t, int sp );
static muse_cell json_share_object_expr( muse_env *env, muse_cell objexpr );
static muse_cell json_read_object_expr( muse_port_t p )
{
	muse_env *env = p->env;
	muse_char c = port_getc(p);
	assert( c == '{' );
	json_skip_whitespace(p);
	
	{
		muse_cell h = _cons( MUSE_NIL, MUSE_NIL );
		int sp = _spos();
		muse_cell t = _cons( _symval(_csymbol(L"list")), MUSE_NIL );
		_setht( h, _symval(_csymbol(L"hashtable")), _cons( t, MUSE_NIL ) );
		_unwind(sp);
		return json_share_object_expr( env, json_read_object_expr_items( env, p, h, t, sp ) );
	}
}

static muse_cell json_read_object_expr_items( muse_env *env, muse_port_t p, muse_cell h, muse_cell t, int sp )
{
	json_skip_whitespace(p);

	if ( port_eof(p) ) 
		return muse_raise_error( env, _csymbol(L"json:end-of-file-in-object"), MUSE_NIL );
	else {
		muse_char c = port_getc(p);
		if ( c == '}' ) {
			return h;
		} else {
			muse_cell key, value;
			port_ungetc(c,p);

			key = json_read_key(p);
			json_skip_whitespace(p);
			if ( port_eof(p) ) {
				return muse_raise_error( env, _csymbol(L"json:end-of-file-in-object"), MUSE_NIL );
			} else {
				muse_char c = port_getc(p);
				if ( c == ':' ) {
					muse_cell assoc;
					value = json_read_expr(p);
					if ( json_is_constant(env, value) ) {
						assoc = _cons( muse_quote( env, _cons( key, value ) ), MUSE_NIL );
					} else {
						assoc = _cons( _cons( _symval(_csymbol(L"cons")), _cons( muse_quote(env,key), _cons( value, MUSE_NIL ) ) ), MUSE_NIL );
					}
					_sett( t, assoc );
					t = assoc;
					_unwind(sp);

					json_skip_whitespace(p);

					{
						muse_char c = port_getc(p);
						if ( c == ',' ) {
							return json_read_object_expr_items( env, p, h, t, sp );
						} else if ( c == '}' ) {
							return h;
						} else {
							return muse_raise_error( env, _csymbol(L"json:object-syntax-error"), h );
						}
					}
				} else {
					return muse_raise_error( env, _csymbol(L"json:object-syntax-error"), h );
				}
			}
		}
	}
}

/**
 * If the object has only constant values, it pre-evalutes the object.
 * so that the expression doesn't need to be evaluated repeatedly.
 */
static muse_cell json_share_object_expr( muse_env *env, muse_cell objexpr )
{
	muse_cell listitems = _tail(_head(_tail(objexpr)));

	while ( listitems ) {
		if ( !json_is_constant( env, _next(&listitems) ) )
			return objexpr;
	}

	return muse_eval( env, objexpr, MUSE_FALSE );
}

static void json_write( muse_port_t p, muse_cell thing ) 
{
	muse_env *env = p->env;
	if ( thing == MUSE_NIL )
		muse_pwrite( p, _csymbol(L"null") );
	else {
		switch ( _cellt(thing) ) {
			case MUSE_INT_CELL:
			case MUSE_FLOAT_CELL: json_write_number( p, thing ); break;
			case MUSE_TEXT_CELL: json_write_string( p, thing ); break;
			case MUSE_NATIVEFN_CELL:
				{
					if ( muse_functional_object_data( env, thing, 'vect' ) ) {
						json_write_array( p, thing );
					} else if ( muse_functional_object_data( env, thing, 'hash' ) ) {
						json_write_object( p, thing ); 
					} else {
						muse_raise_error( env, _csymbol(L"json:invalid-json-type"), thing );
					}
					break;
				}
			case MUSE_SYMBOL_CELL:
				{
					const muse_char *symname = muse_symbol_name( p->env, thing );
					if ( wcscmp( symname, L"true" ) || wcscmp( symname, L"false" ) ) {
						muse_pwrite( p, thing );
					} else {
						muse_raise_error( env, _csymbol(L"json:invalid-keyword"), thing );
					}
					break;
				}
		}
	}
}

static void json_write_number( muse_port_t p, muse_cell num )
{
	muse_pwrite( p, num );
}

static void json_write_string( muse_port_t p, muse_cell str )
{
	int len = 0;
	const muse_char *txt = muse_text_contents( p->env, str, &len );

	port_putc( '"', p );
	{
		int i = 0;
		for ( i = 0; i < len; ++i ) {
			muse_char c = txt[i];
			switch ( c ) {
				case '"':	port_write( "\\\"", 2, p ); break;
				case '\\':	port_write( "\\\\", 2, p ); break;
				case '\b':	port_write( "\\b", 2, p ); break;
				case '\f':	port_write( "\\f", 2, p ); break;
				case '\n':	port_write( "\\n", 2, p ); break;
				case '\r':	port_write( "\\r", 2, p ); break;
				case '\t':	port_write( "\\t", 2, p ); break;
				default:
					port_putc( c, p );
			}
		}
	}
	port_putc( '"', p );
}

static void json_write_array( muse_port_t p, muse_cell arr )
{
	muse_env *env = p->env;
	int len = muse_vector_length( env, arr );
	port_putc( '[', p );
	{
		int i = 0;
		for ( i = 0; i < len; ++i ) {
			json_write(p, muse_vector_get( env, arr, i ));
			if ( i + 1 < len )
				port_putc( ',', p );
		}
	}
	port_putc( ']', p );
}

muse_cell fn_hashtable_to_alist( muse_env *env, void *context, muse_cell args );

static void json_write_object( muse_port_t p, muse_cell obj )
{
	muse_env *env = p->env;
	int sp = _spos();
	muse_cell alist = fn_hashtable_to_alist( env, NULL, _cons(obj,MUSE_NIL) );
	_unwind(sp);
	port_putc( '{', p );
	while ( alist ) {
		muse_cell ht = _next(&alist);
		json_write_string(p, _symname(_head(ht)));
		port_putc(':',p);
		json_write(p,_tail(ht));
		if ( alist )
			port_putc( ',', p );
	}
	port_putc( '}', p );
}
