/**
 * @file muse_builtin_xml.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * Support for writing out XML content. Reading not implemented yet.
 */

#include "muse_builtins.h"
#include "muse_port.h"
#include <string.h>
#include <stdlib.h>

static void write_xml_node( muse_env *env, muse_port_t p, muse_cell xmlnode, int depth );
static int write_xml_child_node( muse_env *env, muse_port_t p, muse_cell xmlnode, int depth );
static void write_tag_attrs( muse_env *env, muse_port_t p, muse_cell attrs );

/**
 * @code (write-xml [port] xml-node [flags]) @endcode
 * Converts an s-expr representation of an XML node (a tag, not
 * an arbitrary node such as attribute nodes) into XML form
 * and writes it out to the given port. The XML node representation
 * is as follows -
 * @code
 * (tag ((attr1 . "value1") (attr2 . "value2") ...)
 *    body1
 *    body2
 *    ...
 *    )
 * @endcode
 *
 * The flags is a list of symbols that indicate features to use.
 * The only one feature you can indicate is 'with-header
 * which will cause 
 * <pre>
 * <?xml version="1.0" encoding="UTF-8"?>
 * </pre>
 * to be written out before the node is written out.
 */
muse_cell fn_write_xml( muse_env *env, void *context, muse_cell args )
{
	muse_cell portcell	= _evalnext(&args);
	muse_port_t port	= _port(portcell);
	muse_cell xmlnode	= MUSE_NIL;
	muse_cell flags		= MUSE_NIL;
	muse_boolean with_header = MUSE_FALSE;

	/* First argument is optionally the port to write to.
	If that wasn't the case, the first argument is the xmlnode.*/
	if ( port )
	{
		xmlnode		= _evalnext(&args);
	}
	else
	{
		xmlnode		= portcell;
		portcell	= MUSE_NIL;
		port		= _stdport(1); /* Use stdout if no port is specified. */
	}

	/* The last optional flags argument. */
	if ( args )
	{
		flags = _evalnext(&args);
	}

	if ( flags )
	{
		if ( muse_find_list_element( env, &flags, _csymbol(L"with-header") ) )
		{
			with_header = MUSE_TRUE;
		}
	}

	if ( with_header )
	{
		const char *header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
		port_write( (void*)header, strlen(header), port );
	}

	write_xml_node( env, port, xmlnode, 0 );

	return MUSE_NIL;
}

static void indent( muse_env *env, muse_port_t port, int depth )
{
	port_putc( '\n', port );
	
	while ( depth-- > 0 )
	{
		port_putc( '\t', port );
	}
}

static void write_xml_node( muse_env *env, muse_port_t port, muse_cell xmlnode, int depth )
{
	muse_assert( _cellt(xmlnode) == MUSE_CONS_CELL );

	if ( xmlnode )
	{
		muse_cell tag		= _head(xmlnode);

		if ( _cellt(tag) == MUSE_CONS_CELL )
		{
			while ( xmlnode )
			{
				write_xml_node( env, port, _head(xmlnode), depth );
				xmlnode = _tail(xmlnode);
			}
		}
		else
		{
			muse_cell attrs		= _head(_tail(xmlnode));
			muse_cell children	= _tail(_tail(xmlnode));

			indent( env,port,depth);
			port_putc( '<', port );
			muse_pwrite( port, tag ); 

			if ( attrs )
				port_putc( ' ', port );

			write_tag_attrs( env, port, attrs );

			if ( children )
			{
				int nested_tag_count = 0;
				port_putc( '>', port );
				while ( children )
				{
					nested_tag_count += write_xml_child_node( env, port, _next(&children), depth+1 );
				}
				if ( nested_tag_count > 0 )
					indent( env,port,depth);
				port_putc( '<', port );
				port_putc( '/', port );
				muse_pwrite( port, tag );
				port_putc( '>', port );
			}
			else
			{
				port_putc( '/', port );
				port_putc( '>', port );
			}
		}
	}
}

static void write_tag_attrs( muse_env *env, muse_port_t port, muse_cell attrs )
{
	while ( attrs )
	{
		muse_cell attr = _next(&attrs);

		/* Write the attribute name. */
		muse_pwrite( port, _head(attr) );
		port_putc( '=', port );

		{
			muse_cell value = _tail(attr);
			switch ( _cellt(value) )
			{
			case MUSE_INT_CELL:
			case MUSE_FLOAT_CELL:
			case MUSE_SYMBOL_CELL:
				port_putc( '"', port );
				muse_pwrite( port, value );
				port_putc( '"', port );
				break;
			case MUSE_TEXT_CELL:
				muse_pwrite( port, value );
				break;
			default:
				MUSE_DIAGNOSTICS3({ fprintf( stderr, "XML error: Invalid attribute value of type %s\n", _typename(value) ); });
			}
		}

		if ( attrs )
			port_putc( ' ', port );
	}
}

static int write_xml_child_node( muse_env *env, muse_port_t p, muse_cell xmlnode, int depth )
{
	if ( _cellt(xmlnode) == MUSE_TEXT_CELL )
	{
		int length = 0;
		const muse_char *text = _text_contents(xmlnode, &length);
		const muse_char *text_end = text + length;

		while ( text < text_end )
		{
			muse_char c = *text++;
			switch ( c )
			{
			case '"': port_write( "&quot;", 6, p ); break;
			case '&': port_write( "&amp;", 5, p ); break;
			case '<': port_write( "&lt;", 4, p ); break;
			case '>': port_write( "&gt;", 4, p ); break;
			case '\'': port_write( "&apos;", 6, p ); break;
			default:
				port_putc( c, p );
			}
		}
		return 0;
	}
	else
	{
		write_xml_node( env, p, xmlnode, depth );
		return 1;
	}
}

void muse_define_xml_codes(muse_env *env)
{
	int sp = _spos();
	_define( _csymbol(L"&amp;"), muse_mk_ctext( env, L"&" ) );
	_define( _csymbol(L"&lt;"), muse_mk_ctext( env, L"<" ) );
	_define( _csymbol(L"&gt;"), muse_mk_ctext( env, L">" ) );
	_define( _csymbol(L"&quot;"), muse_mk_ctext( env, L"\"" ) );
	_define( _csymbol(L"&apos;"), muse_mk_ctext( env, L"'" ) );
	_unwind(sp);
}

static void xml_skip_ignorables( muse_port_t p );
static muse_boolean xml_skip_whitespace( muse_port_t p );
static void ungetbuffer( char *c, size_t n, muse_port_t p );
static muse_boolean xml_comment_start( muse_port_t p );
static muse_boolean xml_skip_comment( muse_port_t p );
static muse_boolean xml_DOCTYPE_start( muse_port_t p );
static muse_boolean xml_skip_DOCTYPE( muse_port_t p );
static muse_boolean xml_proc_start( muse_port_t p );
static muse_boolean xml_skip_proc( muse_port_t p );
static muse_cell xml_read_tag( muse_env *env, muse_port_t p );
static muse_cell xml_tag_attrib_gen( muse_env *env, muse_port_t p, int i, muse_boolean *eol );
static muse_cell xml_read_tag_attribs( muse_env *env, muse_port_t p );
static muse_cell xml_tag_body_gen( muse_env *env, muse_port_t p, int i, muse_boolean *eol );
static muse_cell xml_read_tag_body( muse_env *env, muse_port_t p, muse_cell tag );

/**
 * {xml}
 * 
 * Reads the XML expression immediately following {xml}
 * as a value. This way you can use XML data inline in
 * muSE code. 
 */
muse_cell fn_xml( muse_env *env, void *context, muse_cell args )
{
	return muse_quote( env, xml_read_tag( env, muse_current_port( env, MUSE_INPUT_PORT, NULL ) ) );
}


/**
 * @code (read-xml [port]) @endcode
 *
 * Reads one xml node (a simple subset of xml) and returns it in the 
 * canonical form.
 * @code
 * <tag attr1="v1" attr2="v2">hello <b>world</b></tag>
 * @endcode
 * gets returned as
 * @code
 * (tag ((attr1 . "v1") (attr2 . "v2")) "hello" (b () "world"))
 * @endcode
 *
 *	- Comments are skipped,
 *	- processing instructions are skipped,
 *	- only UTF-8 is supported,
 *	- white space after opening tags and before closing tags are trimmed, 
 *	- all "content" within tags are read in as strings, 
 *	- no support for xml namespaces,
 *	- only reads one xml node at a time,
 *	- xml node name (i.e. tag) is not limited to alpha-numeric characters,
 *	- attr='value' and attr="value" notations are the only ones supported.
 *	- read-xml followed by write-xml should essentially be an identity operation,
 *	- if you don't give a port argument, it reads a node from the standard input.
 *
 * Supports \ref fn_the "the"
 */
muse_cell fn_read_xml( muse_env *env, void *context, muse_cell args )
{
	muse_port_t port;

	if ( args )
	{
		muse_cell portcell	= _evalnext(&args);
		port	= _port(portcell);
	}
	else
	{
		/* If no port argument is given, read from stdin. */
		port = muse_stdport( env, MUSE_STDIN_PORT );
	}

	return muse_add_recent_item( env, (muse_int)fn_read_xml, xml_read_tag( env, port ) );
}

static void xml_skip_ignorables( muse_port_t p )
{
	int skipcount = 0;

	do
	{
		if ( port_eof(p) || p->error )
			return;
		else
		{
			skipcount = 0;
			skipcount += xml_skip_whitespace(p);
			skipcount += xml_skip_comment(p);
			skipcount += xml_skip_whitespace(p);
			skipcount += xml_skip_DOCTYPE(p);
			skipcount += xml_skip_whitespace(p);
			skipcount += xml_skip_proc(p);
		}
	}
	while ( skipcount > 0 );
}

static muse_boolean xml_skip_whitespace( muse_port_t p )
{
	int skipcount = 0;

	while ( !port_eof(p) && p->error == 0 )
	{
		int c = port_getc(p);
		if ( !isspace(c) )
		{
			port_ungetc(c,p);
			break;
		}

		skipcount++;
	}

	return skipcount > 0 ? MUSE_TRUE : MUSE_FALSE;
}

static void ungetbuffer( char *c, size_t n, muse_port_t p )
{
	muse_int i;
	for ( i = n; i > 0; --i )
	{
		port_ungetc(c[i-1],p);
	}
}

static muse_boolean xml_comment_start( muse_port_t p )
{
	char c[4];
	size_t n = port_read( c, 4, p );

	if ( n == 4 && c[0] == '<' && c[1] == '!' && c[2] == '-' && c[3] == '-' )
		return MUSE_TRUE;
	else
	{
		ungetbuffer( c, n, p );
		return MUSE_FALSE;
	}
}

static muse_boolean xml_skip_comment( muse_port_t p )
{
	if ( xml_comment_start(p) )
	{
		while ( !port_eof(p) && p->error == 0 )
		{
			int c = port_getc(p);
			if ( c == '-' )
			{
				/* Check if it is end of comment. */
				int c2 = port_getc(p);
				if ( c2 == '-' )
				{
					int c3 = port_getc(p);
					if ( c3 == '>' )
					{
						/* End of comment, alright. */
						return MUSE_TRUE;
					}
					else
					{
						port_ungetc(c3,p);
						port_ungetc(c2,p);
					}
				}
				else
					port_ungetc(c2,p);
			}
			else if ( c == '<' )
			{
				/* May be beginning of another comment. */
				port_ungetc(c,p);
				{
					muse_boolean nested_comment = xml_skip_comment(p);
					if ( !nested_comment )
						port_getc(p);
				}
			}
		}

		return MUSE_TRUE;
	}
	else
		return MUSE_FALSE;
}

static muse_boolean xml_DOCTYPE_start( muse_port_t p )
{
	char c[10];
	size_t n = port_read( c, 9, p );
	c[9] = '\0';

	if ( n == 9 && strcmp(c,"<!DOCTYPE") == 0 )
		return MUSE_TRUE;
	else
	{
		ungetbuffer( c, n, p );
		return MUSE_FALSE;
	}
}

static muse_boolean xml_skip_DOCTYPE( muse_port_t p )
{
    if ( xml_DOCTYPE_start(p) )
    {
        while ( !port_eof(p) && p->error == 0 )
        {
            int c = port_getc(p);
            if ( c == '>' )
            {
                /* End of DOCTYPE expression. */
                return MUSE_TRUE;
            }
        }
        
        return MUSE_TRUE;
    }
    else
        return MUSE_FALSE;
}

static muse_boolean xml_proc_start( muse_port_t p )
{
	char c[2];
	size_t n = port_read( c, 2, p );

	if ( n == 2 && c[0] == '<' && c[1] == '?' )
		return MUSE_TRUE;
	else
	{
		ungetbuffer( c, n, p );
		return MUSE_FALSE;
	}
}

static muse_boolean xml_skip_proc( muse_port_t p )
{
	if ( xml_proc_start(p) )
	{
		while ( !port_eof(p) && p->error == 0 )
		{
			int c = port_getc(p);
			if ( c == '?' )
			{
				int c2 = port_getc(p);
				if ( c2 == '>' )
				{
					/* End of processing instruction. */
					return MUSE_TRUE;
				}
				else
					port_ungetc(c2,p);
			}
		}

		return MUSE_TRUE;
	}
	else
		return MUSE_FALSE;
}

MUSEAPI muse_cell muse_read_xml_node( muse_port_t p )
{
	return xml_read_tag( p->env, p );
}

static muse_cell xml_read_tag( muse_env *env, muse_port_t p )
{
	int c;

	xml_skip_ignorables(p);

	c = port_getc(p);
	if ( c == '<' )
	{
		char sym[128];
		int symlen = 0;
		for ( symlen = 0; symlen < 127; ++symlen )
		{
			int tc = port_getc(p);
			if ( isspace(tc) || tc == '>' || tc == '/' )
			{
				sym[symlen] = '\0';
				port_ungetc(tc,p);
				break;
			}
			else
				sym[symlen] = tc;
		}

		sym[symlen] = '\0';

		{
			int sp = _spos();
			muse_cell tag = muse_csymbol_utf8(env,sym);
			muse_cell attribs = xml_read_tag_attribs(env,p);
			muse_cell body = xml_read_tag_body(env,p,tag);
			muse_cell tagexpr = _cons(tag,_cons(attribs,body));
			_unwind(sp);
			_spush(tagexpr);
			return tagexpr;
		}
	}
	else
	{
		port_ungetc(c,p);
		return MUSE_NIL;
	}
}

static char *xml_read_utf8_until( char eos, int *length, muse_port_t p )
{
	int maxlen = 64, len = 0;
	char *text = (char*)malloc(maxlen);

	while ( !port_eof(p) && p->error == 0 )
	{
		int c = port_getc(p);
		
		if ( len+1 >= maxlen )
		{
			maxlen *= 2;
			text = (char*)realloc( text, maxlen );
		}

		if ( c == eos )
		{
			(*length) = len;
			text[len] = '\0';
			return text;
		}
		else
		{
			text[len++] = c;
		}
	}

	(*length) = len;
	text[len] = '\0';
	return text;
}

static int read_unquoted_attrib_value(muse_port_t p, char val[256])
{
	int n = 0;

	while ( !port_eof(p) && n < 256 ) {
		int c = port_getc(p);
		if ( !isspace(c) && c != '>' ) {
			if ( c == '/' ) {
				int c2 = port_getc(p);
				if ( !isspace(c2) && c2 != '>' ) {
					val[n++] = c;
					val[n++] = c2;
				} else {
					port_ungetc( c2, p );
					port_ungetc( c, p );
					break;
				}
			} else {
				val[n++] = c;
			}
		} else {
			port_ungetc(c,p);
			break;
		}
	}

	val[n] = '\0';
	return n;
}

static muse_cell xml_tag_attrib_gen( muse_env *env, muse_port_t p, int i, muse_boolean *eol )
{
	int c;

	if ( port_eof(p) || p->error != 0 )
	{
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}

	(*eol) = MUSE_FALSE;
	xml_skip_whitespace(p);
	c = port_getc(p);
	if ( c == '>' )
	{
		/* End of tag start. */
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
	else if ( c == '/' )
	{
		/* End of tag. */
		c = port_getc(p);
		muse_assert( c == '>' );
		ungetbuffer( "</>", 3, p );
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
	else
	{
		/* Read x="y" kind of associations. */
		char sym[128];
		int symlen = 0;
		port_ungetc(c,p);

		for ( symlen = 0; symlen < 127; ++symlen )
		{
			int sc = port_getc(p);
			if ( isspace(sc) )
			{
				sym[symlen] = '\0';
				break;
			}
			else if ( sc == '=' )
			{
				sym[symlen] = '\0';
				port_ungetc(sc,p);
				break;
			}
			else if ( sc == '>' )
			{
				sym[symlen] = '\0';
				port_ungetc(sc,p);
				break;
			}
			else
				sym[symlen] = (char)sc;
		}

		sym[symlen] = '\0';
		xml_skip_whitespace(p);

		{
			muse_cell msym = muse_csymbol_utf8(env,sym);
			int nc = port_getc(p);
			if ( nc == '=' )
			{
				/* Association. Read the following thing as a muse string. */
				xml_skip_whitespace(p);
				{
					int q = port_getc(p);
					if ( q == '"' || q == '\'' )
					{
						int length = 0;
						char *val = xml_read_utf8_until( q, &length, p );
						muse_cell mval = muse_mk_text_utf8( env, val, val + length );
						free(val);

						return _cons( msym, mval );
					}
					else
					{
						/* Its an unquoted attrib value - like 
						attrib=value instead of attrib="value" or
						attrib='value'. So read a run of alpha-numeric and
						underscore characters and allow that to be used
						as the value string instead. */
						port_ungetc(q,p);

						{
							char val[256];
							int n = read_unquoted_attrib_value(p, val);
							return _cons( msym, muse_mk_text_utf8( env, val, val + n ) );
						}

						return _cons(msym, muse_builtin_symbol(env,MUSE_T));						
					}
				}
			}
			else
			{
				/* Flag. */
				port_ungetc(nc,p);
				return _cons(msym, muse_builtin_symbol(env,MUSE_T));
			}
		}
	}
}

static muse_cell xml_read_tag_attribs( muse_env *env, muse_port_t p )
{
	return muse_generate_list( env, (muse_list_generator_t)xml_tag_attrib_gen, p );
}

static void xml_trim_text_whitespace( char **text, size_t *len )
{
	/* Trim leading white space. */
	while ( (*len) > 0 )
	{
		if ( isspace((unsigned char)((*text)[0])) )
		{
			(*len)--;
			(*text)++;
		}
		else
			break;
	}

	/* Trim trailing white space. */
	while ( (*len) > 0 )
	{
		if ( isspace((unsigned char)((*text)[(*len)-1])) )
		{
			(*len)--;
			(*text)[(*len)] = '\0';
		}
		else
			break;
	}
}

static muse_cell xml_parse_amp_code( muse_env *env, muse_port_t p )
{
	const int BMAX = 32;
	muse_char buffer[32];
	int ix = 0;

	int c = port_getc(p);

	if ( c == '&' ) {
		buffer[ix++] = c;
		do {
			c = port_getc(p);
			buffer[ix++] = tolower((muse_char)c);
		} while ( c != ';' && ix + 1 < BMAX );

		buffer[ix] = 0;

		{
			muse_cell code = muse_symbol( env, buffer, buffer+ix );
			muse_cell result = muse_symbol_value( env, code );
			if ( result == code && _cellt(result) != MUSE_TEXT_CELL )
				return muse_raise_error( env, _csymbol(L"error:unsupported-xml-code"), _cons(code,MUSE_NIL) );
			else
				return result;
		}
	} else {
		port_ungetc( c, p );
		return MUSE_NIL;
	}
}

static muse_cell xml_tag_body_gen( muse_env *env, muse_port_t p, int i, muse_boolean *eol )
{
	if ( port_eof(p) || p->error != 0 )
	{
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
	else
	{
		int c = port_getc(p);
		if ( c == '<' )
		{
			int c2 = port_getc(p);
			if ( c2 == '/' )
			{
				/* End of tag. Read until '>' and return end of list. */
				while ( !port_eof(p) && p->error == 0 && c2 != '>' )
					c2 = port_getc(p);
				(*eol) = MUSE_TRUE;
				return MUSE_NIL;
			}
			else
			{
				/* Sub tag. */
				port_ungetc(c2,p);
				port_ungetc(c,p);
				(*eol) = MUSE_FALSE;
				return xml_read_tag(env,p);
			}
		}
		else
		{
			if ( c == '&' )
			{
				/* Support &amp; &lt; and &gt; */
				port_ungetc( c, p );
				c = muse_text_contents( env, xml_parse_amp_code( env, p ), NULL )[0];
			}


			{
				size_t textcap = 128;
				size_t textsize = 0;
				char *text = (char*)malloc( textcap );

				/* Keep the character just read in. */
				text[textsize++] = c;

				while ( !port_eof(p) && p->error == 0 )
				{
					if ( textsize >= textcap )
					{
						textcap *= 2;
						text = (char*)realloc( text, textcap );
					}

					/* Process the next character. */
					{
						int c = port_getc(p);
						if ( c == '<' )
						{
							/* Beginning of sub tag or tag end.*/
							muse_cell result;
							char *trimmedtext = text;
							size_t trimmedtextlen = textsize;
							port_ungetc(c,p);
							text[textsize] = '\0';
							xml_trim_text_whitespace( &trimmedtext, &trimmedtextlen );
							if ( trimmedtextlen == 0 )
							{
								/* If text is entirely white space, skip it. */
								free(text);
								return xml_tag_body_gen( env, p, i, eol );
							}
							else
							{
								result = muse_mk_text_utf8( env, trimmedtext, trimmedtext+trimmedtextlen );
								free(text);
								(*eol) = MUSE_FALSE;
								return result;
							}
						}
						else if ( c == '&' )
						{
							port_ungetc(c,p);
							c = muse_text_contents( env, xml_parse_amp_code( env, p ), NULL )[0];
							text[textsize++] = (char)c;
						}
						else
						{
							text[textsize++] = (char)c;
						}
					}
				}

				text[textsize] = '\0';

				{
					muse_cell result = muse_mk_text_utf8( env, text, text+textsize );
					free(text);
					(*eol) = MUSE_FALSE;
					return result;
				}
			}
		}
	}
}

static muse_cell xml_read_tag_body( muse_env *env, muse_port_t p, muse_cell tag )
{
	xml_skip_ignorables(p);
	return muse_generate_list( env, (muse_list_generator_t)xml_tag_body_gen, p );
}

