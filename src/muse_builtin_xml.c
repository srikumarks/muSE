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

static void write_xml_node( muse_env *env, muse_port_t p, muse_cell xmlnode, int depth );
static void write_xml_child_node( muse_env *env, muse_port_t p, muse_cell xmlnode, int depth );
static void write_tag_attrs( muse_env *env, muse_port_t p, muse_cell attrs );

/**
 * (write-xml [port] xml-node [flags]).
 * Converts an s-expr representation of an XML node (a tag, not
 * an arbitrary node such as attribute nodes) into XML form
 * and writes it out to the given port. The XML node representation
 * is as follows -
 * @code
 * (tag ((attr1 value1) (attr2 value2) ...)
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
			port_putc( '>', port );
			while ( children )
			{
				write_xml_child_node( env, port, _next(&children), depth+1 );
			}
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

static void write_tag_attrs( muse_env *env, muse_port_t port, muse_cell attrs )
{
	while ( attrs )
	{
		muse_cell attr = _next(&attrs);

		/* Write the attribute name. */
		muse_pwrite( port, _head(attr) );
		port_putc( '=', port );

		{
			muse_cell value = _head(_tail(attr));
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

static void write_xml_child_node( muse_env *env, muse_port_t p, muse_cell xmlnode, int depth )
{
	if ( _cellt(xmlnode) == MUSE_TEXT_CELL )
	{
		int length = 0;
		const muse_char *text = _text_contents(xmlnode, &length);
		const muse_char *text_end = text + length;

		while ( text < text_end )
		{
			muse_char c = *text++;
			if ( c == '"' )
				port_write( "&quot;", 6, p );
			else
				port_putc( c, p );
		}
	}
	else
	{
		write_xml_node( env, p, xmlnode, depth );
	}
}
