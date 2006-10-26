/**
 * @file muse_cells.c
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
#include <string.h>
#include <stdarg.h>

/*************************************************/
/* Cell access                                   */
/*************************************************/

/**
 * Returns the type of the cell data referenced
 * by the given cell reference.
 */
muse_cell_t muse_cell_type( muse_cell cell )
{
	return _cellt(cell);
}

/**
 * Returns MUSE_TRUE if the given cell is a function
 * - either native or lambda.
 */
muse_boolean muse_isfn( muse_cell cell )
{
	return _isfn(cell);
}

/**
 * Returns the head of the cons cell referred to
 * by the given cell reference. Symbols, lambdas
 * are also defined using cons cells, so this applies
 * to those kinds of cells as well.
 * 
 * Applicable to cons cells, symbols, lambdas.
 * 
 * @see muse_tail
 */
muse_cell muse_head( muse_cell cell )
{
	return _head(cell);
}

/**
 * Returns the tail of the cons cell referred
 * to by the given cell reference.
 * 
 * Applies to symbols and lambdas in addition
 * to cons cells because they are defined in
 * terms of cons cells.
 * 
 * @see muse_head
 */
muse_cell muse_tail( muse_cell cell )
{
	return _tail(cell);
}

/**
 * Returns the n-th tail of the given list.
 * @code muse_tail_n( list, 0 ) @endcode is
 * equivalent to @code list @endcode
 * The n-th element of an N-element list (n = 1, 2, 3, ..., N) 
 * can be obtained using
 * @code muse_head( muse_tail_n( list, n-1 ) ) @endcode
 */
muse_cell muse_tail_n( muse_cell cell, int n )
{
	while ( n-- > 0 )
		cell = _tail(cell);

	return cell;
}

/**
 * (next list) = For traversing lazy lists.
 *
 * When given a lazy list producer function, it evaluates
 * it and returns the head of the result.
 *
 * When given a lazy list, it acts like cdr if the next item
 * is a cons cell.
 *
 * When given a lazy list where the cdr is a lazy list producer
 * function, it evaluates the producer function and returns
 * the result.
 * 
 * @note Subject to change!
 */
muse_cell muse_next( muse_cell arg )
{
	switch ( _cellt(arg) )
	{
		case MUSE_CONS_CELL :
			arg = _tail(arg);
			switch ( _cellt(arg) )
			{
				case MUSE_LAMBDA_CELL :
				case MUSE_NATIVEFN_CELL:
					return muse_apply( arg, MUSE_NIL, MUSE_FALSE );
				default :
					return arg;
			}
				
		case MUSE_LAMBDA_CELL :
			return muse_apply( arg, MUSE_NIL, MUSE_FALSE );
				
		default :
			return arg;
	}
}

/**
 * Given an integer or float cell, it returns
 * the value cast to a 64-bit integer.
 */
muse_int muse_int_value( muse_cell cell )
{
	switch ( _cellt(cell) )
	{
	case MUSE_INT_CELL : return (muse_int)(_ptr(cell)->i);
	case MUSE_FLOAT_CELL : return (muse_int)(_ptr(cell)->f);
	default : return 0;
	}
}

/**
 * Given an integer or float cell, it returns
 * the value cast to a 64-bit float.
 */
muse_float muse_float_value( muse_cell cell )
{
	switch ( _cellt(cell) )
	{
	case MUSE_INT_CELL : return (muse_float)(_ptr(cell)->i);
	case MUSE_FLOAT_CELL : return (muse_float)(_ptr(cell)->f);
	default : return 0.0;
	}
}

/**
 * Returns a pointer to the characters of the string
 * referenced by the given cell. If the \p length 
 * parameter is non-NULL, the length of the string
 * is stored in that location.
 */
const muse_char *muse_text_contents( muse_cell cell, int *length )
{
	muse_text_cell *t = &_ptr(cell)->text;
	if (	length )
		*length = (int)(t->end - t->start);
	return t->start;
}

/**
 * Returns the string name of the given symbol.
 * Can be used only with a named symbol. 
 * Invalid with an anonymous symbolm you'll get
 * NULL.
 */
const muse_char *muse_symbol_name( muse_cell sym )
{
	return muse_text_contents( _tail(_head(_tail(sym))), NULL );
}

/**
 * Returns the top-most value of the symbol
 * that's on the symbol's value stack.
 */
muse_cell muse_symbol_value( muse_cell sym )
{
	return _head(sym);
}

/*************************************************/
/* Cell editing                                  */
/*************************************************/

/**
 * Sets the given cell's head and tail to the given cell
 * references. Works with symbols, lambdas, cons cells.
 * 
 * @see muse_set_head
 * @see muse_set_tail
 */
muse_cell muse_set_cell( muse_cell cell, muse_cell head, muse_cell tail )
{
	_setht( cell, head, tail );
	return cell;
}

/**
 * Sets the head of the given cell.
 * 
 * @see muse_set_cell
 * @see muse_set_tail
 */
muse_cell muse_set_head( muse_cell cell, muse_cell head )
{
	_ptr(cell)->cons.head = head;
	return cell;
}

/**
 * Sets the tail of the given cell.
 * 
 * @see muse_set_cell
 * @see muse_set_tail
 */
muse_cell muse_set_tail( muse_cell cell, muse_cell tail )
{
	_ptr(cell)->cons.tail = tail;
	return cell;
}

/**
 * Sets the integer value of an int cell.
 */
muse_cell muse_set_int( muse_cell int_cell, muse_int value )
{
	_ptr(int_cell)->i = value;
	return int_cell;
}

/**
 * Sets the float value of a float cell.
 */
muse_cell muse_set_float( muse_cell float_cell, muse_float value )
{
	_ptr(float_cell)->f = value;
	return float_cell;
}

/**
 * Copies the given string to the given text cell, modifying
 * the cell's contents. The previous contents of the cell
 * are freed.
 */
muse_cell muse_set_text( muse_cell text, const muse_char *start, const muse_char *end )
{
	muse_text_cell *t = &_ptr(text)->text;
	
	if ( (end-start) == (t->end - t->start) )
	{
		memcpy( t->start, start, (end-start) * sizeof(muse_char) );
	}
	else
	{
		free(t->start);
		t->start = (muse_char*)malloc( sizeof(muse_char) * (end-start+1) );
		t->end = t->start + (end-start);
		memcpy( t->start, start, sizeof(muse_char) * (end-start+1) );
	}
	
	return text;
}

/**
 * Same as \c muse_set_text, except that it takes a 
 * null terminated c-style string instead.
 */
muse_cell muse_set_ctext( muse_cell text, const muse_char *start )
{
	return muse_set_text( text, start, start + wcslen(start) );
}

/**
 * Sets the value of the symbol. The current top-most
 * value of the symbol on the symbol's value stack is 
 * replaced. 
 * 
 * @see muse_pushdef
 * @see muse_popdef
 */
muse_cell muse_define( muse_cell symbol, muse_cell value )
{
	_def( symbol, value );
	return value;
}

/**
 * Makes the given value the current value of the
 * symbol, without losing the previously assigned
 * set of values. i.e. It pushes the value on the
 * symbol's value stack.
 */
muse_cell muse_pushdef( muse_cell symbol, muse_cell value )
{
//	printf( "symbol %d <- value %d\n", symbol, value );
	_push_binding(symbol);
	_def( symbol, value );
	return symbol;
}

/**
 * Removes the current value on the symbol's value stack
 * and returns it. The cell used to hold a reference
 * to the value is released immediately to the free list
 * because it is not available to the language environment
 * in normal circumstances, unless the program delves
 * into the internal representation of symbols.
 */
muse_cell muse_popdef( muse_cell symbol )
{
	muse_stack *s = &_env()->bindings_stack;
	muse_cell val = _symval(symbol);
	muse_assert( s->top[-2] == symbol );
	s->top -= 2;
	_def(s->top[0], s->top[1]);
	return val;
}

/**
 * Returns the number of elements in the list.
 * The list must be a proper list - i.e. all
 * tails must be cons cells and the tail of the
 * last cell in the list must be MUSE_NIL.
 */
int	muse_list_length( muse_cell list )
{
	int length = 0;
	
	while ( list )
	{
		++length;
		list = _tail(list);
	}
	
	return length;
}

/**
 * Deep copies the given object.
 */
muse_cell muse_dup( muse_cell obj )
{
	if ( obj <= 0 )
		return obj;
	
	switch ( _cellt(obj) )
	{
		case MUSE_INT_CELL		: return muse_mk_int( _ptr(obj)->i );
		case MUSE_FLOAT_CELL	: return muse_mk_float( _ptr(obj)->f );
		case MUSE_CONS_CELL		: 
		{
			muse_cell result = muse_cons( MUSE_NIL, MUSE_NIL );
			int sp = _spos();
			
			_seth( result, muse_dup(_head(obj)) );
			_unwind(sp);
			_sett( result, muse_dup(_tail(obj)) );
			_unwind(sp);
			
			return result;
		}
		default					: return obj;
	}
}

/**
 * Returns the last cell of a list or () if the list is itself ().
 * For example if you have the list '(1 2 3 4), it'll return the cell
 * which will print as (4).
 */
muse_cell muse_list_last( muse_cell list )
{
	muse_cell t = _tail(list);
	
	while ( t )
	{
		list = t;
		t = _tail(t);
	}
	
	return list;
}

/**
 * Appends the tail list to the end of the head list and returns the
 * head list. The head list is thus destructively modified. If the
 * head list is (), then the head list cannot be modified, so the
 * tail list is returned as is.
 */
muse_cell muse_list_append( muse_cell head, muse_cell tail )
{
	if ( head )
	{
		_sett( muse_list_last(head), tail );
		return head;
	}
	else
		return tail;
}

/**
 * Constructs a new list whose contents are that of the given array in
 * the given order.
 * 
 * @param count The number of elements to extract from the array.
 * @param astep The step interval to reach the elements to extract. 
 * Typically, \p astep is 1.
 */
muse_cell muse_array_to_list( int count, const muse_cell *array, int astep )
{
	if ( count <= 0 )
		return MUSE_NIL;
	else
	{
		muse_cell h, t;
		h = t = muse_cons( MUSE_NIL, MUSE_NIL );
		
		_seth( t, *array );
		array += astep;

		while ( --count > 0 )
		{
			muse_cell temp = muse_cons( *array, MUSE_NIL );
			_sett( t, temp );
			t = temp;
			
			array += astep;
		}
		
		return h;
	}
}

/**
 * Constructs a new array with the contents of the list in
 * the given order. If you give a non-NULL \p lengthptr, the length
 * of the array will be stored in it. You should free() the
 * result array when you're done with it.
 */
muse_cell *muse_list_to_array( muse_cell list, int *lengthptr )
{
	if ( !list )
		return NULL;
	else
	{
		int length = muse_list_length(list);
		muse_cell *result = (muse_cell*)malloc( sizeof(muse_cell) * length );
		muse_cell *iter = result;
		
		if ( lengthptr )
			(*lengthptr) = length;
		
		while ( list )
		{
			(*iter++) = _next(&list);
		}
		
		return result;
	}
}

/** 
 * Extracts \p count elements from the list starting with the first element,
 * into the given array. List elements are extracted by stepping by \p lstep
 * each time and array locations into which the extracted elements will be placed
 * are obtained by stepping the array pointer by astep items for each element.
 */
void muse_list_extract( int count, muse_cell list, int lstep, muse_cell *array, int astep )
{
	if ( count <= 0 || !list )
		return;
	else
	{
		int i = 0;
		
		while ( count-- > 0 )
		{
			(*array) = _head(list);
			
			array += astep;
			
			for ( i = lstep; i > 0; --i )
			{
				list = _tail(list);
			}
		}
	}
}

/**
 * Creates a list whose contents are generated by the given generator function.
 * The generator function is called until it return "eol" or end-of-list.
 * \p context is an arbitrary data pointer that's passed to the generator.
 */
muse_cell muse_generate_list( muse_list_generator_t generator, void *context )
{
	muse_cell h, t, v;
	int i = 0, sp = _spos();
	muse_boolean eol = MUSE_FALSE;
	
	h = t = v = MUSE_NIL;
	
	/* eol will be set to MUSE_TRUE if we've reached end of list
		at this point. */
	v = generator( context, i, &eol );
	if ( !eol )
	{
		int sp2;
		h = t = muse_cons( v, MUSE_NIL );
		sp2 = _spos();
		
		while ( ((v = generator( context, ++i, &eol )), !eol) )
		{
			muse_cell c = muse_cons( v, MUSE_NIL );
			_sett( t, c );
			t = c;
			_unwind(sp2);
		}
		
		_unwind(sp);
		_spush(h);
		return h;
	}
	
	_unwind(sp);
	return MUSE_NIL;
}

/**
 * Returns a list of the given items. The format string
 * is a string of single character codes indicating the type of
 * argument to convert to a muse object. 
 *
 * 	- c -> cell
 * 	- i -> 32-bit integer
 * 	- I -> 64-bit integer
 * 	- f -> 64-bit float
 * 	- T -> unicode text string
 * 	- t -> utf8 text string
 * 	- S -> unicode symbol string
 * 	- s -> utf8 symbol string
 */
muse_cell muse_list( const char *format, ... )
{
	muse_cell h, t, c;
	const char *fmt = format;
	int sp;
	va_list args;
	va_start( args, format );
	
	h = t = c = MUSE_NIL;
	sp = _spos();
	
	while ( *fmt )
	{
		c = MUSE_NIL;
		
		switch ( *fmt )
		{
			case 'c' : c = va_arg( args, muse_cell ); break;
			case 'i' : c = muse_mk_int( va_arg(args, int) ); break;
			case 'I' : c = muse_mk_int( va_arg(args, muse_int) ); break;
			case 'f' : c = muse_mk_float( va_arg(args, muse_float) ); break;
			case 'T' : c = muse_mk_ctext( va_arg(args, const muse_char *) ); break;
			case 't' : c = muse_mk_ctext_utf8( va_arg(args, const char *) ); break;
			case 'S' : c = muse_csymbol( va_arg(args, const muse_char *) ); break;
			case 's' : c = muse_csymbol_utf8( va_arg(args, const char *) ); break;
			default :
				fprintf( stderr, "muse_list: Unknown format symbol '%c' in format string '%s'.\n", *fmt, format );
				muse_assert( c );
		}
		
		c = muse_cons( c, MUSE_NIL );
		
		if ( h )
		{
			_sett( t, c );
			t = c;
		}
		else
		{
			h = t = c;
			_unwind(sp);
			_spush(h);
			sp = _spos();
		}

		++fmt;
		_unwind(sp);
	}
	
	va_end( args );
	return h;
}

/**
 * Searches for the given \p element in the given \p list and returns
 * a reference to it.
 * 
 * @param list The list in which to search for the element. The first cell
 * of the list is \c *list.
 *
 * @param element The element to remove. The list item removed is one that
 * satisfies muse_equal().
 *
 * @return If the element was found at the head of the list, it will
 * return the value of \p list. If it was found elsewhere, then it will
 * return a \c ptr such that \c muse_head(*ptr) is the element you searched for
 * and you can remove the element with code like the following -
 * @code
 * muse_cell list, element;
 * muse_cell *ptr = muse_find_list_element( &list, element );
 * if ( ptr )
 *    *ptr = muse_tail(*ptr);
 * @endcode
 * If the element could not be found, the return value is NULL.
 */
muse_cell *muse_find_list_element( muse_cell *list, muse_cell element )
{
	muse_assert( list != NULL && _cellt(*list) == MUSE_CONS_CELL );

	while ( *list )
	{
		if ( muse_equal( element, _head(*list) ) )
		{
			return list;
		}

		list = &(_ptr(*list)->cons.tail);
	}

	return NULL;
}
