/**
 * @file muse_utils.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * Shared brick layer code.
 */

#include "muse_builtins.h"
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include "muse_port.h"
#include "muse_utils.h"


muse_char num2hex( int digit )
{
	if ( digit >= 0 && digit < 10 )
		return '0' + digit;
	else if ( digit >= 10 && digit <= 15 )
		return 'a' + (digit - 10);
	else {
		assert( 0 && "invalid hex digit" );
		return 0;
	}
}

int hex2num( muse_char c )
{
	if ( c >= '0' && c <= '9' )
		return c - '0';
	else if ( c >= 'a' && c <= 'f' )
		return 10 + (c - 'a');
	else if ( c >= 'A' && c <= 'F' )
		return 10 + (c - 'A');
	else {
		assert( 0 && "invalid hex character" );
		return -1;
	}
}

int hex2word( muse_char hex4[4] )
{
	return (hex2num(hex4[0]) << 12) +
			(hex2num(hex4[1]) << 8) +
			(hex2num(hex4[2]) << 4) +
			hex2num(hex4[3]);
}

/**
 * Converts the given 16-bit unicode character c to
 * utf8 and stores the result in the given utf8 buffer.
 * If more than nbytes are needed, the character is not
 * converted and 0 is returned. Otherwise the number of
 * bytes used in the buffer is returned. If the unicode
 * character is not 16-bit, an error value -1 is returned.
 */
int uc16_to_utf8( int c, unsigned char *utf8, int nbytes )
{
	if ( nbytes <= 0 )
		return 0;

	if ( c >= 0 && c <= 0x7F )
	{
		utf8[0] = (unsigned char)c;
		return 1;
	}
	else if ( c >= 0x80 && c <= 0x7FF )
	{
		if ( nbytes < 2 )
			return 0;

		utf8[0] = (unsigned char)(c >> 6) | 0xC0;
		utf8[1] = (unsigned char)(c & 0x3F) | 0x80;
		return 2;
	}
	else if ( c >= 0x800 && c <= 0xFFFF )
	{
		if ( nbytes < 3 )
			return 0;

		utf8[0] = (unsigned char)(c >> 12) | 0xE0;
		utf8[1] = (unsigned char)((c >> 6) & 0x3F) | 0x80;
		utf8[2] = (unsigned char)(c & 0x3F) | 0x80;
		return 3;
	}
	else if ( c >= 0x10000 && c <= 0x10FFFF )
	{
		if ( nbytes < 4 )
			return 0;
		
		utf8[0] = (unsigned char)(c >> 18) | 0xF0;
		utf8[1] = (unsigned char)((c >> 12) & 0x3F) | 0x80;
		utf8[2] = (unsigned char)((c >> 6) & 0x3F) | 0x80;
		utf8[3] = (unsigned char)(c & 0x3F) | 0x80;
		return 4;
	}
	else
		return -1;
}

/**
 * Sets uc16[0] to a unicode character if
 * one utf8 character could be parsed. The return
 * value is the number of bytes consumed.
 */
int utf8_to_uc16( const unsigned char *utf8, muse_char *uc16 )
{
	int c = utf8[0];
	int result = 0;

	if ( (c & 0xF8) == 0xF0 )
	{
		/* 4-byte sequence. */
		result |= (c & 0x07) << 18;
		result |= (utf8[1] & 0x3F) << 12;
		result |= (utf8[2] & 0x3F) << 6;
		result |= (utf8[3] & 0x3F);
		uc16[0] = (muse_char)result;
		return 4;
	}
	else if ( (c & 0xF0) == 0xE0 )
	{
		/* 3-byte sequence. */
		result |= (c & 0x0F) << 12;
		result |= (utf8[1] & 0x3F) << 6;
		result |= (utf8[2] & 0x3F);
		uc16[0] = (muse_char)result;
		return 3;
	}
	else if ( (c & 0xE0) == 0xC0 )
	{
		/* 2-byte sequence. */
		result |= (c & 0x1F) << 6;
		result |= (utf8[1] & 0x3F);
		uc16[0] = (muse_char)result;
		return 2;
	}
	else
	{
		/* Ignore the most significant bit in the other cases. */
		uc16[0] = (c & 0x7F);
		return 1;
	}
}

enum { BUFFER_MAXFRAGLEN = 128 };

typedef struct
{
	int len;
	muse_char chars[BUFFER_MAXFRAGLEN];
} fragment_t;

struct __buffer_t__
{
	int N;
	fragment_t **frags;
};


/**
 * Allocates a new buffer object. The buffer can serve 
 * as a variable length string, which it keeps as a
 * list of fragments of 128 characters each.
 */
buffer_t *buffer_alloc()
{
	buffer_t *b = (buffer_t*)calloc( 1, sizeof(buffer_t) );
	b->N = 1;
	b->frags = (fragment_t**)calloc( 1, sizeof(fragment_t*) );
	b->frags[0] = (fragment_t*)calloc( 1, sizeof(fragment_t) );
	b->frags[0]->len = 0;
	return b;
}

/**
 * Releases all memory allocated for the buffer object.
 */
void buffer_free( buffer_t *b )
{
	int i = 0;
	for ( i = 0; i < b->N; ++i ) {
		free( b->frags[i] );
	}

	free( b->frags );
	free( b );
}

/**
 * Appends one character to the buffer. It is added
 * to the last fragment if there is space in it
 * and if not a new fragment is created.
 */
void buffer_putc( buffer_t *b, muse_char c )
{
	if ( b->frags[b->N-1]->len >= BUFFER_MAXFRAGLEN ) {
		b->frags = (fragment_t**)realloc( b->frags, sizeof(fragment_t*) * (b->N + 1) );
		b->frags[b->N] = (fragment_t*)calloc( 1, sizeof(fragment_t) );
		b->N++;
	}

	// Add character.
	{
		fragment_t *f = b->frags[b->N-1];
		f->chars[f->len++] = c;
	}
}

/**
 * Appends the given string \p s of length \p len.
 * Semantically equivalent to calling buffer_putc() on
 * each of the characters of the string in sequence.
 */
void buffer_puts( buffer_t *b, const muse_char *s, int len )
{
	if ( len > 0 ) {
		fragment_t *f = b->frags[b->N-1];
		int freelen = BUFFER_MAXFRAGLEN - f->len;

		if ( len <= freelen ) {
			memcpy( f->chars + f->len, s, len * sizeof(muse_char) );
			f->len += len;
			return;
		} else {
			memcpy( f->chars + f->len, s, freelen * sizeof(muse_char) );
			f->len += freelen;

			// Grow by one fragment.
			b->frags = (fragment_t**)realloc( b->frags, sizeof(fragment_t*) * (b->N + 1) );
			b->frags[b->N] = (fragment_t*)calloc( 1, sizeof(fragment_t) );
			b->N++;

			buffer_puts( b, s + freelen, len - freelen );
			return;
		}
	}
}

/**
 * Use to convert the contents of the buffer to 
 * a muSE string.
 */
muse_cell buffer_to_string( buffer_t *b, muse_env *env )
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

/**
 * Similar to buffer_to_string(), but gives a muSE symbol instead.
 */
muse_cell buffer_to_symbol( buffer_t *b, muse_env *env )
{
	muse_cell txt = buffer_to_string( b, env );
	int len = 0;
	const muse_char *txtptr = muse_text_contents( env, txt, &len );
	return muse_symbol( env, txtptr, txtptr + len );
}

/**
 * The current length of the buffer in number of characters.
 */
int buffer_length( buffer_t *b )
{
	int len = 0;
	int i = 0;
	for ( i = 0; i < b->N; ++i ) {
		len += b->frags[i]->len;
	}
	return len;
}

/**
 * Gets the character at the i-th position in the buffer.
 * The value of \p i has to be less than the number returned
 * by buffer_length().
 */
muse_char buffer_char( buffer_t *b, int i )
{
	int f = (i / BUFFER_MAXFRAGLEN);
	i %= BUFFER_MAXFRAGLEN;
	if ( f >= 0 && f < b->N ) {
		fragment_t *fr = b->frags[f];
		if ( i >= 0 && i < fr->len ) {
			return fr->chars[i];
		}
	}

	return 0;
}

/**
 * Returns a muSE string which is a substring of the contents of
 * the buffer starting from \p from and containing a maximum of
 * \p len characters. \p from + \p len cannot be greater than
 * the buffer_length().
 */
muse_cell buffer_substring( buffer_t *b, muse_env *env, int from, int len )
{
	int maxlen = buffer_length(b);
	if ( from < 0 || len < 0 || from + len > maxlen )
		return MUSE_NIL;
	else {
		muse_cell text = muse_mk_text( env, (const muse_char *)0, ((const muse_char *)0) + len );
		muse_char *ctext = (muse_char *)muse_text_contents( env, text, NULL );
		int fromFrag = from / BUFFER_MAXFRAGLEN;
		int toFrag = (from + len) / BUFFER_MAXFRAGLEN;

		while ( fromFrag < toFrag ) {
			int fromIx = from % BUFFER_MAXFRAGLEN;
			int n = BUFFER_MAXFRAGLEN - fromIx;
			memcpy( ctext, b->frags[fromFrag]->chars + fromIx, n * sizeof(muse_char) );
			ctext += n;
			from += n;
			++fromFrag;
			len -= n;
		}

		if ( fromFrag == toFrag && len > 0 ) {
			memcpy( ctext, b->frags[fromFrag]->chars + (from % BUFFER_MAXFRAGLEN), len * sizeof(muse_char) );
		}

		return text;
	}
}
