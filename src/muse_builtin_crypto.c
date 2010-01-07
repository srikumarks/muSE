/**
 * @file muse_builtin_crypto.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * A collection of basic cryptographic utilities - sha1 and md5 -
 * that are useful in the online space.
 */

#include "muse_builtins.h"
#include <string.h>
#include <stdlib.h>
#include "muse_port.h"
#include "muse_utils.h"

/**
 * @addtogroup Crypto Cryptographic hashes
 */
/*@{*/
typedef struct {
	unsigned int h0, h1, h2, h3, h4;
} sha1_t;

static sha1_t sha1_init()
{
	sha1_t s;
	s.h0 = 0x67452301;
	s.h1 = 0xEFCDAB89;
	s.h2 = 0x98BADCFE;
	s.h3 = 0x10325476;
	s.h4 = 0xC3D2E1F0;
	return s;
}

static inline unsigned int leftrotate( unsigned int val, int n )
{
	return (val << n) | (val >> (32-n));
}

static sha1_t sha1_update( const unsigned char *chunk, sha1_t s )
{
	unsigned int a, b, c, d, e, f, k;
	unsigned int w[80];
	int j;
	for ( j = 0; j < 16; ++j )
	{
		unsigned int ui = chunk[j*4];
		ui = (ui << 8) | chunk[j*4+1];
		ui = (ui << 8) | chunk[j*4+2];
		ui = (ui << 8) | chunk[j*4+3];
		w[j] = ui;
	}
	for ( j = 16; j < 80; ++j )
	{
		w[j] = leftrotate(w[j-3] ^ w[j-8] ^ w[j-14] ^ w[j-16], 1);
	}

	a = s.h0; b = s.h1; c = s.h2; d = s.h3; e = s.h4;

	for ( j = 0; j < 80; ++j )
	{
		if ( 0 <= j && j < 20 ) {
			f = (b & c) | ((~b) & d);
			k = 0x5A827999;
		} else if ( 20 <= j && j < 40 ) {
			f = (b ^ c ^ d);
			k = 0x6ED9EBA1;
		} else if ( 40 <= j && j < 60 ) {
			f = (b & c) | (b & d) | (c & d);
			k = 0x8F1BBCDC;
		} else {
			f = (b ^ c ^ d);
			k = 0xCA62C1D6;
		}


		{
			unsigned int temp = leftrotate(a,5) + f + e + k + w[j];
			e = d;
			d = c;
			c = leftrotate(b,30);
			b = a;
			a = temp;
		}
	}

	s.h0 += a; s.h1 += b; s.h2 += c; s.h3 += d; s.h4 += e;
	return s;
}

static sha1_t sha1_end( const unsigned char *chunk, size_t chunk_size, size_t total_bytes, sha1_t s )
{
	unsigned char tail[128];
	size_t tail_size = 0;
	//muse_assert( chunk_size < 64 );
	memcpy( tail, chunk, chunk_size );
	tail_size = chunk_size;
	if ( tail_size + 9 > 64 ) {
		memset( tail + chunk_size, 0, 128 - chunk_size );
		tail[chunk_size] = 0x80;
		tail_size = 128;
	} else {
		memset( tail + chunk_size, 0, 64 - chunk_size );
		tail[chunk_size] = 0x80;
		tail_size = 64;
	}

	// Append the length of the original data as a 64-bit big endian number.
	{
		size_t b = (size_t)total_bytes * 8;
		unsigned char *a = tail + tail_size - 8;
		a[7] = (unsigned char)(b & 0xFF); b >>= 8;
		a[6] = (unsigned char)(b & 0xFF); b >>= 8;
		a[5] = (unsigned char)(b & 0xFF); b >>= 8;
		a[4] = (unsigned char)(b & 0xFF); b >>= 8;
		a[3] = (unsigned char)(b & 0xFF); b >>= 8;
		a[2] = (unsigned char)(b & 0xFF); b >>= 8;
		a[1] = (unsigned char)(b & 0xFF); b >>= 8;
		a[0] = (unsigned char)(b & 0xFF);
	}

	{
		unsigned char *b = tail, *b_end = tail + tail_size;
		for ( ; b < b_end; b += 64 )
			s = sha1_update( b, s );
	}

	return s;
}

static void word_as_hex( unsigned int word, muse_char *hex )
{
	hex[0] = num2hex((word >> 28) & 15);
	hex[1] = num2hex((word >> 24) & 15);
	hex[2] = num2hex((word >> 20) & 15);
	hex[3] = num2hex((word >> 16) & 15);
	hex[4] = num2hex((word >> 12) & 15);
	hex[5] = num2hex((word >> 8) & 15);
	hex[6] = num2hex((word >> 4) & 15);
	hex[7] = num2hex(word & 15);
}

static muse_cell sha1_to_str( muse_env *env, sha1_t s )
{
	muse_cell result_str = muse_mk_text( env, (const muse_char *)0, ((const muse_char *)0) + 40 );
	muse_char *str = (muse_char*)muse_text_contents( env, result_str, NULL );
	word_as_hex( s.h0, str );
	word_as_hex( s.h1, str + 8 );
	word_as_hex( s.h2, str + 16 );
	word_as_hex( s.h3, str + 24 );
	word_as_hex( s.h4, str + 32 );

	return result_str;
}

static sha1_t sha1_hash_bytes( const unsigned char *bytes, size_t sz )
{
	sha1_t s = sha1_init();
	size_t i = 0;
	for ( i = 0; i < sz; i += 64 )
	{
		if ( i + 64 < sz )
			s = sha1_update( bytes + i, s );
		else
			return sha1_end( bytes + i, sz - i, sz, s );
	}

	return s;
}

static sha1_t sha1_hash_port( muse_port_t p )
{
	unsigned char chunk[64];
	size_t chunk_size = 0;
	sha1_t s = sha1_init();
	size_t nbytes = 0;

	while ( !port_eof(p) )
	{
		chunk_size = port_read( chunk, 64, p );
		nbytes += chunk_size;

		if ( chunk_size < 64 ) 
			break;

		s = sha1_update( chunk, s );
	}

	return sha1_end( chunk, chunk_size, nbytes, s );
}

/**
 * @code (sha1-hash [bytes|port|string]) @endcode
 *
 * Gives a 40-character hex string representation of the sha1 hash of
 * the given bytes.
 *
 * See http://en.wikipedia.org/wiki/SHA_hash_functions#SHA-1_pseudocode
 * for the algorithm. The code below is basically a C implementation
 * of whats given there.
 */
muse_cell fn_sha1_hash( muse_env *env, void *context, muse_cell args )
{
	muse_cell thing = _evalnext(&args);
	sha1_t s;
	if ( muse_port(env,thing) ) {
		s = sha1_hash_port(muse_port(env,thing));
	} else if ( muse_functional_object_data( env, thing, 'barr' ) ) {
		unsigned char *data = muse_bytes_data( env, thing, 0 );
		size_t nbytes = muse_bytes_size( env, thing );
		s = sha1_hash_bytes( data, nbytes );
	} else if ( _cellt(thing) == MUSE_TEXT_CELL ) {
		int len = 0;
		const muse_char *str = muse_text_contents( env, thing, &len );
		size_t usz = muse_utf8_size( str, len );
		unsigned char *buffer = (unsigned char *)calloc( usz, 1 );
		usz = muse_unicode_to_utf8( buffer, usz, str, len );
		s = sha1_hash_bytes( buffer, usz );
		free(buffer);
	} else {
		return muse_raise_error( env, _csymbol(L"error:bad-sha1-data-source"), _cons( thing, MUSE_NIL ) );
	}

	return muse_add_recent_item( env, (muse_int)fn_sha1_hash, sha1_to_str( env, s ) );
}

void muse_define_crypto( muse_env *env )
{
	int sp = _spos();
	_define( _csymbol(L"sha1-hash"), _mk_nativefn(fn_sha1_hash,NULL) );
	_unwind(sp);
}

/*@}*/
