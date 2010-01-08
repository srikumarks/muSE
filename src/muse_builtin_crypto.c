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
		char *buffer = (char *)calloc( usz, 1 );
		usz = muse_unicode_to_utf8( buffer, usz, str, len );
		s = sha1_hash_bytes( (const unsigned char *)buffer, usz );
		free(buffer);
	} else {
		return muse_raise_error( env, _csymbol(L"error:bad-sha1-data-source"), _cons( thing, MUSE_NIL ) );
	}

	return muse_add_recent_item( env, (muse_int)fn_sha1_hash, sha1_to_str( env, s ) );
}

typedef struct {
	unsigned int h0, h1, h2, h3;
} md5_t;

static md5_t md5_init()
{
	md5_t m;
	m.h0 = 0x67452301;
	m.h1 = 0xEFCDAB89;
	m.h2 = 0x98BADCFE;
	m.h3 = 0x10325476;

	/*
	{
		int i = 0;
		for ( ; i < 64; ++i )
		{
			k[i] = (unsigned int)floor( fabs(sin(i+1)) * pow(2,32) );
			if ( i % 16 == 0 ) printf("\n");
			printf( "0x%x, ", k[i] );

		}
	}
	*/

	return m;
}

static md5_t md5_update( const unsigned char *chunk, md5_t m )
{
	static const unsigned int r[64] =
	{
		7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
		5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
		4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
		6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
	};

	static const unsigned int k[64] =
	{
		0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501, 
		0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821, 
		0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x2441453, 0xd8a1e681, 0xe7d3fbc8, 
		0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a, 
		0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 
		0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x4881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665, 
		0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1, 
		0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
	};

	unsigned int a, b, c, d, f, g, temp;
	unsigned int w[16];
	int i;

	for ( i = 0; i < 16; ++i )
	{
		unsigned int ui = chunk[i*4+3];
		ui = (ui << 8) | chunk[i*4+2];
		ui = (ui << 8) | chunk[i*4+1];
		ui = (ui << 8) | chunk[i*4];
		w[i] = ui;
	}

	a = m.h0; b = m.h1; c = m.h2; d = m.h3;

	for ( i = 0; i < 64; ++i )
	{
		if ( 0 <= i && i < 16 ) {
			f = (b & c) | ((~b) & d);
			g = i;
		} else if ( 16 <= i && i < 32 ) {
			f = (d & b) | ((~ d) & c);
			g = (5 * i + 1) & 15;
		} else if ( 32 <= i && i < 48 ) {
			f = b ^ c ^ d;
			g = (3 * i + 5) & 15;
		} else {
			f = c ^ (b | (~d));
			g = (7 * i) & 15;
		}

		temp = d;
		d = c;
		c = b;
		b = b + leftrotate( (a + f + k[i] + w[g]), r[i] );
		a = temp;
	}

	m.h0 += a;
	m.h1 += b;
	m.h2 += c;
	m.h3 += d;

	return m;
}

static md5_t md5_end( const unsigned char *chunk, size_t chunk_size, size_t total_bytes, md5_t m )
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

	// Append the length of the original data as a 64-bit little endian number.
	{
		size_t b = (size_t)total_bytes * 8;
		unsigned char *a = tail + tail_size - 8;
		a[0] = (unsigned char)(b & 0xFF); b >>= 8;
		a[1] = (unsigned char)(b & 0xFF); b >>= 8;
		a[2] = (unsigned char)(b & 0xFF); b >>= 8;
		a[3] = (unsigned char)(b & 0xFF); b >>= 8;
		a[4] = (unsigned char)(b & 0xFF); b >>= 8;
		a[5] = (unsigned char)(b & 0xFF); b >>= 8;
		a[6] = (unsigned char)(b & 0xFF); b >>= 8;
		a[7] = (unsigned char)(b & 0xFF);
	}

	{
		unsigned char *b = tail, *b_end = tail + tail_size;
		for ( ; b < b_end; b += 64 )
			m = md5_update( b, m );
	}

	return m;
}

static void word_as_hex_le( unsigned int word, muse_char *hex )
{
	hex[6] = num2hex((word >> 28) & 15);
	hex[7] = num2hex((word >> 24) & 15);
	hex[4] = num2hex((word >> 20) & 15);
	hex[5] = num2hex((word >> 16) & 15);
	hex[2] = num2hex((word >> 12) & 15);
	hex[3] = num2hex((word >> 8) & 15);
	hex[0] = num2hex((word >> 4) & 15);
	hex[1] = num2hex(word & 15);
}

static muse_cell md5_to_str( muse_env *env, md5_t m )
{
	muse_cell result_str = muse_mk_text( env, (const muse_char *)0, ((const muse_char *)0) + 32 );
	muse_char *str = (muse_char*)muse_text_contents( env, result_str, NULL );
	word_as_hex_le( m.h0, str );
	word_as_hex_le( m.h1, str + 8 );
	word_as_hex_le( m.h2, str + 16 );
	word_as_hex_le( m.h3, str + 24 );

	return result_str;
}

static md5_t md5_hash_bytes( const unsigned char *bytes, size_t sz )
{
	md5_t m = md5_init();
	size_t i = 0;

	for ( i = 0; i < sz; i += 64 )
	{
		if ( i + 64 < sz )
			m = md5_update( bytes + i, m );
		else
			return md5_end( bytes + i, sz - i, sz, m );
	}

	return m;
}

static md5_t md5_hash_port( muse_port_t p )
{
	unsigned char chunk[64];
	size_t chunk_size = 0;
	md5_t m = md5_init();
	size_t nbytes = 0;

	while ( !port_eof(p) )
	{
		chunk_size = port_read( chunk, 64, p );
		nbytes += chunk_size;

		if ( chunk_size < 64 ) 
			break;

		m = md5_update( chunk, m );
	}

	return md5_end( chunk, chunk_size, nbytes, m );
}

/**
 * @code (md5-hash [bytes|port|string]) @endcode
 *
 * Gives a 32-character hex string representation of the md5 hash of
 * the given bytes.
 *
 * See http://en.wikipedia.org/wiki/MD5#Pseudocode
 * for the algorithm. The code below is basically a C implementation
 * of whats given there.
 */
muse_cell fn_md5_hash( muse_env *env, void *context, muse_cell args )
{
	muse_cell thing = _evalnext(&args);
	md5_t m;
	if ( muse_port(env,thing) ) {
		m = md5_hash_port(muse_port(env,thing));
	} else if ( muse_functional_object_data( env, thing, 'barr' ) ) {
		unsigned char *data = muse_bytes_data( env, thing, 0 );
		size_t nbytes = muse_bytes_size( env, thing );
		m = md5_hash_bytes( data, nbytes );
	} else if ( _cellt(thing) == MUSE_TEXT_CELL ) {
		int len = 0;
		const muse_char *str = muse_text_contents( env, thing, &len );
		size_t usz = muse_utf8_size( str, len );
		char *buffer = (char *)calloc( usz, 1 );
		usz = muse_unicode_to_utf8( buffer, usz, str, len );
		m = md5_hash_bytes( (const unsigned char *)buffer, usz );
		free(buffer);
	} else {
		return muse_raise_error( env, _csymbol(L"error:bad-md5-data-source"), _cons( thing, MUSE_NIL ) );
	}

	return muse_add_recent_item( env, (muse_int)fn_md5_hash, md5_to_str( env, m ) );
}

void muse_define_crypto( muse_env *env )
{
	int sp = _spos();
	_define( _csymbol(L"sha1-hash"), _mk_nativefn(fn_sha1_hash,NULL) );
	_define( _csymbol(L"md5-hash"), _mk_nativefn(fn_md5_hash,NULL) );
	_unwind(sp);
}

/*@}*/
