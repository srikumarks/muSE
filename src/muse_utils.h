/**
 * @file muse_utils.h
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#ifndef __MUSE_UTILS_H__
#define __MUSE_UTILS_H__

BEGIN_MUSE_C_FUNCTIONS

/**
 * @aggtogroup InternalUtilities Internal utility functions
 */
/*@{*/

/**
 * @name Misc utiliries
 */
/*@{*/
muse_char num2hex( int num );
int hex2num( muse_char ch );
int hex2word( muse_char ch[4] );
int utf8_to_uc16( const unsigned char *utf8, muse_char *uc16 );
int uc16_to_utf8( int c, unsigned char *utf8, int nbytes );
/*@}*/


/**
 * @name String buffer
 * An arbitrary length buffer for reading in strings of unknown length fairly efficiently.
 */
/*@{*/
typedef struct __buffer_t__ buffer_t;

buffer_t*	buffer_alloc();
void		buffer_free( buffer_t *b );
void		buffer_putc( buffer_t *b, muse_char c );
void		buffer_puts( buffer_t *b, const muse_char *s, int len );
muse_cell	buffer_to_string( buffer_t *b, muse_env *env );
muse_cell	buffer_to_symbol( buffer_t *b, muse_env *env );
int			buffer_length( buffer_t *b );
muse_char	buffer_char( buffer_t *b, int i );
muse_cell	buffer_substring( buffer_t *b, muse_env *env, int from, int len );
/*@}*/

/*@}*/

END_MUSE_C_FUNCTIONS

#endif // __MUSE_UTILS_H__
