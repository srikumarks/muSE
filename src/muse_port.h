/**
 * @file muse_port.h
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#ifndef __MUSE_PORT_H__
#define __MUSE_PORT_H__

#ifndef __MUSE_OPCODES_H__
#include "muse_opcodes.h"
#endif

BEGIN_MUSE_C_FUNCTIONS

/** @addtogroup FunctionalObjects */
/*@{*/

/** @addtogroup Ports */
/*@{*/

/**
 * Defines the Ports protocol.
 * ALl the \p port arguments are pointers to the 
 * \ref muse_port_base_t data structure.
 */
typedef struct 
{
	muse_functional_object_type_t obj;

	void (*close)( void *port );
		/**<
		 * The close function should release all system resources
		 * allocated by the port object. Similar to the system
		 * fclose() function.
		 */

	size_t (*read)( void *buffer, size_t nbytes, void *port );
		/**<
		 * Should read \p nbytes bytes from the port and copy them
		 * to the given buffer. Can read less than \p nbytes
		 * bytes if that many are not available, but must at least
		 * read 1 byte.
		 *
		 * @return If the port has reached end of stream, then this should 
		 * return 0, else it should return the number of bytes read.
		 */

	size_t (*write)( void *buffer, size_t nbytes, void *port );
		/**<
		 * Should write the given \p nbytes bytes to the \p port.
		 * Must not perform an incomplete write. For network ports,
		 * this means it should continuously try writing any remaining
		 * data until all of it gets written.
		 *
		 * @return \p nbytes upon success and 0 upon error.
		 */

	int (*flush) ( void *port );
		/**<
		 * If a port is buffered at the system level, this should call
		 * the system flush function appropriate for the task.
		 */
} muse_port_type_t;

/**
 * Every port is buffered for input as well as output.
 * This is the buffer object that represents both buffers.
 */
typedef struct
{
	unsigned char *bytes;
	int size, avail, pos;
	size_t fpos;
	muse_int line, column;
} muse_port_buffer_t;

enum { MAX_INDENT_COLS = 128 };

/** 
 * The base port type.
 */
typedef struct _muse_port_base_t
{
	muse_functional_object_t base;
	muse_port_buffer_t in, out;
	int mode;
	int eof, error, pretty_print, tab_size;

	/** Environment owning the port. */
	muse_env *env;

	/** Pretty printing. */
	/*@{*/
	int pp_max_indent_cols;
	int *pp_align_cols;
	int pp_align_level;
	/*@}*/
} muse_port_base_t;

/**
 * Adds all file port definitions such as "open-file".
 */
void muse_define_builtin_fileport(muse_env *env);

/** @name Ports implementation API */
/**
 * A common API provided to all ports. A specific port
 * implementation need not implement any buffering scheme
 * because this base implementation automatically provides
 * one.
 */
/*@{*/
void	port_init( muse_env *env, muse_port_base_t *p );
void	port_destroy( muse_port_base_t *p );
void	port_free( muse_port_t p );

int		port_getc( muse_port_base_t *p );
int		port_ungetc( int c, muse_port_base_t *p );
int		port_putc( int c, muse_port_base_t *p );
void	port_close( muse_port_base_t *p );
int		port_eof( muse_port_base_t *port );
size_t	port_read( void *buffer, size_t nbytes, muse_port_base_t *port );
size_t	port_write( void *buffer, size_t nbytes, muse_port_base_t *port );
int		port_flush( muse_port_base_t *port );
muse_char port_getchar( muse_port_base_t *p );
muse_char port_putchar( muse_char c, muse_port_base_t *p );
muse_char port_ungetchar( muse_char c, muse_port_base_t *p );
/*@}*/

/** @name Pretty printing */
/*@{*/
void pretty_printer_indent( muse_port_t p );
void pretty_printer_unindent( muse_port_t p );
void pretty_printer_line_break( muse_port_t f );
void pretty_printer_move( muse_port_t p, int numc );
/*@}*/
 
/*@}*/
/*@]*/

END_MUSE_C_FUNCTIONS

#endif /* __MUSE_PORT_H__ */