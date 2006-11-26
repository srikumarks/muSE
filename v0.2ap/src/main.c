/**
 * @file muse.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#include "muse.h"

int main( int argc, char **argv )
{
	muse_env *env = muse_init_env(NULL);
	muse_repl();
	muse_destroy_env(env);
	return 0;
}
