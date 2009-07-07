/**
 * @file muse_config.h
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#ifndef __MUSE_CONFIG_H__
#define __MUSE_CONFIG_H__

#define MUSE_VERSION 0x0003
#define MUSE_VERSION_STRING "535"

#ifndef FUSSY_RELEASE
/*	To enable FUSSY_RELEASE builds, uncomment the following #define. */
//#define FUSSY_RELEASE
#endif

/*	To enable diagnostics, set the diagnostics level to a
	number > 0. Setting it to 0 disables diagnostics. Currently
	there are 2 levels - 1 = basic, 2 = detailed. */
#define MUSE_DIAGNOSTICS_LEVEL 2

#if !defined(NDEBUG) || defined(_DEBUG) || defined(FUSSY_RELEASE)
#define MUSE_DEBUG_BUILD
#undef MUSE_DIAGNOSTICS_LEVEL
#define MUSE_DIAGNOSTICS_LEVEL 2
#endif

/*	Define the following constant to 0 to redirect the diagnostics to
	stderr instead of popping up a message box. */
#define MUSE_DIAGNOSTICS_POPS_MESSAGE 1

#endif /* __MUSE_CONFIG_H__ */
