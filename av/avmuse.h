/*
 *  avmuse.h
 *  avmuse
 *
 *  Created by Srikumar Subramanian on 05/08/2007.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */
#ifndef __MV_MUSEM_H__
#define __MV_MUSEM_H__

#ifdef _WIN32
#	define __WIN32__
#	include <glew/include/glew.h>
//#	include <glut/include/glut.h>
//#	include <DevIL/include/IL/il.h>
//#	include <DevIL/include/IL/ilu.h>
//#	include <DevIL/include/IL/ilut.h>
#	include <windows.h>
#	include <OpenAL/include/al.h>
#	include <OpenAL/include/alc.h>
#	include <Movies.h>
#	include <CoreFoundation.h>
#	include <QTML.h>
//#	define _SDL_main_h
//#	include <SDL/include/SDL.h>
#	include "../src/muse.h"
#	include "../src/muse_opcodes.h"
#	include "glhelpers.h"
#else
#	include <OpenGL/gl.h>
#	include <OpenGL/glu.h>
#	include <OpenGL/glext.h>
#	include <OpenAL/al.h>
#	include <OpenAL/alc.h>
#	include <QuickTime/QuickTime.h>
#	include <QuickTime/Movies.h>
#	include <QuickTime/QTML.h>
#	include <CoreFoundation/CoreFoundation.h>
#	include "../src/muse.h"
#	include "../src/muse_opcodes.h"
#	include "glhelpers.h"
#endif



#endif // __MV_MUSEM_H__
