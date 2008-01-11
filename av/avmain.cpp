/*
 *  avmain.cpp
 *  muse
 *
 *  Created by kumar on 24/10/2007.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include "avapi.h"

#ifdef _WIN32
#	define __WIN32__
#	include <glew/include/glew.h>
#	include <glut/include/glut.h>
#	include <DevIL/include/IL/il.h>
#	include <DevIL/include/IL/ilu.h>
#	include <DevIL/include/IL/ilut.h>
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

static muse_env *env = NULL;
using namespace std;

#ifdef _WIN32
GLEWContext *g_glewContext = NULL;
static void avmuse_glewInit() {
	g_glewContext = new GLEWContext;
	glewContextInit(g_glewContext);
}
static void avmuse_glewDestroy() {
	delete g_glewContext;
	g_glewContext = NULL;
}
#else
static void avmuse_glewInit() {}
static void avmuse_glewDestroy() {}
#endif

class Dependencies
{
public:
	Dependencies( int *argcp, char **argv ) {
		glutInit( argcp, argv );
		cout << "GLUT initialized.\n";

		glutInitDisplayMode( GLUT_RGBA | GLUT_ALPHA | GLUT_DOUBLE | GLUT_DEPTH );
		glutCreateWindow("muSE");
		avmuse_glewInit();
		env = avmuse_init();
		avmuse_init_window( env, "muSE", 320, 240 );
		initCallbacks(env);

	}
	
	~Dependencies() {	

		avmuse_destroy(env);
		avmuse_glewDestroy();
	}
	
	void loadSCMFile( int *argcp, char **argv ) {
		// SCM file specified on command line.
		// Load it.
		FILE *f = fopen( (*argcp) ? argv[0] : "musegl.scm", "rb" );
		if ( f == NULL ) {
			cerr << "Unable to open muSE file '" << argv[0] << "'\n";
			throw -1;
		}
		muse_load(env,f);
		fclose(f);
		cout << "Loaded scm file '" << ((*argcp) ? argv[0] : "../../musegl.scm") << "'\n";
	}

	static void initCallbacks(muse_env *env) {
		glutReshapeFunc( reshape );
		glutDisplayFunc( display );
		glutKeyboardFunc( key );
		glutKeyboardUpFunc( keyup );
		glutSpecialFunc( specialKey );
		glutSpecialUpFunc( specialKeyup);
		glutMouseFunc( mouse );
		glutMotionFunc( mouseMotion );
		glutPassiveMotionFunc( passiveMouseMotion );
		glutEntryFunc( entry );
		glutVisibilityFunc( visibility );
	}
	
	static void reshape( int width, int height ) { avmuse_reshape( env, width, height ); }
	static void display() {}
	static void key( unsigned char key, int x, int y ) { avmuse_key_down( env, key, x, y ); }
	static void keyup( unsigned char key, int x, int y ) { avmuse_key_up( env, key, x, y ); }
	static void specialKey( int key, int x, int y ) { avmuse_special_key_down(env,key,x,y); }
	static void specialKeyup( int key, int x, int y ) { avmuse_special_key_up(env,key,x,y); }
	static void mouse( int button, int state, int x, int y ) { avmuse_mouse_button(env,button,state,x,y); }
	static void mouseMotion( int x, int y ) { avmuse_mouse_motion(env,x,y); }
	static void passiveMouseMotion( int x, int y ) { avmuse_passive_mouse_motion(env,x,y); }
	static void entry( int state ) { avmuse_entry(env,state); }
	static void visibility( int state ) { avmuse_visibility(env,state); }
	
};

static void timer( int value )
{
	int dt = avmuse_render(env);
	if ( dt >= 0 )
		glutTimerFunc(dt,timer,1);
}

void avmuse_run(muse_env *env)
{
	glutTimerFunc(1,timer,1);
	glutMainLoop();
}

int main( int argc, char **argv )
{
	Dependencies deps(&argc,argv);
	muse_repl(env);
}
