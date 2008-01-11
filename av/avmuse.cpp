
#include "avmuse.h"
#include "thread.h"
#include "qtserver.h"
#include "avapi.h"
#include <iostream>
#include <vector>
#include <math.h>

using namespace std;

#pragma comment(lib,"glu32")

//muse_env *env = NULL;

static const char * const mouseButtons[] = { "left", "middle", "right" };
static const char * const mouseButtonPos[] = { "down", "up" };
static const int specialFnKeysStart = 1;
static const char * const specialFnKeys[] = {
	"F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12"
};
static const int specialKeysStart = 100;
static const char * const specialKeys[] = {
	"left", "up", "right", "down", "page-up", "page-down", "home", "end", "insert"
};

void register_movie( muse_env *env, qtserver *qt );
void register_image( muse_env *env, qtserver *qt );
void register_font_support( muse_env *env, void *dc );

class avmuse_impl : public avmuse, public WithGLEW {
public:

	muse_cell sym_mouseButtons[3];
	muse_cell sym_mouseButtonPos[2];
	muse_cell sym_specialFnKeys[12];
	muse_cell sym_specialKeys[9];

	muse_cell sym_init, sym_quit, sym_render;
	muse_cell sym_mouse_down, sym_mouse_up, sym_mouse_motion, sym_mouse_drag;
	muse_cell sym_key_down, sym_key_up;
	muse_cell sym_frame_rate, sym_title;

	muse_cell sym_Width, sym_Height, sym_FrameRate;
	muse_cell sym_MouseX, sym_MouseY, sym_Time;
	muse_cell sym_PlayingFPS;

	void *mclock;
	int win;
	muse_int _time_us;
	muse_float _time_secs;
	double interval_ms;
	double framerate;
	Program *program;
	char shaderSource[32768];

	PlayingFPS playingFPS;

	void set_defaults() {
		mclock = NULL;
		win = 0;
		_time_us = 0;
		_time_secs = 0;
		interval_ms = 1000.0/30.0;
		framerate = 30.0;
		program = NULL;
		updateCount = 0;
	}

public:

	avmuse_env *av;
	muse_env *env;
	anim *a;
	scheduler *s;
	watcher *w;
	qtserver *qt;
	int updateCount;
	int mouseX, mouseY;

	avmuse_impl() : WithGLEW( new GLEWContext ) { mouseX = mouseY = 0; }

	void Init( avmuse_env *av ) {
		set_defaults();
		this->av = av;
		qt = qtserver::start(glewGetContext(),this);
		env = muse_init_env(NULL);
		mclock = muse_tick();
		_time_us = muse_elapsed_us(mclock);
		_time_secs = _time_us * 1e-6;

		glewInit();

		initSymbols();
		registerAPI();	
		initGL( av->get_width(), av->get_height() );

	}

	void init_gl() {
	}

	~avmuse_impl() {
		// Stop all current activities of the qt server.
		qt->stop();

		// Release all resources. This may entail some
		// qt related activity which is permitted.
		muse_destroy_env(env);

		// Destroy the qt server.
		delete qt;

		// Destroy opengl stuff.
		delete glewGetContext();
	}
	
	muse_env *get_env() const { return env; }
	double time_secs() const { return _time_secs; }
	long long time_us() const { return _time_us; }

	void initSymbols() {
		SP sp(env);
		initSymbolTable( 3, mouseButtons, sym_mouseButtons );
		initSymbolTable( 2, mouseButtonPos, sym_mouseButtonPos );
		initSymbolTable( 12, specialFnKeys, sym_specialFnKeys );
		initSymbolTable( 9, specialKeys, sym_specialKeys );
		
		sym_init			= _csymbol(L"init");
		sym_quit			= _csymbol(L"quit");
		sym_render			= _csymbol(L"render");
		sym_mouse_down		= _csymbol(L"mouse-down");
		sym_mouse_up		= _csymbol(L"mouse-up");
		sym_mouse_motion	= _csymbol(L"mouse-motion");
		sym_mouse_drag		= _csymbol(L"mouse-drag");
		sym_key_down		= _csymbol(L"key-down");
		sym_key_up			= _csymbol(L"key-up");
		sym_frame_rate		= _csymbol(L"frame-rate");
		sym_title			= _csymbol(L"title");
		
		sym_Width			= _csymbol(L"Width");
		sym_Height			= _csymbol(L"Height");
		sym_FrameRate		= _csymbol(L"FrameRate");
		
		sym_MouseX			= _csymbol(L"MouseX");
		sym_MouseY			= _csymbol(L"MouseY");
		sym_Time			= _csymbol(L"Time");

		sym_PlayingFPS		= _csymbol(L"PlayingFPS");
	}
	
	void initSymbolTable( int n, const char * const names[], muse_cell *syms ) {
		for ( int i = 0; i < n; ++i ) {
			syms[i] = muse_csymbol_utf8( env, names[i] );
		}
	}
	
	muse_cell val( muse_cell sym ) {
		muse_cell v = _symval(sym);
		return (v == sym) ? MUSE_NIL : v;
	}
			
	
	void initGL( GLuint width, GLuint height ) {
		reportGL();
		SP sp(env);
		_define( sym_Width, _mk_int(width) );
		_define( sym_Height, _mk_int(height) );
		_define( sym_FrameRate, _mk_float(30.0) );
		_define( sym_MouseX, _mk_int(0) );
		_define( sym_MouseY, _mk_int(0) );
		_define( sym_Time, _mk_float(0.0) );
		_define( sym_PlayingFPS, _mk_int(0) );
	}
	
	void initCustom() {
		muse_cell init = val(sym_init);
		if ( init ) {
			_apply( init, MUSE_NIL, MUSE_TRUE );
		}
	}
	
	void updateTime() {
		_time_us = muse_elapsed_us(mclock);
		_time_secs = _time_us * 1e-6;
		muse_set_float( env, val(sym_Time), _time_secs );
	}
	
public: // Callbacks  
	
	void reshape( int width, int height ) {
		printf( "reshape(%d,%d)\n", width, height );
		SP sp(env);
		muse_set_int( env, val(sym_Width), width );
		muse_set_int( env, val(sym_Height), height );
		display();
	}
	
	void needsUpdate() {
		++updateCount;
	}


	bool update() {
		updateTime();
		muse_set_int( env, val(sym_PlayingFPS), (muse_int)playingFPS.tick(_time_secs) );
		int count = updateCount;
//XX	count += update_keys();
		count += a->update();
		count += w->update();
		count += s->update();
		updateCount = 0;
		return count > 0;
	}

	void display() {
//XX		printf("display()\n");
		muse_cell r = val(sym_render);
		if ( r ) {
//XX			printf("display\n");

			Render fullWindow(0, 0, av->get_width(), av->get_height());
			{
				//Blend bf( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
				Depth zorder( GL_LESS, 100.0f );
				Projection twod;
				glLoadIdentity();

				// Adjust aspect ratio of display.
				{
					float w = (float)av->get_width();
					float h = (float)av->get_height();
					if ( w > h ) 
						glScalef( h/w, 1.0f, 1.0f );
					else
						glScalef( 1.0f, w/h, 1.0f );
				}

				Model view;
				glLoadIdentity();

				SP sp(env);
				muse_force(env, muse_apply( env, r, MUSE_NIL, MUSE_TRUE, MUSE_FALSE ));
			}
//			av->swap_buffers();
		}
	}
	
	void updateMouse( int x, int y ) {
//		printf( "updateMouse(%d,%d)\n", x, y );
		mouseX = x;
		mouseY = y;
		muse_set_int( env, val(sym_MouseX), x );
		muse_set_int( env, val(sym_MouseY), y );
	}
	
	void key_down( unsigned char keycode, int x, int y ) {
//		printf( "key '%c' at (%d,%d)\n", keycode, x, y );
		genkey( sym_key_down, keycode, x, y );
	}
	
	void key_up( unsigned char keycode, int x, int y ) {
		genkey( sym_key_up, keycode, x, y );
	}

	void genkey( muse_cell updown, unsigned char keycode, int x, int y ) {
//		printf( "key '%c' at (%d,%d)\n", keycode, x, y );
		SP sp(env);
		updateMouse(x,y);
		muse_cell k = val(updown);
		if ( k ) {
			muse_char text[2];
			text[0] = keycode;
			text[1] = 0;
			muse_force(env, muse_apply( env, k, _cons( muse_mk_text(env, text, text+1), MUSE_NIL ), MUSE_TRUE, MUSE_FALSE ));
		}
	}
	
	void special_key_down( int keycode, int x, int y ) {
		gen_specialKey( sym_key_down, keycode, x, y );
	}
	
	void special_key_up( int keycode, int x, int y ) {
		gen_specialKey( sym_key_up, keycode, x, y );
	}

	void gen_specialKey( muse_cell updown, int keycode, int x, int y ) {
//		printf( "specialKey '%d' at (%d,%d)\n", keycode, x, y );
		SP sp(env);
		updateMouse(x,y);
		muse_cell k = val(updown);
		if ( k ) {
			muse_force(env, muse_apply( env, k, _cons( keycode <= 12 
													     ? sym_specialFnKeys[keycode-specialFnKeysStart] 
														 : sym_specialKeys[keycode-specialKeysStart], 
													   MUSE_NIL ),
										MUSE_TRUE, MUSE_FALSE ) );
		}
	}

	void mouse_button( int button, int state, int x, int y ) {
//		printf( "mouse(button=%d,state=%d,%d,%d)\n", button, state, x, y );
		SP sp(env);
		muse_cell m = val(state ? sym_mouse_up : sym_mouse_down);
		updateMouse(x, y );
		if ( m ) {
			muse_force( env, muse_apply( env, m, _cons( sym_mouseButtons[button], MUSE_NIL ), MUSE_TRUE, MUSE_FALSE ) );
		}
	}

	void genMouseMotion( muse_cell sym, int x, int y ) {
//		printf( "mouseMotion(%d,%d)\n", x, y );
		SP sp(env);
		updateMouse(x,y);
		muse_cell m = val(sym);
		if ( m ) {
			muse_force( env, muse_apply( env, m, MUSE_NIL, MUSE_TRUE, MUSE_FALSE ) );
		}		
	}
	
	void mouse_drag( int x, int y ) {
		genMouseMotion( sym_mouse_drag, x, y );
	}
	
	void mouse_motion( int x, int y ) {
		genMouseMotion( sym_mouse_motion, x, y );
	}
	
	void entry( int state ) {
//		printf( "entry(state=%d)\n", state );
	}
	
	void visibility( int visible ) {
//		printf( "visibility(%d)\n", visible );
	}

public: // API
	
	/**
	 * (message ...)
	 * Pops up a message box with the given message.
	 */
	static muse_cell api_message( muse_env *env, avmuse_impl *self, muse_cell args ) {
		muse_cell text = muse_force( env, muse_apply( env, _symval(_csymbol(L"format")), args, MUSE_FALSE, MUSE_FALSE ) );
#ifdef _WIN32
		MessageBox( NULL, muse_text_contents( env, text, NULL ), L"A message from muSE", MB_OK );
#endif
		return MUSE_NIL;
	}

	/**
	 * (frame-rate fps)
	 *
	 * Sets the playback frame rate.
	 */
	static muse_cell api_frame_rate( muse_env *env, avmuse_impl *self, muse_cell args ) { 
		return self->m_frame_rate(args);
	}
	muse_cell m_frame_rate( muse_cell args ) { 
		muse_cell fr = _evalnext(&args);
		framerate = muse_float_value(env,fr);
		av->set_frame_rate(framerate);
		muse_set_float( env, val(sym_FrameRate), framerate );
		return val(sym_FrameRate);
	}
	
	/**
	 * (title "str")
	 *
	 * Sets the title of the opengl window.
	 */
	static muse_cell api_title( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_title(args);
	}

	muse_cell m_title( muse_cell args ) {
		muse_cell t = _evalnext(&args);
		int len = 0;
		const muse_char *str = muse_text_contents( env, t, &len );
		size_t utf8len = muse_utf8_size( str, len );
		char *cstr = new char [utf8len+1];
		muse_unicode_to_utf8( cstr, utf8len+1, str, len );
		av->set_title( cstr );
		delete [] cstr;
		return MUSE_NIL;
	}

	/**
	 * (window-size width height)
	 *
	 * Changes the window size.
	 */
	static muse_cell api_window_size( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_window_size(args);
	}

	muse_cell m_window_size( muse_cell args ) {
		int width = (int)muse_int_value( env, _evalnext(&args) );
		int height = (int)muse_int_value( env, _evalnext(&args) );
		av->reshape( width, height );
		return MUSE_NIL;
	}
	
	/// (model ...transformations-and-rendering-steps...)
	/// A MODELVIEW stack context.
	static muse_cell api_model( muse_env *env, avmuse_impl *self, muse_cell args ) {
		Model m;
		return muse_force( env, muse_do( env, args ) );
	}
	
	/// (projection ...transformations-and-rendering-steps...)
	/// A PROJECTION stack context. 
	/// see ortho and ortho2d
	static muse_cell api_projection( muse_env *env, avmuse_impl *self, muse_cell args ) {
		Projection p;
		return muse_force( env, muse_do( env, args ) );
	}
	
	static void get_float_args( muse_env *env, int n, GLdouble *result, muse_cell &args ) {
		for ( int i = 0; i < n && args; ++i ) {
			result[i] = muse_float_value( env, _evalnext(&args) );
		}
	}
	
	/// (frustum x1 x2 y1 y2 zNear zFar)
	static muse_cell api_frustum( muse_env *env, avmuse_impl *self, muse_cell args ) {
		GLdouble dims[6];
		get_float_args( env, 6, dims, args );
		glFrustum(dims[0],dims[1],dims[2],dims[3],dims[4],dims[6]);
		return MUSE_NIL;
	}

	/// (perspective fovy aspect-ratio zNear zFar)
	static muse_cell api_perspective( muse_env *env, avmuse_impl *self, muse_cell args ) {
		GLdouble dims[4];
		get_float_args( env, 4, dims, args );
		gluPerspective(dims[0],1.0f/*dims[1]*/,dims[2],dims[3]);
		return MUSE_NIL;
	}

	/// (ortho x1 x2 y1 y2 zNear zFar)
	static muse_cell api_ortho( muse_env *env, avmuse_impl *self, muse_cell args ) {
		GLdouble dims[6];
		get_float_args( env, 6, dims, args );
		glOrtho(dims[0],dims[1],dims[2],dims[3],dims[4],dims[6]);
		return MUSE_NIL;
	}
	
	/// (ortho2d x1 x2 y1 y2)
	static muse_cell api_ortho2d( muse_env *env, avmuse_impl *self, muse_cell args ) {
		GLdouble dims[4];
		get_float_args( env, 4, dims, args );
		gluOrtho2D(dims[0],dims[1],dims[2],dims[3]);
		return MUSE_NIL;
	}
	
	/// (lookat eyeX eyeY eyeZ centreX centreY centreZ upX upY upZ)
	static muse_cell api_lookat( muse_env *env, avmuse_impl *self, muse_cell args ) {
		GLdouble vecs[9];
		get_float_args( env, 9, vecs, args );
		gluLookAt(vecs[0],vecs[1],vecs[2],vecs[3],vecs[4],vecs[5],vecs[6],vecs[7],vecs[8]);
		return MUSE_NIL;
	}
	
	/// (rotate angle-deg ex ey ez)
	static muse_cell api_rotate( muse_env *env, avmuse_impl *self, muse_cell args ) {
		GLdouble angle = muse_float_value( env, _evalnext(&args) );
		GLdouble xyz[3];
		get_float_args( env, 3, xyz, args );
		glRotated( angle, xyz[0], xyz[1], xyz[2] );
		return MUSE_NIL;
	}
	
	/// (translate dx dy dz)
	static muse_cell api_translate( muse_env *env, avmuse_impl *self, muse_cell args ) {
		GLdouble xyz[3];
		get_float_args( env, 3, xyz, args );
		glTranslated( xyz[0], xyz[1], xyz[2] );
		return MUSE_NIL;
	}
	
	/// (scale sx sy sz)
	static muse_cell api_scale( muse_env *env, avmuse_impl *self, muse_cell args ) {
		GLdouble xyz[3];
		get_float_args( env, 3, xyz, args );
		glScaled( xyz[0], xyz[1], xyz[2] );
		return MUSE_NIL;
	}

	
	/// do-nothing function.
	static muse_cell fn_shader( muse_env *env, Shader *sh, muse_cell args ) {
		if ( muse_doing_gc(env) ) {
			delete sh;
		}
		
		return MUSE_NIL;
	}
	
	/// (vertex-shader "shader-source")
	/// Compiles the given shader.
	/// See also vertex-shader-file
	static muse_cell api_vertex_shader( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_vertex_shader(args);
	}

	muse_cell m_vertex_shader( muse_cell args ) {
		const muse_char *src = muse_text_contents( env, _evalnext(&args), NULL );
		sprintf( shaderSource, "%ls", src );
		Shader *sh = new VertexShader(glewGetContext());
		sh->source(shaderSource);
		return muse_mk_destructor( env, (muse_nativefn_t)fn_shader, sh );
	}
	
	/// (fragment-shader "shader-source")
	/// Compiles the fragment shader.
	/// See also fragment-shader-file
	static muse_cell api_fragment_shader( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_fragment_shader(args);
	}

	muse_cell m_fragment_shader( muse_cell args ) {
		const muse_char *src = muse_text_contents( env, _evalnext(&args), NULL );
		sprintf( shaderSource, "%ls", src );
		Shader *sh = new FragmentShader(glewGetContext());
		sh->source(shaderSource);
		return muse_mk_destructor( env, (muse_nativefn_t)fn_shader, sh );
	}
	
	/// (vertex-shader-file "filepath")
	static muse_cell api_vertex_shader_file( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_vertex_shader_file(args);
	}

	muse_cell m_vertex_shader_file( muse_cell args ) {
		const muse_char *src = muse_text_contents( env, _evalnext(&args), NULL );
		sprintf( shaderSource, "%ls", src );
		Shader *sh = new VertexShader(glewGetContext());
		sh->sourceFile(shaderSource);
		return muse_mk_destructor( env, (muse_nativefn_t)fn_shader, sh );
	}
	
	/// (fragment-shader-file "filepath")
	static muse_cell api_fragment_shader_file( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_fragment_shader_file(args);
	}

	muse_cell m_fragment_shader_file( muse_cell args ) {
		const muse_char *src = muse_text_contents( env, _evalnext(&args), NULL );
		sprintf( shaderSource, "%ls", src );
		Shader *sh = new FragmentShader(glewGetContext());
		sh->sourceFile(shaderSource);
		return muse_mk_destructor( env, (muse_nativefn_t)fn_shader, sh );
	}

	/// do-nothing program function.
	static muse_cell fn_program( muse_env *env, Program *p, muse_cell args ) {
		if ( muse_doing_gc(env) ) {
			delete p;
		}
		return MUSE_NIL;
	}
	
	/// (program shader-functions...)
	/// Links all the given shaders into a new program object.
	static muse_cell api_program( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_program(args);
	}
	
	muse_cell m_program( muse_cell args ) {
		Program *p = new Program(glewGetContext());
		
		while ( args ) {
			muse_cell sh = _evalnext(&args);
			p->attach(*(Shader*)_ptr(sh)->fn.context);
		}
		
		(*p)();
		p->disengage();
		
		return muse_mk_destructor( env, (muse_nativefn_t)fn_program, p );
	}
	
	/// (with-program ...)
	/// Evaluates expressions in the context of the program. Expressions
	/// can be render passes or uniform variable accesses.
	static muse_cell api_with_program( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_with_program(args);
	}

	muse_cell m_with_program( muse_cell args ) {
		muse_cell p = _evalnext(&args);
		
		program = (Program*)_ptr(p)->fn.context;
		
		{
			WithProgram wp(*program);
			return muse_force( env, muse_do( env, args ) );
		}
	}
	
	/// (f [int|float|vector])
	/// Use to set value of a uniform.
	static muse_cell fn_uniform( muse_env *env, Uniform *u, muse_cell args ) {
		if ( args ) {
			muse_cell v = _evalnext(&args);
			switch ( _cellt(v) ) {
				case MUSE_INT_CELL:
				{
					GLint i = (GLint)muse_int_value(env,v);
					u->set(1,&i,1);
					break;
				}
				case MUSE_FLOAT_CELL:
				{
					GLfloat f = (GLfloat)muse_float_value(env,v);
					u->set(1,&f,1);
					break;
				}
				case MUSE_NATIVEFN_CELL:
				{	// Assume vector type.
					GLfloat fv[4];
					int n = muse_vector_length(env,v);
					for ( int i = 0; i < n; ++i ) {
						fv[i] = (GLfloat)muse_float_value(env,muse_vector_get(env,v,i));
					}
					u->set(n,fv,1);
					break;
				}
			}
		} else {
			if ( muse_doing_gc(env) ) {
				delete u;
				return MUSE_NIL;
			}
		}
		
		return MUSE_NIL;
	}
	
	/// (uniform "variable-name")
	/// Evaluate in a with-program context.
	static muse_cell api_uniform( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_uniform(args);
	}

	muse_cell m_uniform( muse_cell args ) {
		muse_cell name = _evalnext(&args);
		
		char cname[128];
		snprintf( cname, 128, "%ls", muse_text_contents( env, name, NULL ) );
		cname[127] = 0;
		
		return muse_mk_destructor( env, (muse_nativefn_t)fn_uniform, new Uniform( *program, cname ) );
	}
	
	/// (f x y z)
	/// (f x y)
	/// (f value)
	static muse_cell fn_attrib( muse_env *env, Attrib *a, muse_cell args ) {
		if ( args ) {
			int n = 0;
			GLfloat attrs[4];
			
			while ( args && n < 4 ) {
				attrs[n++] = (GLfloat)muse_float_value( env, _evalnext(&args) );
			}
			
			switch ( n ) {
				case 1: (*a)( attrs[0] ); break;
				case 2: (*a)( attrs[0], attrs[1] ); break;
				case 3: (*a)( attrs[0], attrs[1], attrs[2] ); break;
				case 4: (*a)( attrs[0], attrs[1], attrs[3], attrs[4] ); break;
				default:
					throw -1;
			}
		} else {
			if ( muse_doing_gc(env) ) {
				delete a;
				return MUSE_NIL;
			}		
		}
		
		return MUSE_NIL;
	}
	
	/// (attrib "name")
	/// Gets the attribute reference. Evaluate in a with-program context.
	static muse_cell api_attrib( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_attrib(args);
	}

	muse_cell m_attrib( muse_cell args ) {
		muse_cell name = _evalnext(&args);
		
		char cname[128];
		snprintf( cname, 128, "%ls", muse_text_contents( env, name, NULL ) );
		cname[127] = 0;
		
		return muse_mk_destructor( env, (muse_nativefn_t)fn_attrib, new Attrib( *program, cname ) );
	}
	
	///////// Vertex attributes.
	
	/// (color r g b [a=1])
	static muse_cell api_color( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_color(args);
	}

	muse_cell m_color( muse_cell args ) {
		GLdouble rgba[4];
		rgba[3] = 1.0;
		get_float_args( env, 4, rgba, args );
		glColor4d(rgba[0],rgba[1],rgba[2],rgba[3]);
		return MUSE_NIL;
	}
	
	/// (vertex x y [z=0])
	static muse_cell api_vertex( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_vertex(args);
	}

	muse_cell m_vertex( muse_cell args ) {
		GLdouble xyz[3];
		xyz[2] = 0.0;
		get_float_args( env, 3, xyz, args );
		glVertex3d(xyz[0],xyz[1],xyz[2]);
		return MUSE_NIL;
	}
	
	/// (texcoord unit s t)
	static muse_cell api_texcoord( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_texcoord(args);
	}

	muse_cell m_texcoord( muse_cell args ) {
		GLuint ix = (GLuint)muse_int_value( env, _evalnext(&args) );
		GLdouble x = muse_float_value( env, _evalnext(&args) );
		GLdouble y = muse_float_value( env, _evalnext(&args) );
		glMultiTexCoord2d( GL_TEXTURE0 + ix, x, y );
		//glTexCoord2f( x, y );
		return MUSE_NIL;
	}
	
	/// (normal ex ey ez)
	static muse_cell api_normal( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_normal(args);
	}

	muse_cell m_normal( muse_cell args ) {
		GLdouble xyz[3];
		xyz[2] = 0.0;
		get_float_args( env, 3, xyz, args );
		glNormal3d(xyz[0],xyz[1],xyz[2]);
		return MUSE_NIL;
	}
	
	/// (with-textures (list (uniform "tex1") img1 (uniform "tex2") img2 ...) ...body...)
	static muse_cell api_with_textures( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_with_textures(args);
	}

	muse_cell m_with_textures( muse_cell args ) {
		muse_cell bindings = _evalnext(&args);
		GLuint unit = 0;
		muse_cell _bindings = bindings;
		while ( _bindings ) {
			glActiveTexture( GL_TEXTURE0 + unit );
			glEnable( GL_TEXTURE_2D );
			
			muse_cell utex = _next(&_bindings);
			muse_cell im = _next(&_bindings);
			#if 0
			glBindTexture( GL_TEXTURE_2D, ((Image*)(_ptr(im)->fn.context))->tex );
			#else
			((QTImage*)(_ptr(im)->fn.context))->bind(unit);
			#endif
			glUniform1i( ((Uniform*)(_ptr(utex)->fn.context))->name, unit++ );			
		}
		
		muse_cell result = muse_force( env, muse_do( env, args ) );
		
		for ( GLuint i = 0; i < unit; ++i ) {
			glActiveTexture( GL_TEXTURE0 + i );
			glDisable( GL_TEXTURE_2D );
		}
		return result;
	}
	
	/**
	 * (chop f M x1 x2 [N y1 y2]) 
	 *
	 * Chops the range [x1 x2] into M pieces
	 * and calls (f i x1 x2) for each interval.
	 *
	 * If N, y1 and y2 are given, performs 
	 * two dimensional split and calls
	 * (f i x1 x2 j y1 y2) 
	 * for each piece.
	 */
	static muse_cell api_chop( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_chop(args);
	}

	muse_cell m_chop( muse_cell args ) {
		muse_cell fn = _evalnext(&args);
		int M = _intvalue(_evalnext(&args));
		muse_float x1 = _floatvalue(_evalnext(&args));
		muse_float x2 = _floatvalue(_evalnext(&args));

		if ( args ) 
			return m_chop2( fn, M, x1, x2, args );
		else
			return m_chop1( fn, M, x1, x2 );
	}

	muse_cell m_chop1( muse_cell fn, int M, muse_float x1, muse_float x2 ) {
		muse_cell mc = _mk_int(0);
		muse_cell x1c = _mk_float(0);
		muse_cell x2c = _mk_float(0);
		muse_cell fnargs = _cons( mc, _cons( x1c, _cons( x2c, MUSE_NIL ) ) );

		const muse_float dx = (x2 - x1) / M;
		int sp = _spos();
		for ( int i = 0; i < M; ++i ) {
			muse_set_int( env, mc, i );
			muse_set_float( env, x1c, x1 + dx * i );
			muse_set_float( env, x2c, x1 + dx * (i+1) );
			_apply( fn, fnargs, MUSE_TRUE );
			_unwind(sp);
		}

		return MUSE_NIL;
	}

	muse_cell m_chop2( muse_cell fn, int M, muse_float x1, muse_float x2, muse_cell args ) {
		int N = _intvalue(_evalnext(&args));
		muse_float y1 = _floatvalue(_evalnext(&args));
		muse_float y2 = _floatvalue(_evalnext(&args));

		muse_cell mc = _mk_int(0);
		muse_cell x1c = _mk_float(0);
		muse_cell x2c = _mk_float(0);
		muse_cell nc = _mk_int(0);
		muse_cell y1c = _mk_float(0);
		muse_cell y2c = _mk_float(0);
		muse_cell yargs = _cons( nc, _cons( y1c, _cons( y2c, MUSE_NIL ) ) );
		muse_cell fnargs = _cons( mc, _cons( x1c, _cons( x2c, yargs ) ) );

		const muse_float dx = (x2 - x1) / M;
		const muse_float dy = (y2 - y1) / N;
		int sp = _spos();
		for ( int x = 0; x < M; ++x ) {
			muse_set_int( env, mc, x );
			muse_set_float( env, x1c, x1 + dx * x );
			muse_set_float( env, x2c, x1 + dx * (x+1) );
			for ( int y = 0; y < N; ++y ) {
				muse_set_int( env, nc, y );
				muse_set_float( env, y1c, y1 + dy * y );
				muse_set_float( env, y2c, y1 + dy * (y+1) );
				_apply( fn, fnargs, MUSE_TRUE );
				_unwind(sp);
			}
		}

		return MUSE_NIL;
	}

	/**
	 * (quad ..expr...)
	 * Use vertex, texcoord, etc to specify a quad.
	 */
	static muse_cell api_quad( muse_env *env, avmuse_impl *self, muse_cell args ) {
		glBegin( GL_QUADS );
		muse_cell result = _force(_do(args));
		glEnd();
		return result;
	}
	
	static void matmul( double mat[4][4], double vin[4], double vout[4] ) {
		// The matrix is in column order.
		vout[0] = mat[0][0] * vin[0] + mat[1][0] * vin[1] + mat[2][0] * vin[2] + mat[3][0] * vin[3];
		vout[1] = mat[0][1] * vin[0] + mat[1][1] * vin[1] + mat[2][1] * vin[2] + mat[3][1] * vin[3];
		vout[2] = mat[0][2] * vin[0] + mat[1][2] * vin[1] + mat[2][2] * vin[2] + mat[3][2] * vin[3];
		vout[3] = mat[0][3] * vin[0] + mat[1][3] * vin[1] + mat[2][3] * vin[2] + mat[3][3] * vin[3];
	}

	static void copyvec( double vin[4], double vout[4] ) {
		vout[0] = vin[0];
		vout[1] = vin[1];
		vout[2] = vin[2];
		vout[3] = vin[3];
	}

	static void copymat( double matin[4][4], double matout[4][4] ) {
		copyvec( matin[0], matout[0] );
		copyvec( matin[1], matout[1] );
		copyvec( matin[2], matout[2] );
		copyvec( matin[3], matout[3] );
	}

	static void swapmat( double m1[4][4], double m2[4][4] ) {
		double tmp[4][4];
		copymat( m1, tmp );
		copymat( m2, m1 );
		copymat( tmp, m2 );
	}

	/**
	 * (bendy (x y z w) M N mx my)
	 */
	static muse_cell api_bendy( muse_env *env, avmuse_impl *self, muse_cell args ) {
		double pos[4];
		muse_cell xyz = _evalnext(&args);
		pos[0] = _floatvalue(_next(&xyz));
		pos[1] = _floatvalue(_next(&xyz));
		pos[2] = _floatvalue(_next(&xyz));
		pos[3] = _floatvalue(_next(&xyz));

		int M = (int)_intvalue(_evalnext(&args));
		int N = (int)_intvalue(_evalnext(&args));
		double mx[4][4], my[4][4];

		GLint mode = 0;
		glGetIntegerv( GL_MATRIX_MODE, &mode );

		glMatrixMode( GL_MODELVIEW );
		glPushMatrix();
		glLoadIdentity();
		_evalnext(&args);
		glGetDoublev( GL_MODELVIEW_MATRIX, (double*)mx );

		glLoadIdentity();
		_evalnext(&args);
		glGetDoublev( GL_MODELVIEW_MATRIX, (double*)my );
		glPopMatrix();
		glMatrixMode(mode);
		
		double ds = 1.0/M, dt = 1.0/N;

		double *grid = new double [(M+1) * (N+1) * 4];
		double *_grid = grid;
		copyvec( pos, grid );
		for ( int x = 1; x <= M; ++x ) {
			matmul( mx, _grid, _grid + 4 );
			_grid += 4;
		}

		_grid = grid;
		for ( int y = 1; y <= N; ++y, _grid += (M+1)*4 ) {
			double *prevrow = _grid;
			double *nextrow = _grid + (M+1)*4;
			for ( int x = 0; x <= M; ++x, prevrow += 4, nextrow += 4 ) {
				matmul( my, prevrow, nextrow );
			}
		}


		_grid = grid;
		for ( int y = 0; y < N; ++y, _grid += (M+1)*4 ) {
			double *row1 = _grid;
			double *row2 = _grid + (M+1)*4;

			glBegin( GL_QUAD_STRIP );
			glTexCoord2d( 0, 1.0 - y * dt );
			glVertex4f( row1[0], row1[1], row1[2], row1[3] );
			glTexCoord2d( 0, 1.0 - (y+1) * dt );
			glVertex4f( row2[0], row2[1], row2[2], row2[3] );
			
			for ( int x = 1; x < M; ++x, row1 += 4, row2 += 4 ) {
				glTexCoord2d( x * ds, 1.0 - (y+1) * dt );
				glVertex4f( row2[0], row2[1], row2[2], row2[3] );
				glTexCoord2d( x * ds, 1.0 - y * dt );
				glVertex4f( row1[0], row1[1], row1[2], row1[3] );
			}

			glEnd();
		}

		delete [] grid;
		return MUSE_NIL;
	}

	/**
	 * (draw-text (x y [z]) ...args..)
	 * 
	 * Displays the arguments as a string at the bottom of the screen.
	 */
	static muse_cell api_draw_text( muse_env *env, avmuse_impl *self, muse_cell args ) {
		return self->m_draw_text(args);
	}

	muse_cell m_draw_text( muse_cell args ) {
		GLdouble xyz[3] = {0,0,0};
		muse_cell pos = _evalnext(&args);
		get_float_args( env, 3, xyz, pos );
		muse_cell text = _apply(val(_csymbol(L"format")), args, MUSE_FALSE);

		int len = 0;
		const muse_char *str = muse_text_contents(env,text,&len);

		glRasterPos3d( xyz[0],xyz[1],xyz[2] );
		for ( int i = 0; i < len; ++i ) {
			av->draw_char((char)str[i]);
		}
		return text;
	}

	void restart() {
		muse_tock(mclock);
		mclock = muse_tick();
		initCustom();
	}

	typedef muse_cell (*avmuse_nativefn_t)( muse_env *env, avmuse_impl *self, muse_cell args );

	typedef struct {
		const muse_char *name;
		avmuse_nativefn_t fn;
	} muse_registry_t;
	
	void registerAPI() {
		
		static const muse_registry_t k_registry[] = {
		{ L"message",				api_message					},
		{ L"frame-rate",			api_frame_rate				},
		{ L"title",					api_title					},
		{ L"window-size",			api_window_size				},
			
		{ L"model",					api_model					},
		{ L"rotate",				api_rotate					},
		{ L"translate",				api_translate				},
		{ L"scale",					api_scale					},
		{ L"projection",			api_projection				},
		{ L"ortho",					api_ortho					},
		{ L"ortho2d",				api_ortho2d					},
		{ L"frustum",				api_frustum					},
		{ L"perspective",			api_perspective				},

		{ L"vertex-shader",			api_vertex_shader			},
		{ L"fragment-shader",		api_fragment_shader			},
		{ L"vertex-shader-file",	api_vertex_shader_file		},
		{ L"fragment-shader-file",	api_fragment_shader_file	},
		{ L"program",				api_program					},
		{ L"with-program",			api_with_program			},
		{ L"uniform",				api_uniform					},
		{ L"attrib",				api_attrib					},

		{ L"color",					api_color					},
		{ L"vertex",				api_vertex					},
		{ L"texcoord",				api_texcoord				},
		{ L"normal",				api_normal					},
		{ L"with-textures",			api_with_textures			},
		{ L"quad",					api_quad					},
		{ L"bendy",					api_bendy					},

		{ L"chop",					api_chop					},
		{ L"draw-text",				api_draw_text				},

		{ NULL,						NULL						},
		};
		
		SP sp(env);
		
		for ( const muse_registry_t *reg = k_registry; reg->name != NULL; ++reg ) {
			_define( _csymbol(reg->name), _mk_nativefn((muse_nativefn_t)(reg->fn),this) );
		}

		a = anim::create(this);
		s = scheduler::create(this);
		w = watcher::create(this);
		register_movie(env,qt);
		register_image(env,qt);
		register_font_support(env,av->get_dc());
	}

	void render() {
		display();
	}
};

/** avapi **/
avmuse *avmuse::create( avmuse_env *av ) {
	avmuse_impl *a = new avmuse_impl();
	a->Init(av);
	return a;
}


int Scope::g_tablevel = 0;
void *Scope::g_clock = NULL;

void Scope::tab() {
	for ( int i = 0; i < g_tablevel; ++i ) putchar('\t');
}

Scope::Scope( const char *desc, ... ) {
	va_list args;
	tab();
	va_start(args,desc);
	vprintf( desc, args );
	va_end(args);
	putchar('\n');
	++g_tablevel;
	if ( g_clock == NULL ) g_clock = muse_tick();
	beginTime = muse_elapsed_us(g_clock);
}

Scope::~Scope() {
	--g_tablevel;
	muse_int endTime = muse_elapsed_us(g_clock);
	tab();
	printf( "(%d ms)\n", (int)((endTime - beginTime)/1000) );
}
