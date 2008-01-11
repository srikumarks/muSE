#include <windows.h>
#include <windowsx.h>

#include "thread.h"
#include "avapi.h"

#include "../src/muse_port.h"
#include "renderer.h"
#include <gl/gl.h>
#include <string>

using namespace std;

class Renderer_impl : public Task, public avmuse_env, public Renderer {
public:
	enum { FRAMEINTERVAL_us = 16666 };

	Renderer_impl( RenderContext *_plugin ) {
		plugin = _plugin;
		frameInterval_us = FRAMEINTERVAL_us;
		taskCompletion = Box::emptyBox();
		loadPortCompletion = Box::emptyBox();
		loadMIMEType[0] = '\0';
		tasker = Tasker::create();
		tasker->timeCritical(true);
		tasker->submitUrgent( this, (Task::Fn)&Renderer_impl::init );
		taskCompletion->get();
		taskCompletion->empty();
		if ( !avm ) {
			delete taskCompletion;
			delete loadPortCompletion;
			delete tasker;
			throw "Failed to create renderer";
		}
		tasker->submit( this, (Task::Fn)&Renderer_impl::load );
		start();
	}

	~Renderer_impl() {
		tasker->cancel();
		taskCompletion->empty();
		tasker->submitUrgent( this, (Task::Fn)&Renderer_impl::destroy );
		taskCompletion->get();
		delete taskCompletion;
		delete tasker;
	}

	void start() {
		tasker->submit( this );
	}

	void init( Tasker * ) {
		if ( EnableOpenGL( &dc, &rc ) ) {
			avm = avmuse::create(this);
			env = avm->get_env();
			int sp = muse_stack_pos(env);
			muse_define( env, muse_csymbol(env, L"load"), muse_mk_nativefn( env, (muse_nativefn_t)fn_load, this ) );
			muse_define( env, muse_csymbol(env, L"relative-url"), muse_mk_nativefn( env, (muse_nativefn_t)fn_relative_url, this ) );
			muse_stack_unwind(env,sp);
			loadPort = muse_create_memport(env);
			loadPort->mode |= MUSE_PORT_TRUSTED_INPUT;
			timer = muse_tick();
			nextTime_us = muse_elapsed_us(timer) + frameInterval_us;
		} else {
			throw -1;
		}
		taskCompletion->put((void*)1);
	}

	void destroy( Tasker *t ) {
		BOOL result = wglMakeCurrent( dc, rc );
		if (!result) 
			throw -1;

		t->cancel();
		muse_tock(timer);
		if ( loadPort ) {
			muse_destroy_object( env, (muse_functional_object_t*)loadPort );
			loadPort = NULL;
		}
		delete avm;
		DisableOpenGL( plugin->get_window(), dc, rc );
		taskCompletion->put((void*)1);

		timer = NULL;
		loadPort = NULL;
		avm = NULL;
	}

	void run( Tasker *t ) {
		BOOL result = wglMakeCurrent( dc, rc );
		if (!result) 
			throw -1;

		muse_int now_us = muse_elapsed_us(timer);
		if ( nextTime_us > now_us ) {
			Sleep( (DWORD)((nextTime_us - now_us)/1000) );
		} else {
			nextTime_us = now_us;
		}

		avm->update();
		avm->render();
		swap_buffers();

		nextTime_us += frameInterval_us;
		t->submit( this );
	}

	void load( Tasker * ) {
		loadFromPort();
	}

	/**
	 * (load URL [target])
	 *
	 * If target is not given -
	 *
	 * Synchronously loads the given URL. If the URL refers to 
	 * an xml file (i.e. if the word 'xml' appears anywhere in
	 * the mime type string), load parses the xml and returns
	 * it as an s-expression using \ref fn_read_xml "read-xml".
	 * Otherwise the loaded file is expected to be a sequence
	 * of s-expressions each of which will be evaluated and
	 * load itself will evaluate to the last s-expression before
	 * end of stream.
	 *
	 * The URL is resolved *exactly* as it wil be resolved within
	 * a html file presented to the browser. For example, if the
	 * URL only consists of a file name, the browser will attempt
	 * to resolve it relative to the base path of the plugin's
	 * original source stream.
	 *
	 * The URL can refer to a local file or any stream on the network
	 * using any protocol supported by the browser.
	 *
	 * If target is given -
	 *
	 * Causes the browser to load the given URL into the given target
	 * frame or window. The target can be a string or a symbol.
	 * If it is a symbol, the target name will be textual form of
	 * the symbol.
	 *
	 * In this case, the function returns immediately and doesn't
	 * wait for the URL loading to complete.
	 */
	static muse_cell fn_load( muse_env *env, Renderer_impl *r, muse_cell args ) {
		muse_cell url = _evalnext(&args);
		
		// The target argument can be a symbol or a string.
		muse_cell target = args ? _evalnext(&args) : MUSE_NIL;
		if ( _cellt(target) == MUSE_SYMBOL_CELL )
			target = _symname(target);

		return r->load_url(url, target);
	}

	muse_cell load_url( muse_cell url, muse_cell target )
	{
		int len = 0;
		const muse_char *url16 = muse_text_contents(env,url, &len);
		string url8( len, ' ' );
		muse_unicode_to_utf8( &url8[0], len, url16, len );

		if ( target ) {
			// Loading URL into a different target than 
			// ourselves. So we don't create a port and wait on it.
			int tlen = 0;
			const muse_char *target16 = muse_text_contents(env,target,&tlen);
			string target8(tlen,' ');
			muse_unicode_to_utf8( &target8[0], tlen, target16, tlen );
			plugin->fetch_url( url8.c_str(), target8.c_str() );
			return MUSE_NIL;
		} else {
			// We need the stream for ourselves. So load into
			// a port.
			loadPortCompletion->empty();
			loadPort = muse_create_memport(env);
			loadPort->mode |= MUSE_PORT_TRUSTED_INPUT;
			if ( plugin->fetch_url(url8.c_str(), NULL) ) {
				return loadFromPort();
			} else {
				MessageBox( NULL, url16, L"load-url failed", MB_OK );
				return MUSE_NIL;
			}
		}
	}

	muse_cell loadFromPort() {
		muse_port_t p = (muse_port_t)loadPortCompletion->get();
		loadPortCompletion->empty();
		loadPort = NULL;

		int sp = muse_stack_pos(env);
		muse_cell result = MUSE_NIL;

		if ( strstr( loadMIMEType, "xml" ) ) {
			// Read an XML node from the data returned.
			result = muse_read_xml_node(p);
			muse_stack_unwind(env,sp);
			muse_stack_push(env,result);
		} else {
			while ( !port_eof(p) ) {
				muse_cell expr = muse_pread(p);
				if ( expr >= 0 ) {
					result = muse_force( env, muse_eval( env, expr, MUSE_FALSE ) );
				}
				muse_stack_unwind(env,sp);
				muse_stack_push(env,result);
			}
		}

		muse_destroy_object( env, (muse_functional_object_t*)p );

		return result;
	}

	/**
	 * (relative-url rel)
	 * Turns a url specified relative to the current document
	 * into an absolute url.
	 */
	static muse_cell fn_relative_url( muse_env *env, Renderer_impl *r, muse_cell args ) {
		return r->relative_url(args);
	}

	muse_cell relative_url( muse_cell args ) {
		muse_cell rel = _evalnext(&args);
		int len = 0;
		const muse_char *relu = muse_text_contents( env, rel, &len );
		char *relc = (char*)calloc( 1, len + 1 );
		muse_unicode_to_utf8( relc, len, relu, len );
		relc[len] = '\0';
		char *absc = plugin->relative_url(relc);
		muse_cell absu = muse_mk_text_utf8( env, absc, absc + strlen(absc) );
		free(absc);
		free(relc);
		return absu;
	}

public:
	RenderContext *plugin;
	Tasker *tasker;
	HDC dc;
	HGLRC rc;
	Box *taskCompletion;
	Box *loadPortCompletion;
	char loadMIMEType[256];
	muse_port_t loadPort;
	void *timer;
	muse_int nextTime_us;

public:
	avmuse *avm;
	muse_env *env;
	int width, height;
	int mouseX, mouseY;
	int newWidth, newHeight;
	muse_int frameInterval_us;

	avmuse *get_avmuse() { return avm; }

	void setMouse( LPARAM lParam ) {
		mouseX = GET_X_LPARAM(lParam);
		mouseY = GET_Y_LPARAM(lParam);
	}

	void set_frame_rate( float fps ) {
		frameInterval_us = (muse_int)(1000000 / fps);
	}

	void set_title( const char *title )		{}
	int get_width() const					{ return width; }
	int get_height() const					{ return height; }
	void draw_char( char c )				{}
	void swap_buffers()						{ SwapBuffers(dc); }
	void *get_dc() const					{ return dc; }

	void changeSize( Tasker * ) {
		width = newWidth;
		height = newHeight;
		avm->reshape( width, height );
	}

private:
	bool EnableOpenGL(HDC * hDC, HGLRC * hRC)
	{
		if ( plugin->get_window() ) {
			HWND hWnd = plugin->get_window(); 
			PIXELFORMATDESCRIPTOR pfd;
			int format;
			// get the device context (DC)
			*hDC = GetDC( hWnd ); // set the pixel format for the DC
			ZeroMemory( &pfd, sizeof( pfd ) );
			pfd.nSize = sizeof( pfd );
			pfd.nVersion = 1; 
			pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | 
								   PFD_DOUBLEBUFFER;
			pfd.iPixelType = PFD_TYPE_RGBA;
			pfd.cColorBits = 32;
			pfd.cDepthBits = 24;
			pfd.cStencilBits = 8;
			pfd.iLayerType = PFD_MAIN_PLANE;
			format = ChoosePixelFormat( *hDC, &pfd );
			if ( !format )
				return false;

			if ( !SetPixelFormat( *hDC, format, &pfd ) )
				return false;

			// create and enable the render context (RC)
			*hRC = wglCreateContext( *hDC ); 
			if ( !*hRC )
				return false;

			if ( !wglMakeCurrent( *hDC, *hRC ) )
				return false;

			RECT rect;
			GetClientRect( hWnd, &rect );
			width = rect.right - rect.left;
			height = rect.bottom - rect.top;
			return true;
		} else {
			PIXELFORMATDESCRIPTOR pfd;
			int format;
			// get the device context (DC)
			*hDC = plugin->get_dc();
			ZeroMemory( &pfd, sizeof( pfd ) );
			pfd.nSize = sizeof( pfd );
			pfd.nVersion = 1; 
			pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | 
								   PFD_DOUBLEBUFFER;
			pfd.iPixelType = PFD_TYPE_RGBA;
			pfd.cColorBits = 32;
			pfd.cDepthBits = 24;
			pfd.cStencilBits = 8;
			pfd.iLayerType = PFD_MAIN_PLANE;
			format = ChoosePixelFormat( *hDC, &pfd );
			if ( !format )
				return false;

			if ( !SetPixelFormat( *hDC, format, &pfd ) )
				return false;

			// create and enable the render context (RC)
			*hRC = wglCreateContext( *hDC ); 
			if ( !*hRC )
				return false;

			if ( !wglMakeCurrent( *hDC, *hRC ) ) {
				SysErr(L"wglMakeCurrent");
				return false;
			}

			plugin->get_window_dimensions(width,height);
			return true;
		}
	}

	void SysErr(LPTSTR lpszFunction) 
	{ 
		TCHAR szBuf[80]; 
		LPVOID lpMsgBuf;
		DWORD dw = GetLastError(); 

		FormatMessage(
			FORMAT_MESSAGE_ALLOCATE_BUFFER | 
			FORMAT_MESSAGE_FROM_SYSTEM,
			NULL,
			dw,
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			(LPTSTR) &lpMsgBuf,
			0, NULL );

		wsprintf(szBuf, 
			L"%s failed with error %d: %s", 
			lpszFunction, dw, lpMsgBuf); 
	 
		MessageBox(NULL, szBuf, L"Error", MB_OK); 

		LocalFree(lpMsgBuf);
	}

	// Disable OpenGL
	void DisableOpenGL(HWND hWnd, HDC hDC, HGLRC hRC)
	{
		if ( hDC && hRC ) {
			wglMakeCurrent( NULL, NULL );
			wglDeleteContext( hRC );	
			ReleaseDC( hWnd, hDC );
		}
	}    

private: // Renderer implementation
	void write_MIMEType( const char *type ) {
		int n = strlen(type);
		if ( n >= 255 ) n = 255;
		strncpy( loadMIMEType, type, n+1 );
	}

	size_t write_data( void *buffer, size_t nbytes ) {
		return port_write( buffer, nbytes, loadPort );
	}
	
	void write_complete(bool success) { 
		if ( !success )
			port_close(loadPort);
		loadPortCompletion->put(loadPort); 
	}
	
	void resize( int width, int height, bool stop, bool start ) {
		if ( stop )
			tasker->cancel();
		else if ( start ) {
			newWidth = width;
			newHeight = height;
			tasker->submit( this, (Task::Fn)&Renderer_impl::changeSize );
			this->start();
		} else {
			newWidth = width;
			newHeight = height;
			tasker->submit( this, (Task::Fn)&Renderer_impl::changeSize );
		}
	}

private: // UI event handling
	void reshape( int width, int height ) {}

	class KeyTask : public TaskOnce {
	public:
		KeyTask( avmuse *_avm, unsigned char _c, int _sk, bool _down, int _x, int _y ) 
			: avm(_avm), c(_c), special_key(_sk), down(_down), x(_x), y(_y) 
		{}

		avmuse *avm;
		unsigned char c;
		int special_key;
		bool down;
		int x, y;

		void runOnce( Tasker * ) {
			if ( down ) {
				if ( c ) {
					avm->key_down( c, x, y );
				} else {
					avm->special_key_down( special_key, x, y );
				}
			} else {
				if ( c ) {
					avm->key_up( c, x, y );
				} else {
					avm->special_key_up( special_key, x, y );
				}
			}
		}

	};

	void key_down( unsigned char key, int x, int y ) {
		tasker->submit( new KeyTask( avm, key, 0, true, x, y ) );
	}

	void key_up( unsigned char key, int x, int y ) {
		tasker->submit( new KeyTask( avm, key, 0, false, x, y ) );
	}

	void special_key_down( int key, int x, int y ) {
		tasker->submit( new KeyTask( avm, 0, key, true, x, y ) );
	}

	void special_key_up( int key, int x, int y ) {
		tasker->submit( new KeyTask( avm, 0, key, false, x, y ) );
	}

	class MouseButtonTask : public TaskOnce {
	public:
		MouseButtonTask( avmuse *_avm ) : avm(_avm) {}

		avmuse *avm;
		int button;
		int state;
		int x, y;

		void runOnce( Tasker * ) {
			avm->mouse_button(button,state,x,y);
		}
	};

	void mouse_button( int button, int state, int x, int y ) {
		MouseButtonTask *mb = new MouseButtonTask(avm);
		mb->button = button;
		mb->state = state;
		mb->x = x;
		mb->y = y;
		tasker->submit(mb);
	}

	class MouseMotionTask : public TaskOnce {
	public:
		MouseMotionTask( avmuse *_avm ) : avm(_avm) {}
		avmuse *avm;
		int x, y;
		void runOnce( Tasker * ) {
			avm->mouse_drag(x,y);
		}
	};

	void mouse_drag( int x, int y ) {
		MouseMotionTask *mm = new MouseMotionTask(avm);
		mm->x = x;
		mm->y = y;
		tasker->submit(mm);
	}

	class PassiveMouseMotionTask : public MouseMotionTask {
	public:
		PassiveMouseMotionTask( avmuse *_avm ) : MouseMotionTask(_avm) {}
		void runOnce( Tasker * ) {
			avm->mouse_motion(x,y);
		}
	};

	void mouse_motion( int x, int y ) {
		PassiveMouseMotionTask *mm = new PassiveMouseMotionTask(avm);
		mm->x = x;
		mm->y = y;
		tasker->submit(mm);
	}
	
	void entry( int state ) {}
	void visibility( int state ) {}

};

Renderer *Renderer::create( RenderContext *p ) {
	return new Renderer_impl(p);
}
