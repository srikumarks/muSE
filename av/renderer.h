
#ifndef __AVMUSE_RENDERER_H__
#define __AVMUSE_RENDERER_H__

#include "avapi.h"
#include "../src/muse.h"
#include <windows.h>

class RenderContext {
public:
	virtual ~RenderContext() {}

	virtual HWND get_window() const = 0;
	virtual HDC get_dc() const = 0;
	virtual char *relative_url( const char *rel ) const = 0;
	virtual bool fetch_url( const char *url8, const char *target8 ) = 0;
	virtual void get_window_dimensions( int &width, int &height ) const = 0;
};

class Renderer {
public:
	static Renderer *create( RenderContext *p );

	virtual ~Renderer() {}

	virtual avmuse *get_avmuse() = 0;
	virtual void write_MIMEType( const char *type ) = 0;
	virtual size_t write_data( void *buffer, size_t nbytes ) = 0;
	virtual void write_complete(bool) = 0;
	virtual void resize( int width, int height, bool stop, bool start ) = 0;

public: // UI event handling. The renderer dispatches these to the
		// rendering thread. They are identical in form to the
		// avmuse interface, but work asynchronously.

	virtual void reshape( int width, int height ) = 0;
	virtual void key_down( unsigned char key, int x, int y ) = 0;
	virtual void key_up( unsigned char key, int x, int y ) = 0;
	virtual void special_key_down( int key, int x, int y ) = 0;
	virtual void special_key_up( int key, int x, int y ) = 0;
	virtual void mouse_button( int button, int state, int x, int y ) = 0;
	virtual void mouse_drag( int x, int y ) = 0;
	virtual void mouse_motion( int x, int y ) = 0;
	virtual void entry( int state ) = 0;
	virtual void visibility( int state ) = 0;


};

#endif // __AVMUSE_RENDERER_H__