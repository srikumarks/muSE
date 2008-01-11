/*
 *  avapi.h
 *  muse
 *
 *  Created by kumar on 24/10/2007.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef __MUSE_AVAPI_H__
#define __MUSE_AVAPI_H__

#include "../src/muse.h"

class muse {
public:
	virtual ~muse() {}
	virtual muse_env *get_env() const = 0;
	virtual double time_secs() const = 0;
	virtual long long time_us() const = 0;
	virtual void needsUpdate() = 0;
};

class avmuse_env {
public:
	virtual ~avmuse_env() {}

	virtual void set_frame_rate( float fps ) = 0;
	virtual void set_title( const char *title ) = 0;
	virtual void reshape( int width, int height ) = 0;
	virtual int get_width() const = 0;
	virtual int get_height() const = 0;
	virtual void draw_char( char c ) = 0;
	virtual void swap_buffers() = 0;
	virtual void *get_dc() const = 0;
};

class avmuse : public muse
{
public:
	static avmuse *create( avmuse_env *av );

	virtual ~avmuse() {}

	virtual void restart() = 0;

	// If update() returns false, you don't need to call render().
	virtual bool update() = 0;
	virtual void render() = 0;

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

class anim {
public:
	static anim *create( muse *m );
	virtual ~anim() {}
	virtual int update() = 0;
	virtual void add( muse_cell var, muse_float startTime, muse_float endTime, muse_float endVal, muse_cell mapper ) = 0;
};

class scheduler {
public:
	static scheduler *create( muse *m );
	virtual ~scheduler() {}
	virtual int update() = 0;
	virtual void add( muse_cell thunk, muse_float time, muse_float grace ) = 0;
};

class watcher {
public:
	static watcher *create( muse *m );
	virtual ~watcher() {}
	virtual int update() = 0;
	virtual void add( muse_cell val, muse_cell fn, muse_float threshold ) = 0;
};

class SP {
public:
	SP( muse_env *_env ) : env(_env) {
		sp = muse_stack_pos(env);
	}
	~SP() {
		muse_stack_unwind(env,sp);
	}
private:
	int sp;
	muse_env *env;
};

#endif // __MUSE_AVAPI_H__