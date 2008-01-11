/*
 *  image.cpp
 *  muse
 *
 *  Created by kumar on 27/10/2007.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include "avmuse.h"
#include "qtserver.h"

typedef struct {
	muse_functional_object_t base;
	
	muse_cell path;
	QTImage *image;

	muse_cell symReady;
} image_t;

static void image_init( muse_env *env, void *_m, muse_cell args ) {
	image_t *m = (image_t*)_m;
	m->symReady = _csymbol(L"ready");
	muse_cell path = _evalnext(&args);

	m->path = path;
}

static void image_destroy( muse_env *env, void *_m ) {
	image_t *m = (image_t*)_m;
	delete m->image;
	m->image = NULL;
}

static muse_cell image_stat_view( muse_env *env, void *obj ) {
	image_t *m = (image_t*)obj;
	if ( m->image->loaded )
		return m->symReady;
	else
		return MUSE_NIL;
}

static void *image_view( muse_env *env, int id ) {
	switch ( id ) {
		case 'stat': return (void*)image_stat_view;
		default: return NULL;
	}
}

static void image_mark( muse_env *env, void *_m ) {
	image_t *m = (image_t*)_m;
	muse_mark(env, m->path);
}


/**
 * (define im (image "somefile.jpg"))
 * 
 * Usage: 
 * (im texUnit) -> Draws the image with aspect ratio correction.
 * (im texUnit ...) -> Binds the image to the texture unit and
 * evaluates the body. You can use \c quad to draw a rectangle
 * containing the image, or use \ref api_quad "quad" in conjunction with 
 * \ref api_chop "chop" to split the image into pieces.
 */
muse_cell fn_image( muse_env *env, image_t *m, muse_cell args ) {
	int texUnit = (int)muse_int_value( env, _evalnext(&args) );

	if ( args == MUSE_NIL ) {
		// In no-argument form, you just draw the image.
		m->image->draw(texUnit,1.0f);
		return MUSE_NIL;
	} else {
		// Evaluate the arguments in the context of the image image.
		if ( m->image->bind(texUnit) ) {
			muse_cell result = MUSE_NIL;

			{
				Model model;
				model.scale( m->image->aspectRatio );
				result = muse_force( env, muse_do( env, args ) );
			}

			m->image->unbind(texUnit);
			return result;
		} else
			return MUSE_NIL;
	}
}

static muse_functional_object_type_t g_image_type = {
	'muSE',
	'imag',
	sizeof(image_t),
	(muse_nativefn_t)fn_image,
	image_view,
	image_init,
	image_mark,
	image_destroy,
	NULL
};

/// (image "path")
/// Loads a quicktime image. The returned image can be drawn by just calling it as a function
/// with one argument - the texture unit to bind it to.
muse_cell api_image( muse_env *env, qtserver *qt, muse_cell args ) {
	muse_cell obj = muse_mk_functional_object( env, &g_image_type, args );
	image_t *m = (image_t*)_fnobjdata(obj);
	const muse_char *filestr = muse_text_contents( env, m->path, NULL );
	fprintf( stderr, "Opening image '%ls'\n", filestr );
	m->image = qt->imageFromFile(filestr);
	return obj;
}

void register_image( muse_env *env, qtserver *qt ) {
	SP sp(env);
	_define( _csymbol(L"image"), _mk_nativefn( (muse_nativefn_t)api_image, qt ) );
}
