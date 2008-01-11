/*
 *  movie.cpp
 *  muse
 *
 *  Created by kumar on 27/10/2007.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include "avmuse.h"
#include "qtserver.h"

typedef struct {
	muse_cell sym;
	muse_cell cell;
	QTMovie::fparam_get get;
	QTMovie::fparam_set set;
} param_t;

static void init_fparam( muse_env *env, const muse_char *name, QTMovie::fparam_get get, QTMovie::fparam_set set, param_t *p ) {
	p->sym = _csymbol(name);
	p->cell = muse_mk_float(env,0.0);
	p->get = get;
	p->set = set;
}

static void init_iparam( muse_env *env, const muse_char *name, param_t *p ) {
	p->sym = _csymbol(name);
	p->cell = muse_mk_int(env,0);
	p->get = (QTMovie::fparam_get)NULL;
	p->set = (QTMovie::fparam_set)NULL;
}

typedef struct {
	muse_functional_object_t base;
	
	muse_cell path;
	QTMovie *movie;

	enum { 
		TIME, VOLUME, RATE, BALANCE, DURATION, 
		NUMFPARAMS 
	};
	param_t fparams[NUMFPARAMS];

	muse_cell symReady;
} movie_t;

static void movie_init( muse_env *env, void *_m, muse_cell args ) {
	movie_t *m = (movie_t*)_m;
	m->symReady = _csymbol(L"ready");
	muse_cell path = _evalnext(&args);

	m->path = path;
	const muse_char *filestr = muse_text_contents(env,path,NULL);
	
	init_fparam( env, L"time", &QTMovie::time_secs, &QTMovie::seek_secs, m->fparams + movie_t::TIME );
	init_fparam( env, L"volume", &QTMovie::volume, &QTMovie::setVolume, m->fparams + movie_t::VOLUME );
	init_fparam( env, L"rate", &QTMovie::rate, &QTMovie::setRate, m->fparams + movie_t::RATE );	
	init_fparam( env, L"balance", &QTMovie::balance, &QTMovie::setBalance, m->fparams + movie_t::BALANCE );	
	init_fparam( env, L"duration", &QTMovie::duration_secs, (QTMovie::fparam_set)NULL, m->fparams + movie_t::DURATION );	
}

static void movie_destroy( muse_env *env, void *_m ) {
	movie_t *m = (movie_t*)_m;
	delete m->movie;
	m->movie = NULL;
}

static muse_cell movie_stat_view( muse_env *env, void *obj ) {
	movie_t *m = (movie_t*)obj;
	if ( m->movie->loaded )
		return m->symReady;
	else
		return MUSE_NIL;
}

static muse_cell movie_get_prop( muse_env *env, void *self, muse_cell key ) {
	movie_t *m = (movie_t*)self;
	for ( int i = 0; i < movie_t::NUMFPARAMS; ++i ) {
		param_t *p = m->fparams + i;
		if ( key == p->sym ) {
			// Parameter identified.
			muse_set_float( env, p->cell, (m->movie->*(p->get))() );
			return p->cell;
		}
	}
	return MUSE_NIL;
}

static muse_cell movie_put_prop( muse_env *env, void *self, muse_cell key, muse_cell value ) {
	movie_t *m = (movie_t*)self;
	for ( int i = 0; i < movie_t::NUMFPARAMS; ++i ) {
		param_t *p = m->fparams + i;
		if ( key == p->sym ) {
			// Parameter identified.
			(m->movie->*(p->set))( muse_float_value( env, value ) );
			muse_set_float( env, p->cell, (m->movie->*(p->get))() );
			return p->cell;
		}
	}

	return MUSE_NIL;
}

static muse_prop_view_t g_movie_prop_view = {
	movie_get_prop,
	movie_put_prop
};

static void *movie_view( muse_env *env, int id ) {
	switch ( id ) {
		case 'stat': return (void*)movie_stat_view;
		case 'prop': return (void*)&g_movie_prop_view;
		default: return NULL;
	}
}

static void movie_mark( muse_env *env, void *_m ) {
	movie_t *m = (movie_t*)_m;
	muse_mark(env, m->path);
	for ( int i = 0; i < movie_t::NUMFPARAMS; ++i ) {
		muse_mark( env, m->fparams[i].cell );
	}
}


static void movie_update( muse_env *env, movie_t *m ) {
	muse_set_float( env, m->fparams[movie_t::TIME].cell, m->movie->time_secs() );
}

muse_cell fn_movie( muse_env *env, movie_t *m, muse_cell args ) {
	movie_update(env, m);
	int texUnit = (int)muse_int_value( env, _evalnext(&args) );

	if ( args == MUSE_NIL ) {
		// In no-argument form, you just draw the movie.
		m->movie->draw(texUnit,1.0f);
		return MUSE_NIL;
	} else {
		// Evaluate the arguments in the context of the movie image.
		if ( m->movie->bind(texUnit) ) {
			muse_cell result = MUSE_NIL;

			{
				Model model;
				model.scale( m->movie->aspectRatio );
				result = muse_force( env, muse_do( env, args ) );
			}

			m->movie->unbind(texUnit);
			return result;
		} else
			return MUSE_NIL;
	}
}

static muse_functional_object_type_t g_movie_type = {
	'muSE',
	'moov',
	sizeof(movie_t),
	(muse_nativefn_t)fn_movie,
	movie_view,
	movie_init,
	movie_mark,
	movie_destroy,
	NULL
};

/// (movie "path")
/// Loads a quicktime movie. The returned image can be drawn by just calling it as a function
/// with no arguments, or an alpha argument.
muse_cell api_movie( muse_env *env, qtserver *qt, muse_cell args ) {
	muse_cell obj = muse_mk_functional_object( env, &g_movie_type, args );
	movie_t *m = (movie_t*)_fnobjdata(obj);
	const muse_char *filestr = muse_text_contents( env, m->path, NULL );
	fprintf( stderr, "Opening movie '%ls'\n", filestr );
	m->movie = qt->movieFromFile(filestr);
	return obj;
}

void register_movie( muse_env *env, qtserver *qt ) {
	SP sp(env);
	_define( _csymbol(L"movie"), _mk_nativefn( (muse_nativefn_t)api_movie, qt ) );
}
