/*
 *  watcher.cpp
 *  muse
 *
 *  Created by kumar on 27/10/2007.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include "avapi.h"
#include "../src/muse_opcodes.h"
#include <stdlib.h>
#include <memory.h>

// QT objects support the 'stat' view which
// can be watched using 'watch' api.
typedef muse_cell (*status_view_t)( muse_env *env, void *obj );

typedef struct {
	muse_cell val;
	muse_cell_data prev;
	muse_float threshold;
	muse_cell fn;
} watched_item_t;

typedef struct {
	muse_functional_object_t base;
	watcher *w;
	int count, capacity;
	watched_item_t *watches;
} watcher_t;

void watcher_init( muse_env *env, void *_s, muse_cell args ) {
	watcher_t *s = (watcher_t*)_s;
	s->capacity = 32;
	s->watches = (watched_item_t*)calloc( sizeof(watched_item_t), s->capacity );
}

void watcher_mark( muse_env *env, void *_w ) {
	watcher_t *w = (watcher_t*)_w;
	for ( int i = 0; i < w->count; ++i ) {
		muse_mark( env, w->watches[i].val );
		muse_mark( env, w->watches[i].fn );
	}
}

void watcher_destroy( muse_env *env, void *_s ) {
	watcher_t *s = (watcher_t*)_s;
	free( s->watches );
}

static watcher_t *register_watcher( muse_env *env );

class watcher_impl : public watcher {
public:
	muse *m;
	muse_env *env;
	watcher_t *w;

	watcher_impl( muse *_m ) {
		m = _m;
		env = m->get_env();
		w = register_watcher(env);
		w->w = this;
	}

	~watcher_impl() {
	}

	void add( muse_cell val, muse_cell fn, muse_float threshold ) {
		// First search for any watched slots that are complete.
		int N = w->count;
		int slot = -1;
		for ( int i = 0; i < N; ++i ) {
			if ( w->watches[i].val == MUSE_NIL ) {
				slot = i;
				break;
			}
		}
		
		watched_item_t *wi = NULL;
		
		if ( slot >= 0 ) {
			// Slot found.
			wi = w->watches + slot;
		} else {
			// Need to add new slot.
			if ( w->count >= w->capacity ) {
				// Need to increase capacity.
				int newcap = w->capacity * 2;
				w->watches = (watched_item_t*)realloc( w->watches, sizeof(watched_item_t) * newcap );
				memset( w->watches + w->capacity, 0, sizeof(watched_item_t) * (newcap - w->capacity) );
				w->capacity = newcap;
			}
			
			wi = w->watches + (w->count++);
		}
		
		wi->val = val;
		wi->fn = fn;
		wi->threshold = threshold;

		if ( _cellt(val) == MUSE_NATIVEFN_CELL ) {
			try {
				wi->prev.cons.head = stat(val); ///< Save the previous stat.
			} catch ( muse_cell ) {
				// Not a stat-able object.
				wi->val = MUSE_NIL;
				wi->fn = MUSE_NIL;
				wi->threshold = 0.0;
			}
		} else if ( _isnumber(val) ) {
			wi->prev = *(_ptr(wi->val)); ///< Save the current value.
		}  else {
			// Not a stat-able object.
			wi->val = MUSE_NIL;
			wi->fn = MUSE_NIL;
			wi->threshold = 0.0;
		}
		
		if ( wi->val == MUSE_NIL ) {
			muse_message( env, L"(watch >>obj<< ..)", L"Can only watch numbers and 'stat'able objects. Given %t [%m].", val, val );
		}
	}

	muse_cell stat( muse_cell val ) {
		muse_functional_object_t *obj = _fnobjdata(val);
		if ( obj ) {
			if ( obj->type_info->view ) {
				status_view_t s = (status_view_t)obj->type_info->view( env, 'stat' );
				if ( s ) {
					return s(env,obj);
				}
			}
		}
		throw MUSE_NIL;
	}

	void remove( muse_cell val ) {
		for ( int i = 0; i < w->count; ++i ) {
			watched_item_t *wi = w->watches + i;
			if ( wi->val == val ) {
				wi->val = MUSE_NIL;
				wi->fn = MUSE_NIL;
			}
		}
		
		// Reduce the count if watches towards
		// the end have been removed.
		while ( w->count > 0 && w->watches[w->count-1].val == MUSE_NIL )
			--(w->count);
	}

	int trigger( watched_item_t *wi, muse_float diff ) {
		diff = (diff < 0) ? -diff : diff;
		if ( diff > wi->threshold ) {
			// Considerable difference.
			// Update the previous value and call the notification function.
			wi->prev = *(_ptr(wi->val));
			_force( muse_apply( env, wi->fn, MUSE_NIL, MUSE_TRUE, MUSE_FALSE ) );
			return 1;
		} else {
			return 0;
		}
	}

	int update_int( watched_item_t *wi ) {
		muse_float diff = (muse_float)(_intvalue(wi->val) - wi->prev.i);
		return trigger( wi, diff );
	}

	int update_float( watched_item_t *wi ) {
		muse_float diff = _floatvalue(wi->val) - wi->prev.f;
		return trigger( wi, diff );
	}

	int update_stat( watched_item_t *wi ) {
		muse_cell currstat = stat(wi->val);
		if ( currstat != wi->prev.cons.head ) {
			wi->prev.cons.head = currstat;
			_force( muse_apply( env, wi->fn, _cons(currstat,MUSE_NIL), MUSE_TRUE, MUSE_FALSE ) );
			return 1;
		} else {
			return 0;
		}
	}

	int update( watched_item_t *wi ) {
		SP sp(env);
		switch ( _cellt(wi->val) ) {
			case MUSE_INT_CELL :
				return update_int(wi);
			case MUSE_FLOAT_CELL :
				return update_float(wi);
			case MUSE_NATIVEFN_CELL :
				return update_stat(wi);
			default:
				return 0;
		}
	}

	int update() {
		int count = 0;
		watched_item_t *wi = w->watches;
		watched_item_t *wi_end = wi + w->count;
		
		for ( ; wi != wi_end; ++wi ) {
			count += update( wi );
		}

		return count;
	}

};

/**
 * (watch val fn [threshold])
 * (unwatch val)
 */
muse_cell fn_watch_unwatch( muse_env *env, watcher_t *w, muse_cell args ) {
	muse_cell val = _evalnext(&args);
	if ( args ) {
		// Add a watch.
		muse_cell fn = _evalnext(&args);
		muse_float threshold = (_cellt(val) == MUSE_INT_CELL) ? 0.9 : 0.0;
		
		if ( args ) 
			threshold = _floatvalue( _evalnext(&args) );
		
		w->w->add( val, fn, threshold );
		
		return val;
	} else {
		// Remove a watch.
		((watcher_impl*)(w->w))->remove( val );
		return val;		
	}
}

static muse_functional_object_type_t g_watcher_type = {
	'muSE',
	'wtch',
	sizeof(watcher_t),
	(muse_nativefn_t)fn_watch_unwatch,
	NULL,
	watcher_init,
	watcher_mark,
	watcher_destroy,
	NULL
};

static watcher_t *register_watcher( muse_env *env ) {
	SP sp(env);
	muse_cell sym_watch = _csymbol(L"watch");
	muse_cell sym_unwatch = _csymbol(L"unwatch");
	muse_cell fn = muse_mk_functional_object(env, &g_watcher_type, MUSE_NIL ) ;
	_define( sym_watch, fn );
	_define( sym_unwatch, fn );
	return (watcher_t*)muse_functional_object_data( env, fn, 'wtch' );
}

watcher *watcher::create( muse *m ) {
	return new watcher_impl(m);
}
