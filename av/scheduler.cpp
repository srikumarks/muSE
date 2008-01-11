/*
 *  scheduler.cpp
 *  musem
 *
 *  Created by Srikumar Subramanian on 19/08/2007.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include "avapi.h"
#include "../src/muse_opcodes.h"
#include <stdlib.h>
#include <memory.h>

typedef struct {
	muse_cell thunk;
	muse_float time, grace;
} scheduled_item_t;

typedef struct {
	muse_functional_object_t base;
	scheduler *s;
	int count, capacity;
	scheduled_item_t *scheds;
} scheduler_t;

void scheduler_init( muse_env *env, void *_s, muse_cell args ) {
	scheduler_t *s = (scheduler_t*)_s;
	s->capacity = 32;
	s->scheds = (scheduled_item_t*)calloc( sizeof(scheduled_item_t), s->capacity );
}

void scheduler_mark( muse_env *env, void *_s ) {
	scheduler_t *s = (scheduler_t*)_s;
	for ( int i = 0; i < s->count; ++i ) {
		muse_mark( env, s->scheds[i].thunk );
	}
}

void scheduler_destroy( muse_env *env, void *_s ) {
	scheduler_t *s = (scheduler_t*)_s;
	free( s->scheds );
}

static scheduler_t *register_scheduler( muse_env *env );

class scheduler_impl : public scheduler {
public:
	muse *m;
	muse_env *env;
	scheduler_t *s;

	scheduler_impl( muse *_m ) {
		m = _m;
		env = m->get_env();
		s = register_scheduler(env);
		s->s = this;
	}

	~scheduler_impl() {

	}


	void add( muse_cell thunk, muse_float time, muse_float grace ) {
		// First search for any scheduled slots that are complete.
		int N = s->count;
		int slot = -1;
		for ( int i = 0; i < N; ++i ) {
			if ( s->scheds[i].thunk == MUSE_NIL ) {
				slot = i;
				break;
			}
		}
		
		scheduled_item_t *si = NULL;
		
		if ( slot >= 0 ) {
			// Slot found.
			si = s->scheds + slot;
		} else {
			// Need to add new slot.
			if ( s->count >= s->capacity ) {
				// Need to increase capacity.
				int newcap = s->capacity * 2;
				s->scheds = (scheduled_item_t*)realloc( s->scheds, sizeof(scheduled_item_t) * newcap );
				memset( s->scheds + s->capacity, 0, sizeof(scheduled_item_t) * (newcap - s->capacity) );
				s->capacity = newcap;
			}
			
			si = s->scheds + (s->count++);
		}
		
		si->thunk = thunk;
		si->time = time;
		si->grace = grace;
	}

	int update() {
		int count = 0;
		scheduled_item_t *si = s->scheds;
		scheduled_item_t *si_end = si + s->count;
		double time_secs = m->time_secs();
		
		for ( ; si != si_end; ++si ) {
			if ( si->thunk != MUSE_NIL && si->time < time_secs + si->grace ) {
				muse_force( env, muse_apply( env, si->thunk, MUSE_NIL, MUSE_TRUE, MUSE_FALSE ) );
				si->thunk = MUSE_NIL;
				++count;
			}
		}
		
		// Remove the scheduled items that have expired
		// towards the end so that we don't have to run through
		// as many the next time around.
		while ( s->count > 0 && s->scheds[s->count-1].thunk == MUSE_NIL )
		{
			--(s->count);
		}

		return count;
	}


};

/**
* (schedule thunk delta-time [grace])
 */
muse_cell fn_schedule( muse_env *env, scheduler_t *s, muse_cell args ) {
	muse_cell thunk = _evalnext(&args);
	double time_secs = ((scheduler_impl*)(s->s))->m->time_secs();
	muse_float time = time_secs + muse_float_value( env, _evalnext(&args) );
	muse_float grace = 0.0075;
	if ( args ) grace = muse_float_value( env, _evalnext(&args) ); 
	
	s->s->add( thunk, time, grace );
	
	return MUSE_NIL;
}

static muse_functional_object_type_t g_scheduler_type = {
	'muSE',
	'schd',
	sizeof(scheduler_t),
	(muse_nativefn_t)fn_schedule,
	NULL,
	scheduler_init,
	scheduler_mark,
	scheduler_destroy,
	NULL
};

static scheduler_t *register_scheduler( muse_env *env ) {
	SP sp(env);
	muse_cell sym_schedule = _csymbol(L"schedule");
	_define( sym_schedule, muse_mk_functional_object(env, &g_scheduler_type, MUSE_NIL ) );
	return (scheduler_t*)muse_functional_object_data( env, _symval(sym_schedule), 'schd' );
}

scheduler *scheduler::create( muse *m ) {
	return new scheduler_impl(m);
}