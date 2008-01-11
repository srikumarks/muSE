/*
 *  anim.cpp
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

static muse_float mfmax( muse_float f1, muse_float f2 )
{
	return (f1 > f2) ? f1 : f2;
}

static muse_float mfmin( muse_float f1, muse_float f2 )
{
	return (f1 < f2) ? f1 : f2;
}

typedef struct _anim_instance_t {
	struct _anim_instance_t *next;
	muse_cell var;
	bool started;
	muse_float startTime, endTime;
	muse_float startVal, endVal;
	muse_cell mapper;
} anim_instance_t;

typedef struct {
	muse_functional_object_t base;
	anim *a;
	int count, capacity;
	anim_instance_t *anims;
	anim_instance_t **anims_end;
} anim_t;

void anim_init( muse_env *env, void *_a, muse_cell args ) {
	anim_t *a = (anim_t*)_a;
	a->capacity = 32;
	a->count = 0;
	a->anims = (anim_instance_t*)calloc( sizeof(anim_instance_t), a->capacity );
	a->anims_end = &(a->anims);
}

void anim_destroy( muse_env *env, void *_a ) {
	anim_t *a = (anim_t*)_a;
	anim_instance_t *ai = a->anims;
	while (ai)
	{
		anim_instance_t *n = ai->next;
		free(ai);
		ai = n;
	}
	delete a->a;
}

void anim_mark( muse_env *env, void *_a ) {
	anim_t *a = (anim_t*)_a;
	anim_instance_t *ai = a->anims;

	while ( ai ) {
		muse_mark( env, ai->var );
		muse_mark( env, ai->mapper );
		ai = ai->next;
	}
}

static anim_t *register_anim( muse_env *env );

class anim_impl : public anim {
public:
	muse *m;
	muse_env *env;
	anim_t *a;

	anim_impl( muse *_m ) {
		m = _m;
		env = m->get_env();
		a = register_anim(env);
		a->a = this;
	}

	~anim_impl() {

	}

	void add( muse_cell var, muse_float startTime, muse_float endTime, muse_float endVal, muse_cell mapper ) {
		// Find out if the given var is already being animated.
		{
			anim_instance_t *slot = NULL;
			anim_instance_t *i = a->anims;
			while ( i ) {
				if ( i->var == var && (mfmax(endTime,i->endTime) - mfmin(startTime,i->startTime) < (endTime - startTime) + (i->endTime - i->startTime)) ) {
					slot = i;
					break;
				}
				i = i->next;
			}

			if ( slot ) {
				slot->startTime = startTime;
				slot->endTime = endTime;
				slot->startVal = muse_float_value( env, slot->var );
				slot->endVal = endVal;
				slot->mapper = mapper;
				return;
			}
		}
		
		anim_instance_t *ai = (anim_instance_t*)calloc( 1, sizeof(anim_instance_t) );
			
		ai->var = var;
		ai->started = false;
		ai->startTime = startTime;
		ai->endTime = endTime;
		ai->endVal = endVal;
		ai->mapper = mapper;

		ai->next = NULL;
		(*a->anims_end) = ai;
		a->anims_end = &(ai->next);
	}

	int update_float( anim_instance_t *ai ) {
		int count = 0;
		muse_float val = 0;
		bool done = false;
		double time_secs = m->time_secs();
		
		if ( ai->started ) {
			// Animation has started.
			double f = 0.0;
			if ( ai->endTime < time_secs + 0.0005 ) {
				// Too late. Finish up the animation.
				f = 1.0;
				done = true;
			} else if ( ai->startTime + 0.0005 > time_secs ) {
				// Just about starting.
				f = 0.0;
			} else {
				// In the middle.
				f = (time_secs - ai->startTime) / (ai->endTime - ai->startTime);
			}
			
			if ( ai->mapper ) {
				SP sp(env);
				f = muse_float_value( env, _apply( ai->mapper, _cons(_mk_float(f), MUSE_NIL), MUSE_FALSE ) );
			}

			val = ai->startVal + f * (ai->endVal - ai->startVal);
			muse_set_float( env, ai->var, val );
			++count;
		} else {
			// Animation hasn't started yet.			
			if ( ai->endTime <= time_secs ) {
				// Too late, so force value to end value and finish off.
				if ( ai->mapper ) {
					SP sp(env);
					double f = muse_float_value( env, _apply( ai->mapper, _cons(_mk_float(1.0), MUSE_NIL), MUSE_FALSE ) );
					muse_set_float( env, ai->var, ai->startVal + f * (ai->endVal - ai->startVal) );
				} else {
					muse_set_float( env, ai->var, ai->endVal );
				}
				done = true;
				++count;
			} else if ( ai->startTime <= time_secs ) {
				// Maybe slightly delayed, so pick up from where we are.
				ai->started = true;
				ai->startVal = muse_float_value( env, ai->var );
				ai->startTime = time_secs;
			} else {
				// Nothing to do. Too early to start animation.
			}
		}
		
		if ( done ) {
			ai->var = MUSE_NIL;
		}

		return count;
	}

	int update_fn( anim_instance_t *ai ) {
		int count = 0;
		muse_float val = 0;
		bool done = false;
		double time_secs = m->time_secs();
		
		if ( ai->started ) {
			// Animation has started.
			if ( ai->endTime < time_secs + 0.0005 ) {
				// Too late. Finish up the animation.
				val = 1.0;
				done = true;
			} else if ( ai->startTime + 0.0005 > time_secs ) {
				// Just about starting.
				val = 0.0;
			} else {
				// In the middle.
				val = (time_secs - ai->startTime) / (ai->endTime - ai->startTime);
			}
			
			animate_function( ai->var, val );
			++count;
		} else {
			// Animation hasn't started yet.			
			if ( ai->endTime <= time_secs ) {
				// Too late, so force value to end value and finish off.
				animate_function( ai->var, 1.0 );
				done = true;
				++count;
			} else if ( ai->startTime <= time_secs ) {
				// Maybe slightly delayed, so pick up from where we are.
				ai->started = true;
				ai->startTime = time_secs;
				animate_function( ai->var, 0.0 );
				++count;
			} else {
				// Nothing to do. Too early to start animation.
			}
		}
		
		if ( done ) {
			ai->var = MUSE_NIL;
		}

		return count;
	}

	int update( anim_instance_t *ai ) {
		SP sp(env);
		if ( ai->var ) {
			switch ( _cellt(ai->var) ) {
				case MUSE_FLOAT_CELL :
					return update_float(ai);
				case MUSE_LAMBDA_CELL :
					return update_fn(ai);
				default:
					return 0;
			}
		} else {
			return 0;
		}
	}

	int update() {
		int count = 0;
		anim_instance_t **ai = &(a->anims);
		
		while ( *ai ) {
			count += update( *ai );
			if ( (*ai)->var == MUSE_NIL ) {
				// We're done with the animation, remove it.
				anim_instance_t *tmp = (*ai);
				(*ai) = tmp->next;
				free(tmp);
				if ( (*ai) == NULL ) {
					a->anims_end = ai;
				}
			} else {
				ai = &((*ai)->next);
			}
		}

		return count;
	}

	/**
	 * Adds an animation for each component of the
	 * given nested list structure. All cons cells
	 * are stepped into, but apart from float cells,
	 * nothing else is animated.
	 *
	 * You can animate "functions", so to speak.
	 * When given a lambda function that takes one
	 * argument, the function gets called for the
	 * period of the animation with a value from
	 * 0.0 to 1.0. You have to directly pass an animatable
	 * function to (anim ..) for it to be animated.
	 * Functions that are stored inside data structures
	 * are not animated this way. Nor are native
	 * functions animated, even if given in an (anim ..)
	 * expression.
	 */
	void animate_components( muse_cell comp, muse_cell endval, muse_float dur, muse_float startTime, bool callfns, muse_cell mapper ) {
		switch ( _cellt(comp) ) {
			case MUSE_FLOAT_CELL :
			{
				if ( endval ) {
					muse_float endValf = _floatvalue(endval);			
					add(comp, startTime, startTime + dur, endValf, mapper );
				}
			}
			break;
			case MUSE_CONS_CELL :
			{
				muse_assert( _cellt(endval) == MUSE_CONS_CELL );
				// MUSE_NIL for endval means don't animate the corresponding thing.
				if ( comp && endval ) {
					animate_components( _head(comp), _head(endval), dur, startTime, false, mapper );
					animate_components( _tail(comp), _tail(endval), dur, startTime, false, mapper );
				}
			}
			break;
			case MUSE_LAMBDA_CELL:
				if ( callfns )
				{
					/* Functions registered for animation get called with a single
					parameter that goes from 0.0 to 1.0. */
					add( comp, startTime, startTime + dur, 1.0, mapper );
				}
				break;
			default:
				/* Ignore all other types. */
				;
		}
	}

	void animate_function( muse_cell fn, muse_float v ) {
		_force( muse_apply( env, fn, _cons( _mk_float(v), MUSE_NIL ), MUSE_TRUE, MUSE_FALSE ) );
	}

};


/**
 * (anim var val dur [mapper])
 *
 * var can be a float cell or a cons pair where the tail part
 * is a location with the current value and should  be updated.
 */
muse_cell fn_anim( muse_env *env, anim_t *a, muse_cell args ) {
	muse_cell var = _evalnext(&args);
	muse_assert( _cellt(var) == MUSE_FLOAT_CELL || _cellt(var) == MUSE_CONS_CELL );
	muse_cell endval = _evalnext(&args);
	muse_float dur = _floatvalue(_evalnext(&args));
	muse_float startTime = ((anim_impl*)a->a)->m->time_secs();
//	if ( args ) 
//		startTime = _floatvalue(_evalnext(&args));
	
	((anim_impl*)a->a)->animate_components( var, endval, dur, startTime, true, _evalnext(&args) );
	return var;
}

static muse_functional_object_type_t g_anim_type = {
	'muSE',
	'anim',
	sizeof(anim_t),
	(muse_nativefn_t)fn_anim,
	NULL,
	anim_init,
	anim_mark,
	anim_destroy,
	NULL
};

static anim_t *register_anim( muse_env *env ) {
	SP sp(env);
	_define( _csymbol(L"anim"), muse_mk_functional_object(env, &g_anim_type, MUSE_NIL ) );
	return (anim_t*)muse_functional_object_data( env, _symval(_csymbol(L"anim")), 'anim' );	
}

anim *anim::create( muse *m ) {
	return new anim_impl(m);
}
