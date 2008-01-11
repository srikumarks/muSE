/**
 * @file lighting.cpp
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 * @copyright Copyright (c) 2007, Srikumar K. S. and muvee Technologies Pte. Ltd.
 * @brief Contains functions related to lighting.
 */
#include "avmuse.h"
#include "avapi.h"
#include "../src/muse_opcodes.h"

/** @addtogroup AVMuSE */
/*@{*/
/**
 * @defgroup Lighting
 *
 * @name Defining lights
 * - \ref api_light "light" specifies a single light source and sets up
 *   its initial properties. Subsequently, you can change a property of
 *   all the lights in the context of a \ref api_with_lights "with-lights"
 *   declaration. To make this more syntactically intuitive, 
 *   @code (with-light light0 ...) @endcode
 *   is available as a short hand for 
 *   @code (with-lights (list light0) ...) @endcode.
 * - \ref api_with_lights "with-lights" specifies a scene with the
 *   lights given in the first parameter which should be a list of
 *   lights.
 *
 * @name Specifying light properties
 * 
 */
/*@{*/
/*@}*/

/*
(light N
	(spot-exponent [0-128])
	(spot-cut-off [angle])
	(attenuation c k k^2)
	(ambient r g b a)
	(diffuse r g b a)
	(specular r g b a)
	(position x y z w)
	(spot-direction x y z)
	)

(material	
	(ambient r g b a)
	(diffuse r g b a)
	(specular r g b a)
	(emission r g b a)
	(shininess [0-128])
	)

*/

#if 0 // Work in progress. DON'T TOUCH.

typedef struct {
	muse_cell 
		sym_spot_exponent, fn_spot_exponent,
		sym_spot_cut_off, fn_spot_cut_off,
		sym_attenuation, fn_attenuation,
		sym_ambient, fn_ambient,
		sym_diffuse, fn_diffuse,
		sym_specular, fn_specular,
		sym_position, fn_position,
		sym_spot_direction, fn_spot_direction
		;
} light_context_t;

typedef struct {
	muse_functional_object_t base;
	light_context_t *syms;
} light_t;

muse_cell api_light( muse_env *env, light_t *obj, muse_cell args )
{
	//int N = (int)_intvalue(_evalnext(&args));
	return MUSE_NIL;
}

void *light_view( muse_env *env, int id )
{
	return NULL;
}

void light_init( muse_env *env, void *self, muse_cell args )
{
}

void light_mark( muse_env *env, void *self )
{
}

void light_destroy( muse_env *env, void *self )
{
}

static muse_functional_object_type_t g_light_type = {
	'muSE',
	'opgl',
	sizeof(light_t),
	(muse_nativefn_t)api_light,
	light_view,
	light_init,
	light_mark,
	light_destroy,
	NULL
};

#endif