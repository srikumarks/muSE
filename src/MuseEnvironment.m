//
//  MuseEnvironment.m
//  muse
//
//  Created by Srikumar Subramanian on 16/03/2007.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import "MuseEnvironment.h"
#include <muse_opcodes.h>

id muse2obj( muse_env *env, muse_cell arg );
muse_cell obj2muse( muse_env *env, id obj );


static muse_env *env = NULL;
static muse_cell getval( muse_env *env, muse_cell obj, muse_cell key );
static void setval( muse_env *env, muse_cell obj, muse_cell key, muse_cell oldval, muse_cell newval );
static void notify( muse_env *env, muse_cell key, NSString *keyStr, id obj, SEL sel );

@implementation MuseEnvironment

+ (void)initialize
{
	if ( env ) return;
	
	int params[] = {
		MUSE_ENABLE_OBJC, MUSE_TRUE,
		MUSE_OWN_OBJC_AUTORELEASE_POOL, MUSE_FALSE,
		MUSE_END_OF_LIST
	};
	
	env = muse_init_env(params);
	int sp = _spos();
	
	// Locate a "main.scm" file in the application's resource directory.
	NSString *main_scm = [[NSBundle mainBundle] pathForResource:@"main" ofType:@"scm"];
	
	if ( main_scm ) {
		// Load it.
		FILE *f = fopen( [main_scm cString], "rb" );
		muse_assert( f != NULL );
		muse_load( env, f );
		fclose(f);
	} else {
		// main.scm not found.
		muse_message( env, L"main.scm", L"The application's main.scm file was not found in the resource directory." );
	}
	_unwind(sp);
}

- (void)destroy
{
	if ( !env )
		muse_destroy_env(env);
}

+ (BOOL)accessInstanceVariablesDirectly
{
	return NO;
}

- (id) valueForKey: (NSString*)key
{
	int sp = _spos();
	muse_cell sym = muse_csymbol_utf8( env, [key UTF8String] );
	muse_cell val = getval( env, MUSE_NIL, sym );
	
	// If the value is a "property function", calling it with no arguments
	// gives us the value. Otherwise, the value is assumed to be directly
	// specified.
	
	id result = muse2obj( env, val );
	_unwind(sp);
	return result;
}

- (void) setValue: (id)val forKey: (NSString*)key
{
	int sp = _spos();
	muse_cell sym = muse_csymbol_utf8( env, [key UTF8String] );
	notify(env,sym,key,self,@selector(willChangeValueForKey:));
	setval( env, MUSE_NIL, sym, _symval(sym), obj2muse(env,val) );
	notify(env,sym,key,self,@selector(didChangeValueForKey:));
	_unwind(sp);
}

@end

static muse_cell getval( muse_env *env, muse_cell obj, muse_cell key )
{
	muse_cell val = obj ? _tail(muse_get(env,obj,key,MUSE_NIL)) : _symval(key);
	if ( _cellt(val) == MUSE_LAMBDA_CELL ) {
		val = muse_apply( env, val, obj ? _cons(obj,MUSE_NIL) : MUSE_NIL, MUSE_TRUE, MUSE_FALSE );
	}
	return val;
}

static void setval( muse_env *env, muse_cell obj, muse_cell key, muse_cell oldval, muse_cell newval )
{
	muse_cell val = oldval ? oldval : (obj ? _tail(muse_get(env,obj,key,MUSE_NIL)) : _symval(key));
	if ( _cellt(val) == MUSE_LAMBDA_CELL ) {
		val = muse_apply( env, val, (obj ? _cons(obj,_cons(newval,MUSE_NIL)) : _cons(newval,MUSE_NIL)), MUSE_TRUE, MUSE_FALSE );
	} else if ( obj ) {
		muse_cell cont = _cons( newval, MUSE_NIL );
		muse_put( env, obj, key, cont );
		_returncell(cont);
	} else {
		_define( key, newval );
	}
}

static void notify( muse_env *env, muse_cell key, NSString *keyStr, id obj, SEL sel )
{
	[obj performSelector:sel withObject:keyStr];
	muse_cell depsSym = _csymbol(L"@dependencies");
	muse_cell deps = _symval(depsSym);
	switch ( _cellt(deps) ) {
		case MUSE_CONS_CELL : // Assoc list.
			deps = _tail(muse_assoc(env,deps,key));
			break;
		case MUSE_SYMBOL_CELL : // Object.
			deps = _tail(muse_get(env,deps,key,MUSE_NIL));
			break;
		case MUSE_LAMBDA_CELL : // fn(key) that returns a list of keys to update when key changes.
			deps = muse_apply( env, deps, _cons(key,MUSE_NIL), MUSE_TRUE, MUSE_FALSE );
			break;
	}
	muse_assert( _cellt(deps) == MUSE_CONS_CELL );
	while ( deps ) {
		muse_cell dep = _next(&deps);
		[obj performSelector:sel withObject:muse2obj(env,dep)];
	}
}
