/**
 * @file muse_objc.m
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */
 
#import <Foundation/Foundation.h>
#include <objc/objc-runtime.h>
#include <muse_opcodes.h>


typedef char name_t[256];

void w2n( const muse_char *w, char *n )
{
	do { (*n++) = (char)(*w); } while (*w++);
}

typedef struct 
{
	SEL sel;
	muse_cell sym;
	muse_cell sym_self;
	NSInvocation *invoc;
	NSMethodSignature *sig;
	muse_boolean compiled;
} compiled_sel_t;

muse_cell fn_send( muse_env *env, void *context, muse_cell args );
muse_cell fn_send_super( muse_env *env, void *context, muse_cell args );
muse_cell fn_objc_sel( muse_env *env, compiled_sel_t *sel, muse_cell args );
muse_cell fn_objc_obj( muse_env *env, id obj, muse_cell args );
muse_cell fn_objc_class( muse_env *env, Class c, muse_cell args );
muse_cell fn_expand_objc_expression( muse_env *env, void *context, muse_cell args );
id muse2obj( muse_env *env, muse_cell arg );
muse_cell obj2muse( muse_env *env, id obj );
muse_cell mk_objc_obj( muse_env *env, id obj );
Class objc_class( muse_env *env, muse_cell obj );
id objc_object( muse_env *env, muse_cell obj );

@interface MuseObject : NSObject
{
	muse_env *env;
	muse_cell obj;
	muse_cell sendfn;
	muse_cell vec2listfn;
}
- (id)initWithMuseEnv:(muse_env*)env object:(muse_cell)obj;
- (void)museMark;
- (void)forwardInvocation: (NSInvocation*)invocation;
- (muse_cell)invocation2arglist: (NSInvocation*)invocation;
- (muse_cell) sendMuseMessage:(muse_cell)msg withArgs:(muse_cell)args;
- (id)typicalUIMessage:(id)arg;
@end

void compile_sel( muse_env *env, id obj, compiled_sel_t *sel )
{
	Class c = (Class)obj->isa;
	Method m = class_getInstanceMethod( c, sel->sel );
	if ( m == NULL )
		m = class_getClassMethod( c, sel->sel );
	//muse_assert( m != NULL );

	sel->sig = [obj methodSignatureForSelector:sel->sel];
	if ( sel->sig ) {
		[sel->sig retain];
		
		sel->invoc = [NSInvocation invocationWithMethodSignature: sel->sig];
		[sel->invoc retain];
		[sel->invoc setSelector: sel->sel];
	}
	
	sel->sym_self = _csymbol(L"self");
	sel->compiled = MUSE_TRUE;
}

id alist2dict( muse_env *env, muse_cell alist ) {
	NSMutableDictionary *dict = [NSMutableDictionary dictionary];
	while ( alist ) {
		muse_cell pair = _next(&alist);
		[dict setObject:muse2obj(env,_tail(pair)) forKey:muse2obj(env,_head(pair))];
	}
	return dict;
}

muse_cell objs2list_iter( muse_env *env, NSEnumerator *objs, int i, muse_boolean *eol )
{
	id obj = [objs nextObject];
	if ( obj == nil ) {
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	} else {
		(*eol) = MUSE_FALSE;
		return obj2muse(env,obj);
	}
}

id muse2obj( muse_env *env, muse_cell arg )
{
	switch ( _cellt(arg) ) {
		case MUSE_TEXT_CELL : {
			// Convert to NSString.
			int length = 0;
			const muse_char *text = muse_text_contents( env, arg, &length );
			unichar *buffer = (unichar*)malloc( (length+1) * sizeof(unichar) );
			buffer[length] = 0;
			int i;
			for ( i = 0; i < length; ++i ) {
				buffer[i] = (unichar)text[i];
			}
			id result = [NSString stringWithCharacters:buffer length:length];
			free(buffer);
			return result;
		}
		case MUSE_SYMBOL_CELL : {
			// Convert to NSString.
			muse_cell name = _symname(arg);
			if ( _cellt(name) == MUSE_TEXT_CELL )
				return muse2obj(env,name);
			else
				return nil;
		}
		case MUSE_INT_CELL : {
			// Convert to NSNumber
			muse_int i = muse_int_value(env,arg);
			return [NSNumber numberWithLongLong:i];
		}
		case MUSE_FLOAT_CELL : {
			muse_float f = muse_float_value(env,arg);
			return [NSNumber numberWithDouble:f];
		}
		case MUSE_NATIVEFN_CELL : {
			muse_nativefn_t fn = _ptr(arg)->fn.fn;
			void *ctxt = _ptr(arg)->fn.context;
			
			if ( fn == (muse_nativefn_t)fn_objc_obj || fn == (muse_nativefn_t)fn_objc_class )
				return (id)ctxt;
			
			if ( fn == (muse_nativefn_t)fn_objc_sel )
				return (id)(((compiled_sel_t*)ctxt)->sel);
			
			muse_functional_object_t *fobj = NULL;
			
			if ( fobj = muse_functional_object_data( env, arg, 'vect' ) ) {
				// Convert vector to NSArray.
				int length = muse_vector_length(env, arg);
				NSMutableArray *array = [NSMutableArray array];
				int i;
				for ( i = 0; i < length; ++i ) {
					[array addObject:muse2obj(env,muse_vector_get(env,arg,i))];
				}
				return array;
			}
			
			if ( fobj = muse_functional_object_data( env, arg, 'hash' ) ) {
				// Convert hashtable to dictionary.
				muse_cell ht2alist = _symval(_csymbol(L"hashtable->alist"));
				muse_cell alist = _eval(_cons(ht2alist,_cons(arg,MUSE_NIL)));
				return alist2dict(env,alist);
			}

			muse_assert( !"Unknown function object. Cannot convert to NSObject!" );
			return nil;
		}
		case MUSE_CONS_CELL : {
			// Convert a list to an NSArray.
			NSMutableArray *array = [NSMutableArray array];
			muse_cell list = arg;
			while ( list ) {
				muse_cell item = _next(&list);
				[array addObject:muse2obj(env,item)];
			}
			return array;
		}
	}

	muse_assert( !"Unsupported type!" );
	return nil;
}

muse_cell obj2muse( muse_env *env, id obj ) {
	if ( [obj isKindOfClass:[NSNumber class]] ) {
		NSNumber *num = (NSNumber*)obj;
		muse_int i = [num longLongValue];
		muse_float f = [num doubleValue];
		if ( (muse_float)i != f )
			return muse_mk_float( env, f );
		else
			return muse_mk_int( env, i );
	}
	
	if ( [obj isKindOfClass:[NSString class]] ) {
		// Convert strings to text.
		NSString *str = (NSString*)obj;
		int length = [str length];
		muse_char *buffer = (muse_char*)malloc( sizeof(muse_char) * (length+1) );
		unichar *ubuffer = (unichar*)malloc( sizeof(unichar) * (length+1) );
		buffer[length] = 0;
		ubuffer[length] = 0;
		[str getCharacters:ubuffer];
		int i;
		for ( i = 0; i < length; ++i ) {
			buffer[i] = (muse_char)ubuffer[i];
		}
		free(ubuffer);
		muse_cell text = muse_mk_text( env, buffer, buffer + length );
		free(buffer);
		return text;
	}		
	
	if ( [obj isKindOfClass:[NSArray class]] ) {
		// Convert NSArray to vector.
		int length = [obj count];
		muse_cell vec = muse_mk_vector( env, length );
		int i;
		int sp = _spos();
		for ( i = 0; i < length; ++i ) {
			muse_vector_put( env, vec, i, obj2muse( env, [obj objectAtIndex:i] ) );
			_unwind(sp);
		}
		return vec;
	}

	if ( [obj isKindOfClass:[NSDictionary class]] ) {
		// Convert NSDictionary to hashtable.
		NSEnumerator *keys = [obj keyEnumerator];
		muse_cell ht = muse_mk_hashtable(env,[obj count]);
		id key = nil;
		int sp = _spos();
		while ( key = [keys nextObject] ) {
			muse_hashtable_put( env, ht, obj2muse(env,key), obj2muse(env,[obj objectForKey:key]) );
			_unwind(sp);
		}
		return ht;
	}
	
	if ( [obj isKindOfClass:[NSSet class]] ) {
		// Convert a set to a list.
		NSEnumerator *objs = [obj objectEnumerator];
		muse_cell list = muse_generate_list( env, (muse_list_generator_t)objs2list_iter, objs );
		return list;
	}
	
	muse_assert( !"Unsupported NSObject type! Cannot convert to muSE." );
	return obj2muse( env, [obj description] );
}

typedef union 
{
	char ch;
	short sh;
	int i;
	muse_int q;
	float f;
	double d;
	SEL sel;
	Class c;
	id obj;
} thing_t;

muse_cell muse_objc_msgSend( muse_env *env, id obj, compiled_sel_t *sel, muse_cell args )
{
	int argIndex = 2;
	id *ptrargs[8]; ///< Support for upto 8 pointer return values.
	int numptrargs = 0;
	
	while ( args )
	{
		muse_cell arg = _next(&args);
		const char *type = [sel->sig getArgumentTypeAtIndex:argIndex];

		if ( type[0] == '^' ) {
			switch ( type[1] ) {
				case '@' : {
					// This field is a pointer to an object.
					// You can either pass \c () to indicate you're not interested in the
					// return value, or you can create a temp object that will be
					// filled in with the returned object using @code (@object) @endcode.
					muse_assert( !arg || (_cellt(arg) == MUSE_NATIVEFN_CELL && _ptr(arg)->fn.fn == (muse_nativefn_t)fn_objc_obj && _ptr(arg)->fn.context == NULL) );
					void *p = arg ? &(_ptr(arg)->fn.context) : NULL;
					muse_assert( numptrargs+1 < 8 );
					ptrargs[numptrargs++] = (id*)p;
					[sel->invoc setArgument:&p atIndex:argIndex];
					break;
				}
				default:
					muse_assert( !"Unsupported pointer type!" );
					return MUSE_NIL;
			}
		} else {
			id obj = muse2obj(env,arg);
			thing_t thing;
			
			switch ( type[0] ) {
				case 'c' : case 'C' : {
					thing.ch = [obj charValue];
					break;
				}
				case 's' : case 'S' : {
					thing.sh = [obj shortValue];
					break;
				}
				case 'i': case 'I' : case 'l' : case 'L' : {
					thing.i = [obj intValue];
					break;
				}
				case 'q' : case 'Q' : {
					thing.q = [obj longLongValue];
					break;
				}
				case 'f': {
					thing.f = [obj floatValue];
					break;
				}
				case 'd': {
					thing.d = [obj doubleValue];
					break;
				}
				case ':' : case '#' : case '@' : {
					thing.obj = obj;
					break;
				}
				default : 
					muse_assert( !"Unsupported type!" );
					return MUSE_NIL;
			}
			
			[sel->invoc setArgument:&thing atIndex:argIndex];		
		}
		
		++argIndex;
	}
	
	[sel->invoc invokeWithTarget:obj];
	
	// Make sure any returned pointers are retained correctly.
	while ( numptrargs > 0 ) {
		id i = *ptrargs[--numptrargs];
		if ( i )
			[i retain];
	}
	
	const char *rettype = [sel->sig methodReturnType];
	switch ( rettype[0] ) {
		case 'v' : break;
		case 'c' : case 'C' : {
			char ch = '\0';
			[sel->invoc getReturnValue:&ch];
			return muse_mk_int( env, ch );
		}
		case 's' : case 'S' : {
			short sh = 0;
			[sel->invoc getReturnValue:&sh];
			return muse_mk_int( env, sh );
		}
		case 'i' : case 'I' : case 'l' : case 'L' : {
			int l = 0;
			[sel->invoc getReturnValue:&l];
			return muse_mk_int( env, l );
		}
		case 'q' : case 'Q' : {
			muse_int q = 0;
			[sel->invoc getReturnValue:&q];
			return muse_mk_int( env, q );
		}
		case 'f' : {
			float  f = 0;
			[sel->invoc getReturnValue:&f];
			return muse_mk_float( env, f );
		}
		case 'd' : {
			double f = 0;
			[sel->invoc getReturnValue:&f];
			return muse_mk_float( env, f );
		}
		case '@' : {
			id retobj = nil;
			[sel->invoc getReturnValue:&retobj];
			return mk_objc_obj(env, retobj);
		}
		default: 
			muse_assert( !"Unsupported object type!" );
			return MUSE_NIL;
	}
	
	return MUSE_NIL;
}

muse_cell fn_objc_sel( muse_env *env, compiled_sel_t *sel, muse_cell args )
{
	if ( muse_doing_gc(env) )
	{
		[sel->invoc release];
		[sel->sig release];
		free(sel);
	}
	else
	{
		// The first argument is the object.
		muse_cell objcell = _evalnext(&args);
		id obj = objc_object(env,objcell);
		Class c = objc_class(env,objcell);
		
		// The head can be an already resolved class or an
		// unresolved class symbol. Try to resolve it.
		if ( !c && _cellt(objcell) == MUSE_SYMBOL_CELL )
			c = objc_class( env, resolve_objc_class( env, objcell ) );
		
		if ( c ) {
			// The reference is resolved to an objc class.
			// send the appropriate selector to the class.			
			if ( !sel->compiled )
				compile_sel( env, c, sel );
			
			return muse_objc_msgSend( env, c, sel, muse_eval_list( env, args ) );
		} else if ( obj ) {
			// The reference is to a MuseWrapper or some other object.
			// Send the appropriate selector to the object.						
			if ( [obj isKindOfClass:[MuseObject class]] ) {
				MuseObject *mo = (MuseObject*)obj;
				return [mo sendMuseMessage: sel->sym withArgs: muse_eval_list(env,args)];
			} else {
				if ( !sel->compiled )
					compile_sel( env, obj, sel );
				
				return muse_objc_msgSend( env, obj, sel, muse_eval_list( env, args ) );
			}
		} else if ( objcell == muse_builtin_symbol( env, MUSE_SUPER ) ) {
			// Not an objc symbol and refers to the super tree.
			muse_cell obj = _symval(sel->sym_self);
			return fn_send_super( env, NULL, _cons( _qq(_tail(muse_get_prop(env,obj,objcell))), 
													_cons( obj,
														   _cons( _qq(objcell), args ) ) ) );																	  
		} else if ( _cellt(objcell) == MUSE_SYMBOL_CELL ) {
			// Not an objc class symbol.
			// Send the message to the muSE object.
			return fn_send( env, NULL, _cons( _qq(objcell), _cons( _qq(sel->sym), args ) ) );
		}
	}
	
	return MUSE_NIL;
}

muse_cell mk_sel( muse_env *env, const muse_char *wname )
{
	SEL s = nil;
	muse_cell sym = MUSE_NIL, val = MUSE_NIL;

	/* Get the selector id. */
	{
		char nname[256];
		w2n( wname, nname );
		s = sel_getUid(nname);
	}
	
	/* Decorate the selector symbol with a "[@:]" prefix so that it lands 
		in a different namespace. */
	{
		muse_char selname[256];
		swprintf( selname, 255, L"[@:]%ls", wname );		
		sym = _csymbol(selname);
		val = _symval(sym);
	}

	if ( _cellt(val) == MUSE_NATIVEFN_CELL && _ptr(val)->fn.fn == (muse_nativefn_t)fn_objc_sel ) {
		/* Already compiled selector. */
		return val;
	} else {
		muse_assert( val == sym );
		
		compiled_sel_t *cs = (compiled_sel_t*)calloc( 1, sizeof(compiled_sel_t) );
		cs->sel = s;
		cs->sym = sym;
		if ( s )
			return _define( sym, _mk_nativefn( (muse_nativefn_t)fn_objc_sel, cs ) );
		else
			return MUSE_NIL;
	}
}

muse_cell parse_sel( muse_env *env, muse_cell args )
{
	muse_char selname[256];
	int selname_len = 0;
	
	selname[selname_len] = '\0';
	
	while ( args ) 
	{
		muse_cell selpart = _head(args);
		args = _tail(_tail(args));
		muse_assert( _cellt(selpart) == MUSE_SYMBOL_CELL );
		muse_cell name = _symname(selpart);
		muse_assert( name != MUSE_NIL );
		int len = 0;
		const muse_char *text = muse_text_contents( env, name, &len );
		memcpy( selname + selname_len, text, sizeof(muse_char) * (len + 1) );
		selname_len += len;
	}
	
	return mk_sel( env, selname );
}

muse_cell selargs_extractor( muse_env *env, muse_cell *argv, int i, muse_boolean *eol )
{
	/* The next item will be a method part. The item following that will be 
	the value for that part. */
	if ( *argv ) 
	{
		(*eol) = MUSE_FALSE;
		(*argv) = _tail(*argv);
		if ( *argv )
			return _next(argv);
		else {
			(*eol) = MUSE_TRUE;
			return MUSE_NIL;
		}
	}
	else
	{
		(*eol) = MUSE_TRUE;
		return MUSE_NIL;
	}
}

muse_cell parse_selargs( muse_env *env, muse_cell exprargs )
{
	return muse_generate_list( env, (muse_list_generator_t)selargs_extractor, &exprargs );
}

muse_cell fn_objc_class( muse_env *env, Class c, muse_cell args )
{
	// Nothing to do.
	return MUSE_NIL;
}

muse_cell fn_objc_obj( muse_env *env, id obj, muse_cell args )
{
	if ( muse_doing_gc(env) ) 
	{
		[obj release];
	}
	else if ( !args ) 
	{
		// When an objc object is used in the function position
		// in a normal schame expression, it is taken as a forced
		// conversion to a corresponding scheme object.
		return obj2muse( env, obj );
	}
	
	return MUSE_NIL;
}

muse_cell mk_objc_obj( muse_env *env, id obj )
{
	muse_cell objcell = muse_mk_destructor( env, (muse_nativefn_t)fn_objc_obj, obj );
	if ( obj )
		[obj retain];
	return objcell;
}

muse_cell resolve_objc_class( muse_env *env, muse_cell className )
{
	const muse_char *wname = muse_symbol_name(env,className);
	
	if ( _cellt(className) == MUSE_SYMBOL_CELL && wname != NULL ) {
		char name[256];
		w2n( wname, name );
		
		Class c = objc_getClass(name);
		return c ? muse_define( env, className, muse_mk_nativefn( env, (muse_nativefn_t)fn_objc_class, c ) ) : MUSE_NIL;
	} else {
		return MUSE_NIL;
	}
}

Class objc_class( muse_env *env, muse_cell obj )
{
	if ( _cellt(obj) == MUSE_NATIVEFN_CELL && _ptr(obj)->fn.fn == (muse_nativefn_t)fn_objc_class )
		return (Class)(_ptr(obj)->fn.context);
	else
		return NULL;
}

id objc_object( muse_env *env, muse_cell obj )
{
	if ( _cellt(obj) == MUSE_NATIVEFN_CELL && _ptr(obj)->fn.fn == (muse_nativefn_t)fn_objc_obj )
		return (id)(_ptr(obj)->fn.context);
	else
		return nil;
}

muse_cell fn_expand_objc_expression( muse_env *env, void *context, muse_cell args ) 
{
	muse_cell head = _next(&args);
	muse_cell sel = parse_sel( env, args );
	muse_cell selargs = parse_selargs( env, args );
	return _cons( sel, _cons( head, selargs ) );
}

muse_cell fn_selector( muse_env *env, void *context, muse_cell args )
{
	muse_cell name = _evalnext(&args);
	if ( _cellt(name) == MUSE_SYMBOL_CELL )
		name = _symname(name);
	
	muse_assert( _cellt(name) == MUSE_TEXT_CELL );
	const muse_char *text = muse_text_contents( env, name, NULL );
	return mk_sel( env, text );
}

muse_cell fn_object( muse_env *env, void *context, muse_cell args );

void init_objc_bridge( muse_env *env )
{
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

	int sp = _spos();
	muse_define( env, _csymbol(L"@selector"), muse_mk_nativefn( env, fn_selector, NULL ) );
	muse_define( env, _csymbol(L"@object"), muse_mk_nativefn( env, fn_object, NULL ) );
	_unwind(sp);
	
	env->objc_pool = pool;
}

void destroy_objc_bridge( muse_env *env )
{
	if ( env->objc_pool ) 
	{
		muse_assert( env->parameters[MUSE_ENABLE_OBJC] );
		[((NSAutoreleasePool*)env->objc_pool) release];
		env->objc_pool = NULL;
	}
}

muse_cell invocation2arglist( muse_env *env, NSInvocation *invocation );

@implementation MuseObject
- (id)initWithMuseEnv:(muse_env*)_env object:(muse_cell)_obj
{
	env = _env;
	obj = _obj;
	sendfn = _symval(_csymbol(L"<-"));
	vec2listfn = _symval(_csymbol(L"vector->list"));
	return self;
}

- (void)museMark
{
	muse_mark( env, obj );
}

- (muse_cell) sendMuseMessage:(muse_cell)msg withArgs:(muse_cell)args
{
	return muse_apply( env, sendfn, _cons(obj,_cons(msg,args)), MUSE_TRUE );
}

- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector
{
	NSMethodSignature *sig = [super methodSignatureForSelector:aSelector];
	return sig ? sig : [super methodSignatureForSelector:@selector(typicalUIMessage:)];
}

- (void)forwardInvocation: (NSInvocation*)invocation
{
	int sp = _spos();
	SEL sel = [invocation selector];
	muse_cell museSel = muse_csymbol_utf8(env,sel_getName(sel));
	muse_cell result = muse_apply( env, sendfn, _cons(obj,_cons(museSel,[self invocation2arglist:invocation])), MUSE_TRUE );
	id resultObj = nil;
	resultObj = muse2obj(env,result);
	muse_assert( [[invocation methodSignature] methodReturnType][0] == '@' );
	[invocation setReturnValue:&resultObj];
	_unwind(sp);
}

- (id)typicalUIMessage:(id)arg
{
	return nil;
}

- (muse_cell) invocation2arglist: (NSInvocation *)invocation
{
	NSMethodSignature *sig = [invocation methodSignature];
	int N = [sig numberOfArguments];

	muse_cell vec = muse_mk_vector( env, N-2 );
	int sp = _spos();
	int i;
	
	for ( i = 2; i < N; ++i ) {
		muse_cell val = MUSE_NIL;
		const char *type = [sig getArgumentTypeAtIndex:i];
		switch( type[0] ) {
			case 'i' : case 'I' : case 'l' : case 'L' : {
				long arg = 0;
				[invocation getArgument:&arg atIndex:i];
				val = muse_mk_int( env, arg );
				break;
			}
			case 'c' : case 'C' : {
				char arg = 0;
				[invocation getArgument:&arg atIndex:i];
				val = muse_mk_int( env, arg );
				break;
			}
			case 's' : case 'S' : {
				short arg = 0;
				[invocation getArgument:&arg atIndex:i];
				val = muse_mk_int( env, arg );
				break;
			}
			case 'f' : {
				float arg = 0.0f;
				[invocation getArgument:&arg atIndex:i];
				val = muse_mk_float( env, arg );				
				break;
			}
			case 'd' : {
				double arg = 0.0f;
				[invocation getArgument:&arg atIndex:i];
				val = muse_mk_float( env, arg );				
				break;
			}
			case '@' : {
				id arg = nil;
				[invocation getArgument:&arg atIndex:i];
				if ( [arg isKindOfClass:[MuseObject class]] ) {
					val = ((MuseObject*)arg)->obj;
				} else {
					val = mk_objc_obj(env,arg);
				}
				break;
			}
			default: {
				muse_assert( !"Unsupported invocation argument type!" );
				break;
			}
		}
		muse_vector_put( env, vec, i-2, val );
		_unwind(sp);
	}
	
	return muse_apply( env, vec2listfn, _cons( vec, MUSE_NIL ), MUSE_TRUE );
}

@end

muse_cell fn_object( muse_env *env, void *context, muse_cell args ) 
{
	if ( args ) {
		muse_cell museobj = _evalnext(&args);
		muse_assert( _cellt(museobj) == MUSE_SYMBOL_CELL );
		MuseObject *it = [[MuseObject alloc] initWithMuseEnv: env object: museobj];
		return mk_objc_obj(env,it);
	} else {
		return mk_objc_obj(env,nil);
	}
}
