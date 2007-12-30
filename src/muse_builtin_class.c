/**
 * @file muse_builtin_class.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */


#include "muse_builtins.h"

/**
 * (class name super-tree  plist).
 * Syntax -
 * @code
 * (class class-name-symbol
 *        list-of-super-classes
 *        class-property-list...)
 * @endcode
 * 
 * Returns a new class object - a symbol that has a certain
 * set of properties that can be inherited by "derived" objects.
 * muSE's class system is prototype based. A class is like an
 * object prototype and the inheritance tree of an object actually
 * traces which objects it is prototyped from. Almost the only
 * difference between \ref fn_class "class" and \ref fn_new "new" 
 * is the syntax.
 *
 * The \p list-of-super-classes parameter is a list each of
 * whose elements are evaluated and the result taken to be a
 * super class. This list therefore does not need to have the
 * \c list function call at the head.
 *
 * The \p class-property-list is not part consists of two-element
 * lists where the head specifies the property symbol (in unquoted form)
 * and the second element specifies the value of the property or method.
 * 
 * For example, here is a \c &lt;size&gt; class that has a width
 * and height and can compute its area.
 * @code
 * (class <size>			; The symbol <size> will have the class object as its value.
 *        ()            		; Empty super tree
 *        (width 320)		; Default width of 320
 *        (height 240)		; Default height of 240
 *        (area (fn (self) 	; area method to compute the area of a size object.
 *                (* (-> self 'width) 
 *                   (-> self 'height)))))
 * @endcode
 *
 * The notion of a class in muSE is very dynamic. You can add 
 * properties and methods to classes after defining them, you can
 * change the inheritance hierarchy of an object at run time, etc.
 *
 * Classes are also values and for all practical purposes are 
 * indistinguishable from objects. So definitions can be within
 * modules, exported from modules, etc. just like other values.
 * Also, you can have private class definitions within modules,
 * just like normal values.
 *
 * If you want to refer to a class by value before its
 * definition, you can "forward declare" it like this -
 * @code (class ClassName) @endcode 
 * What this does is to define a class with an empty body
 * and an empty supers list so that a following definition
 * can add to the properties and supers.
 * 
 * @see fn_new   
 */
muse_cell fn_class( muse_env *env, void *context, muse_cell args )
{
	muse_cell className = _next(&args);
	muse_cell currentClassDef = _symval(className);
	muse_cell classDef = MUSE_NIL;
	int sp = 0;

	/* We do the define right at the beginning so that
	methods can create instances of this class without
	referring to it by name alone. */
	if ( currentClassDef == className ) {
		classDef = muse_mk_anon_symbol(env);
		_define( className, classDef );
	} else {
		/* Extend the current class definition. 
		This way, all objects that inherit from this
		class automatically get the extended features. */
		classDef = currentClassDef;
	}

	sp = _spos();

	/* Specify the super list. If the class is already defined,
	this new definition serves to extend the previous one. The
	new super list gets priority over the earlier defined ones. */
	{
		muse_cell newsupers = muse_eval_list( env, _next(&args) );
		muse_cell supers = _get_prop( classDef, _builtin_symbol(MUSE_SUPER) );
		if ( supers )
			_sett( supers, muse_list_append( env, newsupers, _tail(supers) ) );
		else
			_put_prop( classDef, _builtin_symbol(MUSE_SUPER), newsupers );
	}

	while ( args )
	{
		muse_cell pty = _head(args);
		
		_put_prop( classDef, _head(pty), _eval(_head(_tail(pty))) );
		
		_unwind(sp);

		args = _tail(args);
	}

	return classDef;
}

/**
 * (new supers plist).
 * Syntax -
 * @code
 * (new [super-class | list-of-super-classes]
 *      optional-property-list)
 * @endcode
 * 
 * Creates a new object with the given properties overriding the class's properties.
 * The list of super classes of an object is available as the object's \c super
 * property. You can get this property using the expression -
 * @code
 * (-> obj 'super)
 * @endcode
 * 
 * For example, here is an instance of the \c <size> class -
 * @code
 * (define pal-video-size (new '<size> '((width . 720) (height . 576))))
 * @endcode
 * 
 * Now you can compute the number of pixels in a PAL video frame using
 * the expression -
 * @code
 * (<- pal-video-size 'area)
 * @endcode
 * 
 * @see fn_send
 * @see fn_class
 */
muse_cell fn_new( muse_env *env, void *context, muse_cell args )
{
	if ( !args )
		return _mk_anon_symbol();
	else
	{
		muse_cell className = _evalnext(&args);
		muse_cell obj = _mk_anon_symbol();
	
		muse_cell supers = MUSE_NIL;
		switch ( _cellt(className) )
		{
			case MUSE_SYMBOL_CELL : supers = _cons( className, MUSE_NIL ); break;
			case MUSE_CONS_CELL : supers = className; break;
			default :
				MUSE_DIAGNOSTICS({ 
					if ( !muse_expect( env, L"(new >>class/-or-supers<< ...)",
									   L"v|??|", className, MUSE_SYMBOL_CELL, MUSE_CONS_CELL ) )
					{
						muse_message( env, L"(new >>class-or-supers<< ...)",
									  L"new's first argument is expected to be a class symbol\n"
									  L"or a list of super class symbols." );
					}
				});
		}
		
		_put_prop( obj, _builtin_symbol(MUSE_SUPER), supers );
		
		if ( args )
		{
			/* Append the given plist to it. */
			muse_list_append( env, _tail(obj), _evalnext(&args) );
		}
		
		return obj;
	}
}

static muse_cell search_class_hierarchy( muse_env *env, muse_cell classHierarchy, muse_cell member )
{
	while ( classHierarchy )
	{
		muse_cell parent = _head(classHierarchy);

		/* Search specified class. */
		muse_cell result = _get_prop( parent, member );
		if ( result )
			return result;

		/* Method not available in class. Search super classes. */
		result = search_class_hierarchy( env, _tail(_get_prop( parent, _builtin_symbol(MUSE_SUPER) )), member );
		if ( result )
			return result;
		
		classHierarchy = _tail(classHierarchy);
	}
	
	return MUSE_NIL;
}

/**
 * Searches the given object and its inheritance hierarchy for 
 * the given member property. Evaluates to the property-value
 * cons pair if it was found anywhere in the hierarchy, or to
 * () (= MUSE_NIL) if the member doesn't exist.
 */
MUSEAPI muse_cell muse_search_object( muse_env *env, muse_cell obj, muse_cell member )
{
	muse_cell prop = _get_prop( obj, member );
	
	if ( prop )
		return prop;
	else
	{
		muse_cell classHierarchy = _tail(_get_prop( obj, _builtin_symbol(MUSE_SUPER) ));
		return search_class_hierarchy( env, classHierarchy, member );
	}
}


/**
 * (<- obj msg [args]).
 * Syntax -
 * @code
 * (<- obj message-symbol arg1 arg2 ...)
 * @endcode
 * The notation <tt>&lt;-</tt> for \c fn_send is to imply the notion of
 * sending a message to the object. "Sending a message" is the same as
 * "invoking a method". A message handler is the looked up in the object's
 * inheritance hierarchy and invoked with the object as the first
 * argument, usually named \c self. The additional arguments are passed to
 * the message handler function as is.
 * 
 * \c fn_send is technically equivalent to writing -
 * @code
 * ((-> obj message-symbol) obj arg1 arg2 ...)
 * @endcode
 * 
 * @see fn_new
 * @see fn_obj_pty
 */
muse_cell fn_send( muse_env *env, void *context, muse_cell args )
{
	muse_cell obj			= _evalnext(&args);
	muse_cell memberName	= _evalnext(&args);
	muse_cell member		= muse_search_object( env, obj, memberName );
	muse_cell memberVal		= _tail(member);
	
	/* If we're calling send, the member is expected to be a function
	   we can call. */
	if ( memberVal )
		return _lapply( memberVal, _cons( obj, muse_eval_list(env,args) ), MUSE_TRUE );
	else
		return MUSE_NIL;
}

/**
 * (<<- [class(es)] obj method-symbol arg1 arg2 etc).
 * 
 * Use for invoking methods in specific super classes of an objecct
 * instead of following the object's original inheritance hierarchy.
 * This super class method invocation is normally for use within
 * message handler functions and is technically equivalent to -
 * @code
 * ((-> super-class method-symbol) obj arg1 arg2 etc)
 * @endcode
 *
 * You can give a list of super-classes to search for the given method.
 * This way, a message handler can surreptituosly change the inheritance 
 * hierarchy.
 *
 * This function is really very liberal with what it accepts.
 * It is not necessary for the given classes to be anywhere in
 * the object's inheritance hierarchy, for example. So you can
 * do delegation within a single method.
 * 
 * @see fn_send
 * @see fn_new
 */
muse_cell fn_send_super( muse_env *env, void *context, muse_cell args )
{
	muse_cell classes		= _evalnext(&args);
	muse_cell obj			= _evalnext(&args);
	muse_cell methodName	= _evalnext(&args);
	muse_cell methodEntry	= (_cellt(classes) == MUSE_CONS_CELL) 
								? search_class_hierarchy( env, classes, methodName )
								: muse_search_object( env, classes, methodName );
	muse_cell method		= _tail(methodEntry);
	
	if ( method )
		return _lapply( method, _cons( obj, muse_eval_list(env,args) ), MUSE_TRUE );
	else
		return MUSE_NIL;
}

/**
 * (-> obj pty [value]).
 * Syntax to retrieve an object's property -
 * @code
 * (-> obj property-name)
 * @endcode
 * 
 * Syntax to set an object's property -
 * @code
 * (-> obj property-name value)
 * @endcode
 * 
 * The \c -> notation is to mimic the C++ style indirection
 * operator for accessing an object's member variables and functions.
 * 
 * @see fn_send
 * @see fn_new
 */
muse_cell fn_obj_pty( muse_env *env, void *context, muse_cell args )
{
	muse_cell obj			= _evalnext(&args);
	muse_cell memberName	= _evalnext(&args);
	
	if ( args )
	{
		/* Argument given. We should set the member value. */
		return _tail( muse_put_prop( env, obj, memberName, _evalnext(&args) ) );
	}
	else
	{
		/* Argument not given. We should get the member value. */
		return _tail(muse_search_object( env, obj, memberName ));
	}
}

/**
 * Either x == type or type inherits from any of the super classes
 * of x.
 */
static muse_cell common_ancestor( muse_env *env, muse_cell x, muse_cell type )
{
	if ( x == type )
		return x;
	else {
		muse_cell supers = _get_prop( x, _builtin_symbol(MUSE_SUPER) );
		if ( supers ) {
			_next(&supers);
			while ( supers ) {
				muse_cell ca = common_ancestor( env, type, _head(supers) );
				if ( ca ) return ca;
				else _step(&supers);
			}
		} 

		return MUSE_NIL;
	}
}

/**
 * (isa? type x)
 *
 * Compares types. If type and x are of the same non object type,
 * it returns type. If type and x are objects and they both have one
 * common super class, it returns the common super class. If nont
 * of these conditions are satisfied, it returns ().
 */
muse_cell fn_isa_p( muse_env *env, void *context, muse_cell args )
{
	muse_cell type = _evalnext(&args);
	muse_cell x = _evalnext(&args);

	if ( _cellt(type) == MUSE_SYMBOL_CELL && _cellt(x) == MUSE_SYMBOL_CELL ) {
		return common_ancestor( env, x, type );		
	} else {
		return _cellt(x) == _cellt(type) ? type : MUSE_NIL;
	}
}