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
 *        class-property-list)
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
 * For example, here is a \c &lt;size&gt; class that has a width
 * and height and can compute its area.
 * @code
 * (class '<size>			; The symbol <size> is itself the class.
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
 * @see fn_new   
 */
muse_cell fn_class( muse_env *env, void *context, muse_cell args )
{
	muse_cell className = _evalnext(&args);

	_sett( _tail(className), MUSE_NIL );
	
	_put_prop( className, _builtin_symbol(MUSE_SUPER), _evalnext(&args) );

	while ( args )
	{
		muse_cell pty = _head(args);
		
		_put_prop( className, _head(pty), _eval(_head(_tail(pty))) );
		
		args = _tail(args);
	}

	return className;
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
muse_cell muse_search_object( muse_env *env, muse_cell obj, muse_cell member )
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
