/**
 * @file muse_win32_com.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * OLE automation interface in muSE. You can create COM objects given
 * their "progids", invoke their methods and get/set their properties
 * via IDispatch.
 */

#include "muse_builtins.h"

#ifdef MUSE_PLATFORM_WINDOWS

#include "muse_port.h"
#include <stdlib.h>
#include <memory.h>
#include <objbase.h>

#pragma comment(lib, "ole32")
#pragma comment(lib, "oleaut32")

typedef struct
{
	muse_cell name;
	DISPID dispid;
} name_dispid_assoc_t;

typedef enum
{
	ComObjectEnumerabilityUnknown = 0,
	ComObjectEnumerable = 1,
	ComObjectNotEnumerable = -1
} com_object_enumerability_t;

typedef struct
{
	com_object_enumerability_t enumerability;
	muse_cell sym_Count, sym_Item;
	DISPID count, item;
} com_object_enumerate;

typedef struct 
{
	muse_functional_object_t base;

	muse_boolean outval; ///< Set to MUSE_TRUE if you want this object to 
						 ///< be passed by reference to an invocation to get
						 ///< a result value.

	IDispatch *idispatch;	///< We can only call methods and access properties
							///< via the IDispatch interface. 

	ITypeInfo *itypeinfo;	///< Typeinfo object obtained from the IDispatch implementation.
	TYPEATTR *typeattr;

	com_object_enumerate monad;
					///< Indicates whether monadic stuff can be applied to this object.

	int maxargs;	///< The maximum number of arguments you can pass to Invoke.
	VARIANT *argv;	///< An argument array that you can use.
					///< See com_need_args(), com_prepare_args() and com_unprepare_args().

	name_dispid_assoc_t dispid_cache[16];
		/**<
		 * The mapping from the symbol to DISPID is cached in a 
		 * 16-element hashtable for fast access. The hash of used
		 * is the [0-15] value obtained by taking the four low order 
		 * bits of the symbol's cell index. The hashtable is destructive
		 * and speeds up access of only the recently used symbols.
		 */
} com_object_t;

static void com_init( muse_env *env, void *p, muse_cell args );
static void com_mark( muse_env *env, void *p );
static void com_destroy( muse_env *env, void *p );
static void com_write( muse_env *env, void *ptr, void *port );
static muse_cell com_get_prop( muse_env *env, void *self, muse_cell key, muse_cell argv );
static muse_cell com_put_prop( muse_env *env, void *self, muse_cell key, muse_cell argv );
static DISPID com_dispid( muse_env *env, com_object_t *obj, muse_cell name );
muse_cell fn_com_object( muse_env *env, com_object_t *obj, muse_cell args );
static void *com_view( muse_env *env, int id );
muse_cell fn_com_create( muse_env *env, void *context, muse_cell args );
muse_cell fn_com_release( muse_env *env, void *context, muse_cell args );

// Monad view functions
static muse_boolean com_object_enumerable( muse_env *env, com_object_t *self );
static muse_cell com_size( muse_env *env, void *self );
static muse_cell com_map( muse_env *env, void *self, muse_cell fn );
static muse_cell com_join( muse_env *env, void *self, muse_cell objlist, muse_cell reduction_fn );
static muse_cell com_collect( muse_env *env, void *self, muse_cell predicate, muse_cell mapper, muse_cell reduction_fn );
static muse_cell com_reduce( muse_env *env, void *self, muse_cell reduction_fn, muse_cell initial );
static muse_cell com_iterator( muse_env *env, com_object_t *self, muse_iterator_callback_t callback, void *context );

static muse_prop_view_t g_com_prop_view = {
	com_get_prop,
	com_put_prop
};

static muse_monad_view_t g_com_monad_view =
{
	com_size,
	com_map,
	com_join,
	com_collect,
	com_reduce
};

static muse_functional_object_type_t g_com_object_type =
{
	'muSE',
	'wcom',
	sizeof(com_object_t),
	(muse_nativefn_t)fn_com_object,
	com_view,
	com_init,
	NULL,
	com_destroy,
	com_write
};

/**
 * COM objects support the 'prop' view. Therefore
 * you can get and set properties of COM objects
 * using the polymorphic "get" and "put" functions.
 */
static void *com_view( muse_env *env, int id )
{
	switch ( id )
	{
	case 'prop' : return &g_com_prop_view;
	case 'mnad' : return &g_com_monad_view;
	case 'iter' : return com_iterator;
	default : return NULL;
	}
}

/**
 * Given a COM object, com_get_details fetches its
 * typeinfo and other data. 
 */
static void com_get_details( muse_env *env, com_object_t *obj )
{
	UINT typeInfoCount = 0;
	HRESULT hr = S_OK;

	if ( obj->idispatch )
	{
		obj->idispatch->lpVtbl->GetTypeInfoCount( obj->idispatch, &typeInfoCount );
		if ( typeInfoCount > 0 )
		{
			obj->itypeinfo = NULL;

			hr = obj->idispatch->lpVtbl->GetTypeInfo( obj->idispatch, 1, 0, &(obj->itypeinfo) );
			if ( SUCCEEDED(hr) )
			{
				hr = obj->itypeinfo->lpVtbl->GetTypeAttr( obj->itypeinfo, &(obj->typeattr) );
				muse_assert( SUCCEEDED(hr) );
			}
		}
	}
}

/**
 * @code (com-release obj) @endcode
 *
 * Releases the object and marks it as usable as an output value.
 */
static void com_release( com_object_t *obj )
{
	if ( obj->typeattr )
	{
		obj->itypeinfo->lpVtbl->ReleaseTypeAttr( obj->itypeinfo, obj->typeattr );
		obj->typeattr = NULL;
	}

	if ( obj->itypeinfo )
	{
		obj->itypeinfo->lpVtbl->Release(obj->itypeinfo);
		obj->itypeinfo = NULL;
	}

	if ( obj->idispatch )
	{
		obj->idispatch->lpVtbl->Release(obj->idispatch);
		obj->idispatch = NULL;
	}

	if ( obj->argv )
	{
		free( obj->argv );
		obj->maxargs = 0;
		obj->argv = NULL;
	}

	obj->outval = MUSE_TRUE;
}

/**
 * Makes sure that the argument vector as enough capacity to
 * hold the given number of arguments.
 */
static void com_need_args( com_object_t *obj, int n )
{
	if ( obj->maxargs < n )
	{
		obj->argv = (VARIANT*)realloc( obj->argv, sizeof(VARIANT) * n );
		memset( obj->argv + obj->maxargs, 0, (n - obj->maxargs) * sizeof(VARIANT) );
		obj->maxargs = n;
	}
}

/**
 * Ensures that argument vector is properly initialized.
 */
static void com_prepare_args( com_object_t *obj, int n )
{
	com_need_args( obj, n );
}

/**
 * Cleans up after an Invoke() and ensures that details of
 * returned COM objects are correctly updated.
 */
static void com_unprepare_args( muse_env *env, com_object_t *obj, int n )
{
	int i;
	for ( i = 0; i < n; ++i )
	{
		if ( obj->argv[i].vt == (VT_BYREF | VT_BSTR) )
		{
			// Update string result by destroying the Sys string
			// and allocating a muSE one for the cell.
			BSTR *pbstrVal = (obj->argv[i].pbstrVal);
			size_t len = wcslen( *pbstrVal );
			muse_char *copy = (muse_char*)calloc( len+1, sizeof(muse_char) );
			memcpy( copy, *pbstrVal, len * sizeof(muse_char) );
			SysFreeString(*pbstrVal);
			pbstrVal[0] = copy;
			pbstrVal[1] = copy + len;
			obj->argv[i].vt = 0;
		}
		else if ( obj->argv[i].vt == (VT_BYREF | VT_DISPATCH) )
		{
			// Update returned COM object.
			com_object_t *resultObj = (com_object_t*)(((char*)obj->argv[i].ppdispVal) - (int)(muse_int)(&(((com_object_t*)0)->idispatch)));
			resultObj->outval = MUSE_FALSE;
			com_get_details( env, resultObj );
			obj->argv[i].vt = 0;
		}

		VariantClear( obj->argv + i );
	}
}

/**
 * When a COM object is garbage collected, it is
 * Release()d.
 */
static void com_destroy( muse_env *env, void *p )
{
	com_object_t *obj = (com_object_t*)p;
	
	com_release(obj);
	
	CoUninitialize();
}

/**
 * @code (com-create "progid") @endcode
 *	and
 * @code (com-create 'progid) @endcode
 *	are both acceptable.
 *
 * If you don't give a progid, you can use the result object
 * as an outval.
 */
static void com_init( muse_env *env, void *p, muse_cell args )
{
	com_object_t *obj = (com_object_t*)p;
	const muse_char *progid = NULL;

	CoInitialize(NULL);

	if ( args == MUSE_NIL )
	{
		obj->outval = MUSE_TRUE;
		return;
	}
	else
	{
		muse_cell arg = muse_evalnext( env, &args );

		if ( muse_cell_type(arg) == MUSE_SYMBOL_CELL )
			progid = muse_symbol_name( env, arg );
		else if ( muse_cell_type(arg) == MUSE_TEXT_CELL )
			progid = muse_text_contents( env, arg, NULL );
		else
			return;

		{
			CLSID clsid;
			HRESULT hr = S_OK;

			hr = CLSIDFromProgID( progid, &clsid );
			if ( SUCCEEDED(hr) )
			{
				hr = CoCreateInstance( &clsid, NULL, CLSCTX_INPROC_SERVER, &IID_IDispatch, (void**)&(obj->idispatch) );
				if ( FAILED(hr) || obj->idispatch == NULL )
				{
					muse_raise_error( env, _csymbol(L"com-error:create-failed"), _cons( arg, MUSE_NIL ) );
					return;
				}

				com_get_details( env, obj );

				// Set the "recent item" reference so that
				// a (com-create "Hello") can be referred to
				// as (the 'Hello).
				switch ( _cellt(arg) ) 
				{
				case MUSE_SYMBOL_CELL: muse_add_recent_item( env, arg, obj->base.self ); break;
				case MUSE_TEXT_CELL: muse_add_recent_item( env, _csymbol(progid), obj->base.self ); break;
				default:;
				}
			}
			else
			{
				muse_raise_error( env, _csymbol(L"com-error:bad-progid"), _cons( arg, MUSE_NIL ) );
			}
		}
	}
}

static void com_write( muse_env *env, void *ptr, void *port )
{
	com_object_t *obj = (com_object_t*)ptr;
	if ( obj->idispatch && obj->outval == MUSE_FALSE )
		port_write( "<com_object>", 12, port );
	else
		port_write( "<com_byref>", 11, port );
}

/**
 * Returns a muSE equivalent of the given VARIANT.
 * Useful for returning values from Invoke() calls.
 */
static muse_cell variant2cell( muse_env *env, VARIANT *v )
{
	muse_cell val = MUSE_NIL;

	switch ( v->vt )
	{
	case VT_BOOL:
		val = v->boolVal ? muse_builtin_symbol( env, MUSE_T ) : MUSE_NIL;
		break;
	case VT_BSTR:
		val = muse_mk_ctext( env, v->bstrVal );
		break;
	case VT_I1:
	case VT_I2:
	case VT_I4:
		val = muse_mk_int( env, v->intVal );
		break;
	case VT_I8:
		val = muse_mk_int( env, v->llVal );
		break;
	case VT_UI1:
	case VT_UI2:
	case VT_UI4:
		val = muse_mk_int( env, v->uintVal );
		break;
	case VT_UI8:
		val = muse_mk_int( env, v->ullVal );
		break;
	case VT_R4:
		val = muse_mk_float( env, v->fltVal );
		break;
	case VT_R8:
		val = muse_mk_float( env, v->dblVal );
		break;
	case VT_UNKNOWN:
		{
			IUnknown *iunknown = v->punkVal;
			IDispatch *idispatch = NULL;
			if ( iunknown )
			{
				HRESULT hr = iunknown->lpVtbl->QueryInterface( iunknown, &IID_IDispatch, &idispatch );
				if ( SUCCEEDED(hr) && idispatch != NULL )
				{
					val = fn_com_create( env, NULL, MUSE_NIL );

					{
						com_object_t *obj = (com_object_t*)muse_functional_object_data( env, val, 'wcom' );
						obj->idispatch = idispatch;
						obj->outval = MUSE_FALSE;
						com_get_details( env, obj );
					}
				}
				else
					muse_raise_error( env, _csymbol(L"com-error:unscriptable-object"), MUSE_NIL );
			}
		}
		break;
	case VT_DISPATCH:
		{
			val = fn_com_create( env, NULL, MUSE_NIL );
			{
				com_object_t *obj = (com_object_t*)muse_functional_object_data( env, val, 'wcom' );
				obj->idispatch = v->pdispVal;
				obj->idispatch->lpVtbl->AddRef( obj->idispatch );
				obj->outval = MUSE_FALSE;
				com_get_details( env, obj );
			}
		}
		break;
	case VT_EMPTY:
	case VT_NULL:
		break;
	default:
		muse_raise_error( env, _csymbol(L"com-error:unknown-type"), _cons( muse_mk_int( env, v->vt ), MUSE_NIL ) );
	}

	return val;
}

static muse_cell tag_byref( muse_env *env, void *context, muse_cell args )
{
	return MUSE_NIL;
}

static muse_cell fn_byref( muse_env *env, void *context, muse_cell args )
{
	return muse_mk_nativefn( env, tag_byref, (void*)(muse_int)muse_evalnext( env, &args ) );
}

/**
 * Returns a VARIANT equivalent of the given muSE cell.
 * Useful for passing muSE objects as arguments.
 *
 * You can mark a cell as "byref" using "(byref var)"
 * when you pass in. If you do that, the cell's contents
 * get updated upon return.
 */
static void cell2variant( muse_env *env, muse_cell cell, VARIANT *var )
{
	VariantInit(var);

	switch ( muse_cell_type(cell) )
	{
	case MUSE_CONS_CELL:
		muse_assert(cell == MUSE_NIL);
		var->vt = VT_I8;
		var->llVal = 0;
		break;
	case MUSE_INT_CELL:
		{
			muse_int i64 = muse_int_value( env, cell );
			int i32 = (int)i64;
			if ( i64 == (muse_int)i32 ) {
				var->vt = VT_I4;
				var->intVal = i32;
			} else {
				var->vt = VT_I8;
				var->llVal = i64;
			}
		}
		break;
	case MUSE_FLOAT_CELL:
		var->vt = VT_R8;
		var->dblVal = muse_float_value( env, cell );
		break;
	case MUSE_SYMBOL_CELL:
		var->vt = VT_BSTR;
		var->bstrVal = SysAllocString( muse_symbol_name( env, cell ) );
		break;
	case MUSE_TEXT_CELL:
		var->vt = VT_BSTR;
		var->bstrVal = SysAllocString( muse_text_contents( env, cell, NULL ) );
		break;
	case MUSE_NATIVEFN_CELL:
		{
			muse_nativefn_t fn = NULL;
			void *context = muse_nativefn_context( env, cell, &fn );
			if ( fn == tag_byref )
			{
				muse_cell outcell = (muse_cell)(muse_int)context;
				switch ( muse_cell_type(outcell) )
				{
				case MUSE_INT_CELL:
					var->vt = VT_BYREF | VT_I8;
					var->pllVal = &(_ptr(outcell)->i);
					break;
				case MUSE_FLOAT_CELL:
					var->vt = VT_BYREF | VT_R8;
					var->pdblVal = &(_ptr(outcell)->f);
					break;
				case MUSE_TEXT_CELL:
					var->vt = VT_BYREF | VT_BSTR;
					free( _ptr(outcell)->text.start );
					_ptr(outcell)->text.start = NULL;
					_ptr(outcell)->text.end = NULL;
					var->pbstrVal = &(_ptr(outcell)->text.start);
					break;
				case MUSE_NATIVEFN_CELL:
					{
						com_object_t *obj = (com_object_t*)muse_functional_object_data( env, outcell, 'wcom' );
						if ( obj )
						{
							com_release(obj);
							var->vt = VT_BYREF | VT_DISPATCH;
							var->ppdispVal = &(obj->idispatch);
						}
						else
							muse_raise_error( env, _csymbol(L"com-error:unsupported-object-type"), MUSE_NIL );
					}
					break;

				default:
					muse_raise_error( env, _csymbol(L"com-error:unsupported-cell-type"), MUSE_NIL );
				}
			}
			else
			{
				com_object_t *obj = (com_object_t*)muse_functional_object_data( env, cell, 'wcom' );
				if ( obj )
				{
					if ( obj->outval )
					{
						var->vt = VT_BYREF | VT_DISPATCH;
						var->ppdispVal = &(obj->idispatch);
					}
					else
					{
						var->vt = VT_DISPATCH;
						var->pdispVal = obj->idispatch;
					}
				}
			}
		}
		break;
	default:
		muse_raise_error( env, _csymbol(L"com-error:unsupported-cell-type"), MUSE_NIL );
	}
}

/**
 * Converts the given argument list to the variant vector
 * in reverse order - i.e. first element of argv maps to
 * the last element of the variant vector. If count < 0,
 * then count is determined to be the length of argv,
 * otherwise count elements are extracted from argv.
 */
static int argv2variants( muse_env *env, muse_cell argv, VARIANT *variants, int count )
{
	if ( count < 0 )
		count = muse_list_length( env, argv );

	{
		int i = 0;
		for ( i = 0; i < count; ++i )
		{
			cell2variant( env, muse_head( env, argv ), variants + count - i - 1 );
			argv = muse_tail( env, argv );
		}
	}

	return count;
}

/**
 * Cached symbol->DISPID mapping.
 */
static DISPID com_dispid( muse_env *env, com_object_t *obj, muse_cell name )
{
	muse_assert( muse_cell_type(name) == MUSE_SYMBOL_CELL );

	{
		int hash = (_celli(name) & 0xF);
		name_dispid_assoc_t *name2dispid = obj->dispid_cache + hash;
		if ( name2dispid->name == name )
		{
			return name2dispid->dispid;
		}
		else
		{
			const muse_char *propname = muse_symbol_name( env, name );
			DISPID propid = DISPID_UNKNOWN;
			HRESULT hr = obj->idispatch->lpVtbl->GetIDsOfNames( obj->idispatch, &IID_NULL, (LPOLESTR*)&propname, 1, 0, &propid );
			if ( SUCCEEDED(hr) )
			{
				name2dispid->name = name;
				name2dispid->dispid = propid;
				return propid;
			}
			else
			{
				return DISPID_UNKNOWN;
			}
		}
	}
}

/**
 * @code (get COMOBJECT 'property) @endcode
 * @code (get COMOBJECT 'arrayprop index) @endcode
 */
static muse_cell com_get_prop( muse_env *env, void *self, muse_cell key, muse_cell argv )
{
	com_object_t *obj = (com_object_t*)self;
	muse_cell val = MUSE_NIL;

	DISPID propid = com_dispid( env, obj, key );
	if ( propid != DISPID_UNKNOWN )
	{
		HRESULT hr = S_OK;
		VARIANT result;
		DISPPARAMS params;

		params.cArgs = muse_list_length( env, argv );
		params.cNamedArgs = 0;
		params.rgdispidNamedArgs = NULL;
		params.rgvarg = NULL;

		if ( params.cArgs > 0 )
		{
			com_prepare_args( obj, params.cArgs );
			argv2variants( env, argv, obj->argv, params.cArgs );
			params.rgvarg = obj->argv;
		}

		VariantInit(&result);
		hr = obj->idispatch->lpVtbl->Invoke( obj->idispatch, propid, &IID_NULL, 0, DISPATCH_PROPERTYGET, &params, &result, NULL, NULL );
		if ( SUCCEEDED(hr) )
		{
			val = variant2cell( env, &result );

			VariantClear(&result);
			if ( params.cArgs > 0 )
			{
				com_unprepare_args( env, obj, params.cArgs );
			}
		}
		else
		{
			VariantClear(&result);
			if ( params.cArgs > 0 )
			{
				com_unprepare_args( env, obj, params.cArgs );
			}

			val = muse_raise_error( env, _csymbol(L"error:key-not-found"), _cons( obj->base.self, _cons( key, MUSE_NIL ) ) );
		}
	}
	else
		val = muse_raise_error( env, _csymbol(L"error:key-not-found"), _cons( obj->base.self, _cons( key, MUSE_NIL ) ) );

	return muse_add_recent_item( env, key, val );
}

/**
 * @code (put COMOBJECT 'property value) @endcode
 * @code (put COMOBJECT 'arrayprop index value) @endcode
 */
static muse_cell com_put_prop( muse_env *env, void *self, muse_cell key, muse_cell argv )
{
	com_object_t *obj = (com_object_t*)self;
	muse_cell value = muse_head( env, argv );

	DISPID propid = com_dispid( env, obj, key );
	if ( propid != DISPID_UNKNOWN )
	{
		DISPPARAMS params;
		DISPID dispidNamed = DISPID_PROPERTYPUT;
		int numparams = muse_list_length( env, argv );
		com_prepare_args( obj, numparams );
		
		params.cArgs = numparams;
		params.cNamedArgs = 1;
		params.rgdispidNamedArgs = &dispidNamed;
		params.rgvarg = obj->argv;

		argv2variants( env, argv, obj->argv, numparams );

		if ( numparams > 0 )
		{
			HRESULT hr = obj->idispatch->lpVtbl->Invoke( obj->idispatch, propid, &IID_NULL, 0, DISPATCH_PROPERTYPUT, &params, NULL, NULL, NULL );
		}

		com_unprepare_args( env, obj, numparams );
	}
	else
		value = muse_raise_error( env, _csymbol(L"error:key-not-found"), _cons( obj->base.self, _cons( key, MUSE_NIL ) ) );

	return muse_add_recent_item( env, key, value );
}

/**
 * @code (COMOBJECT 'method ...args...) @endcode
 */
muse_cell fn_com_object( muse_env *env, com_object_t *obj, muse_cell args )
{
	muse_cell method = muse_evalnext( env, &args );
	muse_cell retval = MUSE_NIL;

	muse_assert( muse_cell_type(method) == MUSE_SYMBOL_CELL );

	{
		DISPID fnid = com_dispid( env, obj, method );
		if ( fnid != DISPID_UNKNOWN )
		{
			DISPPARAMS dispparams;
			muse_cell params = muse_eval_list( env, args );
			int numparams = muse_list_length( env, params );
			com_prepare_args( obj, numparams );

			dispparams.cArgs = numparams;
			dispparams.cNamedArgs = 0;
			dispparams.rgdispidNamedArgs = NULL;
			dispparams.rgvarg = obj->argv;

			argv2variants( env, params, obj->argv, numparams );
			/*
			{
				int i = 0;
				muse_cell iter = params;
				for ( i = 0; i < numparams; ++i )
				{
					muse_cell p = muse_head( env, iter );

					// The order of parameters passed in the variant
					// array is last-to-first.
					cell2variant( env, p, obj->argv + numparams - i - 1 );
					
					iter = muse_tail( env, iter );
				}
			}
			*/

			{
				VARIANT result;
				VariantInit(&result);
			
				{
					HRESULT hr = obj->idispatch->lpVtbl->Invoke( obj->idispatch, fnid, &IID_NULL, 0, DISPATCH_METHOD, &dispparams, &result, NULL, NULL );
					if ( SUCCEEDED(hr) )
					{
						retval = variant2cell( env, &result );
					}
				}

				VariantClear(&result);

				com_unprepare_args( env, obj, numparams );
			}
		}
		else
			retval = muse_raise_error( env, _csymbol(L"error:method-not-found"), _cons( obj->base.self, _cons( method, MUSE_NIL ) ) );
	}

	return muse_add_recent_item( env, method, retval );
}

// Monad and iterator views
static muse_boolean com_object_enumerable( muse_env *env, com_object_t *self )
{
	switch (self->monad.enumerability)
	{
	case ComObjectEnumerable:		return MUSE_TRUE;
	case ComObjectNotEnumerable:	return MUSE_FALSE;
	case ComObjectEnumerabilityUnknown:
		// Find out. For enumerability, the Count()
		// and Item() methods must be defined.
		{
			int sp = _spos();
			DISPID count, item;
			self->monad.sym_Count = _csymbol(L"Count");
			self->monad.sym_Item = _csymbol(L"Item");
			count = com_dispid( env, self, self->monad.sym_Count );
			item = com_dispid( env, self, self->monad.sym_Item );
			if ( count != DISPID_UNKNOWN && item != DISPID_UNKNOWN ) 
			{
				// Enumerable.
				self->monad.enumerability = ComObjectEnumerable;
				self->monad.count = count;
				self->monad.item = item;
			}
			else
			{
				self->monad.enumerability = ComObjectNotEnumerable;
			}						

			return com_object_enumerable( env, self );
		}
	}

	return MUSE_NIL;
}

static muse_cell com_object_item( muse_env *env, com_object_t *self, int i, int sp )
{
	muse_cell result = com_get_prop( env, self, self->monad.sym_Item, _cons( muse_mk_int(env,i), MUSE_NIL ) );
	_unwind(sp);
	_spush(result);
	return result;
}

static muse_cell com_size( muse_env *env, void *obj )
{
	com_object_t *self = (com_object_t*)obj;

	if ( com_object_enumerable(env,self) )
	{
		return com_get_prop( env, self, self->monad.sym_Count, MUSE_NIL );
	}
	else
	{
		return muse_mk_int( env, 0 );
	}
}

static muse_cell com_map( muse_env *env, void *obj, muse_cell fn )
{
	com_object_t *self = (com_object_t*)obj;

	if ( com_object_enumerable(env,self) )
	{
		muse_cell ncell = com_size( env, obj );
		muse_int n = muse_int_value( env, ncell );
		if ( n > MAXINT32 )
			return muse_raise_error( env, _csymbol(L"error:too-many-items"), _cons( self->base.self, _cons( ncell, MUSE_NIL ) ) );
		else
		{
			muse_cell vec = muse_mk_vector( env, (int)n );
			int sp = _spos();
			int i = 0;
			for ( i = 0; i < n; ++i ) 
			{
				muse_cell item = com_object_item( env, self, i, sp );
				if ( fn ) 
				{
					item = muse_apply( env, fn, _cons( item, MUSE_NIL ), MUSE_TRUE, MUSE_FALSE );
				}
				else
				{
					// If fn == MUSE_NIL, then just create a vector
					// of the objects contained in the given COM object.
				}

				muse_vector_put( env, vec, i, item );
				_unwind(sp);
			}

			return vec;
		}
	}
	else
	{
		return MUSE_NIL;
	}
}

static muse_cell fn_com_vectorize( muse_env *env, void *context, muse_cell args )
{
	muse_cell arg = _evalnext(&args);
	com_object_t *obj = (com_object_t*)muse_functional_object_data( env, arg, 'wcom' );
	if ( obj ) 
		return com_map( env, obj, MUSE_NIL );
	else
		return muse_raise_error( env, _csymbol(L"com-error:invalid-object"), _cons( arg, MUSE_NIL ) );
}

static muse_cell com_vectorize_all( muse_env *env, muse_cell coms )
{
	return fn_map( env, NULL, _cons( muse_mk_nativefn( env, fn_com_vectorize, NULL ), _cons( coms, MUSE_NIL ) ) );
}

static muse_cell com_join( muse_env *env, void *obj, muse_cell objlist, muse_cell reduction_fn )
{
	com_object_t *self = (com_object_t*)obj;

	if ( com_object_enumerable(env,self) )
	{
		// Convert all of the objects into vectors using com_map
		// and join the vectors. A bit inefficient, but a lot of COM
		// stuff is. So I'm not bothering to optimize this unless we
		// need to deal with this kind of thing very frequently :P

		return fn_join( env, NULL, com_vectorize_all( env, _cons( self->base.self, objlist ) ) );
	}
	else
	{
		return MUSE_NIL;
	}
}

static muse_cell com_collect( muse_env *env, void *obj, muse_cell predicate, muse_cell mapper, muse_cell reduction_fn )
{
	com_object_t *self = (com_object_t*)obj;

	if ( com_object_enumerable(env,self) )
	{
		// Convert all COM objects to vectors and run Collect on them.
		muse_cell asvector = com_map( env, obj, MUSE_NIL );

		return fn_collect( env, NULL, _cons( asvector, _cons( predicate, _cons( mapper, _cons( reduction_fn, MUSE_NIL ) ) ) ) );
	}
	else
	{
		return MUSE_NIL;
	}
}

static muse_cell com_reduce( muse_env *env, void *obj, muse_cell reduction_fn, muse_cell initial )
{
	com_object_t *self = (com_object_t*)obj;

	if ( com_object_enumerable(env,self) )
	{
		// Convert all COM objects to vectors and run Collect on them.
		muse_cell asvector = com_map( env, obj, MUSE_NIL );

		return fn_reduce( env, NULL, _cons( reduction_fn, _cons( initial, _cons( asvector, MUSE_NIL ) ) ) );
	}
	else
	{
		return MUSE_NIL;
	}
}

static muse_cell com_iterator( muse_env *env, com_object_t *self, muse_iterator_callback_t callback, void *context )
{
	if ( com_object_enumerable(env,self) )
	{
		muse_cell ncell = com_size( env, self );
		muse_int n = muse_int_value( env, ncell );
		if ( n > MAXINT32 )
			return muse_raise_error( env, _csymbol(L"error:too-many-items"), _cons( self->base.self, _cons( ncell, MUSE_NIL ) ) );
		else
		{
			int sp = _spos();
			int i = 0;
			for ( i = 0; i < n; ++i ) 
			{
				muse_cell item = com_object_item( env, self, i, sp );
				muse_boolean cont = callback( env, self, context, item );
				_unwind(sp);
				if ( !cont )
					break;
			}
		}
	}

	return MUSE_NIL;
}


/**
 * @code (com-create "progid") @endcode
 *
 * Creates a new COM object with the given progid.
 * The resulting object can be used in much the same way
 * as anything created using @code (object ...) @endcode.
 *
 * Property access is using the usual dot notation, or via
 * the variadic \ref fn_get "get" and \ref fn_put "put"
 * functions. 
 *
 * Method invocation takes the form -
 *
 * @code (<obj> 'MethodName arg1 arg2 ...) @endcode
 *
 * Do note that methods that take zero arguments are NOT
 * properties that you can access using "get". get/put
 * properties are only those which are declared using the
 * "propget" and "propput" attributes in the IDL spec.
 */
muse_cell fn_com_create( muse_env *env, void *context, muse_cell args )
{
	return muse_mk_functional_object( env, &g_com_object_type, args );
}

/**
 * @code (com-release COMOBJECT) @endcode
 *
 * Releases COMOBJECT and makes it available for use as an outval.
 */
muse_cell fn_com_release( muse_env *env, void *context, muse_cell args )
{
	muse_cell obj = muse_evalnext(env, &args);

	com_object_t *comobj = (com_object_t*)muse_functional_object_data( env, obj, 'wcom' );
	if ( comobj )
	{
		com_release(comobj);
	}
	else
	{
		muse_raise_error( env, _csymbol(L"com-error:cannot-release-object"), _cons( obj, MUSE_NIL ) );
	}

	return MUSE_NIL;
}

/**
 * Adds OLE access facilities to muSE.
 */
void muse_register_com_support( muse_env *env )
{
	int sp = muse_stack_pos(env);
	_define( _csymbol(L"com-create"), _mk_nativefn( fn_com_create, NULL ) );
	_define( _csymbol(L"com-release"), _mk_nativefn( fn_com_release, NULL ) );
	_define( _csymbol(L"byref"), _mk_nativefn( fn_byref, NULL ) );
	muse_stack_unwind(env,sp);
}

#else /* MUSE_PLATFORM_WINDOWS */

void muse_register_com_support( muse_env *env )
{
}

#endif
