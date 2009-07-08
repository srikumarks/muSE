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


static muse_prop_view_t g_com_prop_view = {
	com_get_prop,
	com_put_prop
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
			else
				muse_assert( 0 );
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
					muse_message( env, L"com", L"Failed to instantiate class [%s]", progid );
				}

				com_get_details( env, obj );
			}
			else
			{
				muse_message( env, L"com-create", L"Invalid COM object name '%s'", progid );
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
					muse_message( env, L"com", L"Given IUnknown does not support IDispatch!" );
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
		muse_message( env, L"get", L"Unknown type code %d", v->vt );
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
		var->vt = VT_I8;
		var->llVal = muse_int_value( env, cell );
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
							muse_message( env, L"byref", L"Unsupported object type! Expected COM object." );
					}

					// To support com_object_t.
				default:
					muse_message( env, L"out", L"Unsupported output cell type!" );
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
		muse_message( env, L"cell2variant", L"Unsupported cell type!" );
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
			DISPID propid = 0;
			HRESULT hr = obj->idispatch->lpVtbl->GetIDsOfNames( obj->idispatch, &IID_NULL, (LPOLESTR*)&propname, 1, 0, &propid );
			if ( SUCCEEDED(hr) )
			{
				name2dispid->name = name;
				name2dispid->dispid = propid;
				return propid;
			}
			else
			{
				return 0;
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
	if ( propid )
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
		}
		else
			muse_message( env, L"get", L"Unable to get property [%s]", muse_symbol_name(env,key) );

		VariantClear(&result);
		if ( params.cArgs > 0 )
		{
			com_unprepare_args( env, obj, params.cArgs );
		}
	}
	else
		muse_message( env, L"get", L"Invalid property name [%s]", muse_symbol_name(env,key) );

	return val;
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
	if ( propid )
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
		muse_message( env, L"put", L"Invalid property name [%s]", muse_symbol_name(env,key) );

	return value;
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
		if ( fnid )
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
			muse_message( env, L"fn_com_object", L"Invalid method name [%s]", muse_symbol_name(env,method) );
	}

	return retval;
}

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
		muse_message( env, L"com-release", L"Invalid argument! Expecting COM object." );
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
