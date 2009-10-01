/**
 * @file muse_image_info.cpp
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2009 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#include "muse_opcodes.h"
#include "muse_port.h"

#if MUSE_PLATFORM_WINDOWS
#include <windows.h>
#include <gdiplus.h>

using namespace Gdiplus;
#pragma comment(lib,"gdiplus")

typedef struct 
{
	muse_functional_object_t base;
	muse_cell path;			/**< Path to image file. */
	int width, height;		/**< Width and height in pixels. */
	muse_float hres, vres;	/**< Horizontal and vertical resolutions in dots per inch. */
	SizeF physicalDim;		/**< Physical dimensions in whatever units the file chooses. */
	PropertyItem *items;	/**< Vector of property items. */
	size_t count;			/**< Number of property items. */
	size_t byte_size;		/**< Total byte size of all properties. */
	muse_cell cache;		/**< Hashtable of symbolic-tag to value mappings that cache the property access. */
	muse_boolean cache_complete;	/**< When MUSE_TRUE, it means you need not look beyond the cache any more. */
} image_properties_t;

typedef struct
{
	GdiplusStartupInput gdiplusStartupInput;
	ULONG_PTR gdiplusToken;
} gdiplus_info_t;

/**
 * Converts a symbolic tag into a numeric property id.
 */
static PROPID tag_to_code( muse_env *env, muse_cell tag );

/**
 * Converts a numeric property id into a symbolic tag.
 */
static muse_cell code_to_tag( muse_env *env, PROPID code );

/**
 * Takes care of releasing Gdiplus properly.
 */
static muse_cell fn_gdiplus( muse_env *env, gdiplus_info_t *g, muse_cell args )
{
	if ( muse_doing_gc(env) )
	{
		GdiplusShutdown( g->gdiplusToken );
		delete g;
	}

	return MUSE_NIL;
}

/**
 * Converts a few of the property types into muSE values.
 *	-# ASCII values are turned into strings. Trailing spaces are trimmed.
 *	-# Byte arrays are converted into corresponding bytes objects.
 *	-# Short, Long and SLong values are turned into vector of integers.
 *	-# Rational and SRational values are turned into vector of floats.
 * Note that if the number of items is 1 (even for bytes), the value is 
 * not a vector of size 1, but is simply a value. This is convenient since 
 * there are many such parameters.
 */
muse_cell convert_image_property( muse_env *env, image_properties_t *info, PropertyItem *pi, bool ignore_undefined )
{
	switch ( pi->type )
	{
	case PropertyTagTypeASCII :
		{
			const char *str = (const char *)pi->value;
			if ( str[pi->length-1] == 0 )
			{
				// Properly NULL terminated.
				// Trim trailing space.
				size_t len = pi->length;
				while ( len > 0 && (str[len-1] == '\0' || str[len-1] == ' ') )
					--len;
				return muse_mk_text_utf8( env, str, str + len );
			}
			else
				return muse_mk_text_utf8( env, str, str + pi->length );
		}

	case PropertyTagTypeByte :
		{
			size_t N = pi->length;
			if ( N == 1 )
			{
				// A single byte is interpreted as an integer value.
				return _mk_int(((unsigned char *)pi->value)[0]);
			}
			else
			{
				// An array of bytes is converted into a bytes object,
				// which is more compact than a vector.
				muse_cell value = muse_mk_bytes( env, N );
				void *data = muse_bytes_data( env, value, 0 );
				memcpy( data, pi->value, N );
				return value;
			}
		}

	case PropertyTagTypeLong :
		{
			size_t N = pi->length/4;
			if ( N == 1 )
			{
				return _mk_int( ((unsigned long *)pi->value)[0] );
			}
			else
			{
				muse_cell value = muse_mk_vector( env, (int)N );
				int sp = _spos();
				for ( size_t j = 0; j < N; ++j )
				{
					muse_vector_put( env, value, (int)j, muse_mk_int( env, ((unsigned long *)pi->value)[j] ) );
					_unwind(sp);
				}
				return value;
			}
		}

	case PropertyTagTypeRational :
		{
			size_t N = pi->length/8;
			if ( N == 1 )
			{
				const unsigned long *v = ((const unsigned long *)pi->value);
				return muse_mk_float( env, (muse_float)v[0] / (muse_float)v[1] );
			}
			else
			{
				muse_cell value = muse_mk_vector( env, (int)N );
				int sp = _spos();
				for ( size_t j = 0; j < N; ++j )
				{
					muse_vector_put( env, value, (int)j, muse_mk_float( env, (muse_float)(((unsigned long *)pi->value)[j*2]) / (muse_float)(((unsigned long *)pi->value)[j*2+1]) ) );
					_unwind(sp);
				}
				return value;
			}
		}

	case PropertyTagTypeShort :
		{
			size_t N = pi->length/2;
			if ( N == 1 )
			{
				return _mk_int( ((unsigned short *)pi->value)[0] );
			}
			else
			{
				muse_cell value = muse_mk_vector( env, (int)N );
				int sp = _spos();
				for ( size_t j = 0; j < N; ++j )
				{
					muse_vector_put( env, value, (int)j, muse_mk_int( env, ((unsigned short *)pi->value)[j] ) );
					_unwind(sp);
				}
				return value;
			}
		}

	case PropertyTagTypeSLONG :
		{
			size_t N = pi->length/4;
			if ( N == 1 )
			{
				return _mk_int( ((long*)pi->value)[0] );
			}
			else
			{
				muse_cell value = muse_mk_vector( env, (int)N );
				int sp = _spos();
				for ( size_t j = 0; j < N; ++j )
				{
					muse_vector_put( env, value, (int)j, muse_mk_int( env, ((long *)pi->value)[j] ) );
					_unwind(sp);
				}
				return value;
			}
		}

	case PropertyTagTypeSRational :
		{
			size_t N = pi->length/8;
			if ( N == 1 )
			{
				const long *v = (const long *)pi->value;
				return _mk_float( (muse_float)v[0]/(muse_float)v[1] );
			}
			else
			{
				muse_cell value = muse_mk_vector( env, (int)N );
				int sp = _spos();
				for ( size_t j = 0; j < N; ++j )
				{
					muse_vector_put( env, value, (int)j, muse_mk_float( env, (muse_float)(((long *)pi->value)[j*2]) / (muse_float)(((long *)pi->value)[j*2+1]) ) );
					_unwind(sp);
				}
				return value;
			}
		}

	case PropertyTagTypeUndefined :
	default:
		if ( ignore_undefined )
			return MUSE_NIL;
		else
			return muse_raise_error( env, _csymbol(L"image-properties:invalid-tag"), _cons( info->base.self, _cons( _mk_int(pi->id), MUSE_NIL ) ) );
	}
}

/**
 * Runs through the array of available properties looking for the given id
 * and converts the found property item if any to a muSE value, or MUSE_NIL
 * if none was found.
 *
 * @param ignore_undefined When set to false, will cause an undefined property to raise an exception.
 */
static muse_cell lookup_image_property( muse_env *env, image_properties_t *info, PROPID id, bool ignore_undefined )
{
	for ( size_t i = 0, N = info->count; i < N; ++i )
	{
		PropertyItem *pi = info->items + i;
		if ( pi->id == id )
		{
			return convert_image_property( env, info, pi, ignore_undefined );
		}
	}

	return MUSE_NIL;
}

/**
 * Looks into the cache hashtable for the value of the given numeric or symbolic key.
 */
static muse_cell check_cache( muse_env *env, image_properties_t *info, muse_cell key )
{
	return muse_hashtable_get( env, info->cache, _cellt(key) == MUSE_INT_CELL ? code_to_tag( env, (PROPID)_intvalue(key) ) : key );
}

/**
 * Adds the given association to the cache, turning numeric codes into symbolic tags.
 */
static muse_cell cache_property( muse_env *env, image_properties_t *info, muse_cell id, muse_cell value )
{
	int sp = _spos();

	if ( value )
	{
		// Cache the value.
		muse_hashtable_put( env, 
							info->cache, 
							_cellt(id) == MUSE_INT_CELL ? code_to_tag( env, (PROPID)_intvalue(id) ) : id, 
							value );
	}

	_unwind(sp);
	return value;
}

/**
 * Special properties that are also accessed symbolically. These include -
 *	# 'size
 *	# 'physical-size
 *	# 'resolution
 *	# 'path
 * This function also supports the symbolic way of accessing the metadata.
 */
static muse_cell symbolic_property( muse_env *env, image_properties_t *info, muse_cell sym )
{
	int sp = _spos();
	muse_cell key = _csymbol(L"size");
	if ( sym == key )
	{
		muse_cell v = muse_mk_vector( env, 2 );
		muse_vector_put( env, v, 0, _mk_int(info->width) );
		muse_vector_put( env, v, 1, _mk_int(info->height) );
		_unwind(sp);
		_spush(v);
		return v;
	}

	key = _csymbol(L"physical-size");
	if ( sym == key )
	{
		muse_cell v = muse_mk_vector( env, 2 );
		muse_vector_put( env, v, 0, _mk_float(info->physicalDim.Width) );
		muse_vector_put( env, v, 1, _mk_float(info->physicalDim.Height) );
		_unwind(sp);
		_spush(v);
		return v;
	}

	key = _csymbol(L"resolution");
	if ( sym == key )
	{
		muse_cell v = muse_mk_vector( env, 2 );
		muse_vector_put( env, v, 0, _mk_float(info->hres) );
		muse_vector_put( env, v, 1, _mk_float(info->vres) );
		_unwind(sp);
		_spush(v);
		return v;
	}

	key = _csymbol(L"path");
	if ( sym == key )
		return info->path;

	PROPID pid = tag_to_code( env, sym );
	if ( pid )
		return lookup_image_property( env, info, pid, false );
	else
		return muse_raise_error( env, _csymbol(L"image-properties:invalid-tag"), _cons( info->base.self, _cons( sym, MUSE_NIL ) ) );
}

/**
 * Same as symbolic_property - except that it checks the cache first and
 * caches the result if there was a cache miss.
 */
static muse_cell cached_symbolic_property( muse_env *env, image_properties_t *info, muse_cell key )
{
	// Check the cache first.
	muse_cell cached_result = muse_get( env, info->cache, key, MUSE_NIL );
	if ( cached_result )
		return cached_result;

	return cache_property( env, info, key, symbolic_property( env, info, key ) );
}

static void image_properties_destroy( muse_env *env, void *ptr );

/**
 * The main property accessor function.
 */
muse_cell fn_get_image_property( muse_env *env, image_properties_t *info, muse_cell args )
{
	muse_cell idc = _evalnext(&args);

	if ( info->cache == MUSE_NIL )
	{
		// Initialize the cache.
		info->cache = muse_mk_hashtable( env, 8 );
	}

	if ( _cellt(idc) == MUSE_SYMBOL_CELL )
	{
		// Support (prop 'all) to retrieve all the values.
		if ( idc == _csymbol(L"all") )
		{
			if ( !info->cache_complete )
			{
				// Repeated calls to 'all should not do repeated computations 
				// of the same cache.
				int sp = _spos();

				for ( size_t i = 0, N = info->count; i < N; ++i )
				{
					PropertyItem *pi = info->items + i;
					muse_cell pid = _mk_int(pi->id);

					// Check the cache first.
					if ( muse_get( env, info->cache, pid, MUSE_NIL ) == MUSE_NIL )
					{
						cache_property( env, info, pid, convert_image_property( env, info, pi, true ) );
					}

					_unwind(sp);
				}

				cached_symbolic_property( env, info, _csymbol(L"size") );
				cached_symbolic_property( env, info, _csymbol(L"physical-size") );
				cached_symbolic_property( env, info, _csymbol(L"resolution") );
				cached_symbolic_property( env, info, _csymbol(L"path") );
			}

			info->cache_complete = MUSE_TRUE;

			// Release some memory.
			image_properties_destroy( env, info );

			// Just return the complete cache.
			return info->cache;
		}
		else
		{
			return cached_symbolic_property( env, info, idc );
		}
	}
	else if ( _cellt(idc) == MUSE_INT_CELL )
	{
		// Check the cache first.
		muse_cell cached_result = muse_get( env, info->cache, idc, MUSE_NIL );
		if ( cached_result || info->cache_complete )
			return cached_result;

		return cache_property( env, info, idc, lookup_image_property( env, info, (PROPID)_intvalue(idc), false ) );
	}
	else
	{
		return muse_raise_error( env, _csymbol(L"image-properties:invalid-tag"), _cons( info->base.self, _cons( idc, MUSE_NIL ) ) );
	}
}

/**
 * For properly disposing of a Gdiplus image.
 */
static muse_cell fn_gdiplus_image( muse_env *env, Image *im, muse_cell args )
{
	if ( muse_doing_gc(env) )
		delete im;
	return MUSE_NIL;
}

/**
 * Initializes Gdiplus once and sets up its destruction
 * when the muSE environment is destroyed.
 */
static void init_gdiplus( muse_env *env )
{
	muse_cell symGdiplus = _csymbol(L"{{Gdiplus}}");

	// Check if Gdiplus has been initialized.
	if ( symGdiplus == _symval(symGdiplus) )
	{
		// Undefined. Initialize Gdiplus and protect it with a destructor.
		gdiplus_info_t *gi = new gdiplus_info_t;
		Status s = GdiplusStartup( &(gi->gdiplusToken), &(gi->gdiplusStartupInput), NULL );
		if ( s == Ok )
			_define( symGdiplus, muse_mk_destructor( env, (muse_nativefn_t)fn_gdiplus, gi ) );
		else
		{
			muse_raise_error( env, _csymbol(L"image-properties:gdiplus-error"), MUSE_NIL );
			return;
		}
	}
}

static const muse_char *nameTagToCodeTable = L"{{ImagePropertyTagToCodeTable}}";
static const muse_char *nameCodeToTagTable = L"{{ImagePropertyCodeToTagTable}}";

/**
 * Constructs a two-way hashtable of property codes to symbolic tags and vice versa.
 */
static void init_tag_table( muse_env *env )
{
	int sp = _spos();

	muse_cell symTagToCodeTable = _csymbol(nameTagToCodeTable);
	muse_cell symCodeToTagTable = _csymbol(nameCodeToTagTable);

	muse_cell tag2code = _symval(symTagToCodeTable);
	muse_cell code2tag = _symval(symCodeToTagTable);

	if ( tag2code != symTagToCodeTable && code2tag != symCodeToTagTable )
		return; // Already initialized.

	tag2code = muse_mk_hashtable( env, 217 );
	code2tag = muse_mk_hashtable( env, 217 );

	_define( symTagToCodeTable, tag2code );
	_define( symCodeToTagTable, code2tag );

	_unwind(sp);

	struct code_to_tag_t
	{
		const muse_char *tag;
		PROPID code;
	};

	static const code_to_tag_t k_code_to_tag[] =
	{
		{L"gps-ver",	0x0000},
		{L"gps-latitude-ref",	0x0001},
		{L"gps-latitude",	0x0002},
		{L"gps-longitude-ref",	0x0003},
		{L"gps-longitude",	0x0004},
		{L"gps-altitude-ref",	0x0005},
		{L"gps-altitude",	0x0006},
		{L"gps-gps-time",	0x0007},
		{L"gps-gps-satellites",	0x0008},
		{L"gps-gps-status",	0x0009},
		{L"gps-gps-measure-mode",	0x000a},
		{L"gps-gps-dop",	0x000b},
		{L"gps-speed-ref",	0x000c},
		{L"gps-speed",	0x000d},
		{L"gps-track-ref",	0x000e},
		{L"gps-track",	0x000f},
		{L"gps-img-dir-ref",	0x0010},
		{L"gps-img-dir",	0x0011},
		{L"gps-map-datum",	0x0012},
		{L"gps-dest-lat-ref",	0x0013},
		{L"gps-dest-lat",	0x0014},
		{L"gps-dest-long-ref",	0x0015},
		{L"gps-dest-long",	0x0016},
		{L"gps-dest-bear-ref",	0x0017},
		{L"gps-dest-bear",	0x0018},
		{L"gps-dest-dist-ref",	0x0019},
		{L"gps-dest-dist",	0x001a},
		{L"new-subfile-type",	0x00fe},
		{L"subfile-type",	0x00ff},
		{L"image-width",	0x0100},
		{L"image-height",	0x0101},
		{L"bits-per-sample",	0x0102},
		{L"compression",	0x0103},
		{L"photometric-interp",	0x0106},
		{L"thresh-holding",	0x0107},
		{L"cell-width",	0x0108},
		{L"cell-height",	0x0109},
		{L"fill-order",	0x010a},
		{L"document-name",	0x010d},
		{L"image-description",	0x010e},
		{L"equip-make",	0x010f},
		{L"equip-model",	0x0110},
		{L"strip-offsets",	0x0111},
		{L"orientation",	0x0112},
		{L"samples-per-pixel",	0x0115},
		{L"rows-per-strip",	0x0116},
		{L"strip-bytes-count",	0x0117},
		{L"min-sample-value",	0x0118},
		{L"max-sample-value",	0x0119},
		{L"x-resolution",	0x011a},
		{L"y-resolution",	0x011b},
		{L"planar-config",	0x011c},
		{L"page-name",	0x011d},
		{L"x-position",	0x011e},
		{L"y-position",	0x011f},
		{L"free-offset",	0x0120},
		{L"free-byte-counts",	0x0121},
		{L"gray-response-unit",	0x0122},
		{L"gray-response-curve",	0x0123},
		{L"t4-option",	0x0124},
		{L"t6-option",	0x0125},
		{L"resolution-unit",	0x0128},
		{L"page-number",	0x0129},
		{L"transfer-function",	0x012d},
		{L"software-used",	0x0131},
		{L"date-time",	0x0132},
		{L"artist",	0x013b},
		{L"host-computer",	0x013c},
		{L"predictor",	0x013d},
		{L"white-point",	0x013e},
		{L"primary-chromaticities",	0x013f},
		{L"color-map",	0x0140},
		{L"halftone-hints",	0x0141},
		{L"tile-width",	0x0142},
		{L"tile-length",	0x0143},
		{L"tile-offset",	0x0144},
		{L"tile-byte-counts",	0x0145},
		{L"ink-set",	0x014c},
		{L"ink-names",	0x014d},
		{L"number-of-inks",	0x014e},
		{L"dot-range",	0x0150},
		{L"target-printer",	0x0151},
		{L"extra-samples",	0x0152},
		{L"sample-format",	0x0153},
		{L"s-min-sample-value",	0x0154},
		{L"s-max-sample-value",	0x0155},
		{L"transfer-range",	0x0156},
		{L"jpeg-proc",	0x0200},
		{L"jpeg-inter-format",	0x0201},
		{L"jpeg-inter-length",	0x0202},
		{L"jpeg-restart-interval",	0x0203},
		{L"jpeg-lossless-predictors",	0x0205},
		{L"jpeg-point-transforms",	0x0206},
		{L"jpeg-q-tables",	0x0207},
		{L"jpeg-dc-tables",	0x0208},
		{L"jpeg-ac-tables",	0x0209},
		{L"ycbcrcoefficients",	0x0211},
		{L"ycbcrsubsampling",	0x0212},
		{L"ycbcrpositioning",	0x0213},
		{L"ref-black-white",	0x0214},
		{L"gamma",	0x0301},
		{L"icc-profile-descriptor",	0x0302},
		{L"srgb-rendering-intent",	0x0303},
		{L"image-title",	0x0320},
		{L"resolution-x-unit",	0x5001},
		{L"resolution-y-unit",	0x5002},
		{L"resolution-x-length-unit",	0x5003},
		{L"resolution-y-length-unit",	0x5004},
		{L"print-flags",	0x5005},
		{L"print-flags-version",	0x5006},
		{L"print-flags-crop",	0x5007},
		{L"print-flags-bleed-width",	0x5008},
		{L"print-flags-bleed-width-scale",	0x5009},
		{L"halftone-lpi",	0x500a},
		{L"halftone-lpi-unit",	0x500b},
		{L"halftone-degree",	0x500c},
		{L"halftone-shape",	0x500d},
		{L"halftone-misc",	0x500e},
		{L"halftone-screen",	0x500f},
		{L"jpeg-quality",	0x5010},
		{L"grid-size",	0x5011},
		{L"thumbnail-format",	0x5012},
		{L"thumbnail-width",	0x5013},
		{L"thumbnail-height",	0x5014},
		{L"thumbnail-color-depth",	0x5015},
		{L"thumbnail-planes",	0x5016},
		{L"thumbnail-raw-bytes",	0x5017},
		{L"thumbnail-size",	0x5018},
		{L"thumbnail-compressed-size",	0x5019},
		{L"color-transfer-function",	0x501a},
		{L"thumbnail-data",	0x501b},
		{L"thumbnail-image-width",	0x5020},
		{L"thumbnail-image-height",	0x5021},
		{L"thumbnail-bits-per-sample",	0x5022},
		{L"thumbnail-compression",	0x5023},
		{L"thumbnail-photometric-interp",	0x5024},
		{L"thumbnail-image-description",	0x5025},
		{L"thumbnail-equip-make",	0x5026},
		{L"thumbnail-equip-model",	0x5027},
		{L"thumbnail-strip-offsets",	0x5028},
		{L"thumbnail-orientation",	0x5029},
		{L"thumbnail-samples-per-pixel",	0x502a},
		{L"thumbnail-rows-per-strip",	0x502b},
		{L"thumbnail-strip-bytes-count",	0x502c},
		{L"thumbnail-resolution-x",	0x502d},
		{L"thumbnail-resolution-y",	0x502e},
		{L"thumbnail-planar-config",	0x502f},
		{L"thumbnail-resolution-unit",	0x5030},
		{L"thumbnail-transfer-function",	0x5031},
		{L"thumbnail-software-used",	0x5032},
		{L"thumbnail-date-time",	0x5033},
		{L"thumbnail-artist",	0x5034},
		{L"thumbnail-white-point",	0x5035},
		{L"thumbnail-primary-chromaticities",	0x5036},
		{L"thumbnail-ycbcrcoefficients",	0x5037},
		{L"thumbnail-ycbcrsubsampling",	0x5038},
		{L"thumbnail-ycbcrpositioning",	0x5039},
		{L"thumbnail-ref-black-white",	0x503a},
		{L"thumbnail-copy-right",	0x503b},
		{L"luminance-table",	0x5090},
		{L"chrominance-table",	0x5091},
		{L"frame-delay",	0x5100},
		{L"loop-count",	0x5101},
		{L"global-palette",	0x5102},
		{L"index-background",	0x5103},
		{L"index-transparent",	0x5104},
		{L"pixel-unit",	0x5110},
		{L"pixel-per-unit-x",	0x5111},
		{L"pixel-per-unit-y",	0x5112},
		{L"palette-histogram",	0x5113},
		{L"copyright",	0x8298},
		{L"exif-exposure-time",	0x829a},
		{L"exif-f-number",	0x829d},
		{L"exif-ifd",	0x8769},
		{L"icc-profile",	0x8773},
		{L"exif-exposure-prog",	0x8822},
		{L"exif-spectral-sense",	0x8824},
		{L"gps-ifd",	0x8825},
		{L"exif-iso-speed",	0x8827},
		{L"exif-oecf",	0x8828},
		{L"exif-ver",	0x9000},
		{L"exif-dt-orig",	0x9003},
		{L"exif-dt-digitized",	0x9004},
		{L"exif-comp-config",	0x9101},
		{L"exif-comp-bpp",	0x9102},
		{L"exif-shutter-speed",	0x9201},
		{L"exif-aperture",	0x9202},
		{L"exif-brightness",	0x9203},
		{L"exif-exposure-bias",	0x9204},
		{L"exif-max-aperture",	0x9205},
		{L"exif-subject-dist",	0x9206},
		{L"exif-metering-mode",	0x9207},
		{L"exif-light-source",	0x9208},
		{L"exif-flash",	0x9209},
		{L"exif-focal-length",	0x920a},
		{L"exif-maker-note",	0x927c},
		{L"exif-user-comment",	0x9286},
		{L"exif-dt-subsec",	0x9290},
		{L"exif-dt-orig-s-s",	0x9291},
		{L"exif-dt-dig-s-s",	0x9292},
		{L"exif-fpx-ver",	0xa000},
		{L"exif-color-space",	0xa001},
		{L"exif-pix-x-dim",	0xa002},
		{L"exif-pix-y-dim",	0xa003},
		{L"exif-related-wav",	0xa004},
		{L"exif-interop",	0xa005},
		{L"exif-flash-energy",	0xa20b},
		{L"exif-spatial-fr",	0xa20c},
		{L"exif-focal-x-res",	0xa20e},
		{L"exif-focal-y-res",	0xa20f},
		{L"exif-focal-res-unit",	0xa210},
		{L"exif-subject-loc",	0xa214},
		{L"exif-exposure-index",	0xa215},
		{L"exif-sensing-method",	0xa217},
		{L"exif-file-source",	0xa300},
		{L"exif-scene-type",	0xa301},
		{L"exif-cfa-pattern",	0xa302},
		{NULL,0}
	};

	for ( const code_to_tag_t *p = k_code_to_tag; p->tag != NULL; ++p )
	{
		muse_cell tag = _csymbol(p->tag);
		muse_cell code = _mk_int(p->code);
		muse_hashtable_put( env, tag2code, tag, code );
		muse_hashtable_put( env, code2tag, code, tag );
		_unwind(sp);
	}

	_unwind(sp);
}

static PROPID tag_to_code( muse_env *env, muse_cell tag )
{
	muse_cell table = _symval(_csymbol(nameTagToCodeTable));
	muse_cell value = muse_hashtable_get( env, table, tag );
	return value ? (PROPID)_intvalue(value) : (PROPID)0;
}

static muse_cell code_to_tag( muse_env *env, PROPID code )
{
	muse_cell table = _symval(_csymbol(nameCodeToTagTable));
	return muse_hashtable_get( env, table, _mk_int(code) );
}

static void image_properties_init( muse_env *env, void *ptr, muse_cell args )
{
	image_properties_t *im = (image_properties_t*)ptr;

	init_gdiplus(env);
	init_tag_table(env);

	muse_cell path = _evalnext(&args);
	muse_cell gimc = MUSE_NIL;
	Image *gim = NULL;
	int sp = _spos();

	while ( true )
	{
		// Throw until we're given a valid path.
		while ( _cellt(path) != MUSE_TEXT_CELL )
		{
			path = muse_raise_error( env, _csymbol(L"error:string-expected"), _cons( path, MUSE_NIL ) );
			_unwind(sp);
		}

		// Throw until we're given a valid image file.
		gim = Image::FromFile( muse_text_contents( env, path, NULL ) );
		if ( !gim || gim->GetLastStatus() != Ok )
		{
			delete gim;
			path = muse_raise_error( env, _csymbol(L"image-properties:file-error"), _cons( path, MUSE_NIL ) );
		}
		else
		{
			gimc = muse_mk_destructor( env, (muse_nativefn_t)fn_gdiplus_image, gim );
			// Keep that on the stack.
			break;
		}

		_unwind(sp);
	}
	
	// At this point, the path and image are valid.

	im->path = path;
	im->width = gim->GetWidth();
	im->height = gim->GetHeight();
	im->hres = gim->GetHorizontalResolution();
	im->vres = gim->GetVerticalResolution();
	gim->GetPhysicalDimension(&(im->physicalDim));

	UINT propSize, numProps;
	gim->GetPropertySize( &propSize, &numProps );

	im->byte_size = propSize;
	im->count = numProps;

	if ( propSize > 0 )
	{
		im->items = (PropertyItem*)calloc( im->byte_size, 1 );
		Status s =	gim->GetAllPropertyItems( propSize, numProps, im->items );
		if ( s != Ok )
		{
			im->count = 0; // Setting count to 0 will cause all property lookup to be skipped.
			return;
		}

		delete (Image*)(_ptr(gimc)->fn.context);
		_ptr(gimc)->fn.context = NULL;
	}
}

static void image_properties_mark( muse_env *env, void *ptr )
{
	muse_mark( env, ((image_properties_t*)ptr)->path );
	muse_mark( env, ((image_properties_t*)ptr)->cache );
}

static void image_properties_destroy( muse_env *env, void *ptr )
{
	image_properties_t *im = (image_properties_t*)ptr;

	free(im->items);
	im->items = NULL;
	im->count = 0;
	im->byte_size = 0;
}

static muse_cell image_properties_get( muse_env *env, void *self, muse_cell key, muse_cell argv )
{
	image_properties_t *im = (image_properties_t*)self;
	if ( _cellt(key) == MUSE_INT_CELL || _cellt(key) == MUSE_SYMBOL_CELL ) {
		muse_cell v = fn_get_image_property( env, im, _cons(_qq(key),MUSE_NIL) );
		return argv ? muse_get( env, v, _head(argv), _tail(argv) ) : v;
	} else {
		return MUSE_NIL;
	}
}

static muse_prop_view_t g_image_properties_prop_view =
{
	image_properties_get,
	NULL
};

static void *image_properties_view( muse_env *env, int id )
{
	switch ( id )
	{
		case 'prop' : return &g_image_properties_prop_view;
		default : return NULL;
	}
}

muse_functional_object_type_t g_image_properties_type =
{
	'muSE',
	'exif',
	sizeof(image_properties_t),
	(muse_nativefn_t)fn_get_image_property,
	image_properties_view,
	image_properties_init,
	image_properties_mark,
	image_properties_destroy,
	NULL
};
#endif

/**
 * @code (image-properties path-to-image) @endcode
 * 
 * Creates an object using which you can access the metadata of the given image file.
 * The returned object supports \ref fn_get "get".
 *
 * The metadata can be accessed either by symbolic key or by numeric tag code,
 * as specified in the Microsoft documentation.
 *   
 *   - <a href="http://msdn.microsoft.com/en-us/library/ms534413(VS.85).aspx">Image property tag constants</a>
 *   - <a href="http://msdn.microsoft.com/en-us/library/ms534414(VS.85).aspx">Image property tag type constants</a>
 *
 * The symbolic names of the tags are derived from the constant declarations
 * as follows -
 *	-# A name of the form \c PropertyTagGpsDestDistRef becomes \c gps-dest-dist-ref
 *	   i.e. the \c PropertyTag prefix is dropped and the camel case is
 *	   converted into Scheme convention.
 *	-# For names with abbreviations in them such as \c PropertyTagJPEGQuality,
 *	   the lower case form preserves the abbreviation - 
 *	   so \c PropertyTagJPEGQuality becomes \c jpeg-quality 
 *	-# YCbCr is also converted into lower case as ycbcr -
 *     so \c PropertyTagThumbnailYCbCrCoefficients becomes \c thumbnail-ycbcr-coefficients
 *
 * The following extra properties are also exposed -
 * <table border="0" cellspacing="0">
 * <tr><td>\c 'size</td>	<td>@code {vector width-pixels height-pixels} @endcode</td></tr>
 * <tr><td>\c 'physical-size</td>	<td>@code {vector physical-width physical-height} @endcode</td></tr>
 * <tr><td>\c 'resolution</td>	<td>@code {vector hres vres} @endcode</td></tr>
 * <tr><td>\c 'path</td>	<td>@code "path/to/file.jpg" @endcode</td></tr>
 * </table>
 *
 *	@exception image-properties:gdiplus-error
 *  <table border="0" cellspacing="0" width="80em">
 *	<tr><td>\b Handler</td>		<td>@code (fn (resume 'image-properties:gdiplus-error) ...) @endcode </td></tr>
 *	<tr><td>\b When</td>		<td>Initializing Gdiplus failed when \c image-properties function is called. </td></tr>
 *	<tr><td>\b Consequence</td>	<td>Cannot use image-properties feature. </td></tr>
 *	<tr><td>\b Resume</td>		<td>Useless </td></tr>
 *	</table>
 *
 *	@exception image-properties:invalid-tag
 *  <table border="0" cellspacing="0" width="80em">
 *	<tr><td>\b Handler</td>		<td>@code (fn (resume 'image-properties:invalid-tag props code/tag) ...) @endcode </td></tr>
 *	<tr><td>\b When</td>		<td>A property with an invalid tag is requested. The tag can be numeric or symbolic.
 *								If the tag is not a number or a symbol, then this exception means
 *								that you have to pass a number or a symbol. \p props is the properties objects
 *								which raised the exception. (i.e. this exception is not raised by \c image-properties itself,
 *								but by the object it returns.)</td></tr>
 *	<tr><td>\b Consequence</td>	<td>Cannot retrieve property.</td></tr>
 *	<tr><td>\b Resume</td>		<td>By passing the expected result.</td></tr>
 *	</table>
 *	
 *	@exception error:string-expected
 *  <table border="0" cellspacing="0" width="80em">
 *	<tr><td>\b Handler</td>		<td>@code (fn (resume 'error:string-expected alt) ...) @endcode </td></tr>
 *	<tr><td>\b When</td>		<td>\c image-properties is called with a non-path value.</td></tr>
 *	<tr><td>\b Consequence</td>	<td>Cannot read image properties.</td></tr>
 *	<tr><td>\b Resume</td>		<td>By passing a valid string path to file.</td></tr>
 *	</table>
 *	
 *	@exception image-properties:file-error
 *  <table border="0" cellspacing="0" width="80em">
 *	<tr><td>\b Handler</td>		<td>@code (fn (resume 'image-properties:file-error path) ...) @endcode </td></tr>
 *	<tr><td>\b When</td>		<td>There is a problem opening the image file.</td></tr>
 *	<tr><td>\b Consequence</td>	<td>Properties cannot be read.</td></tr>
 *	<tr><td>\b Resume</td>		<td>By passing a path to a valid image.</td></tr>
 *	</table>
 *
 *	@exception image-properties:not-supported
 *  <table border="0" cellspacing="0" width="80em">
 *	<tr><td>\b Handler</td>		<td>@code (fn (resume 'image-properties:not-supported) ...) @endcode </td></tr>
 *	<tr><td>\b When</td>		<td>The \c image-properties feature is not available on this platform.
 *								Currently (r619) this feature is available only on Win32.</td></tr>
 *	</table>
 *
 * @par Sample session
 * @code
; Get properties of an image
> (define p (image-properties "somewhere/something.jpg"))

; Get size
> p.size
{vector 320 240}

; Get physical size
> p.physical-size
{vector 320.0 240.0}

; Get resolution
> p.resolution
{vector 96.0 96.0}

; Get file path
> p.path
somewhere/something.jpg

; Get thumbnail width (PropertyTagThumbnailWidth)
> (p 0x5013)
32

; Get thumbnail width symbolically (PropertyTagThumbnailWidth)
> p.thumbnail-width
32
> (p 'thumbnail-width)
32
> (get p 'thumbnail-width)
32

; If you want to get a full hashtable of the properties, you access the 
; 'all field as shown below. (Note: You shouldn't modify the contents of the
; hashtable.)
> p.all
{hashtable '(.....)}
 * @endcode
 */
muse_cell fn_image_properties( muse_env *env, void *context, muse_cell args )
{
#if MUSE_PLATFORM_WINDOWS
	return muse_add_recent_item( env, (muse_int)fn_image_properties, muse_mk_functional_object( env, &g_image_properties_type, args ) );
#else
	return muse_raise_error( env, _csymbol(L"image-properties:not-supported"), MUSE_NIL );
#endif
}

extern "C" void muse_define_image_properties( muse_env *env )
{
#if MUSE_PLATFORM_WINDOWS
	int sp = _spos();
	_define( _csymbol(L"image-properties"), muse_mk_nativefn( env, fn_image_properties, NULL ) );
	_unwind(sp);
#endif
}
