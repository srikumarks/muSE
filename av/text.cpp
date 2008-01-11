
#include "avmuse.h"
#include "avapi.h"

struct font_t
{
	HFONT font;
	LOGFONT desc;
	GLuint base;
	GLuint numChars;
};

extern "C" muse_cell fn_format( muse_env *env, void *context, muse_cell args );

muse_cell font_render_text( muse_env *env, font_t *f, muse_cell args ) {
	if ( args ) {
		muse_cell text = fn_format( env, NULL, args );
		int textlen = 0;
		const muse_char *textc = muse_text_contents( env, text, &textlen );
		glListBase(f->base);
		glCallLists( textlen, GL_UNSIGNED_SHORT, textc );
		return MUSE_NIL;
	} else {
		if ( muse_doing_gc(env) ) {
			if ( f->base ) glDeleteLists( f->base, f->numChars );
			DeleteObject(f->font);
			delete f;
		}

		return MUSE_NIL;
	}
}

struct dc_t { void *dc; };

/**
 * (font3d ...)
 *
 * Loads a font for rendering ASCII characters in 3D. The
 * result is a function you can use like \c print to place
 * your 3D text into the scene. You can apply normal geometric
 * transformations on the placed text.
 */
muse_cell fn_font3d( muse_env *env, dc_t *dc, muse_cell args ) {
	if ( args ) {
		font_t *f = new font_t;

		// Frozen with defaults for the moment.
		f->desc.lfHeight = -12;
		f->desc.lfWidth = 12;
		f->desc.lfEscapement = 0;
		f->desc.lfOrientation = 0;
		f->desc.lfWeight = FW_NORMAL;
		f->desc.lfItalic = 0;
		f->desc.lfUnderline = 0;
		f->desc.lfStrikeOut = 0;
		f->desc.lfCharSet = ANSI_CHARSET;
		f->desc.lfOutPrecision = OUT_DEFAULT_PRECIS;
		f->desc.lfClipPrecision = CLIP_DEFAULT_PRECIS;
		f->desc.lfQuality = DEFAULT_QUALITY;
		f->desc.lfPitchAndFamily = DEFAULT_PITCH | FF_ROMAN;
		f->desc.lfFaceName[0] = 0;

		f->font = CreateFontIndirect( &(f->desc) );
		if (f->font == NULL) {
			delete f;
			return MUSE_NIL;
		}

		f->numChars = 256;
		f->base = glGenLists(f->numChars);

		HDC hdc = (HDC)(dc->dc);
		SelectObject( hdc, f->font );
		wglUseFontOutlines( hdc, 0, 256, f->base, 0.0f, 1.0f, WGL_FONT_POLYGONS, NULL );
		return muse_mk_destructor( env, (muse_nativefn_t)font_render_text, f );
	} else {
		if ( muse_doing_gc(env) ) 
			delete dc;
		return MUSE_NIL;
	}
}

void register_font_support( muse_env *env, void *dc ) {
	SP sp(env);
	dc_t *dcp = new dc_t;
	dcp->dc = dc;
	_define( _csymbol(L"font3d"), _mk_destructor((muse_nativefn_t)fn_font3d,dcp) );
}