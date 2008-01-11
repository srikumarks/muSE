/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* ***** BEGIN LICENSE BLOCK *****
* Version: MPL 1.1/GPL 2.0/LGPL 2.1
*
* The contents of this file are subject to the Mozilla Public License Version
* 1.1 (the "License"); you may not use this file except in compliance with
* the License. You may obtain a copy of the License at
* http://www.mozilla.org/MPL/
*
* Software distributed under the License is distributed on an "AS IS" basis,
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
* for the specific language governing rights and limitations under the
* License.
*
* The Original Code is mozilla.org code.
*
* The Initial Developer of the Original Code is
* Netscape Communications Corporation.
* Portions created by the Initial Developer are Copyright (C) 1998
* the Initial Developer. All Rights Reserved.
*
* Contributor(s):
*
* Alternatively, the contents of this file may be used under the terms of
* either the GNU General Public License Version 2 or later (the "GPL"), or
* the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
* in which case the provisions of the GPL or the LGPL are applicable instead
* of those above. If you wish to allow use of your version of this file only
* under the terms of either the GPL or the LGPL, and not to allow others to
* use your version of this file under the terms of the MPL, indicate your
* decision by deleting the provisions above and replace them with the notice
* and other provisions required by the GPL or the LGPL. If you do not delete
* the provisions above, a recipient may use your version of this file under
* the terms of any one of the MPL, the GPL or the LGPL.
*
* ***** END LICENSE BLOCK ***** */

#include "stdafx.h"
#include <windows.h>
#include <windowsx.h>

#include "plugin.h"
#include "renderer.h"
#include "../src/muse_port.h"
#include <gl/gl.h>

#pragma comment(lib,"opengl32")



//////////////////////////////////////
//
// general initialization and shutdown
//
NPError NS_PluginInitialize()
{
	return NPERR_NO_ERROR;
}

void NS_PluginShutdown()
{
}

/////////////////////////////////////////////////////////////
//
// construction and destruction of our plugin instance object
//
nsPluginInstanceBase * NS_NewPluginInstance(nsPluginCreateData * aCreateDataStruct)
{
	if(!aCreateDataStruct)
		return NULL;

	nsPluginInstance * plugin = new nsPluginInstance(aCreateDataStruct->instance);

	// now is the time to tell Mozilla that we are windowless
	//NPN_SetValue(aCreateDataStruct->instance, NPPVpluginWindowBool, NULL);

	return plugin;
}

void NS_DestroyPluginInstance(nsPluginInstanceBase * aPlugin)
{
	if(aPlugin)
		delete (nsPluginInstance *)aPlugin;
}

FILE *fout = NULL;

////////////////////////////////////////
//
// nsPluginInstance class implementation
//
nsPluginInstance::nsPluginInstance(NPP aInstance) : nsPluginInstanceBase(),
mInstance(aInstance),
mInitialized(FALSE),
mBaseURL(NULL)
{
	mhWnd = NULL;
	fout = fopen("c:\\temp\\avmuse.txt", "wt");
}

nsPluginInstance::~nsPluginInstance()
{
}

static LRESULT CALLBACK PluginWinProc(HWND, UINT, WPARAM, LPARAM);

NPBool nsPluginInstance::init(NPWindow* aWindow)
{
	mhWnd = NULL;
	mhDC = NULL;
	init_display(aWindow);
	mInitialized = TRUE;
	return TRUE;
}

void nsPluginInstance::shut()
{
	destroy_display(NULL);
	mInitialized = FALSE;

	if ( mBaseURL ) {
		free(mBaseURL);
		mBaseURL = NULL;
	}
}

NPBool nsPluginInstance::init_display( NPWindow *aWindow )
{ 
	if(aWindow == NULL)
		return FALSE;

	//  if ( aWindow->type == NPWindowTypeWindow )
	//	  MessageBox( NULL, "NPWindowTypeWindow", "WindowType", MB_OK );
	//  else if ( aWindow->type == NPWindowTypeDrawable )
	//	  MessageBox( NULL, "NPWindowTypeDrawable", "WindowType", MB_OK );

	if ( aWindow->type == NPWindowTypeWindow ) {
		mhWnd = (HWND)aWindow->window;
		if(mhWnd == NULL)
			return FALSE;
	} else {
		mhDC = (HDC)aWindow->window;
		if ( mhDC == NULL )
			return FALSE;
	}

	fetchingURLWithNotification = false;

	// sMessageBox( NULL, "init_display", "msg", MB_OK );

	// subclass window so we can intercept window messages and
	// do our drawing to it
	if ( aWindow->type == NPWindowTypeWindow ) {
		lpOldProc = SubclassWindow(mhWnd, PluginWinProc);

		// associate window with our nsPluginInstance object so we can access 
		// it in the window procedure	
		lpOldProc = (WNDPROC)SetWindowLongPtr( mhWnd, GWLP_USERDATA, (LONG_PTR)this );

		// Message to initiate URL fetching.
		mFetchURLMessage = RegisterWindowMessage( "avmuse_fetch_url" );

		EnableWindow( mhWnd, TRUE );
		HWND wnd = SetFocus( mhWnd );
		if ( !wnd )
			MessageBox( NULL, "SetFocus error", "error", MB_OK );
	}

	mWindow = aWindow;

	renderer = Renderer::create(this);
	mouseX = mouseY = 0;

	return TRUE;
}

void nsPluginInstance::destroy_display( NPWindow *w )
{
	if ( mWindow ) {
		//		MessageBox( NULL, "destroy_display", "msg", MB_OK );
		// subclass it back
		delete renderer;
		renderer = NULL;

		if ( mWindow->type == NPWindowTypeWindow ) {
			SubclassWindow(mhWnd, lpOldProc);
		}

		mhWnd = NULL;
		mWindow = NULL;
		mhDC = NULL;
	}
}

NPError nsPluginInstance::SetWindow(NPWindow* aWindow)
{
	//	MessageBox( NULL, "SetWindow", "msg", MB_OK );
	if ( !mWindow ) {
		if ( init_display(aWindow) )
			return NPERR_NO_ERROR;
		else {
			if ( aWindow != mWindow ) 
				MessageBox( NULL, "Window pointer change not allowed!", "plugin", MB_OK );
			return NPERR_GENERIC_ERROR;
		}
	} else {
		if ( aWindow->type == NPWindowTypeWindow ) {
			if ( mhWnd != (HWND)aWindow->window )
				MessageBox( NULL, "Window change not allowed!", "plugin", MB_OK );
		} else {
			if ( mhDC != (HDC)aWindow->window ) 
				MessageBox( NULL, "HDC change not allowed!", "plugin", MB_OK );
		}

		return NPERR_NO_ERROR;
	}
}

NPBool nsPluginInstance::isInitialized()
{
	return mInitialized;
}

const char * nsPluginInstance::getVersion()
{
	return NPN_UserAgent(mInstance);
}

NPError nsPluginInstance::NewStream(NPMIMEType type, NPStream* stream, 
									NPBool seekable, uint16* stype)
{
	//	MessageBox( NULL, "NewStream", "msg", MB_OK );
	if ( !mWindow )
		return NPERR_GENERIC_ERROR;

	setBaseURL( stream->url );
	renderer->write_MIMEType(type);

	return NPERR_NO_ERROR;
}

void nsPluginInstance::setBaseURL( const char *url ) {
	if ( !mBaseURL ) {
		// This is the stream of the document itself.
		// Keep the base URL so that relative paths
		// can be resolved.
		int len = strlen(url);
		mBaseURL = (char*)calloc( 1, len+1 );
		memcpy( mBaseURL, url, len );

		// Remove the trailing file path.
		char *p = mBaseURL + len - 1;
		while ( p > mBaseURL && p[0] != '/' )
			--p;
		if ( p[0] == '/' )
			p[1] = 0;
	}
}

int32 nsPluginInstance::Write(NPStream *stream, int32 offset, int32 len, void *buffer) {

	return (int32)renderer->write_data( buffer, len );
}

NPError nsPluginInstance::DestroyStream(NPStream *stream, NPError reason) { 
	if ( !fetchingURLWithNotification )
		renderer->write_complete(reason == NPRES_DONE);
	return NPERR_NO_ERROR; 
}

void nsPluginInstance::URLNotify(const char* url, NPReason reason, void* notifyData) {
	renderer->write_complete( reason == NPRES_DONE );
}

static int specialCode( WPARAM wParam ) {
	int special = -1;
	switch ( wParam ) {
	case VK_LEFT:		special = 100; break;
	case VK_UP:			special = 101; break;
	case VK_RIGHT:		special = 102; break;
	case VK_DOWN:		special = 103; break;
	case VK_NUMPAD8:	special = 104; break;
	case VK_NUMPAD2:	special = 105; break;
	case VK_HOME:		special = 106; break;
	case VK_END:		special = 107; break;
	case VK_INSERT:		special = 108; break;
	default:
		if ( wParam >= VK_F1 && wParam <= VK_F12 ) {
			special = (int)(1 + wParam - VK_F1);
		}
	}
	return special;
}


LRESULT nsPluginInstance::Proc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
	switch (msg) {
	case WM_SETFOCUS:
		fprintf(fout,"Proc(WM_SETFOCUS,0x%x,0x%x)\n", wParam, lParam);
			SetFocus(mhWnd);
			break;
	case WM_SIZE:
		fprintf(fout,"Proc(WM_SIZE,0x%x,0x%x)\n", wParam, lParam);
		{
			int width = LOWORD(lParam);
			int height = HIWORD(lParam);

			switch ( wParam ) {
			case SIZE_MAXIMIZED:
			case SIZE_RESTORED:
				renderer->resize( width, height, false, true );
				return 1;
			case SIZE_MAXSHOW:
				renderer->resize( width, height, false, false );
				return 1;
			case SIZE_MINIMIZED:
				renderer->resize( 0, 0, true, false );
				return 1;
			default:;
			}		
		}
		return 1;
		break;
	case WM_ERASEBKGND:
		fprintf(fout,"Proc(WM_ERASEBKGND,0x%x,0x%x)\n", wParam, lParam);
		break;
	case WM_CLOSE:
		fprintf(fout,"Proc(WM_CLOSE,0x%x,0x%x)\n", wParam, lParam);
		delete renderer;
		renderer = NULL;
		return 1;
	case WM_PAINT:
		return 1;
	case WM_KEYDOWN:
	case WM_KEYUP:
		fprintf(fout,"Proc(%s,0x%x,0x%x)\n", (msg == WM_KEYDOWN) ? "WM_KEYDOWN" : "WM_KEYUP", wParam, lParam);
		{
			int sk = specialCode(wParam);
			if ( sk > 0 ) {
				fprintf( fout, "special key %d\n", sk );
				if ( msg == WM_KEYDOWN )
					renderer->special_key_down( sk, mouseX, mouseY );
				else
					renderer->special_key_up( sk, mouseX, mouseY );
			}

// Looks like the following block isn't necessary.
// The WM_CHAR message gets sent twice if we do this as well.
// Funny that I needed to do the TranslateMessage() a while back
// in order to get the WM_CHAR messages. Now the WM_CHAR is 
// coming through automatically. Maybe something changed in
// Firefox?
#if 0
			MSG m;
			memset(&m,0,sizeof(m));
			m.hwnd = mhWnd;
			m.lParam = lParam;
			m.wParam = wParam;
			m.message = msg;
			/*return*/ TranslateMessage( &m );
#endif
			break;
		}
	case WM_CHAR:
		fprintf(fout,"Proc(WM_CHAR,0x%x,0x%x)\n", wParam, lParam);
		if ( lParam & (1 << 31) )
			renderer->key_up( (unsigned char)wParam, mouseX, mouseY );
		else
			renderer->key_down( (unsigned char)wParam, mouseX, mouseY );
//		return 1;
		break;
	case WM_MOUSEMOVE:
		fprintf(fout,"Proc(WM_MOUSEMOVE,0x%x,0x%x)\n", wParam, lParam);
		mouseX = GET_X_LPARAM(lParam);
		mouseY = GET_Y_LPARAM(lParam);
		if ( wParam & (MK_LBUTTON | MK_MBUTTON | MK_RBUTTON) ) {
			renderer->mouse_drag( mouseX, mouseY  );
		} else {
			renderer->mouse_motion( mouseX, mouseY );
		}
		//return 1;
		break;
	case WM_LBUTTONDOWN:
	case WM_MBUTTONDOWN:
	case WM_RBUTTONDOWN:
		{
			fprintf(fout,"Proc(WM_[LMR]BUTTONDOWN,0x%x,0x%x)\n", wParam, lParam);
			int button = (msg == WM_LBUTTONDOWN) ? 0 : (msg == WM_MBUTTONDOWN ? 1 : 2);
			mouseX = GET_X_LPARAM(lParam);
			mouseY = GET_Y_LPARAM(lParam);
			renderer->mouse_button( button, 0, mouseX, mouseY );
		//return 1;
		break;
		}
	case WM_LBUTTONUP:
	case WM_MBUTTONUP:
	case WM_RBUTTONUP:
		{
			fprintf(fout,"Proc(WM_[LMR]BUTTONUP,0x%x,0x%x)\n", wParam, lParam);
			int button = (msg == WM_LBUTTONUP) ? 0 : (msg == WM_MBUTTONUP ? 1 : 2);
			mouseX = GET_X_LPARAM(lParam);
			mouseY = GET_Y_LPARAM(lParam);
			renderer->mouse_button( button, 1, mouseX, mouseY );
		//return 1;
		break;
		}
	case WM_MOUSEACTIVATE:
		fprintf(fout,"Proc(WM_MOUSEACTIVATE,0x%x,0x%x)\n", wParam, lParam);
		SetFocus(mhWnd);
		return MA_ACTIVATE;
	default:
		//fprintf( fout, "0x%x (0x%x,0x%x,0x%x)\n", hWnd, msg, wParam, lParam );
		if ( msg == mFetchURLMessage ) {
			fprintf(fout,"Proc(avmuse_fetch_url,0x%x,0x%x)\n", wParam, lParam);
			fetchingURLWithNotification = true;
			NPError err = NPN_GetURLNotify( mInstance, mURLToFetch, NULL, this );
			if ( err != NPERR_NO_ERROR )
				renderer->write_complete(false);
			return 1;
		}
		fprintf(fout,"Proc(0x%04x,0x%x,0x%x)\n", msg, wParam, lParam);
		break;
	}

	//return CallWindowProc( lpOldProc, mhWnd, msg, wParam, lParam );
	return DefWindowProc( mhWnd, msg, wParam, lParam );
}

static LRESULT CALLBACK PluginWinProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	nsPluginInstance *p = (nsPluginInstance*)GetWindowLongPtr(hWnd,GWLP_USERDATA);
	if ( p )
		return p->Proc( hWnd, msg, wParam, lParam );
	else
		return TRUE;
}


// Plugin implementation
HWND nsPluginInstance::get_window() const {
	if ( mWindow->type == NPWindowTypeWindow )
		return (HWND)mWindow->window;
	else
		return mhWnd;
}

HDC nsPluginInstance::get_dc() const {
	if ( mWindow->type == NPWindowTypeDrawable )
		return (HDC)mWindow->window;
	else
		return mhDC;
}

char *nsPluginInstance::relative_url( const char *rel ) const {
	int len = strlen(rel);
	if ( !mBaseURL ) {
		char *absurl = (char*)calloc(1, len+1);
		memcpy( absurl, rel, len );
		return absurl;
	} else {
		int baseLen = strlen(mBaseURL);
		char *absurl = (char*)calloc( 1, baseLen + len + 1 );
		memcpy( absurl, mBaseURL, baseLen );
		memcpy( absurl + baseLen, rel, len+1 );
		return absurl;
	}
}

bool nsPluginInstance::fetch_url( const char *url8, const char *target8 ) {
	if ( target8 ) {
		// If we're loading to another target, we can load from any thread.
		// Also we don't need to get the data as a stream, so we just call
		// GetURL and be done withit.
		NPError err = NPN_GetURL( mInstance, url8, target8 );
		return err == NPERR_NO_ERROR;
	} else {
		strcpy( mURLToFetch, url8 );
		PostMessage( mhWnd, mFetchURLMessage, 0, 0 );
		return true;
	}
}

void nsPluginInstance::get_window_dimensions( int &width, int &height ) const {
	width = mWindow->width;
	height = mWindow->height;
}
