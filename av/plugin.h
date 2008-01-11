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

#ifndef __PLUGIN_H__
#define __PLUGIN_H__

#include "pluginbase.h"
#include "avapi.h"
#include "thread.h"
#include "renderer.h"

class nsPluginInstance : public nsPluginInstanceBase, public RenderContext
{
public:
  nsPluginInstance(NPP aInstance);
  ~nsPluginInstance();

  NPBool init(NPWindow* aWindow);
  void shut();
  NPBool isInitialized();

  void Pop(const char *msg) { MessageBox( NULL, msg, "POP", MB_OK ); }
  virtual NPError SetWindow(NPWindow* pNPWindow);
  virtual NPError NewStream(NPMIMEType type, NPStream* stream, 
                            NPBool seekable, uint16* stype);
  virtual NPError DestroyStream(NPStream *stream, NPError reason);
  virtual void    StreamAsFile(NPStream* stream, const char* fname) { return; }
  virtual int32   WriteReady(NPStream *stream)                      { return 0x0fffffff; }
  virtual int32   Write(NPStream *stream, int32 offset, 
                        int32 len, void *buffer);
  virtual void    Print(NPPrint* printInfo)                         { return; }
  virtual uint16  HandleEvent(void* event) { Pop("HandleEvent"); return 0; }
  virtual void    URLNotify(const char* url, NPReason reason, 
                            void* notifyData);
  virtual NPError GetValue(NPPVariable variable, void *value)       { 
	  return NPERR_NO_ERROR; 
  }
  virtual NPError SetValue(NPNVariable variable, void *value)       { 
	  return NPERR_NO_ERROR; 
  }
 
  // locals
  const char * getVersion();

public:
  NPP mInstance;
  NPBool mInitialized;

public:
  NPBool init_display( NPWindow *w );
  void destroy_display( NPWindow *w );

  NPWindow *mWindow;
  HWND mhWnd;
  HDC mhDC;
  WNDPROC lpOldProc;
  UINT mFetchURLMessage;
  char *mBaseURL;
  char mURLToFetch[1024];

  Renderer *renderer;

public:

	LRESULT Proc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam );
	int mouseX, mouseY;

private: // RenderContext implementation
	HWND get_window() const;
	HDC get_dc() const;
	char *relative_url( const char *rel ) const;
	bool fetch_url( const char *url8, const char *target8 );
	void get_window_dimensions( int &width, int &height ) const;
	bool fetchingURLWithNotification;

private:
	void setBaseURL( const char *url );
};

#endif // __PLUGIN_H__
