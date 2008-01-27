
#include "avmuse.h"
#include "qtserver.h"
#include <iostream>
#include <string>
#include "glhelpers.h"

#pragma comment(lib, "qtmlclient.lib")

using namespace std;

typedef basic_string<unsigned short> unicode_string;

static void msgbox( const wchar_t *msg ) {
	return;
	int choice = MessageBoxW( NULL, msg, L"movie", MB_ABORTRETRYIGNORE );
	switch ( choice ) {
		case IDABORT: exit(0);
		case IDRETRY: DebugBreak(); break;
		case IDIGNORE:;
	}
}

unicode_string narrow( const wstring &ws ) {
	unicode_string s;
	s.reserve( ws.length() + 1 );

	for ( size_t i = 0, N = ws.length(); i < N; ++i ) {
		s.append( 1, (unsigned short)ws[i] );
	}

	return s;
}

int pof2ge( int n ) {
	int p = 1;
	while ( p < n ) 
		p <<= 1;
	return p;
}

int ulimit( int n, int lim ) {
	return (n <= lim) ? n : lim;
}

class QuickTimeInit : public TaskOnce {
public:
	void runOnce( Tasker * ) {
	#ifdef _WIN32
		InitializeQTML(0);
	#endif
		EnterMovies();
		cout << "QuickTime initialized.\n";
		//MessageBox( NULL, L"QuickTime initialized!", L"muSE says..", MB_OK );
	}
};

class QuickTimeDestroy : public TaskOnce {
public:
	void runOnce( Tasker * ) {
		ExitMovies();
		#ifdef _WIN32
		TerminateQTML();
		#endif
		cout << "QuickTime de-initialized.\n";
	}
};

class QuickTimeIdler : public Task {
public:
	void run( Tasker * ) {
		MoviesTask( NULL, 10 );
#ifdef _WIN32
		Sleep(5);
#else
		usleep(5000);
#endif
	}
};

class qtserver_impl : public qtserver, public WithGLEW {
	Tasker *qt;

public:

	muse *av;

	qtserver_impl( void *context, muse *av ) : WithGLEW( (GLEWContext*)context ) {
		this->av = av;
		qt = Tasker::create( new QuickTimeInit(),
							 new QuickTimeDestroy(),
							 new QuickTimeIdler() );
	}

	~qtserver_impl() {
		delete qt;
		qt = NULL;
	}

	void stop() {
		qt->stop();
	}

	void needsUpdate() {
		av->needsUpdate();
	}

	QTImage *imageFromFile( const wchar_t *path );
	QTMovie *movieFromFile( const wchar_t *path );

	/// Appends the given task to the task queue.
	void submit( Task *task ) {
		qt->submit(task);
	}

	void submitUrgent( Task *task ) {
		qt->submitUrgent(task);
	}

	/// Invokes the given method of the given task.
	void submit( Task *task, Task::Fn fn ) {
		qt->submit( task, fn );
	}

	void submitUrgent( Task *task, Task::Fn fn ) {
		qt->submit( task, fn );
	}
};

qtserver *qtserver::start( void *context, muse *av ) {
	return new qtserver_impl(context, av);
}

void getImageSizeInfo( ImageInfo *data, const Rect &bounds ) {
	data->naturalWidth = bounds.right - bounds.left;
	data->naturalHeight = bounds.bottom - bounds.top;
	data->aspectRatio = float(data->naturalWidth) / float(data->naturalHeight);
	data->texWidth = ulimit( pof2ge(data->naturalWidth), data->maxTexWidth );
	data->texHeight = ulimit( pof2ge(data->naturalHeight), data->maxTexHeight );	
}

//////////////// ============ QTImage implementation =========== ////////////////

class QTImageFileData : public QTImage {
public:
	qtserver_impl *server;
	unicode_string path;
	bool loading;
	unsigned int *pixels;
	unsigned int rowBytes;
	
	void init( qtserver_impl *server, const wchar_t *wpath ) {
		this->server = server;
		path = narrow(wpath);
		naturalWidth = naturalHeight = texWidth = texHeight = 0;
		maxTexWidth = QTSERVER_MAX_TEX_WIDTH;
		maxTexHeight = QTSERVER_MAX_TEX_HEIGHT;
		aspectRatio = 0;
		infoReady = loaded = loading = false;
		pixels = NULL;
		rowBytes = 0;
		lastAccessTime_us = server->av->time_us();
	}
};

class QTImageFileLoader : public Task {
public:
	QTImageFileLoader( QTImageFileData *_data ) : data(_data) {
		dataRef = NULL;
		dataRefType = 0;
	}

	~QTImageFileLoader() {
		msgbox(L"Image loader destructor");
	}

	void getDataRef() {
		CFStringRef pathStr = NULL;
		pathStr = CFStringCreateWithCharacters( NULL, data->path.c_str(), data->path.length() );
		OSErr err = QTNewDataReferenceFromURLCFString( pathStr, 0, &dataRef, &dataRefType );
		if ( err == noErr ) {
			// It is a URL
		} else {
			// It can be a normal path.
			ensure( QTNewDataReferenceFromFullPathCFString(pathStr,kQTNativeDefaultPathStyle,0,&dataRef,&dataRefType));
		}
//X		CFRelease(pathStr);
	}

	void getInfo( Tasker *tasker ) {
		getDataRef();

		if ( !data->infoReady ) {
			GraphicsImportComponent importer = NULL;
			ensure( GetGraphicsImporterForDataRef(dataRef,dataRefType,&importer) );
			DisposeHandle(dataRef);
			dataRef = NULL;
			dataRefType = 0;
			
			Rect bounds = {0,0,0,0};
			ensure( GraphicsImportGetNaturalBounds( importer, &bounds ) );

			getImageSizeInfo( data, bounds );

			ImageDescriptionHandle imdesc = NULL;
			ensure( GraphicsImportGetImageDescription( importer, &imdesc ) );
			data->hasAlpha = ((*imdesc)->depth == 32);

			CloseComponent( importer );
			data->infoReady = true;
		}
	}

	void load( Tasker *tasker ) {
		getDataRef();
		GraphicsImportComponent importer = NULL;
		ensure( GetGraphicsImporterForDataRef(dataRef,dataRefType,&importer) );

		Rect bounds = { 0, 0, data->texWidth, data->texHeight };
		GWorldPtr gworld = NULL;
		unsigned int rowBytes = data->texWidth * 4;
		unsigned int *pixels = new unsigned int [data->texWidth * data->texHeight];
		ensure( NewGWorldFromPtr( &gworld, k32BGRAPixelFormat, &bounds, NULL, NULL, 0, (Ptr)pixels, rowBytes ) );
		ensure( GraphicsImportSetGWorld( importer, gworld, NULL ) );

		MatrixRecord m;
		SetIdentityMatrix(&m);
		ScaleMatrix( &m, 
					 X2Fix( float(data->texWidth) / data->naturalWidth ),
					 X2Fix( float(data->texHeight) / data->naturalHeight ),
					 0, 
					 0 );
//		ScaleMatrix( &m,
//					 X2Fix(1.0),
//					 X2Fix(-1.0),
//					 0,
//					 X2Fix( data->texHeight / 2 ) );
		ensure( GraphicsImportSetMatrix( importer, &m ) );

		ensure( GraphicsImportDraw( importer ) );
		DisposeGWorld( gworld );
		CloseComponent( importer );

		data->rowBytes = rowBytes;
		data->pixels = pixels;
		data->loading = false;
		data->loaded = true;
		data->server->needsUpdate();
	}

	void run( Tasker *tasker ) {
		getInfo(tasker);
		load(tasker);
	}

private:
	QTImageFileData *data;
	Handle dataRef;
	OSType dataRefType;

	void ensure( OSErr e ) {
		if ( e != noErr ) {

//			delete this;
// Shouldn't delete this object 'cos a reference to it is held by the 
// QTImageFileData object.

			throw e;
		}
	}
};

static void bind_image( void *context, GLuint tex, int texUnit  ) {
	WithGLEW glewGetContext((GLEWContext*)context);
	glActiveTexture( GL_TEXTURE0 + texUnit );
	glEnable( GL_TEXTURE_2D );
	glBindTexture(GL_TEXTURE_2D, tex);
	glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
}

static void unbind_image( void *context, int texUnit ) {
	WithGLEW glewGetContext((GLEWContext*)context);
	glActiveTexture( GL_TEXTURE0 + texUnit );
	glDisable( GL_TEXTURE_2D );
}

static void draw_image( void *context, int texUnit, GLuint tex, float aspectRatio, float alpha, bool invert ) {
	bind_image(context, tex, texUnit);
	{
		Model model;
		model.scale( aspectRatio );

		glPushAttrib( GL_CURRENT_BIT );
		glColor4f(1,1,1,alpha);
		glBegin( GL_QUADS );

		if ( invert ) {
			glTexCoord2f(0,1);
			glVertex3f(-1,-1,0);
			glTexCoord2f(1,1);
			glVertex3f(1,-1,0);
			glTexCoord2f(1,0);
			glVertex3f(1,1,0);
			glTexCoord2f(0,0);
			glVertex3f(-1,1,0);
		} else {
			glTexCoord2f(0,0);
			glVertex3f(-1,-1,0);
			glTexCoord2f(1,0);
			glVertex3f(1,-1,0);
			glTexCoord2f(1,1);
			glVertex3f(1,1,0);
			glTexCoord2f(0,1);
			glVertex3f(-1,1,0);
		}
		
		glEnd();		
		glPopAttrib();
	}
	unbind_image( context, texUnit );
}

void draw_stub( float alpha ) {
	return;

	glMatrixMode( GL_MODELVIEW );
	glPushMatrix();
	glScalef( 4.0/3.0, 1.0, 1.0 );
	glPushAttrib( GL_CURRENT_BIT );
	glBegin( GL_QUADS );
	{
		glColor4f(0.5,0.5,0.5, alpha * 0.15 );
		glVertex3f(0,0,0);
		glVertex3f(1,0,0);
		glVertex3f(1,1,0);
		glVertex3f(0,1,0);
	}
	glEnd();		
	glPopAttrib();
	glPopMatrix();
}


class QTImageFile : public QTImageFileData, public WithGLEW {
public:
	QTImageFileLoader *loader;
	GLuint tex;

	QTImageFile( void *context, qtserver_impl *server, const wchar_t *wpath )
		: WithGLEW((GLEWContext*)context)
	{
		init(server, wpath);
		tex = 0;
		loader = new QTImageFileLoader( this );
		load();
	}

	~QTImageFile() {
		unload();
		delete loader;
	}

	void load() {
		if ( !loaded ) {
			loading = true;
			server->submit( loader );//, (Task::Fn)&QTImageFileLoader::load );
		}
	}

	void unload() {
		if ( loaded ) {
			if ( pixels ) {
				delete [] pixels;
				pixels = NULL;
				rowBytes = 0;
			}
			
			if ( tex ) {
				glDeleteTextures( 1, &tex );
				tex = 0;
			}
			
			loading = false;
			loaded = false;
		}
	}

	void commit() {
		glGenTextures( 1, &tex );
		glBindTexture( GL_TEXTURE_2D, tex );
		glTexParameteri( GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE );
		glPixelStorei( GL_UNPACK_ROW_LENGTH, texWidth );
		#ifdef _WIN32
		GLuint format = GL_BGRA_EXT;
		#else
		GLuint format = GL_BGRA;
		#endif
		GLuint components = hasAlpha ? GL_RGBA : GL_RGB;
		glTexImage2D( GL_TEXTURE_2D, 0, components, texWidth, texHeight, 0, format, GL_UNSIGNED_BYTE, pixels );
		glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
		glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
	}

	bool prepare() {
		if ( !loaded ) return false;
		if ( !tex ) commit();
		return true;
	}

	bool bind( int texUnit ) {
		if ( !prepare() ) return false;

		bind_image(glewGetContext(), tex, texUnit);
		return true;
	}

	void unbind( int texUnit ) {
		unbind_image(glewGetContext(),texUnit);
	}

	void draw( int texUnit, float alpha ) {
		lastAccessTime_us = server->av->time_us();
		if ( !prepare() ) {
			draw_stub(alpha);
			return;
		}

		draw_image( glewGetContext(), texUnit, tex, aspectRatio, alpha, true );
	}
};

QTImage *qtserver_impl::imageFromFile( const wchar_t *path ) {
	return new QTImageFile(glewGetContext(), this,path);
}


//////////////// ============ QTMovie implementation =========== ////////////////

class QTMovieData : public QTMovie {
public:
	qtserver_impl *server;
	unicode_string path;
	bool loading;
	unsigned int *pixels;
	unsigned int rowBytes;	
	Movie movie;
	bool frameReady; ///< When true, means frame referred to by frameIx can be dumped to opengl texture.
	int frameIx;	///< Can be 0 or 1 to indicate which of the two frames in the double buffering scheme is ready.
	int activeFrameIx; ///< The frame that the movie is currently playing into.

	virtual void unloaded() = 0;
};

static OSErr s_updateMovieTex( Movie m, long refCon );

class QTMovieDataLoader : public Task {
public:
	QTMovieDataLoader( QTMovieData *_data ) : data(_data) {
		dataRef = NULL;
		dataRefType = 0;
	}
	
	~QTMovieDataLoader() {
		DisposeHandle(dataRef);
	}
	
	void getDataRef() {
		CFStringRef pathStr = NULL;
		pathStr = CFStringCreateWithCharacters( NULL, data->path.c_str(), data->path.length() );
		OSErr err = QTNewDataReferenceFromURLCFString( pathStr, 0, &dataRef, &dataRefType );
		if ( err == noErr ) {
			// It is a URL
		} else {
			// It can be a normal path.
			ensure( "DataReferenceFromFile", QTNewDataReferenceFromFullPathCFString(pathStr,kQTNativeDefaultPathStyle,0,&dataRef,&dataRefType));
		}
//X		CFRelease(pathStr);	
	}

	static OSErr s_frameReadyCB( Movie m, long refCon ) {
		QTMovieDataLoader *self = (QTMovieDataLoader*)refCon;
		return self->frameReadyCB(m);
	}
	
	OSErr frameReadyCB( Movie m ) {
		if ( data->frameReady ) {
			// The prevously generated frame (frameIx) has not been
			// consumed yet. So continue to render to the same box
			// as before.
		} else {
			// Copy to the other box.
			int size = data->texWidth * data->texHeight;
			memcpy( data->pixels + size, data->pixels, size * 4 );
			data->frameReady = true;
			data->server->needsUpdate();
		}
		
		return noErr;
	}
	
	static void s_preprerollComplete(Movie m, OSErr err, void *refCon) {
		QTMovieDataLoader *self = (QTMovieDataLoader*)refCon;
		self->preprerollComplete(m, err);
	}
	
	void preprerollComplete(Movie m, OSErr err) {
		if ( err == noErr ) {
			data->loading = false;
			data->loaded = true;
			data->playing = false;
			data->server->needsUpdate();
		} else {
			fprintf( stderr, "QuickTime error %d opening movie '%ls'\n", err, data->path.c_str() );
		}
	}
	
	void run( Tasker *tasker ) {
		getDataRef();
		short resid = movieInDataForkResID;
		ensure( "OpenMovie", NewMovieFromDataRef(&(data->movie),newMovieActive,&resid,dataRef,dataRefType) );
		
		Rect bounds = {0,0,0,0};
		GetMovieBox(data->movie,&bounds);
		
		getImageSizeInfo( data, bounds );
		
		data->hasAlpha = false;
		data->infoReady = true;
		
		if ( data->naturalWidth * data->naturalHeight != 0 ) {
			GWorldPtr gworld = NULL;
			unsigned int rowBytes = data->texWidth * 4;
			unsigned int *pixels = new unsigned int [2 * data->texWidth * data->texHeight];
			Rect gworldBounds = { 0, 0, data->texHeight, data->texWidth };
				
			ensure( "NewGWorld", NewGWorldFromPtr( &gworld, k32BGRAPixelFormat, &gworldBounds, NULL, NULL, 0, (Ptr)pixels, rowBytes ) );
			SetMovieGWorld( data->movie, gworld, NULL );
			data->rowBytes = rowBytes;
			data->pixels = pixels;
			data->timescale = GetMovieTimeScale(data->movie);
			data->duration = GetMovieDuration(data->movie);

			Rect box = { 0, 0, data->texHeight, data->texWidth };
			SetMovieBox(data->movie,&box);
			SetMovieRate(data->movie,X2Fix(0));
			SetMovieActive(data->movie,TRUE);
			SetMovieDrawingCompleteProc( data->movie, movieDrawingCallWhenChanged, s_frameReadyCB, (long)this );
			
			data->frameReady = false;
			data->frameIx = 0;
			data->activeFrameIx = 0;
		}

		// This will apply to audio-only movies as well as for audio-visual movies.
		{
			Fixed prefRate = GetMoviePreferredRate(data->movie);
			ensure( "PrePreroll", PrePrerollMovie( data->movie, 0, prefRate, s_preprerollComplete, this ) );
		}
	}

	void unload( Tasker *tasker ) {
		GWorldPtr gworld = NULL;
		GetMovieGWorld(data->movie, &gworld, NULL);
		SetMovieGWorld(data->movie,NULL,NULL);
		DisposeGWorld(gworld);
		data->loading = data->loaded = false;
		delete [] data->pixels;
		data->pixels = NULL;
		data->rowBytes = 0;
		data->unloaded();		
	}
	
private:
	QTMovieData *data;
	Handle dataRef;
	OSType dataRefType;
	
	void ensure( const char *operation, OSErr e ) {
		if ( e != noErr ) {
			fprintf( stderr, "%s failed with error %d\n", operation, e );

//			delete this;
// Shouldn't delete this object 'cos a reference to it is held by the 
// QTMovieData object.

			throw e;
		}
	}
};

class QTMoviePreroller : public TaskOnce {
public:
	QTMoviePreroller( qtserver_impl *server, Movie m, TimeValue t, Fixed r ) : movie(m), time(t), rate(r) {
		server->submit( this );
	}
	
	void runOnce( Tasker *tasker ) {
		PrerollMovie( movie, time, rate );
	}
private:
	Movie movie;
	TimeValue time;
	Fixed rate;
};

class QTMovieUnloader : public TaskOnce {
public:
	QTMovieUnloader( QTMovieDataLoader *_loader ) 
		: loader(_loader) 
	{}

	~QTMovieUnloader() {}

	void runOnce( Tasker *tasker ) {
		loader->unload(tasker);
	}
private:
	QTMovieDataLoader *loader;
};

class QTMovieFile : public QTMovieData, public WithGLEW {
public:
	QTMovieDataLoader *loader;
	GLuint tex;
	Box *unloadNotification;
	
	QTMovieFile( void *context, qtserver_impl *server, const wchar_t *wpath ) 
		: WithGLEW( (GLEWContext*)context )
	{
		this->server = server;
		path = narrow(wpath);
		naturalWidth = naturalHeight = texWidth = texHeight = 0;
		maxTexWidth = QTSERVER_MAX_TEX_WIDTH;
		maxTexHeight = QTSERVER_MAX_TEX_HEIGHT;
		aspectRatio = 0;
		infoReady = loaded = loading = false;
		pixels = NULL;
		rowBytes = 0;
		tex = 0;
		movie = NULL;
		loader = new QTMovieDataLoader(this);
		server->submit(loader);
	}
	
	~QTMovieFile() {
		SetMovieActive(movie,FALSE);
		unloadNotification = Box::emptyBox();
		server->submit( new QTMovieUnloader(loader) );
		unloadNotification->get();
		delete loader;
		delete unloadNotification;
		if ( tex ) {
			glDeleteTextures( 1, &tex );
			tex = 0;
		}
	}
	
	void unloaded() {
		unloadNotification->put((void*)1);
	}

	void prepare( double t, double rate ) {
		new QTMoviePreroller( server, movie, (TimeValue)(t * timescale + 0.5), X2Fix(rate) );
	}

	void play() {
		if ( !playing )
			SetMovieRate(movie,X2Fix(1.0));
		playing = true;		
	}
	
	void pause() {
		SetMovieRate(movie,X2Fix(0));
		playing = false;
	}
	
	void seek( time_t time ) {
		SetMovieTimeValue(movie,time);
	}
	
	time_t time() {
		return GetMovieTime(movie,NULL);
	}
	
	void seek_secs( double t ) {
		seek( (time_t)(t * timescale + 0.5) );
	}
	
	double time_secs() {
		return time() / (double)timescale;
	}
	
	double volume() {
		return Fix2X( ((Fixed)GetMovieVolume(movie)) << 8 );
	}
	
	void setVolume( double v ) {
		SetMovieVolume( movie, X2Fix(v) >> 8 );
	}

	double rate() {
		return Fix2X(GetMovieRate(movie));
	}
	
	void setRate( double rate ) {
		SetMovieRate(movie,X2Fix(rate));
	}
	
	double balance() {
		Float32 b = 0;
		GetMovieAudioBalance(movie, &b, 0);
		return b;
	}
	
	void setBalance( double b ) {
		SetMovieAudioBalance(movie, (Float32)b, 0);
	}

	double duration_secs() {
		return duration / (double)timescale;
	}

	void unbind(int texUnit) {
		unbind_image( glewGetContext(), texUnit );
	}
	
	void commit() {
		glGenTextures( 1, &tex );
		glBindTexture( GL_TEXTURE_2D, tex );
		glTexParameteri( GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE );
		glPixelStorei( GL_UNPACK_ROW_LENGTH, texWidth );
		#ifdef _WIN32
		GLuint format = GL_BGRA_EXT;
		#else
		GLuint format = GL_BGRA;
		#endif
		GLuint components = hasAlpha ? GL_RGBA : GL_RGB;
		glTexImage2D( GL_TEXTURE_2D, 0, components, texWidth, texHeight, 0, format, GL_UNSIGNED_BYTE, pixels );
		glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
		glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
	}
	
	bool prepare() {
		if ( !loaded ) return false;
		if ( !tex ) commit();
		
		if ( frameReady ) {
			updatePixels( texWidth * texHeight );
			frameReady = false;
		}

		return true;
	}
	
	void updatePixels( int offset ) {
//		printf("time = %d ms\n", (int)(lastAccessTime_us / 1000) );
		glBindTexture( GL_TEXTURE_2D, tex );
		glPixelStorei( GL_UNPACK_ROW_LENGTH, texWidth );
		#ifdef _WIN32
		GLuint format = GL_BGRA_EXT;
		#else
		GLuint format = GL_BGRA;
		#endif
		glTexSubImage2D(GL_TEXTURE_2D,0,0,0,texWidth,texHeight,format,GL_UNSIGNED_BYTE,pixels+offset);
	}
	
	bool bind( int texUnit ) {
		if ( !prepare() ) return false;
		
		bind_image(glewGetContext(), tex, texUnit);
		return true;
	}

	void draw( int texUnit, float alpha ) {
		lastAccessTime_us = server->av->time_us();
		if ( !prepare() ) {
			draw_stub( alpha );
			return;
		}

		draw_image(glewGetContext(), texUnit, tex,aspectRatio,alpha,true);
	}
};

QTMovie *qtserver_impl::movieFromFile( const wchar_t *path ) {
	return new QTMovieFile( glewGetContext(), this, path );
}
