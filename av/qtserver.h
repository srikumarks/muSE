
#ifndef __MUSE_QTSERVER_H__
#define __MUSE_QTSERVER_H__

#include "avapi.h"
#include "thread.h"

enum {
	QTSERVER_MAX_TEX_WIDTH = 512,
	QTSERVER_MAX_TEX_HEIGHT = 512
};

// QT objects support the 'stat' view which
// can be watched using 'watch' api.
typedef muse_cell (*status_view_t)( muse_env *env, void *obj );

class QTImage;
class QTMovie;

class qtserver {
public:

	static qtserver *start( void *context, muse *av );

	virtual ~qtserver() {}

	virtual void needsUpdate() = 0;
	virtual QTImage *imageFromFile( const wchar_t *path ) = 0;
	virtual QTMovie *movieFromFile( const wchar_t *path ) = 0;
	virtual void stop() = 0;

	/// Submitting tasks ...
	/// We mimic the interface of Tasker here so that
	/// the qtserver implementation can decide to create
	/// multiple service threads and decide which thread to
	/// service a task on-the-fly.

	/// Appends the given task to the task queue.
	virtual void submit( Task *task ) = 0;
	virtual void submitUrgent( Task *task ) = 0;

	/// Invokes the given method of the given task.
	virtual void submit( Task *task, Task::Fn fn ) = 0;
	virtual void submitUrgent( Task *task, Task::Fn fn ) = 0;

};

struct ImageInfo
{
	bool infoReady;
	int naturalWidth, naturalHeight;
	float aspectRatio;
	bool hasAlpha;
	int maxTexWidth, maxTexHeight;
	int texWidth, texHeight;

	bool loaded;
	double lastAccessTime_us;
};

class QTImage : public ImageInfo {
public:
	virtual ~QTImage() {}
	virtual bool bind( int texUnit ) = 0;
	virtual void unbind( int texUnit ) = 0;
	virtual void draw( int texUnit, float alpha ) = 0;
};

class QTMovie : public QTImage {
public:	
	virtual ~QTMovie() {}

	typedef long long time_t;
	
	bool playing;
	time_t timescale, duration;

	virtual void prepare( double t, double rate ) = 0;
	virtual void play() = 0;
	virtual void pause() = 0;
	virtual double rate() = 0;
	virtual void setRate( double rate ) = 0;
	virtual double volume() = 0;
	virtual void setVolume( double v ) = 0;
	virtual double balance() = 0;
	virtual void setBalance( double b ) = 0;
	virtual void seek( time_t time ) = 0;
	virtual time_t time() = 0;
	virtual void seek_secs( double t ) = 0;
	virtual double time_secs() = 0;
	virtual double duration_secs() = 0;
	
	typedef double (QTMovie::*fparam_get)();
	typedef void (QTMovie::*fparam_set)( double v );
	
};



#endif // __MUSE_QTSERVER_H__