
#include "thread.h"

#ifdef _WIN32
#include <windows.h>
#else
#include <pthread.h>
#endif

#include <iostream>
#include <deque>

#ifdef _WIN32
typedef LPTHREAD_START_ROUTINE ThreadProc_t;
#else
typedef void *(*ThreadProc_t)(void*);
#endif

Thread::Thread()
{
#ifdef _WIN32
	m_handle = CreateThread( NULL, 0, (ThreadProc_t)Thread::proc, (LPVOID)this, CREATE_SUSPENDED, NULL );
#else
	pthread_create( (pthread_t*)&m_handle, NULL, (ThreadProc_t)Thread::proc, this );
#endif
	if ( m_handle == NULL )
		throw "Thread creation failed!";
}

Thread::~Thread()
{
#ifdef _WIN32
	CloseHandle( m_handle );
#else
	pthread_detach( (pthread_t)m_handle );
#endif
}

void Thread::start()
{
#ifdef _WIN32
	ResumeThread( m_handle );
#endif
}

void Thread::join()
{
#ifdef WIN32
	WaitForSingleObject( (HANDLE)m_handle, INFINITE );
#else
#endif
}

void Thread::timeCritical( bool tc ) 
{
#ifdef WIN32
	SetThreadPriority( (HANDLE)m_handle, tc ? THREAD_PRIORITY_TIME_CRITICAL : THREAD_PRIORITY_NORMAL );
#else
#endif
}

void * Thread::proc( Thread *t )
{
	t->run();
	return 0;
}

/////////////// Tasker ///////////////////////

class FnTask : public Task {
public:
	FnTask( Task *_task, Task::Fn _fn ) : task(_task), fn(_fn) {
		if ( !task || !fn ) {
			MessageBoxA(NULL, "Invalid function pointer!", "FnTask", MB_OK );
			throw -1;
		}
	}

	void run( Tasker *tasker ) {
		(task->*fn)(tasker);
		delete this;
	}

	void cancelled() {
		delete this;
	}

private:
	Task *task;
	Task::Fn fn;
};

class TaskerImpl : public Tasker
{
public:
	TaskerImpl( Task *_init, Task *_destroy, Task *_idler )
		: init(_init), destroy(_destroy), idler(_idler)
	{
		if ( init ) init->queued();
		if ( destroy ) destroy->queued();
		if ( idler ) idler->queued();

#ifdef _WIN32
		InitializeCriticalSection( &mutex );
		waiter = CreateEvent( NULL, TRUE, FALSE, NULL );
#else
		pthread_mutex_init( &mutex, NULL );
		pthread_cond_init( &waiter, NULL );
#endif
		start();
	}

	~TaskerImpl() 
	{
		enter();
		cancel();

		// Send NULL task to indicate end of tasker's life.
		tasks.push_back( NULL );
#ifdef _WIN32
		SetEvent( waiter );
		leave();
		join();
#else
		pthread_cond_signal(&waiter);
		leave();
		join();
#endif

		// Delete handles.
#ifdef _WIN32
		CloseHandle(waiter);
		DeleteCriticalSection( &mutex );
#else
		pthread_cond_destroy(&waiter);
		pthread_mutex_destroy(&mutex);
#endif
	}

private:
	typedef std::deque<Task*> TaskList;

	class QueueMutex {
	public:
		QueueMutex( TaskerImpl *t ) : tasker(t) { tasker->enter(); }
		~QueueMutex() { tasker->leave(); }
	private:
		TaskerImpl *tasker;
	};

	class QueueUnMutex {
	public:
		QueueUnMutex( TaskerImpl *t ) : tasker(t) { tasker->leave(); }
		~QueueUnMutex() { tasker->enter(); }
	private:
		TaskerImpl *tasker;
	};

	void submit( Task *task ) 
	{
		if ( task ) {
			QueueMutex _(this);
			tasks.push_back(task);
			task->queued();
			if ( !idler ) 
			{
			#ifdef _WIN32
				SetEvent( waiter );
			#else
				pthread_cond_signal( &waiter );
			#endif
			}
		}
	}

	void submitUrgent( Task *task ) 
	{
		if ( task ) {
			QueueMutex _(this);
			tasks.push_front(task);
			task->queued();
			if ( !idler ) 
			{
			#ifdef _WIN32
				SetEvent( waiter );
			#else
				pthread_cond_signal( &waiter );
			#endif
			}
		}
	}

	void submit( Task *task, Task::Fn fn ) 
	{
		submit( new FnTask( task, fn ) );
	}

	void submitUrgent( Task *task, Task::Fn fn ) 
	{
		submitUrgent( new FnTask( task, fn ) );
	}

	void cancel() 
	{
		QueueMutex _(this);
		for ( size_t i = 0; i < tasks.size(); ++i ) {
			Task *t = tasks[i];
			t->cancelled();
		}
		tasks.clear();
	}

	class stop_notification : public TaskOnce
	{
	public:
		stop_notification( Box *_box ) : box(_box) {}

		void runOnce( Tasker * ) {
			box->put( (void*)1 );
		}

	private:
		Box *box;
	};

	void stop()
	{
		Box *b = NULL;
		{
			QueueMutex _(this);
			cancel();

			b = Box::emptyBox();
			submitUrgent( new stop_notification(b) );
		}
		b->get();
		delete b;
	}

	void run() 
	{
		if ( init ) { 
			init->run( this ); 
			init = NULL;
		}

		bool stayalive = true;

		while ( stayalive ) {

			if ( idler ) {
				try {
					while ( tasks.size() == 0 ) {
						idler->run( this );
					}
				} catch ( int e ) {
					if ( e != 0 ) {
						std::cerr << "Error: Idler error " << e;
						idler = NULL;
					}
				} catch ( ... ) {
					std::cerr << "Error: Idler exception!\n";
					idler = NULL;
				}
			} else {
				QueueMutex _(this);
				bool notasks = tasks.empty();

				if ( notasks ) {
					QueueUnMutex __(this);
				#ifdef _WIN32
					WaitForSingleObject( waiter, INFINITE );
					ResetEvent( waiter );
				#else
					pthread_cond_wait( &waiter, &mutex );
				#endif
				}
			}

			bool notasks = false;
			while ( !notasks ) {

				// Take one task.
				Task *task = NULL;
				{
					QueueMutex _(this);
					notasks = tasks.empty();
					if ( !notasks ) {
						task = tasks.front();
						tasks.pop_front();
					}
				}

				// Check for exit condition.
				if ( task == NULL ) {
					stayalive = notasks;
					break;
				}

				notasks = false;

				try {
					task->run( this );
				} catch ( int e ) {
					if ( e != 0 ) {
						std::cerr << "Error: Task error " << e;
					}
				} catch ( ... ) {
					std::cerr << "Error: Unknown exception in task!\n";
				}
			}
		}

		if ( destroy ) {
			destroy->run( this );
			destroy = NULL;
		}
	}

#ifdef _WIN32
	void enter() { EnterCriticalSection( &mutex ); }
	void leave() { LeaveCriticalSection( &mutex ); }
	CRITICAL_SECTION mutex;
	HANDLE waiter;
#else
	void enter() { pthread_mutex_lock( &mutex ); }
	void leave() { pthread_mutex_unlock( &mutex ); }
	pthread_mutex_t mutex;
	pthread_cond_t waiter;
#endif
	
	Task *init, *destroy, *idler;
	TaskList tasks;
};

Tasker *Tasker::create( Task *init, Task *destroy, Task *idler )
{
	return new TaskerImpl( init, destroy, idler );
}

Tasker *Tasker::create()
{
	return new TaskerImpl( NULL, NULL, NULL );
}

void TaskOnce::run( Tasker *t ) {
	runOnce(t);
	delete this;
}

void TaskOnce::cancelled() {
	delete this;
}

////////////////////////////////////////////////////////////
// Box implementation.

class Box_impl : public Box {
public:
	Box_impl() : data(NULL) {
		InitializeCriticalSection(&mutex);
		event = CreateEvent( NULL, TRUE, FALSE, NULL ); ///< Manual reset event.
	}

	~Box_impl() {
		CloseHandle(event);
		DeleteCriticalSection(&mutex);
	}

	void *get() {
		Mutex m(*this);
		if ( data )
			return data;
		else {
			Unmutex u(*this);
			WaitForSingleObject( event, INFINITE );
			return data;
		}
	}

	void put( void *_data ) {
		Mutex m(*this);
		data = _data;
		SetEvent(event);
	}

	void empty() {
		Mutex m(*this);
		data = NULL;
		ResetEvent(event);
	}

	bool isEmpty() {
		Mutex m(*this);
		return data == NULL;
	}

private:
	void *data;

private:

	CRITICAL_SECTION mutex;
	HANDLE event;

	class Mutex {
	public:
		Mutex( Box_impl &b ) : cs(&(b.mutex)) {
			EnterCriticalSection(cs);
		}
		~Mutex() {
			LeaveCriticalSection(cs);
		}
	private:
		CRITICAL_SECTION *cs;
	};

	class Unmutex {
	public:
		Unmutex( Box_impl &b ) : cs(&(b.mutex)) {
			LeaveCriticalSection(cs);
		}
		~Unmutex() {
			EnterCriticalSection(cs);
		}
	private:
		CRITICAL_SECTION *cs;
	};
};

Box *Box::emptyBox() {
	return new Box_impl();
}