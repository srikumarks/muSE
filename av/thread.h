
#ifndef __MUSEM_THREAD_H__
#define __MUSEM_THREAD_H__

class Thread;
class Tasker;
class Task;

/// Derive from Thread and override the run() method
/// with your activities. A thread is always created
/// in suspended mode. You need to call start() to
/// start the thread.
class Thread {
public:
	Thread();
	virtual ~Thread();

	void start();
	void join();
	void timeCritical( bool );

protected:
	virtual void run() = 0;

private:
	void *m_handle;
	static void * proc( Thread * );
};

/// A task is an action that is intended to be executed in 
/// a tasker - a thread which maintains a queue of tasks
/// and executes them in sequence.
class Task {
public:

	typedef void (Task::*Fn)( Tasker * );

	virtual ~Task() {}

	virtual void queued() {}; ///< Called when submitted but not run yet.
	virtual void cancelled() {}; ///< Called when removed from queue without running.

	virtual void run( Tasker *tasker ) = 0; ///< Called by the tasker for the main activity.
};

class TaskOnce : public Task {
public:
	void cancelled();
	void run( Tasker *t );

	virtual void runOnce( Tasker * ) = 0;
};

/// A thread which maintains a queue of tasks 
/// to run. Submit tasks to a tasker using submit()
/// and cancel all tasks using cancel().
class Tasker : public Thread {
public:

	/// init is called before all tasks.
	/// destroy is called after all tasks.
	/// If the idler is non-NULL, then it
	/// will be run repeatedly instead of
	/// waiting for an event.
	static Tasker *create( Task *init, Task *destroy, Task *idler );
	static Tasker *create();

	/// Appends the given task to the task queue.
	virtual void submit( Task *task ) = 0;
	virtual void submitUrgent( Task *task ) = 0;

	/// Invokes the given method of the given task.
	virtual void submit( Task *task, Task::Fn fn ) = 0;
	virtual void submitUrgent( Task *task, Task::Fn fn ) = 0;

	/// Removes all pending tasks from the task queue.
	virtual void cancel() = 0;

	/// Stops all queue processing.
	virtual void stop() = 0;
};

/// Synchronization primitive that lets you pass a single
/// pointer data between two processes. Essentially a
/// one element queue.
class Box {
public:

	virtual ~Box() {}

	/// Creates a box with no contents/
	/// Calling get() will block until another process
	/// calls put().
	static Box *emptyBox();

	/// Returns the contents of the box. If the box is
	/// empty, it stalls until some other process calls put().
	/// Subsequently, calling get() again will not stall,
	/// unless the box has been empty().
	virtual void *get() = 0;

	/// Replaces the current contents of the box with the
	/// new data. Releases all processes waiting on this
	/// box (via get()).
	virtual void put( void *data ) = 0;

	/// Empties the box.
	virtual void empty() = 0;

	/// Tells you whather the box is empty or not
	/// - effectively telling you whether get() will block or not.
	virtual bool isEmpty() = 0;
};

#endif // __MUSEM_THREAD_H__