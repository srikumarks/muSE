/**
 * @file muse.c
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd. 
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 */

#include "muse_opcodes.h"
#include "muse_builtins.h"
#include "muse_port.h"
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <string.h>
#ifdef MUSE_PLATFORM_WINDOWS
#include <windows.h>
#else
#include <sys/time.h>
#endif

/**
 * String names for the various cell types, intended for
 * debugging and reporting use.
 * 
 * @see _typename()
 */
const char *g_muse_typenames[] =
{
	"MUSE_CONS_CELL",
	"MUSE_LAMBDA_CELL",
	"MUSE_SYMBOL_CELL",
	"MUSE_NATIVEFN_CELL",
	"MUSE_INT_CELL",
	"MUSE_FLOAT_CELL",
	"MUSE_TEXT_CELL",
	"MUSE_LAZY_CELL"
};

static void init_stack( muse_stack *s, int size )
{
	s->size = size;
	s->top = s->bottom = (muse_cell*)calloc( size, sizeof(muse_cell) );
}

static void destroy_stack( muse_stack *s )
{
	if ( s->bottom )
	{
		free( s->bottom );
		s->bottom = s->top = NULL;
		s->size = 0;
	}
}

static muse_boolean realloc_stack( muse_stack *s, int new_size )
{
	if ( new_size != s->size )
	{
		muse_cell *p = (muse_cell*)realloc( s->bottom, new_size * sizeof(muse_cell) );
		if ( p )
		{
			s->top = p + (s->top - s->bottom);
			s->bottom = p;
			s->size = new_size;
			return MUSE_TRUE;
		}
		else
			return MUSE_FALSE;
	}
	else	
		return MUSE_TRUE;
}

static void init_heap( muse_env *env, muse_heap *heap, int heap_size )
{
	heap_size				= (heap_size + 7) & ~7;
	heap->size_cells		= heap_size;
	heap->cells				= (muse_cell_data*)calloc( heap_size, sizeof(muse_cell_data) );
	heap->free_cells		= _cellati(1); /* 0 is not in free list as its a fixed cell. */
	heap->free_cell_count	= heap_size - 1;
	heap->marks				= (unsigned char *)calloc( heap_size >> 3, 1 );
	heap->keep				= (unsigned char *)calloc( heap_size >> 3, 1 );

	/* Initialize free list */
	{
		int i, i_end;
		muse_cell_data *c = _ptr(_cellati(1));
		for ( i = 1, i_end = heap_size-1; i < i_end; ++i, ++c )
		{
			c->cons.head = MUSE_NIL;
			c->cons.tail = _cellati(i+1);
		}
	}
}

static void destroy_heap( muse_heap *heap )
{
	if ( heap->cells )
	{
		free(heap->cells);
		heap->cells = NULL;
		heap->size_cells = 0;
		free(heap->marks);
		free(heap->keep);
		heap->free_cells = 0;
		heap->free_cell_count = 0;
	}
}

static unsigned char *crealloc( unsigned char *mem, size_t orig_size, size_t new_size )
{
	unsigned char *mem2 = realloc( mem, new_size );

	if ( mem2 )
	{
		memset( mem2 + orig_size, 0, new_size - orig_size );
	}

	return mem2;
}

static muse_boolean grow_heap( muse_heap *heap, int new_size )
{
	new_size = (new_size + 7) & ~7;
	
	fprintf(stderr, "\n(growing heap to %d)\n", new_size);
	if ( new_size <= heap->size_cells )
		return MUSE_TRUE;

	{
		muse_cell_data *p = (muse_cell_data*)realloc( heap->cells, new_size * sizeof(muse_cell_data) );
		if ( p )
		{
			unsigned char *m = crealloc( heap->marks, heap->size_cells >> 3, new_size >> 3 );
			unsigned char *k = crealloc( heap->keep, heap->size_cells >> 3, new_size >> 3 );
			if ( m )
			{
				heap->cells = p;
				heap->marks = m;
				heap->keep = k;

				/* Collect the newly allocated cells into the free list. */				
				{
					int i = heap->size_cells;
					int i_end = new_size - 1;
					muse_cell_data *c = p + i;
					for ( ; i < i_end; ++i, ++c )
					{
						c->cons.head = MUSE_NIL;
						c->cons.tail = _cellati(i+1);
					}
					heap->cells[i_end].cons.head = MUSE_NIL;
					heap->cells[i_end].cons.tail = heap->free_cells;
					heap->free_cells = _cellati( heap->size_cells );
					heap->free_cell_count += (new_size - heap->size_cells);
					heap->size_cells = new_size;
				}
				
				return MUSE_TRUE;
			}
			else
			{
				heap->cells = (muse_cell_data*)realloc( p, heap->size_cells * sizeof(muse_cell_data) );;
				return MUSE_FALSE;
			}
		}
		else
			return MUSE_FALSE;
	}
}

static const struct _bs { int builtin; const muse_char *symbol; } k_builtin_symbol_table[] =
{
	{ MUSE_NIL,					NULL		},
	{ MUSE_T,					L"T"		},
	{ MUSE_QUOTE,				L"quote"	},
	{ MUSE_CLASS,				L"class"	},
	{ MUSE_SELF,				L"self"		},
	{ MUSE_SUPER,				L"super"	},
	{ MUSE_DOC,					L"doc"		},
	{ MUSE_CODE,				L"code"		},
	{ MUSE_SIGNATURE,			L"signature"},
	{ MUSE_USAGE,				L"usage"	},
	{ MUSE_BRIEF,				L"brief"	},
	{ MUSE_DESCR,				L"descr"	},
	{ MUSE_TIMEOUT,				L"timeout"	},
	{ MUSE_DEFINE,				L"define"	},
	{ MUSE_TRAP_POINT,			L"{{trap}}" },
	{ MUSE_DEFAULT_EXCEPTION_HANDLER, L"{{*default-exception-handler*}}" },
	{ MUSE_CLOSURE,				L"closure"	},
	{ MUSE_NAME,				L"name"		},
	{ MUSE_IT,					L"it"		},
	{ MUSE_THE,					L"the"		},
	{ MUSE_TIMEOUTVAR,			L"{{timeout}}"	},
	{ MUSE_XMLSPLICE,			L"++"		},
	{ MUSE_GENERIC_INVOKE,		L"*invoke*"	},
	{ -1,						NULL		},
};

static void init_builtin_symbols( muse_env *env, muse_cell *s )
{
	const struct _bs *bs = k_builtin_symbol_table;
	while ( bs->builtin >= 0 )
	{
		if ( bs->symbol == NULL )
			s[bs->builtin]	= bs->builtin;
		else
			s[bs->builtin] = _csymbol( bs->symbol );
		
		++bs;
	}
}

static void init_parameters( muse_env *env, const int *parameters )
{
	static int k_default_parameter_values[MUSE_NUM_PARAMETER_NAMES] =
	{
		0,		/* Ignored */
		65536,	/* MUSE_HEAP_SIZE */
		80,		/* MUSE_GROW_HEAP_THRESHOLD */
		4096,	/* MUSE_STACK_SIZE */
		4096,	/* MUSE_MAX_SYMBOLS */
		0,		/* MUSE_DISCARD_DOC */
		1,		/* MUSE_PRETTY_PRINT */
		4,		/* MUSE_TAB_SIZE */
		1,		/* MUSE_DEFAULT_ATTENTION */
#if __APPLE__	
		1,		/* MUSE_ENABLE_OBJC */
		1,		/* MUSE_OWN_OBJC_AUTORELEASE_POOL */
#else
		0,		/* MUSE_ENABLE_OBJC */
		0,		/* MUSE_OWN_OBJC_AUTORELEASE_POOL */
#endif
		MUSE_TRUE	/* MUSE_ENABLE_TRACE */
	};

	/* Initialize default values. */
	env->parameters = (int*)calloc( MUSE_NUM_PARAMETER_NAMES, sizeof(int) );
	memcpy( env->parameters, k_default_parameter_values, sizeof(k_default_parameter_values) );
	
	/* Set overridden parameters. */
	if ( parameters )
	{
		while ( parameters[0] )
		{
			if ( parameters[0] > 0 && parameters[0] < MUSE_NUM_PARAMETER_NAMES )
			{
				env->parameters[parameters[0]] = parameters[1];
				parameters += 2;
			}
			else
			{
				fprintf( stderr, "muse: Invalid muse_init_env parameter %d!\n", *parameters );
				break;
			}
		}
	}
}

/**
 * Creates a new muse environment.
 *
 * @param parameters is an int array of param-value pairs. The last entry 
 * should be MUSE_END_OF_LIST which need not be given a value.
 *
 * @see muse_env_parameter_name_t
 */
MUSEAPI muse_env *muse_init_env( const int *parameters )
{
	muse_env *env = (muse_env*)calloc( 1, sizeof(muse_env) );
	
	env->stack_base = (void*)&parameters;
	init_parameters( env, parameters );
	
	init_heap( env, &env->heap, env->parameters[MUSE_HEAP_SIZE] );
	init_stack( &env->symbol_stack, env->parameters[MUSE_MAX_SYMBOLS] );
	
	/* The symbol stack is not really a stack. Its an array of buckets 
	containing lists of symbols and is of fixed size. We use a hashing
	algorithm to uniquify a symbol. */
	env->symbol_stack.top = env->symbol_stack.bottom + env->symbol_stack.size;

	/* Start a time reference point. */
	env->timer = muse_tick();

	/* Create the main process. */
	{
		SAVE_STACK_POINTER( saved_sp );

		{
			muse_process_frame_t *p = create_process( env, env->parameters[MUSE_DEFAULT_ATTENTION], MUSE_NIL, saved_sp );
			env->current_process = p;
			init_process_mailbox( p );
			prime_process( p );

			/* Immediately switch to running state. */
			p->state_bits = MUSE_PROCESS_RUNNING;
		}
	}

	/* Make sure the built-in symbol initialization doesn't use any net stack space. */
	{
		int sp = _spos();
		env->builtin_symbols = (muse_cell*)calloc( MUSE_NUM_BUILTIN_SYMBOLS, sizeof(muse_cell) );
		init_builtin_symbols( env, env->builtin_symbols );
		
		muse_load_builtin_fns(env);
		_unwind(sp);
	}

#if __APPLE__
	/* Initialize objective C if defined. */
	if ( env->parameters[MUSE_ENABLE_OBJC] ) 
		init_objc_bridge(env);
#else
	env->parameters[MUSE_ENABLE_OBJC] = MUSE_FALSE;
#endif
	
	return env;
}

/**
 * Destroys the given environment. If the given
 * environment is the current environment, then
 * the current environment is set to NULL and
 * no muse calls can subsequently be made.
 */
MUSEAPI void muse_destroy_env( muse_env *env )
{
#if __APPLE__
	/* Deallocate objc pool if enabled. */
	destroy_objc_bridge(env);
#endif
	
	/* Mark all processes as dead. */
	{
		muse_process_frame_t *cp = env->current_process;
		muse_process_frame_t *p = cp;
		do
		{
			p->state_bits = MUSE_PROCESS_DEAD;
			p = p->next;
		}
		while ( p != cp );
	}

	muse_gc(env, 0);
	free_process( env->current_process );
	muse_tock(env->timer);
	free(env->builtin_symbols);
	env->builtin_symbols = NULL;
	destroy_stack( &env->symbol_stack );
	destroy_heap( &env->heap );
	free( env->parameters );
	free( env->slots );
	free( env );
}

/**
 * The environment maintains a fixed number of "slots"
 * which are memory locations of size muse_int that are
 * addressed using a four-character int id. The int in
 * the location can be used for any purpose and is 
 * intended for use by plugins to store and manage 
 * sub-system state. The returned pointer to muse_int
 * is valid for the lifetime of the environment and
 * can always be retrieved given the slotid.
 */
MUSEAPI muse_int *muse_slot( muse_env *env, int slotid )
{
	/* If no slots have been allocated, do that. */
	if ( env->slot_capacity == 0 ) {
		env->slot_capacity = MUSE_MAX_SLOTS;
		env->num_slots = 0;
		env->slots = (muse_slot_t*)calloc( env->slot_capacity, sizeof(muse_slot_t) );
	}

	/* Search the slots for an id match. */
	{
		int i = 0, N = env->num_slots;
		for ( ; i < N; ++i ) {
			if ( env->slots[i].id == slotid )
				return &(env->slots[i].value);
		}
	}

	/* No such slot allocated. Allocate a new one. */
	if ( env->num_slots >= env->slot_capacity ) {
		/* No more space! */
		muse_raise_error( env, _csymbol(L"fatal-error:no-slots-available"), MUSE_NIL );
		return NULL;
	}

	/* Add a new slot. */
	{
		int ix = env->num_slots++;
		env->slots[ix].id = slotid;
		return &(env->slots[ix].value);
	}
}

/**
 * Allocates a new cons cell with the given head 
 * and tail cells. This is the primary constructor
 * in the whole of muse out of which all muse objects
 * are built. A new cell for the cons operation is
 * taken from the heap's free list. If no cell is 
 * available the garbage collector is invoked to
 * collect the unreferenced cells into the free list.
 * If absolutely no cells are available, then the heap
 * is grown in order to get a free cell to allocate
 * to the cons. The newly allocated cell is placed on
 * the stack so that it won't be inadvertently garbage
 * collected. This way, you can safely write expressions
 * of the form @code _cons( _cons(a,b), _cons(c,d) ) @endcode 
 * 
 * The cons operation does not fail unless there is
 * absolutely no system memory available for the new cell.
 */
MUSEAPI muse_cell muse_cons( muse_env *env, muse_cell head, muse_cell tail )
{
	if ( env->heap.free_cells == MUSE_NIL )
	{
		/* Make sure that the given head and tail
		 * will not be accidentally freed by the gc
		 * operation,m by pushing them onto the stack. 
		 */
		int sp = _spos();
		_spush(head);
		_spush(tail);
		muse_gc(env,1);
		_unwind(sp);
		
		if ( env->heap.free_cells == MUSE_NIL )
		{
			fprintf( stderr, "\t\t\tNo free cells!\n" );
			grow_heap( &env->heap, env->heap.size_cells * 2 );
		}
	}

	{
		muse_cell c = _takefreecell();
		_setht( c, head, tail );
		_spush(c);
		return c;
	}
}

/**
 * Allocates a new integer cell to hold the given integer. 
 * The newly allocated cell is placed on the stack.
 */
MUSEAPI muse_cell muse_mk_int( muse_env *env, muse_int i )
{
	muse_cell c = _setcellnct( _cons( 0, 0 ), MUSE_INT_CELL );
	_ptr(c)->i = i;
	return c;
}

/**
 * Allocates a new float cell to hold the given integer.
 * The newly allocated cell is placed on the stack.
 */
MUSEAPI muse_cell muse_mk_float( muse_env *env, muse_float f )
{
	muse_cell c = _setcellnct( _cons( 0, 0 ), MUSE_FLOAT_CELL );
	_ptr(c)->f = f;
	return c;
}

/**
 * Returns the current stack position. You can
 * subsequently unwind to the position by calling
 * \c muse_stack_unwind(), passing the position
 * as its argument.
 * 
 * A muse enviroment maintains a stack of cell references
 * which will not be garbage collected the next time the gc
 * is invoked. Every API function that uses \ref _cons
 * takes at least one new cell from the free list and
 * places it on the stack. Such functions include \ref _mk_int,
 * \ref _mk_float and family, the \ref muse_list and
 * related functions and so on - anything that will need to
 * allocate a new cell.
 * 
 * Once a reference to a cell is placed into another cell,
 * either by using \ref _define on a named symbol, or
 * by placing it in the property list of a named symbol or
 * by adding it to a list that is itself protected by being
 * on the stack, the stack space occupied by the cell can be
 * released. This is done using \ref muse_stack_unwind.
 * 
 * If a function does not allocate an unreferenced cell, but
 * creates temporary cells, it should reset the stack position
 * before it returns. This is typically done as follows -
 * @code
 * void my_func()
 * {
 *     int sp = muse_stack_pos();
 *     / * ... code that allocates new cells ... * /
 *     muse_stack_unwind(sp);
 * }
 * @endcode
 * 
 * If a function returns as its result a newly constructed compound
 * data structure referenced by a single muse_cell reference, it is
 * advisable for it to place it on the stack, but release all other
 * stack references to the constituent elements. For example -
 * @code
 * muse_cell my_make_blooper( int arg )
 * {
 *     int sp = muse_stack_pos();
 *     muse_cell result = MUSE_NIL;
 *     / * .. compute the data structure and store the reference in "result". * /
 *	   muse_unwind_stack(sp);
 *     muse_stack_push(result);
 *     return result;
 * }
 * @endcode
 * 
 * @see muse_stack_unwind, muse_stack_push
 */
MUSEAPI int	muse_stack_pos(muse_env *env)
{
	return _spos();
}

/**
 * Unwinds the stack to the given position.
 * This discards all references to cells that
 * have been kept on the stack. Temporary
 * objects which were placed on the stack
 * that aren't referred to elsewhere will be
 * available for garbage collection.
 * 
 * @see muse_stack_pos
 */
MUSEAPI void muse_stack_unwind( muse_env *env, int stack_pos )
{
	_unwind(stack_pos);
}

/**
 * Saves the given object on the stack so that
 * it will not be garbage collected when gc
 * is next invoked.
 * 
 * @see muse_stack_pos
 */
MUSEAPI muse_cell muse_stack_push( muse_env *env, muse_cell obj )
{
	_spush(obj);
	return obj;
}

/**
 * Gets the current depth of the bindings stack.
 * You can use this to locally introduce new bindings
 * using muse_pushdef() and when you're done with the
 * local definitions, use muse_bindings_stack_unwind()
 * to return the state of the bindings to where they were
 * when you got the bindings stack position.
 *
 * @see muse_bindings_stack_unwind()
 */
MUSEAPI int muse_bindings_stack_pos( muse_env *env )
{
	return _bspos();
}

/**
 * @see muse_bindings_stack_pos()
 */
MUSEAPI void muse_bindings_stack_unwind( muse_env *env, int pos )
{
	_unwind_bindings(pos);
}

/**
 * A mechanism for gathering call trace information.
 * "Push" the trace information at the start of your call context
 * pop it at the end.
 */
MUSEAPI void muse_trace_push( muse_env *env, const muse_char *label, muse_cell fn, muse_cell arglist )
{
	muse_traceinfo_t *ti = &(env->current_process->traceinfo);
	int i = ti->depth % ti->size;
	muse_trace_t *t = ti->data + i;
	t->sp = _spos();
	t->label = label;
	t->fn = fn;
	t->argv = arglist;
	++(ti->depth);
}

/**
 * @see muse_trace_push().
 */
MUSEAPI void muse_trace_pop( muse_env *env )
{
	muse_traceinfo_t *ti = &(env->current_process->traceinfo);
	muse_assert( ti->depth > 0 );
	--(ti->depth);
}

/**
 * Generates a report of the stuff on the trace stack at the
 * point of call.
 */
MUSEAPI size_t muse_trace_report( muse_env *env, size_t numchars, muse_char *buffer )
{
	muse_traceinfo_t *ti = &(env->current_process->traceinfo);
	int top = ti->depth - 1;
	int bottom = ti->depth - ti->size;
	int depthindex = 0;
	size_t chars = 0;
	if ( bottom < 0 ) bottom = 0;

	if ( top >= bottom && chars < numchars )
		chars += muse_sprintf( env, buffer+chars, numchars-chars, L"\n\n--- deep context ---\n" );

	for (  ; bottom <= top && chars < numchars; ++bottom, ++depthindex )
	{
		int i = bottom % ti->size;
		muse_trace_t *t = ti->data + i;
		size_t startpos = chars;
		
		if ( t->label == NULL && t->fn == MUSE_NIL )
		{
			--depthindex;
			continue; // Not much info in the stack trace.
		}

		if ( depthindex > 0 )
		{
			int j;
			for ( j = 0; j < depthindex && chars+2 < numchars; ++j )
			{
				buffer[chars++] = ' ';
				buffer[chars++] = ' ';
			}

			buffer[chars] = '\0';
		}


		if ( t->label && numchars > chars )
		{
			chars += muse_sprintf( env, buffer+chars, numchars-chars, L"%s ", t->label );
		}

		if ( t->fn && numchars > chars )
		{
			muse_cell name = meta_getname(env,t->fn);
			if ( name )
				chars += muse_sprintf( env, buffer+chars, numchars-chars, L"%m ", name );
			else if ( t->label == NULL )
			{
				// No label or name. Skip this one since it
				// is not going to be of much info.
				chars = startpos;
				buffer[chars] = '\0';
				--depthindex;
				continue;
			}
		}

		if ( numchars > chars )
		{
			chars += muse_sprintf( env, buffer+chars, numchars-chars, L"%m\n", t->argv );
		}
	}

	return chars;
}

static void add_special( muse_env *env, muse_cell special )
{
	_lpush( _cons( special, 0 ), &env->specials );
}

/**
 * Copies the given text and creates a new text cell to
 * store it. The newly allocated cell is placed on the stack.
 * If you pass NULL for \p start, then this creates a new
 * text cell of the needed length (= end-start), but
 * stores null characters in it. You can subsequently
 * change the contents of the text cell.
 * 
 * @internal The cell is also placed on the specials list
 * so that the memory required to hold the cell can be
 * destroyed when the text cell is no longer needed and is
 * garbage collected.
 */ 
MUSEAPI muse_cell muse_mk_text( muse_env *env, const muse_char *start, const muse_char *end )
{
	muse_cell c			= _setcellnct( _cons( 0, 0 ), MUSE_TEXT_CELL );
	muse_cell_data *d	= _ptr(c);
	
	d->text.start		= (muse_char *)malloc( (end - start + 1) * sizeof(muse_char) );
	d->text.end			= d->text.start + (end - start);
	*(d->text.end)		= 0;

	/* If start is NULL, it means we only know the length
	and we're supposed to create a blank string of that length. */
	if ( start )
	{
		memcpy( d->text.start, start, sizeof(muse_char) * (end - start) );
	}

	add_special(env,c);
		
	return c;
}

/**
 * Same as \c muse_mk_text() except that it takes a UTF8 string
 * and converts it into a unicode string and stores it.
 */
MUSEAPI muse_cell muse_mk_text_utf8( muse_env *env, const char *start, const char *end )
{
	muse_cell c			= _setcellnct( _cons( 0, 0 ), MUSE_TEXT_CELL );
	muse_text_cell *t	= &_ptr(c)->text;
	int len				= (int)(end - start);

	t->start			= (muse_char*)calloc( muse_unicode_size(start, len), 1 );
	t->end				= t->start + muse_utf8_to_unicode( t->start, len, start, len );

	add_special(env,c);
	
	return c;
}

/**
 * Same as \c muse_mk_text() except that it assumes a null
 * terminated string.
 */
MUSEAPI muse_cell muse_mk_ctext( muse_env *env, const muse_char *start )
{
	return muse_mk_text( env, start, start + wcslen(start) );
}

/**
 * Same as \c muse_mk_text_utf8() except that it assumes
 * a null terminated string as input.
 */
MUSEAPI muse_cell muse_mk_ctext_utf8( muse_env *env, const char *start )
{
	return muse_mk_text_utf8( env, start, start + strlen(start) );
}

/**
 * Creates a new cell that stores a C function. The 
 * C function can be passed its own context data that
 * is not managed by muSE. The function pointer is given
 * in \p fn and the context data is given in \p context.
 * Using the context pointer, it is possible to encode
 * a call to a C++ member function, by writing a static 
 * wrapper function.
 */
MUSEAPI muse_cell muse_mk_nativefn( muse_env *env, muse_nativefn_t fn, void *context )
{
	muse_cell c			= _setcellnct( _cons( 0, 0 ), MUSE_NATIVEFN_CELL );
	muse_cell_data *p	= _ptr(c);
	
	p->fn.fn			= fn;
	p->fn.context		= context;
	
	return c;
}

/**
 * A destructor is a native function that also gets
 * called with no arguments when the function is 
 * garbage collected.
 */
MUSEAPI muse_cell muse_mk_destructor( muse_env *env, muse_nativefn_t fn, void *context )
{
	muse_cell f = _mk_nativefn( fn, context );
	add_special(env, f);
	return f;
}

static muse_cell lookup_symbol( muse_env *env, const muse_char *start, const muse_char *end, muse_int *out_hash )
{
	muse_int hash = muse_hash_text( start, end, MUSE_SYMBOL_CELL );
	
	muse_stack *ss = _symstack();
	
	if ( out_hash )
		*out_hash = hash;

	/* The symbols are allocated over a set of buckets according to 
		their hashes. All symbols with the same hash are assigned to the
		same bucket, which essentially contains a linear linked list of
		symbols. After finding the bucket, we just do a linear search
		through the symbols in the list to find the one that matches,
		if there is one. */
	{
		int bucket = (int)(((hash % ss->size) + ss->size) % ss->size);
		muse_cell slist = ss->bottom[bucket];
		
		while ( slist )
		{
			muse_cell considering = _next(&slist);
			muse_cell symdef = _head(_tail(considering));
			
			if ( _ptr(_head(symdef))->i == hash )
			{
				/* Maybe found. */
				muse_text_cell t = _ptr(_tail(symdef))->text;
				if ( (t.end - t.start) == (end - start) && wcscmp( t.start, start ) == 0 )
				{
					/* Found. */
					return considering;
				}
			}
		}
		
		return MUSE_NIL;
	}
}

/**
 * Includes the given symbol in the symbol table
 * and makes its value process-local. Note that no check
 * is performed to see whether the symbol is already
 * interned or not.
 */
MUSEAPI muse_cell muse_intern_symbol( muse_env *env, muse_cell sym, int local_ix, muse_int hash )
{
	muse_stack *ss = _symstack();

	muse_assert( _cellt(sym) == MUSE_SYMBOL_CELL );

	/* Define the symbol to be itself in all processes. */
	{
		muse_process_frame_t *cp = env->current_process;
		muse_process_frame_t *p = cp;

		do
		{
			/* Make sure we have enough storage for defined symbols 
			in each process. */
			if ( local_ix >= p->locals.size )
			{
				muse_assert( local_ix < 2 * p->locals.size );
				if ( realloc_stack( &(p->locals), 2 * p->locals.size ) == MUSE_FALSE )
					muse_raise_error( env, MUSE_NIL, MUSE_NIL );
			}
			
			p->locals.bottom[local_ix] = sym;
			p->locals.top = p->locals.bottom + env->num_symbols;
			p = p->next;
		}
		while ( p != cp );
	}

	/* Add the symbol to its hash bucket. */
	{
		int bucket = (int)(((hash % ss->size) + ss->size) % ss->size);
		ss->bottom[bucket] = _cons( sym, ss->bottom[bucket] );
	}

	/* Set the head of the symbol to refer to the local index */
	_seth( sym, _localcell(local_ix) );

	return sym;
}

/**
 * Returns a reference to a named symbol with the given name.
 * All symbols with the same name have identical symbol references.
 * Therefore you can compare whether two symbols are the same
 * by comparing their cell references.
 * 
 * @internal
 * A symbol is a compound constructed out of cons cells.
 * 	- symbol = (value . ((hash . name) . plist))
 * 	- values = (symbol . () )
 * 	- plist = () or (( key . value ) . plist)
 * 
 * \c values is a stack of symbol value definitions
 * that you can push and pop using \c muse_pushdef() and
 * \c muse_popdef().
 */
MUSEAPI muse_cell muse_symbol( muse_env *env, const muse_char *start, const muse_char *end )
{
	int p = -1;
	muse_int hash = 0;
	muse_cell sym = lookup_symbol( env, start, end, &hash );
	
	if ( sym )
		return sym;
	else
	{
		muse_stack *ss = _symstack();
		int local_ix = _newlocal();
		
		/* sym -> ( . ) */
		p = _spos();
		sym = _setcellt( _cons( _localcell(local_ix), MUSE_NIL ), MUSE_SYMBOL_CELL );
		
		{
			muse_cell name = muse_mk_text( env, start, end );
			
			/* symplist -> ( ( hash . name ) . nil ) */
			muse_cell symplist = _cons( _cons( _mk_int( hash ),
													   name ),
											MUSE_NIL );
			
			/* sym -> ( sym . symplist ) */
			_sett( sym, symplist );

			muse_intern_symbol( env, sym, local_ix, hash );
		}
		
		_unwind(p);
		
		return sym;
	}
}

/**
 * Same as \c muse_symbol(), but takes a c-style null
 * terminated unicode character string.
 */
MUSEAPI muse_cell muse_csymbol( muse_env *env, const muse_char *sym )
{
	return muse_symbol( env, sym, sym + wcslen(sym) );
}

/**
 * Same as \c muse_symbol() except that it takes a
 * UTF8 string.
 */
MUSEAPI muse_cell muse_symbol_utf8( muse_env *env, const char *start, const char *end )
{
	int utf8_len = (int)(end - start);
	muse_char *s = (muse_char*)calloc( muse_unicode_size(start, utf8_len), 1 );
	int len = (int)muse_utf8_to_unicode( s, utf8_len, start, utf8_len );
	
	{
		muse_cell c = muse_symbol( env, s, s + len );
		free(s);
		return c;
	}
}

/**
 * Same as \c muse_symbol_utf8() except that it takes
 * a c-style null terminated utf8 string.
 */
MUSEAPI muse_cell muse_csymbol_utf8( muse_env *env, const char *sym )
{
	return muse_symbol_utf8( env, sym, sym + strlen(sym) );
}

/**
 * Returns a symbol reference corresponding to the given
 * symbol index. This a convenience function.
 * 
 * @see muse_builtin_symbol_t
 */
MUSEAPI muse_cell muse_builtin_symbol( muse_env *env, muse_builtin_symbol_t s )
{
	muse_assert( s >= 0 && s < MUSE_NUM_BUILTIN_SYMBOLS );
	return env->builtin_symbols[s];
}

/**
 * An anonymous symbol is similar to a named symbol, except
 * that it is not stored permanently on the symbol stack.
 * It has a property list as well and a value stack,
 * but the symbol name cell is nil. The hash code of 
 * an anonymous symbol is the integer value of cell
 * reference itself, so comparing using the hash is
 * the same as comparing the cell reference itself.
 * 
 * Anonymous symbols are used to represent objects
 * in muSE's object system. An object's properties are
 * stored in the plist of an anonymous symbol.
 */
MUSEAPI muse_cell muse_mk_anon_symbol(muse_env *env)
{
	muse_cell sym = _setcellt( _cons( 0, 0 ), MUSE_SYMBOL_CELL );
	
	int p = _spos();
	
	muse_cell symval	= sym;
	muse_cell symplist	= _cons( _cons( _mk_int(sym),
												MUSE_NIL ),
									 MUSE_NIL );
	
	_setht( sym, 0, symplist );	
	_unwind( p );
	
	return sym;
}

/**
 * Prior to garbage collection, muse_mark is called
 * on all cells which are referenced somewhere and
 * should not be garbage collected. The unmarked cells
 * are then swept into the free list.
 */
MUSEAPI void muse_mark( muse_env *env, muse_cell c )
{
CONTINUE_MUSE_MARK:
	if ( c > 0 && !_ismarked(c) )
	{
		_mark(c);
		
		if ( _iscompound(c) )
		{
			/* We need to quick-unquote components of
			compound structures if they have been quick-quoted.
			For example, macros are specified as lambda expressions
			with a quick-quoted formals list. 
			
			Using _quq() here incurs some significant penalty
			at gc time, but we need it for correctness.

			TODO: Find out if there's a more optimal way to handle this. */

			muse_mark( env, _quq(_head(c)) );

			/* Explicit tail call elimination to support debug builds. 

			Compiler optimizations are turned off in debug builds and 
			so it is easy to run into stack overflow conditions when
			debugging with real data. Release build code should do
			exactly what's done below to eliminate tail calls, but
			I'm doing it by hand so that it takes effect in debug
			builds as well. 
			
			TODO: I'm not sure whether the explicit tail call 
			elimination done here will cause the compiler to
			generate less optimal code in the release build. That
			remains to be measured. */
			{
				c = _quq(_tail(c));
				goto CONTINUE_MUSE_MARK;
			}
		}
		else
		{
			/* If the cell is a functional object, mark all cells whose
			references it holds as well. */

			muse_functional_object_t *obj = _fnobjdata(c);
			if ( obj && obj->type_info->mark )
			{
				obj->type_info->mark(env,obj);
			}
		}
	}
}

static void mark_stack( muse_env *env, muse_stack *stack )
{
	muse_cell *bottom = stack->bottom;
	muse_cell *top = stack->top;
	
	while ( bottom < top )
		muse_mark( env, *bottom++ );
}

static void free_text( muse_env *env, muse_cell t )
{
	if ( t )
	{
		muse_text_cell *c = &_ptr(t)->text;
		if ( c->start )
			free( c->start );
		c->start = c->end = NULL;
	}
}

void free_unused_specials( muse_env *env, muse_cell *specials )
{
	muse_cell *cp = specials;
	muse_cell c = *cp;
	
	while ( c )
	{
		muse_cell s = _head(c);

		if ( _ismarked( s ) )
		{
			/* Leave special untouched. */
			_mark(c);
			cp = &_ptr(c)->cons.tail;
			c = *cp;
		}
		else
		{
			/* 
				Release the special. 
				For a text cell, we need to release the memory allocated for the string.
				If a nativefn is in the specials list, it means that it needs to be
				executed in this phase. This can help take care of running destructors
				for objects that are really allocated in native-c code.
			 */
			switch ( _cellt(s) )
			{
				case MUSE_TEXT_CELL			: free_text(env,s); break;
				case MUSE_NATIVEFN_CELL		: 
					{
						muse_functional_object_t *data = _fnobjdata(s);
						if ( data )
						{
							muse_destroy_object( env, data );
						}
						else
							_apply( s, MUSE_NIL, MUSE_FALSE );
					}
					break;
				
				default:;
			}
			
			_step(&c);
			*cp = c;
		}
	}
}

void collect_free_cells( muse_env *env, muse_heap *heap )
{
	muse_cell f = MUSE_NIL;
	int marks_size = heap->size_cells;
	unsigned char *marks = heap->marks;
	int i, j, fcount;
	
	/* The nil cell is never freed. */
	_mark(MUSE_NIL);
	
	for ( i = 0, fcount = 0; i < marks_size; )
	{
		if ( !marks[i>>3] )
		{
			/* 
			   Grab 8 cells into the free list at one shot. 
			   This is a significant optimization since the
			   mark bits don't need to be checked in the
			   inner loop.
			*/
			muse_cell_data *p = _ptr(_cellati(i));
			muse_cell_data *p_end = p + 7;
			for ( j = i; p < p_end; ++p, ++i )
			{
				p->cons.head = MUSE_NIL;
				p->cons.tail = _cellati(i+1);
			}
			
			p_end->cons.head = MUSE_NIL;
			p_end->cons.tail = f;
			f = _cellati(j);
			fcount += 8;
			++i;
		}
		else if ( marks[i>>3] == 0xFF )
		{
			// Opportunity to skip 8 marked cells.
			i += 8;
		}
		else
		{
			/* Check each cell and add to free list if it is not marked. */
			for ( j = i + 8; i < j; ++i )
			{
				muse_cell c = _cellati(i);
				
				if ( !_ismarked(c) )
				{
					_ptr(c)->cons.head = MUSE_NIL;
					_ptr(c)->cons.tail = f;
					f = c;
					++fcount;
				}
			}
		}
	}
	
	heap->free_cell_count = fcount;
	heap->free_cells = f;
}


void muse_gc_impl( muse_env *env, int free_cells_needed );

/**
 * Collects all the garbage cells - i.e. unreferenced
 * cells. Cells referenced on the various stacks 
 * including the symbol stack are not collected.
 * \c _cons() automaticallly invokes this function
 * when it cannot take a cell from the free list.
 * 
 * @param free_cells_needed Tells the garbage collector
 * to ensure that these many free cells are available 
 * after collection. If simply collecting unreferenced
 * cells is insufficient, the collector will grow the heap
 * by a sufficient amount. If this parameter is <= 0,
 * it means muse environment is being destroyed and 
 * the gc call simply destroys all the specials.
 */
MUSEAPI void muse_gc( muse_env *env, int free_cells_needed )
{
	MUSE_DIAGNOSTICS3({
		fprintf(stderr, "Gc...");
		fflush(stderr);
	});

	{
#if MUSE_DIAGNOSTICS_LEVEL >= 3
		muse_int time_taken;
		void *timer = muse_tick();
#endif

		env->collecting_garbage = MUSE_TRUE;
		enter_atomic(env);
		muse_gc_impl( env, free_cells_needed );
		leave_atomic(env);
		env->collecting_garbage = MUSE_FALSE;

		MUSE_DIAGNOSTICS3({
			time_taken = muse_tock(timer);
			fprintf(stderr, "done. (free cells = %d)\n", _heap()->free_cell_count);		
			fprintf( stderr, "(time taken = " MUSE_FMT_INT " microseconds)\n", time_taken );
			fflush( stderr );
		});
	}
}

/**
 * You can call this to know whether the garbage collection
 * is in progress. This is useful inside destructor 
 * functions to decide whether to do destruction or some other
 * operation.
 */
MUSEAPI muse_boolean muse_doing_gc( muse_env *env )
{
	return env->collecting_garbage;
}

/**
 * Copies the marks to the keep vector so that
 * whatever needs to  survive garbage collection
 * is preserved.
 */
static void keep_marks( muse_heap *heap )
{
	memcpy( heap->keep, heap->marks, heap->size_cells >> 3 );
}

/**
 * Copies the keep vector to the mark vector
 * so that the old values of the mark vector
 * are restored. Note that after this call,
 * the keep vector is considered to be garbage.
 */
static void mark_keep( muse_heap *heap )
{
	/* Simply swap the two pointers. */
	unsigned char *temp = heap->keep;
	heap->keep = heap->marks;
	heap->marks = temp;
}

/**
 * Marks all cells in the heap as unmarked.
 * Used only in muse_destroy_env when everything
 * needs to go.
 */
static void unmark_all_cells( muse_heap *heap )
{
	memset( heap->marks, 0, heap->size_cells >> 3 );
	memset( heap->keep, 0, heap->size_cells >> 3 );
}

void muse_gc_impl( muse_env *env, int free_cells_needed )
{
	muse_heap *heap = _heap();
	
	if ( free_cells_needed <= 0 || heap->free_cells == MUSE_NIL || heap->free_cell_count < free_cells_needed * 2 )
	{
		/* We need to gc. */
		
		if ( free_cells_needed > 0 )
		{
			// If the process is in an atomic block, don't do GC,
			// but simply grow the heap by the necessary amount.
			if ( env->current_process->atomicity == 1 )
			{
				/* 1. Save the current mark vector. */
				keep_marks( heap );
				
				_mark(0);

				/* 2. Mark all symbols and their values and plists. */
				mark_stack( env, _symstack() );
				
				/* 3. Mark references held by every process. */
				{
					muse_process_frame_t *cp = env->current_process;
					muse_process_frame_t *p = cp;

					do 
					{
						mark_process(p);
						p = p->next;
					}
					while ( p != cp );
				}

				/* 4. Go through the specials list and release 
					  everything that isn't referenced. */
				free_unused_specials( env, &env->specials );
				
				/* 5. Collect whatever is unmarked into the free list. */
				collect_free_cells( env, _heap() );

				/* 6. Restore the mark vector to the marks for the
				cells that must survive gc. */
				mark_keep( heap );
			}
			
			if ( heap->free_cell_count < (100 - env->parameters[MUSE_GROW_HEAP_THRESHOLD]) * heap->size_cells / 100 )
			{
				/* We're still too close to the edge here. Allocate 
				   enough memory. */
				int new_size = heap->size_cells;
				int opt_size = 2 * (heap->size_cells - heap->free_cell_count + free_cells_needed);
				while ( new_size < opt_size )
					new_size *= 2;
				
				grow_heap( heap, new_size );
			}
		}
		else
		{
			/* Go through the specials list and release 
			everything that isn't referenced. We have to release
			everythign when shutting down. free_cells_needed <= 0
			indicates that we're shutting down. */

			unmark_all_cells( heap );
			_mark( process_id(env->current_process) );
			free_unused_specials( env, &env->specials );
		}
	}
}

static muse_functional_object_t *muse_create_object( muse_env *env, muse_functional_object_type_t *type_info )
{
	muse_assert( type_info && type_info->magic_word == 'muSE' );
	muse_assert( type_info->size >= sizeof(muse_functional_object_t) );

	{
		muse_functional_object_t *obj = (muse_functional_object_t*)calloc( 1, type_info->size );
		obj->magic_word		= 'muSE';
		obj->type_info		= type_info;
		return obj;
	}
}

static muse_functional_object_t *muse_init_object( muse_env *env, muse_functional_object_t *obj, muse_cell init_args )
{
	if ( obj->type_info->init )
		obj->type_info->init( env, obj, init_args );
	return obj;
}

/**
 * Just creates an object without allocating a cell for it.
 * It is the caller's responsibility to call muse_destroy_object()
 * once she's done with it.
 */
MUSEAPI muse_functional_object_t *muse_create_and_init_object( muse_env *env, muse_functional_object_type_t *type_info, muse_cell init_args )
{
	return muse_init_object( env, muse_create_object( env, type_info ), init_args );
}

/**
 * Counterpart of muse_create_and_init_object.
 */
MUSEAPI void muse_destroy_object( muse_env *env, muse_functional_object_t *obj )
{
	if ( obj ) {
		if ( obj->type_info->destroy )
			obj->type_info->destroy( env, obj );
		free(obj);
	}
}

/**
 * Creates a new functional object instance based on the given type
 * information. The type of the returned cell is a native function,
 * so that the object can be used in the function position.
 */
MUSEAPI muse_cell muse_mk_functional_object( muse_env *env, muse_functional_object_type_t *type_info, muse_cell init_args )
{
	muse_functional_object_t *obj = muse_create_object( env, type_info );
	muse_cell fn = _mk_nativefn( obj->type_info->fn, obj );
	obj->self = fn;
	add_special(env,fn);
	muse_init_object( env, obj, init_args );
	return fn;
}

/**
 * Returns the data pointer of the functional object, or
 * NULL if the object is not a functional object.
 */
MUSEAPI muse_functional_object_t *muse_functional_object_data( muse_env *env, muse_cell fobj, int type_word )
{
	muse_functional_object_t *obj = _fnobjdata(fobj);
	
	if ( obj && (!type_word || type_word == obj->type_info->type_word) )
		return obj;
	else
		return NULL;
}

/**
 * @addtogroup Processes
 */
/*@{*/
/**
 * @defgroup Processes_impl Internals
 *
 * A muSE environment contains a circular list of processes called
 * the "process ring" which it cycles through, spending a bit of
 * attention on each process before switching to the next. Therefore
 * a process that is not in the ring never gets run and one in the
 * ring will always get a chance to run at some point as long as
 * no process executes infinitely blocking code.
 *
 * A process's main task is to repeatedly evalaute a thunk until the thunk
 * evaluates to a non-nil value, which is the thunk's way of saying "I'm done".
 * This allows one to run server processes.
 *
 * Each process gets its own set of stacks (including a C-stack) and its own
 * namespace so symbol definitions made within a process do not affect
 * symbol definitions in other processes.
 */
/*@{*/

/**
 * Creates a new process and returns a pointer to the frame of the newly created process.
 * The new process is initially in the "paused" state. It will not run initially because
 * it is not yet part of the process ring - which is a circular list of processes
 * that the evaluator cycles through in order. Call prime_process() to place a process
 * in the ring.
 *
 * @param env The environment in which to create the new process.
 * @param attention The number of reductions that this process can perform before
 *					the evaluator switches to the next process.
 * @param thunk The created process is setup to evaluate this thunk in a loop until
 *				it evaluates to a non-nil value.
 * @param sp You can optionally supply a C stack pointer that points to the base of
 *			 the stack of the newly created process. When creating the main process
 *			 structure, this pointer must be NULL.
 */
muse_process_frame_t *create_process( muse_env *env, int attention, muse_cell thunk, void *sp )
{
	muse_process_frame_t *p = (muse_process_frame_t*)calloc( 1, sizeof(muse_process_frame_t) );

	muse_assert( attention > 0 );

	p->env						= env;
	p->attention				= attention;
	p->remaining_attention		= attention;
	p->state_bits				= MUSE_PROCESS_PAUSED;
	p->thunk					= thunk;
	
	/* Create all the stacks. */
	init_stack( &p->stack,			env->parameters[MUSE_STACK_SIZE]		);
	init_stack( &p->bindings_stack, env->parameters[MUSE_STACK_SIZE] * 2	);
		/**< 
		 * We need a x2 in the size for the bindings stack 
		 * above because the bindings stack is an array of 
		 * symbol-value pairs.
		 */
	init_stack( &p->locals,			env->parameters[MUSE_MAX_SYMBOLS]		);

	/* Create the trace info. */
	p->traceinfo.size = 32;
	p->traceinfo.depth = 0;
	p->traceinfo.data = (muse_trace_t*)calloc( p->traceinfo.size, sizeof(muse_trace_t) );

	if ( sp == NULL )
	{
		/* This is not the main process. Create a stack frame. 
		The cstack frame is different from the other stack frame
		in that it grows down. So top is the place the SP has to
		jump to when entering the process and it'll be decremented
		as more items gets pushed onto it. */
		init_stack( &p->cstack, env->parameters[MUSE_STACK_SIZE] );
		p->cstack.top = p->cstack.bottom + p->cstack.size - 4;
	}
	else
	{
		/* Its the main process. We just use the main stack frame as is. */
		p->cstack.top = (muse_cell*)sp;
	}

	/* Initialize the queue pointers. */
	p->next = p->prev = p;

	/* Initialize the recent items. */
	muse_init_recent( &(p->recent), 2, 16 );

	/* Copy all the currently defined symbols over to the new process. */
	if ( env->current_process )
	{
		memcpy( p->locals.bottom, env->current_process->locals.bottom, sizeof(muse_cell) * env->num_symbols );

		/* Also set the current_port settings to stdin/out/err. */
		p->current_port[MUSE_STDIN_PORT] = muse_stdport( env, MUSE_STDIN_PORT );
		p->current_port[MUSE_STDOUT_PORT] = muse_stdport( env, MUSE_STDOUT_PORT );
		p->current_port[MUSE_STDERR_PORT] = muse_stdport( env, MUSE_STDERR_PORT );
		p->current_port[MUSE_INPUT_PORT] = p->current_port[MUSE_STDIN_PORT];
	}

	return p;
}

/**
 * A process's mailbox is a linked list whose head gives the pid of the process.
 * Messages are added to the mailbox at the tail of this linked list and popped
 * off for processing at the head.
 */
muse_process_frame_t *init_process_mailbox( muse_process_frame_t *p )
{
	/* Messages get appended at the end of the mailbox
	and popped off at the beginning. */
	muse_env *env = p->env;
	int sp = _spos();
	p->mailbox = _cons( _mk_destructor( (muse_nativefn_t)fn_pid, p ), MUSE_NIL );
	p->mailbox_end = p->mailbox;
	_unwind(sp);
	return p;
}

/**
 * A function to wrap getting the process ID of a process from the
 * process's frame structure. The process id is to be found at the
 * head of the given process's mailbox.
 */
muse_cell process_id( muse_process_frame_t *p )
{
	muse_env *env = p->env;
	return _head(p->mailbox);
}

/**
 * Should be called after create_process to get it rolling.
 * A newly created process (using create_process()) is not part of the 
 * process ring until it is "primed" using this function. A "primed" process
 * is initially in the "virgin" state. When the process is first switched to
 * in switch_to_process(), it comes alive.
 */
muse_boolean prime_process( muse_process_frame_t *process )
{
	muse_env *env = process->env;

	if ( env->current_process && env->current_process != process )
	{
		muse_assert( env->current_process->next != process );

		/* Insert the process into the circular queue of processes,
		right after the current process. */
		{
			muse_process_frame_t *temp	= env->current_process->next;
			env->current_process->next	= process;
			process->next				= temp;
			process->prev				= env->current_process;
			temp->prev					= process;
		}
	}

	/* The process is being primed for first run.
	Just save the current state. */
	process->state_bits = MUSE_PROCESS_VIRGIN;

	return MUSE_TRUE;
}

static muse_env *g_env = NULL;

/**
 * Evaluates a process's thunk in a loop until the thunk 
 * evaluates to a non-nil value. Once the thunk completes
 * evaluation, the process dies and evaluation switches to
 * the next process.
 */
muse_boolean run_process()
{
	muse_env *env = g_env;
	
	if ( env->current_process->cstack.size > 0 && env->current_process->thunk )
	{
		/* Evaluate the thunk. Since we now support tail recursion,
		a server process can be expressed as an infinite loop, so we don't
		need a special looping capability implemented here. */
		_apply( env->current_process->thunk, MUSE_NIL, MUSE_TRUE );

		/* Process completed. Switch to the next one. */
		return remove_process( env->current_process );
	}
	else
		return MUSE_TRUE;
}

/**
 * Immediately switches attention to the given process. If the given
 * process is in the "virgin" state, run_process() is called on it.
 * If the process is in the "paused" state, it checks whether the
 * pause has timed out and if so switches to the process. If the process
 * is still waiting, then it tries all processes in the process ring
 * before giving the process another chance. 
 *
 * Note that switch_to_process() will always return \c MUSE_TRUE,
 * - i.e. it will either block indefinitely or successfuly switch to the
 * given process.
 */
muse_boolean switch_to_process( muse_env *env, muse_process_frame_t *process )
{
SWITCH_TO_PROCESS:
	if ( env->current_process == process )
	{
		/* We reach here only on deadlock conditions. However,
		there may be a timeout in effect, in which case we shouldn't
		use up CPU time just looping and should sleep like a good
		citizen. */
		if ( process->state_bits & MUSE_PROCESS_WAITING )
			muse_sleep( 1000 );
		else
			return MUSE_TRUE;
	}

	if ( process->state_bits & (MUSE_PROCESS_RUNNING | MUSE_PROCESS_VIRGIN) )
	{
		/* The process is running. Save current process state
		and switch to the given process. */

		if ( env->current_process->state_bits == MUSE_PROCESS_DEAD || setjmp( env->current_process->jmp ) == 0 )
		{
			env->current_process = process;

			if ( env->current_process->state_bits & MUSE_PROCESS_VIRGIN )
			{
				env->current_process->state_bits = MUSE_PROCESS_RUNNING;

				/* Change the SP for the virgin process. */
				g_env = env;
				CHANGE_STACK_POINTER(env->current_process->cstack.top);

				return run_process();
			}
			else
				longjmp( env->current_process->jmp, 1 );
		} 

		return MUSE_TRUE;
	}

	if ( process->state_bits & MUSE_PROCESS_WAITING )
	{
		/* If process is waiting for a message and it got one, the
		fn_pid would have activated the process already. If it is still
		waiting, check the timeout value in order to resume the process. */

		if ( process->state_bits & MUSE_PROCESS_HAS_TIMEOUT )
		{
			/* Check timeout. */
			muse_int elapsed_us = muse_elapsed_us(env->timer);
			if ( elapsed_us >= process->timeout_us )
				process->state_bits = MUSE_PROCESS_RUNNING;
		}

		if ( process->state_bits == MUSE_PROCESS_RUNNING && env->current_process != process )
			goto SWITCH_TO_PROCESS;
	}

	/* Can't run this one. Try the next process. */
	process = process->next;
	goto SWITCH_TO_PROCESS;
}

/**
 * Goes around the processes list and returns to the same point
 * after one round-robin cycle. procrastinate() will not 
 * switch to the next process if an atomic operation is going on.
 */
muse_boolean procrastinate( muse_env *env )
{
	if ( env->current_process->atomicity == 0 )
		return switch_to_process( env, env->current_process->next );
	else
		return MUSE_TRUE;
}

/**
 * Used by muse_apply() to decrement the attention spent on the
 * current process.
 */
void yield_process( muse_env *env, int spent_attention )
{
	muse_process_frame_t *p = env->current_process;
	
	if ( p == p->next ) {
		if ( p->num_eval_timeouts == 0 ) return;
		check_timeout(env);
		return;
	}

	if ( p->atomicity == 0 )
	{
		/* Not in an atomic block. So check remaining attention. */
		if ( p->remaining_attention <= 0 )
		{
			/* Give time to the next process. */
			if ( p->num_eval_timeouts > 0 ) check_timeout(env);
			p->remaining_attention = p->attention;
			switch_to_process( env, p->next );
		}
		else
			p->remaining_attention -= spent_attention;
	}
}

/**
 * Returns \c MUSE_TRUE if the current process is the main process
 * and \c MUSE_FALSE otherwise.
 */
muse_boolean is_main_process( muse_env *env )
{
	return (env->current_process->cstack.size > 0) ? MUSE_FALSE : MUSE_TRUE;
}

/**
 * Removes the given process from the process ring and marks it
 * as "dead". It then switches to the next process in the ring so that
 * the ring can continue.
 */
muse_boolean remove_process( muse_process_frame_t *process )
{
	muse_env *env = process->env;

	/* First take the process out of the ring. */
	muse_process_frame_t 
		*prev = process->prev,
		*next = process->next;

	if ( next && next != process )
	{
		muse_assert( prev != process );
		next->prev = prev;
	}

	if ( prev && prev != process )
	{
		muse_assert( next != process );
		prev->next = next;
	}

	process->state_bits = MUSE_PROCESS_DEAD;
	process->next = process->prev = NULL;

	if ( env->current_process == process )
	{
		if ( process == next )
		{
			muse_assert( !"All processes exited!" );
			exit(0); /* All processes exited! */
		}
		else
			return switch_to_process( env, next );
	}
	else
		return MUSE_TRUE;
}

/**
 * Used to send asynchronous messages to a process and serves
 * as a process's identifier. Suppose the message sending
 * expression has the form @code (p x1 x2 x3 .. xN) @endcode,
 * where \c p is the id of the target process, the target process
 * will receive the message in a similar form 
 * @code (q x1 x2 x3 ... xN) @endcode where \c q is the id of the
 * sending process. This way the received message has information 
 * about the \b sending process and therefore the receiving process
 * can reply to the sending process. Therefore, a process can
 * receive and process message using pattern matching \ref syntax_case "case" 
 * expression.
 *
 * @code
 * (case (receive)
 *    ((pid . msg-pattern-1) expr-1)
 *    ((pid . msg-pattern-2) expr-2)
 *    ...
 *    )
 * @endcode
 *
 * It is a useful convention to use a symbolic constant at the head of
 * a message so that it can be explicitly pattern matched against for dispatch.
 * For example -
 * @code
 * (case (receive)
 *   ((pid 'MsgType1 . args) expr-1)
 *   ((pid 'MsgType2 . args) expr-2)
 *   ...
 *   )
 * @endcode
 */
muse_cell fn_pid( muse_env *env, muse_process_frame_t *p, muse_cell args )
{
	int sp = _spos();

	/* The PID of a process is stored in the head of the mailbox list.
	The tail of the mailbox list consists of the message queue. */
	if ( args && p->state_bits != MUSE_PROCESS_DEAD )
	{
		/* The list of arguments is the message in its full structure.
		To this, we prepend the pid of the sending process and append
		the result to the message queue of our process. */
		muse_cell msg = _cons( process_id(env->current_process), muse_eval_list(env,args) );

		post_message( p, msg );

		_unwind(sp);

		return _builtin_symbol( MUSE_T );
	}

	if ( muse_doing_gc(env) && p->state_bits == MUSE_PROCESS_DEAD )
	{
		/* Cleanup process memory. */
		free_process(p);
	}

	return MUSE_NIL;
}

/**
 * Marks all references held by the given process. Called prior to
 * garbage collection.
 */
void mark_process( muse_process_frame_t *p )
{
	muse_env *env = p->env;
	mark_stack( env, &p->stack );
	mark_stack( env, &p->bindings_stack );
	mark_stack( env, &p->locals );
	muse_mark( env, p->thunk );
	muse_mark( env, p->mailbox );
	muse_mark_recent( env, &(p->recent) );
}

/**
 * Frees process data structure memory.
 */
void free_process( muse_process_frame_t *p )
{
	muse_clear_recent( &(p->recent) );
	destroy_stack( &p->locals );
	destroy_stack( &p->bindings_stack );
	destroy_stack( &p->stack );
	free(p->traceinfo.data);
	p->traceinfo.data = NULL;
	p->traceinfo.size = p->traceinfo.depth = 0;
	free(p);
}

/**
 * Enters an atomic block that should be evaluated
 * without switching to another process.
 * @see leave_atomic()
 */
void enter_atomic(muse_env *env)
{
	env->current_process->atomicity++;
}

/**
 * Leaves the current atomic block that should be evaluated
 * without switching to another process. Atomic blocks can be
 * nested, but must always be matched. Use the syntax_atomic
 * muSE function to mark an atomic do block.
 * @see enter_atomic()
 */
void leave_atomic(muse_env *env)
{
	env->current_process->atomicity--;
}

/**
 * Appends the given message (which, in general, should include the 
 * sending process's pid at the head) to the mailbox of the given 
 * process. 
 *
 * @see fn_post
 */
void post_message( muse_process_frame_t *p, muse_cell msg )
{
	muse_env *env = p->env;

	muse_cell msg_entry = _cons( msg, MUSE_NIL );

	_sett( p->mailbox_end, msg_entry );

	p->mailbox_end = msg_entry;

	if ( p->state_bits & MUSE_PROCESS_WAITING )
	{
		if ( !(p->waiting_for_pid) || p->waiting_for_pid == process_id(env->current_process) )
			p->state_bits = MUSE_PROCESS_RUNNING;
	}
}

typedef struct _timeout_info_t
{
	struct _timeout_info_t *prev;
	muse_cell prevcell;
	muse_cell id;
	void *timer;
	muse_int timeout_us;
} timeout_info_t;

static muse_cell fn_timeout_var( muse_env *env, timeout_info_t *ti, muse_cell args )
{
	if ( muse_doing_gc(env) )
	{
		muse_tock(ti->timer);
		free(ti);
	}

	return MUSE_NIL;
}

/**
 * Adds another level of timeout nesting.
 */
void push_timeout( muse_env *env, muse_cell id, muse_int timeout_us )
{
	int sp = _spos();
	muse_cell sym = _builtin_symbol(MUSE_TIMEOUTVAR);
	muse_cell curr_timeout = _symval(sym);
	
	timeout_info_t *next_timeout = (timeout_info_t*)calloc( 1, sizeof(timeout_info_t) );
	if ( curr_timeout != sym )
	{
		next_timeout->prev = (timeout_info_t*)muse_nativefn_context( env, curr_timeout, NULL );
	}

	next_timeout->prevcell		= curr_timeout;
	next_timeout->id			= id;
	next_timeout->timer			= muse_tick();
	next_timeout->timeout_us	= timeout_us;

	_pushdef( _builtin_symbol(MUSE_TIMEOUTVAR), muse_mk_destructor( env, (muse_nativefn_t)fn_timeout_var, next_timeout ) );
	_unwind(sp);

	env->current_process->num_eval_timeouts++;
}

/**
 * Checks whether any timeout has expired
 * and if so raises an exception to the effect.
 * The structure of the exception is -
 *		('timeout 'id given-us elapsed-us)
 */
void check_timeout( muse_env *env )
{
	int sp = _spos();
	muse_cell sym = _builtin_symbol(MUSE_TIMEOUTVAR);
	muse_cell ticell = _symval(sym);
	if ( ticell != sym ) {
		timeout_info_t *ti = (timeout_info_t*)(timeout_info_t*)muse_nativefn_context( env, ticell, NULL );
		while ( ti != NULL ) {
			muse_int elapsed_us = muse_elapsed_us(ti->timer);
			if ( elapsed_us >= ti->timeout_us ) {
				// Timed out.
				int bsp = _bspos();
				_pushdef( sym, ti->prevcell );
				muse_raise_error( env, _builtin_symbol(MUSE_TIMEOUT), muse_list( env, "cII", ti->id, ti->timeout_us, elapsed_us ) );
				_unwind_bindings(bsp);

				// Disable the current timeout on resume.
				_define( sym, ti->prevcell );
			}

			ti = ti->prev;
		}
	}
	_unwind(sp);
}

/**
 * Initializes the scoped recent calculations data structure.
 */
void muse_init_recent( recent_t *r, int capacity, int depth )
{
	r->entries.capacity = capacity;
	r->entries.top = 0;
	r->entries.vec = (recent_entry_t*)calloc( sizeof(recent_entry_t), r->entries.capacity );
	
	r->contexts.capacity = depth;
	r->contexts.top = 0;
	r->contexts.vec = (recent_context_t*)calloc( sizeof(recent_context_t), r->contexts.capacity );
}

/**
 * Frees the scoped recent calculations data structure.
 */
void muse_clear_recent( recent_t *r )
{
	free( r->entries.vec );
	free( r->contexts.vec );
	r->entries.capacity = r->entries.top = 0;
	r->contexts.capacity = r->contexts.top = 0;
}

/**
 * Makes a copy of all the recent info upto
 * the present execution point.
 */
recent_t muse_copy_recent( recent_t *r )
{
	recent_t cpy;
	int ne = r->entries.top + 1;
	int nc = r->contexts.top + 1;
	muse_init_recent( &cpy, ne, nc );
	memcpy( cpy.entries.vec, r->entries.vec, sizeof(recent_entry_t) * ne );
	memcpy( cpy.contexts.vec, r->contexts.vec, sizeof(recent_context_t) * nc );
	cpy.entries.top = r->entries.top;
	cpy.contexts.top = r->contexts.top;
	return cpy;
}

/**
 * Takes a saved "recent" data structure and restores
 * it into the given destination.
 */
void muse_restore_recent( recent_t *r, recent_t *dest )
{
	muse_clear_recent(dest);
	(*dest) = muse_copy_recent(r);
}

/**
 * Marks all objects remembered in the given recent-scope.
 */
void muse_mark_recent_context( muse_env *env, recent_t *r, int ctxt )
{
	int base = r->contexts.vec[ctxt].base;
	int top = r->contexts.vec[ctxt].top;
	
	int j = 0;
	for ( j = base; j < top; ++j ) {
		muse_mark( env, r->entries.vec[j].value );
	}
}

/**
 * Marks all objects in the recent data structure.
 */
void muse_mark_recent( muse_env *env, recent_t *r )
{
	int i;
	for ( i = 0; i <= r->entries.top; ++i ) {
		muse_mark( env, r->entries.vec[i].value );
	}
}

/**
 * 8 recent items are stored indexed by a 64-bit key.
 * You can look up a recent item by giving your key.
 */
muse_boolean muse_find_recent_item( muse_env *env, muse_int key, muse_cell *value )
{
	muse_process_frame_t *p = env->current_process;
	recent_t *r = &(p->recent);
	recent_context_t *s = r->contexts.vec + r->contexts.top;
	int i = 0, n = 0;
	
	while ( s >= r->contexts.vec && n < MUSE_MAX_RECENT_ITEMS ) {
	
		for ( i = s->depth - 1; i >= 0 && n < MUSE_MAX_RECENT_ITEMS; --i, ++n ) {
			recent_entry_t *e = r->entries.vec + (s->base + i % MUSE_MAX_RECENT_ITEMS);
			if ( e->key == key ) {
				(*value) = e->value;
				return MUSE_TRUE;
			}
		}
		
		if ( s->prev < s->base ) {
			muse_assert( s-1 >= r->contexts.vec );
			--s;
		} else
			break;
	}
	
	return MUSE_FALSE;
}

/**
 * Locates the most recent lazy item in the recents list.
 */
recent_entry_t *muse_find_recent_lazy_item( muse_env *env )
{
	muse_process_frame_t *p = env->current_process;
	recent_t *r = &(p->recent);
	recent_context_t *s = r->contexts.vec + r->contexts.top;
	int i = 0, n = 0;
	
	while ( s >= r->contexts.vec && n < MUSE_MAX_RECENT_ITEMS ) {
	
		for ( i = s->depth - 1; i >= 0 && n < MUSE_MAX_RECENT_ITEMS; --i, ++n ) {
			recent_entry_t *e = r->entries.vec + (s->base + i % MUSE_MAX_RECENT_ITEMS);
			if ( _cellt(e->value) == MUSE_LAZY_CELL )
				return e;
		}
		
		if ( s->prev < s->base ) {
			muse_assert( s-1 >= r->contexts.vec );
			--s;
		} else
			break;
	}
	
	return NULL;
}

/**
 * You can add a new "recent" item using this function.
 * Only the 8 most recent items are kept. The return value
 * is \p value itself. 
 */
muse_cell muse_add_recent_item( muse_env *env, muse_int key, muse_cell value )
{
	if ( key ) {
		muse_process_frame_t *p = env->current_process;
		recent_t *r = &(p->recent);
		recent_context_t *rc = r->contexts.vec + r->contexts.top;
		int i = rc->base + rc->depth % MUSE_MAX_RECENT_ITEMS;
		
		if ( i >= r->entries.capacity ) {
			int newcaps = r->entries.capacity * 2;
			r->entries.vec = (recent_entry_t*)realloc( r->entries.vec, sizeof(recent_entry_t) * newcaps );
			r->entries.capacity = newcaps;
		}
		
		r->entries.vec[i].key = key;
		r->entries.vec[i].value = value;
		
		rc->depth++;
		rc->top = r->entries.top = rc->base + (rc->depth > MUSE_MAX_RECENT_ITEMS ? MUSE_MAX_RECENT_ITEMS : rc->depth);
			/**< r->entries.top always refers to the absolute top of the recent list. */
	}
	return value;
}

/**
 * Moves the most recent item added to the recents list
 * to the end of the list.
 */
void muse_withdraw_recent_item( muse_env *env )
{
	muse_process_frame_t *p = env->current_process;
	recent_t *r = &(p->recent);
	recent_context_t *rc = r->contexts.vec + r->contexts.top;
	
	if ( rc->depth > 0 ) {
		rc->depth--;
		rc->top = rc->base + (rc->depth > MUSE_MAX_RECENT_ITEMS ? MUSE_MAX_RECENT_ITEMS : rc->depth);
	}
}

static recent_context_t *muse_push_recent_scope_base( muse_env *env )
{
	muse_process_frame_t *p = env->current_process;
	recent_t *r = &(p->recent);
	
	if ( r->contexts.top + 1 >= r->contexts.capacity ) {
		int newcaps = r->contexts.capacity * 2;
		r->contexts.vec = (recent_context_t*)realloc( r->contexts.vec, sizeof(recent_context_t) * newcaps );
		r->contexts.capacity = newcaps;
	}

	r->contexts.top++;
	
	{
		recent_context_t *rc = r->contexts.vec + r->contexts.top;
		rc->base = rc[-1].top;
		rc->prev = rc->base;
		rc->top = rc->base;
		rc->depth = 0;
		return rc;
	}
}

/**
 * Enter a scope so that previous recent values are temporarily forgotten.
 */
void muse_push_recent_scope( muse_env *env )
{
	muse_push_recent_scope_base(env);
}

void muse_push_copy_recent_scope( muse_env *env )
{
	recent_context_t *rc = muse_push_recent_scope_base(env);
	rc->prev = rc[-1].prev;
}

/**
 * Exit from the scope with a result so that the innards of a computation
 * are forgotten.
 */
muse_cell muse_pop_recent_scope( muse_env *env, muse_int key, muse_cell value )
{
	muse_process_frame_t *p = env->current_process;
	recent_t *r = &(p->recent);
	
	r->contexts.top--;

	muse_assert( r->contexts.top >= 0 );

	return muse_add_recent_item( env, key, value );
}

/**
 * Saves the current value of it and reset it to "it".
 * Returns the bindings stack pos before saving it.
 */
int muse_forget_it_temporarily( muse_env *env )
{
	int bp = _bspos();
	_pushdef( _builtin_symbol(MUSE_IT), _builtin_symbol(MUSE_IT) );
	return bp;
}

/*@}*/
/*@}*/
