/**
 * @file muse_regexp.cpp
 * @author Srikumar K. S. (mailto:kumar@muvee.com)
 *
 * Copyright (c) 2006 Jointly owned by Srikumar K. S. and muvee Technologies Pte. Ltd.
 *
 * All rights reserved. See LICENSE.txt distributed with this source code
 * or http://muvee-symbolic-expressions.googlecode.com/svn/trunk/LICENSE.txt
 * for terms and conditions under which this software is provided to you.
 *
 * Implements functional vectors for constant time random access
 * to a collection of objects.
 */

#include "muse_builtins.h"
#include "muse_port.h"
#include <stdlib.h>
#include <memory.h>
#include <boost/xpressive/xpressive.hpp>

using namespace boost::xpressive;

typedef struct
{
	muse_functional_object_t base;
    muse_cell expr;
    wcregex *re;
} regexp_t;

static void regexp_init( muse_env *env, void *ptr, muse_cell args )
{
	regexp_t *re = (regexp_t*)ptr;
    re->expr = _evalnext(&args);
    muse_assert(_cellt(v->expr) == MUSE_TEXT_CELL);
    re->re = new wcregex;
    (*(re->re)) = wcregex::compile(_text_contents(re->expr, NULL));
}

static void regexp_mark( muse_env *env, void *ptr )
{
	regexp_t *re = (regexp_t*)ptr;
    muse_mark(env, re->expr);
}

static void regexp_destroy( muse_env *env, void *ptr )
{
    regexp_t *re = (regexp_t*)ptr;
    delete re->re;
    re->re = NULL;
}

/**
 * Writes out the regular expression to the given port in such a
 * way that the expression written out is converted
 * to a vector by a trusted read operation.
 */
static void regexp_write( muse_env *env, void *ptr, void *port )
{
	regexp_t *re = (regexp_t*)ptr;
	muse_port_t p = (muse_port_t)port;
	
    static const muse_char constructor[] = L"{regexp ";
    for (int i = 0; constructor[i]; ++i) {
        port_putchar(constructor[i], p);
    }
    muse_pwrite(p, re->expr);
    port_putchar('}', p);
}

static muse_cell fn_regexp( muse_env *env, regexp_t *re, muse_cell args ) {
    return regex_match(_text_contents(_evalnext(&args), NULL), (*(re->re))) ? _builtin_symbol(MUSE_T) : MUSE_NIL;
}

static muse_cell regexp_format( muse_env *env, void *self ) {
    regexp_t *re = (regexp_t*)self;
    return re->expr;
}

static muse_format_view_t g_regexp_format_view =
{
	regexp_format
};

static void *regexp_view( muse_env *env, int id ) {
    switch (id) {
        case 'frmt':    return &g_regexp_format_view;
        default:        return NULL;
    }
}

static muse_functional_object_type_t g_regexp_type =
{
	'muSE',
	'regx',
	sizeof(regexp_t),
	(muse_nativefn_t)fn_regexp,
	regexp_view,
	regexp_init,
	regexp_mark,
	regexp_destroy,
	regexp_write
};

// {regexp "pattern"}
static muse_cell fn_regexp_compile(muse_env *env, void *context, muse_cell args) {
    muse_cell regexp = _mk_functional_object(&g_regexp_type, args);
    return muse_add_recent_item(env, (muse_int)fn_regexp_compile, regexp);
}

// (regexp-match re input) -> vector
// Entire input must match.
static muse_cell fn_regexp_match(muse_env *env, void *context, muse_cell args) {
    muse_cell regexp = _evalnext(&args);
    regexp_t *re = (regexp_t*)_functional_object_data(regexp, 'regx');
    muse_cell txt = _evalnext(&args);
    wcmatch what;
    if (regex_match(_text_contents(txt, NULL), what, (*(re->re)))) {
        muse_cell vec = muse_mk_vector(env, what.size());
        int sp = _spos();
        {
            int i, N;
            for (i = 0, N = what.size(); i < N; ++i) {
                const wcsub_match str = what[i];
                muse_vector_put(env, vec, i, muse_mk_text(env, str.first, str.second));
                _unwind(sp);
            }
        }
        return muse_add_recent_item(env, (muse_int)fn_regexp_match, vec);
    } else {
        return muse_add_recent_item(env, (muse_int)fn_regexp_match, MUSE_NIL);
    }
}

// (regexp-search re input) -> vector
// Sub-string match.
static muse_cell fn_regexp_search(muse_env *env, void *context, muse_cell args) {
    muse_cell regexp = _evalnext(&args);
    regexp_t *re = (regexp_t*)_functional_object_data(regexp, 'regx');
    muse_cell txt = _evalnext(&args);
    wcmatch what;
    if (regex_search(_text_contents(txt, NULL), what, (*(re->re)))) {
        muse_cell vec = muse_mk_vector(env, what.size());
        int sp = _spos();
        {
            int i, N;
            for (i = 0, N = what.size(); i < N; ++i) {
                const wcsub_match str = what[i];
                muse_vector_put(env, vec, i, muse_mk_text(env, str.first, str.second));
                _unwind(sp);
            }
        }
        return muse_add_recent_item(env, (muse_int)fn_regexp_match, vec);
    } else {
        return muse_add_recent_item(env, (muse_int)fn_regexp_match, MUSE_NIL);
    }
}

// (regexp-replace-one re format input) -> text
// Replaces first occurrence of re in input with format.
static muse_cell fn_regexp_replace_one(muse_env *env, void *context, muse_cell args) {
    muse_cell regexp = _evalnext(&args);
    regexp_t *re = (regexp_t*)_functional_object_data(regexp, 'regx');
    muse_cell format = _evalnext(&args);
    muse_cell input = _evalnext(&args);
    
    std::wstring result = regex_replace(_text_contents(input, NULL), (*(re->re)), _text_contents(format, NULL), regex_constants::format_first_only);
    return muse_mk_text(env, result.c_str(), result.c_str() + result.length());
}

// (regexp-replace re format input) -> text
// Replaces all occurrence of re in input with format.
static muse_cell fn_regexp_replace(muse_env *env, void *context, muse_cell args) {
    muse_cell regexp = _evalnext(&args);
    regexp_t *re = (regexp_t*)_functional_object_data(regexp, 'regx');
    muse_cell format = _evalnext(&args);
    muse_cell input = _evalnext(&args);
    
    std::wstring result = regex_replace(_text_contents(input, NULL), (*(re->re)), _text_contents(format, NULL), regex_constants::format_all);
    return muse_mk_text(env, result.c_str(), result.c_str() + result.length());
}

static const struct regexp_fns_t { const muse_char *name; muse_nativefn_t fn; } g_regexp_fns[] =
{
	{	L"regexp",              fn_regexp_compile		},
	{	L"regexp-match",        fn_regexp_match         },
	{	L"regexp-search",       fn_regexp_search        },
    {   L"regexp-replace-one",  fn_regexp_replace_one   },
    {   L"regexp-replace",      fn_regexp_replace       },
	{	NULL,                   NULL                    },
};

extern "C" void muse_define_builtin_type_regexp(muse_env *env)
{
	int sp = _spos();
	const struct regexp_fns_t *fns = g_regexp_fns;
	for ( ; fns->name; ++fns )
	{
		_define( _csymbol(fns->name), _mk_nativefn( fns->fn, NULL ) );
		_unwind(sp);
	}
}
