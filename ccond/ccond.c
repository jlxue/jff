/*
 * Copyright (c) 2008, Liu Yubao.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   - Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials
 *     provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 * GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ccond.h"


#ifdef CCOND_DEBUG
#undef ccond_error
#undef find_restart
#endif


CCOND_TLS   HandlerBind*    _last_handler_bind;
CCOND_TLS   RestartCase*    _last_restart_case;


#ifdef  HAVE_EXEC_INFO_H
#include <unistd.h>
#include <execinfo.h>

void dump_backtrace(void)
{
    void* buffer[64];
    fprintf(stderr, "Backtrace:\n");
    backtrace_symbols_fd(buffer, backtrace(buffer, 64), STDOUT_FILENO);
}
#else
#define dump_backtrace()
#endif

#ifdef CCOND_DEBUG
void dump_handler_binds(void)
{
    HandlerBind* h;
    int n;

    if (NULL == _last_handler_bind)
        return;

    fprintf(stderr, "Handler binds:\n");
    h = _last_handler_bind;
    n = 0;
    do {
        HandlerPair* p = h->binds;
        int i = 1;

        fprintf(stderr, "  [%2d] at %s:%d (stack addr=%p)\n", n, h->file, h->line, h);
        while (NULL != p->name) {
            fprintf(stderr, "    %2d handler=%p name=%s\n", i, p->handler, p->name);
            ++p;
            ++i;
        }

        ++n;
        h = h->prev;
    } while (NULL != h);

    fprintf(stderr, "\n");
}

void dump_restart_cases(void)
{
    RestartCase* r;
    int n;

    if (NULL == _last_restart_case)
        return;

    fprintf(stderr, "Restart cases:\n");
    r = _last_restart_case;
    n = 0;
    do {
        const char** names = r->names;
        int i = 1;

        fprintf(stderr, "  [ %2d] at %s:%d (stack addr=%p)\n", n, r->file, r->line, r);
        while (NULL != *names) {
            fprintf(stderr, "    %2d name=%s\n", i, *names);
            ++names;
            ++i;
        }

        ++n;
        r = r->prev;
    } while (NULL != r);

    fprintf(stderr, "\n");
}
#endif

int ccond_init(void)
{
    _last_handler_bind = NULL;
    _last_restart_case = NULL;

    return 0;
}

void ccond_signal(Condition *c)
{
    HandlerBind* h = _last_handler_bind;

    assert(NULL != c);

    while (NULL != h) {
        HandlerPair* p = h->binds;

        CCOND_CHECK_GUARD(*h);
        while (NULL != p->name) {
            if (c->_name == p->name || 0 == strcmp(c->_name, p->name)) {
                (*p->handler)(c);
            }
            ++p;
        }

        h = h->prev;
    }
}

#ifdef CCOND_DEBUG
void ccond_error(Condition* c, const char* file, int line)
#else
void ccond_error(Condition* c)
#endif
{
    ccond_signal(c);

#ifdef CCOND_DEBUG
    fprintf(stderr, "No handler found to process condition \"%s\" at %s:%d\n",
            c->_name, file, line);
    dump_handler_binds();
#else
    fprintf(stderr, "No handler found to process condition \"%s\"\n", c->_name);
#endif

    dump_backtrace();
    abort();
}

#ifdef CCOND_DEBUG
RestartCase* find_restart(const char* name, const char* file, int line)
#else
RestartCase* find_restart(const char* name)
#endif
{
    RestartCase* r = _last_restart_case;

    assert(NULL != name);

    while (NULL != r) {
        const char** names = r->names;

        CCOND_CHECK_GUARD(*r);
        while (NULL != *names) {
            if (name == *names || 0 == strcmp(name, *names)) {
                r->val = (names - r->names) / sizeof(char*) + 1;
                return r;
            }
            ++names;
        }

        r = r->prev;
    }

#ifdef CCOND_DEBUG
    fprintf(stderr, "Can't find restart \"%s\" at %s:%d\n", name, file, line);
    dump_restart_cases();
#endif
    return NULL;
}

void invoke_restart(RestartCase* r, void* arg /* can't point into stack */)
#if CCOND_ENABLE_CXX_EXCEPTION
    throw(ccond_exception)
#endif
{
    /* No chance to signal CONTROL-ERROR condition. */
    if (NULL == r)
        dump_backtrace();
    assert(NULL != r);

    _last_restart_case = r->prev;
    while (! CCOND_STACK_CONTAIN(_last_handler_bind, r))
        _last_handler_bind = _last_handler_bind->prev;

    r->arg = arg;
#if CCOND_ENABLE_CXX_EXCEPTION
    throw ccond_exception(r);
#else
    CCOND_LONGJMP(r->env, r->val);
#endif
}

