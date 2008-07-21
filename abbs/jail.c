/* vi:set tw=72 ts=4 sw=4 et nowrap ft=c: */

/*
 * jail.c
 *
 *  restrict intercepted processes to call open(), opendir()
 *  and exec() family. Note this works only for dynamically
 *  linked programs.
 *
 * Usage:
 *
 * $ gcc -Wall -fPIC -c jail.c
 * $ gcc -shared -o libjail.so jail.o -ldl
 *
 * A more restricted vim:
 * $ LD_PRELOAD=`pwd`/libjail.so \
 *      _R_ALLOW_OPEN_READ="a.c;/usr/share/vim/*;/usr/share/terminfo/*;/etc/vim/*;$HOME/.vim*" \
 *      _R_ALLOW_OPEN_WRITE="a.c;/tmp/*;$HOME/.viminfo;$HOME/.viminfo.tmp" \
 *      _R_ALLOW_OPENDIR="/usr/share/vim/*;$HOME/.vim/*" \
 *      rvim a.c
 *
 * Author:
 *  Liu Yubao <yubao.liu@gmail.com>
 *
 * License:
 *  LGPL v2.1
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * ChangeLog:
 *  2008-07-17  Liu Yubao
 *      * derived from my encoding-wrapper.c, can intercept open(),
 *        open64(), creat(), creat64(), mkdir(), rmdir(), opendir(),
 *        execve()
 *      * remove duplicate code, make parse_env() more robust
 *
 */

#define _GNU_SOURCE

#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dirent.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <fnmatch.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "env.h"


#ifndef LIBC_NAME
#define LIBC_NAME           "libc.so.6"
#endif


#define LVALUE_FP(fp)   *(void**)(&fp)

#define LOOKUP_SYMBOL(func) \
    LVALUE_FP(real_##func) = dlsym(dl_handle, #func);   \
    DBG("real_" #func "=%p\n", real_##func);        \
    if ((error = dlerror()) != NULL) goto ERROR

#ifdef DEBUG
#define DBG(format, ...) fprintf(stderr, "[%s:%d] %s(): " format,   \
                    __FILE__, __LINE__, __func__, ##__VA_ARGS__)
#else
#define DBG(format, ...)
#endif

#define strlen(s) (NULL != (s) ? strlen(s) : 0)

#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)

/* --------------------global variables -------------------------*/
static void* dl_handle = NULL;

static char** patterns_open_read = NULL;
static char** patterns_open_write = NULL;
static char** patterns_mkdir = NULL;
static char** patterns_rmdir = NULL;
static char** patterns_opendir = NULL;
static char** patterns_execve = NULL;

static char* empty_patterns[] = { NULL };


static int (*real_creat)(const char *pathname, mode_t mode)     = NULL;
static int (*real_creat64)(const char *pathname, mode_t mode)   = NULL;
static int (*real_open)(const char *pathname, int flags, ...)   = NULL;
static int (*real_open64)(const char* pathname, int flags, ...) = NULL;
static int (*real_mkdir)(const char *pathname, mode_t mode)     = NULL;
static int (*real_rmdir)(const char *pathname)                  = NULL;
static DIR* (*real_opendir)(const char* name)                   = NULL;
static int (*real_execve)(const char *filename,
              char *const argv[], char *const envp[])           = NULL;
/* link, unlink, rename, remove ? */


/* --------------------utility functions-------------------------*/
static char** parse_env(const char* envname)
{
    int i, n, len;
    const char *env, *p, *p2;
    char** patterns;

    env = getenv(envname);
    DBG("env: %s=%s\n", envname, env);
    if (0 == strlen(env))
        return empty_patterns;

    p = env;
    n = 0;
    while (NULL != (p = strchr(p, ';'))) {
        ++n;
        ++p;
    }

    patterns = (char**)calloc(n + 2, sizeof(char*));
    if (NULL == patterns)
        return NULL;

    p = p2 = env;
    n = 0;
    while (NULL != (p = strchr(p2, ';'))) {
        len = p - p2;
        if (0 == len)
            continue;
        patterns[n] = strndup(p2, len);
        DBG("patterns[%d]=%s\n", n, patterns[n]);
        if (NULL == patterns[n])
            goto L_error;
        ++n;
        p2 = ++p;
    }
    if ('\0' != *p2) {
        patterns[n] = strdup(p2);
        DBG("patterns[%d]=%s\n", n, patterns[n]);
        if (NULL == patterns[n])
            goto L_error;
        ++n;
    }
    if (0 == n) {
        free(patterns);
        return empty_patterns;
    }

    return patterns;

L_error:
    for (i = 0; i < n; ++i)
        free(patterns[i]);
    free(patterns);
    return NULL;
}


static int check_patterns(const char* pathname, char** patterns)
{
    char** p = patterns;

    DBG("pathname=%s, patterns=%p\n", pathname, patterns);

    while (NULL != *p) {
        if (0 == fnmatch(*p, pathname, 0)) {
            DBG("match: %s, %s\n", *p, pathname);
            return 0;
        }
        ++p;
    }

    return -1;
}


static int check_perm(const char* pathname, const char* envname,
                      char*** patterns)
{
    if (NULL == *patterns) {
        *patterns = parse_env(envname);
        if (NULL == *patterns) {
            errno = ENOMEM;
            return -1;
        }
    }

    if (empty_patterns == *patterns || check_patterns(pathname, *patterns)) {
        errno = EACCES;
        return -1;
    }

    return 0;
}


/*---------------------load orignal functions-------------------- */

__attribute__((constructor)) void initialize(void)
{
    char* error;

    DBG("enter\n");

    dl_handle = dlopen(LIBC_NAME, RTLD_LAZY);
    if (!dl_handle) {
        fprintf(stderr, "dlopen() failes:%s\n", dlerror());
        exit(EXIT_FAILURE);
    }

    dlerror();      /* Clear any existing error. */

    LOOKUP_SYMBOL(creat);
    LOOKUP_SYMBOL(creat64);
    LOOKUP_SYMBOL(open);
    LOOKUP_SYMBOL(open64);
    LOOKUP_SYMBOL(mkdir);
    LOOKUP_SYMBOL(rmdir);
    LOOKUP_SYMBOL(opendir);

    DBG("leave\n");
    return;

ERROR:
    fprintf(stderr, "dlsym() fails:%s\n", error);
    exit(EXIT_FAILURE);
}

__attribute__((destructor)) void finalize(void)
{
    DBG("enter, dl_handle=%p\n", dl_handle);
    dlclose(dl_handle);
    DBG("leave\n");
}

/* -----------------------system calls --------------------------- */

int creat(const char *pathname, mode_t mode)
{
    DBG("pathname=%s, mode=%d\n", pathname, mode);

    if (check_perm(pathname, RESTRICTED_ALLOW_OPEN_WRITE_ENV,
                   &patterns_open_write))
        return -1;

    return real_creat(pathname, mode);
}


int creat64(const char *pathname, mode_t mode)
{
    DBG("pathname=%s, mode=%d\n", pathname, mode);

    if (check_perm(pathname, RESTRICTED_ALLOW_OPEN_WRITE_ENV,
                   &patterns_open_write))
        return -1;

    return real_creat64(pathname, mode);
}


int open(const char *pathname, int flags, ...)
{
    int rwflag = flags & 3;

    DBG("pathname=%s, flags=%d\n", pathname, flags);


    if (O_WRONLY == rwflag || O_RDWR == rwflag) {
        if (check_perm(pathname, RESTRICTED_ALLOW_OPEN_WRITE_ENV,
                       &patterns_open_write))
            return -1;
    }

    if (O_RDONLY == rwflag) {
        if (check_perm(pathname, RESTRICTED_ALLOW_OPEN_READ_ENV,
                       &patterns_open_read))
            return -1;
    }

    if (O_CREAT == (flags & O_CREAT)) {
        va_list ap;
        mode_t mode;

        va_start(ap, flags);
        mode = va_arg(ap, mode_t);
        va_end(ap);

        return real_open(pathname, flags, mode);
    } else
        return real_open(pathname, flags);
}


int open64(const char *pathname, int flags, ...)
{
    int rwflag = flags & 3;

    DBG("pathname=%s, flags=%d\n", pathname, flags);


    if (O_WRONLY == rwflag || O_RDWR == rwflag) {
        if (check_perm(pathname, RESTRICTED_ALLOW_OPEN_WRITE_ENV,
                       &patterns_open_write))
            return -1;
    }

    if (O_RDONLY == rwflag) {
        if (check_perm(pathname, RESTRICTED_ALLOW_OPEN_READ_ENV,
                       &patterns_open_read))
            return -1;
    }

    if (O_CREAT == (flags & O_CREAT)) {
        va_list ap;
        mode_t mode;

        va_start(ap, flags);
        mode = va_arg(ap, mode_t);
        va_end(ap);

        return real_open64(pathname, flags, mode);
    } else
        return real_open64(pathname, flags);
}


int mkdir(const char *pathname, mode_t mode)
{
    DBG("pathname=%s, mode=%d\n", pathname, mode);

    if (check_perm(pathname, RESTRICTED_ALLOW_MKDIR_ENV,
                   &patterns_mkdir))
        return -1;

    return real_mkdir(pathname, mode);
}


int rmdir(const char *pathname)
{
    DBG("pathname=%s\n", pathname);

    if (check_perm(pathname, RESTRICTED_ALLOW_RMDIR_ENV,
                   &patterns_rmdir))
        return -1;

    return real_rmdir(pathname);
}


DIR* opendir(const char* pathname)
{
    DBG("pathname=%s\n", pathname);

    if (check_perm(pathname, RESTRICTED_ALLOW_OPENDIR_ENV,
                   &patterns_opendir))
        return NULL;

    return real_opendir(pathname);
}


int execve(const char *filename,
           char *const argv[], char *const envp[])
{
    DBG("filename=%s\n", filename);

    if (check_perm(filename, RESTRICTED_ALLOW_EXEC_ENV,
                   &patterns_execve))
        return -1;

    return real_execve(filename, argv, envp);
}

