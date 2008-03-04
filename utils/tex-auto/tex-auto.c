/* vi:set tw=72 ts=8 sw=8 noet nowrap ft=c: */

/*
 * tex-auto.c
 * 	install latex packages automatically by intercepting
 * 	system calls with LD_PRELOAD.
 *
 * Usage:
 *
 * $ gcc -Wall -fPIC -c tex-auto.c
 * $ gcc -shared -o libtex-auto.so tex-auto.o -ldl
 * $ perl tex-auto.pl &
 * $ ./cat.sh file1 file2 file3
 *
 *
 * Test with gcc 4.0.3 (Ubuntu 4.0.3-1ubuntu5), glibc 2.3.6, kernel
 * 2.6.15-28-686 on Ubuntu Dapper beta.
 *
 * Author:
 * 	dieken at newsmth BBS <http://newsmth.net>, 2007-11-22
 *
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <errno.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <dirent.h>
#include <utime.h>
#include <unistd.h>


#define O_RDONLY	     00
#define O_WRONLY	     01

#define MAX_PATH_LEN		256

#ifndef LIBC_NAME
#define LIBC_NAME			"libc.so.6"
#endif


#define LVALUE_FP(fp)	*(void**)(&fp)

#define LOOKUP_SYMBOL(func)	\
	LVALUE_FP(real_##func) = dlsym(dl_handle, #func);	\
	DBG("real_" #func "=%p\n", real_##func);		\
	if ((error = dlerror()) != NULL) goto ERROR

#ifdef DEBUG
#define DBG(format, ...) fprintf(stderr, "[%s:%d] %s(): " format,	\
	       			__FILE__, __LINE__, __func__, ##__VA_ARGS__)
#else
#define DBG(format, ...)
#endif

/* XXX: why gftp calls access(NULL, ...) ? */
#define strlen(s) (NULL != (s) ? strlen(s) : 0)

#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)

/* --------------------global variables -------------------------*/
void* dl_handle = NULL;
char infifo[MAX_PATH_LEN] = "";
char outfifo[MAX_PATH_LEN] = "";

/*
 * according to linux-2.6/include/linux/syscalls.h
 */


/* in /usr/include/sys/stat.h: stat() and lstat() are linked statically
 * into /usr/lib/libc_nonshared.a, so we have to intercept the internal
 * functions __xstat() and __xlstat(), is there any other simple and
 * portable way?
 */
int (*real___xstat)(int __ver, const char *__filename, struct stat *__stat_buf) = NULL;
int (*real___lxstat)(int __ver, const char *__filename, struct stat *__stat_buf) = NULL;
int (*real___xstat64)(int __ver, const char *__filename, struct stat64 *__stat_buf) = NULL;
int (*real___lxstat64)(int __ver, const char *__filename, struct stat64 *__stat_buf) = NULL;

int (*real_open)(const char *pathname, int flags, mode_t mode) 		= NULL;
int (*real_open64)(const char *pathname, int flags, mode_t mode) 	= NULL;
int (*real_access)(const char *pathname, int mode) 			= NULL;


/*---------------------load orignal functions-------------------- */

__attribute__((constructor)) void initialize(void)
{
	char* error;
	char* homedir;
	int homedirlen;

	DBG("enter initialize\n");

	dl_handle = dlopen(LIBC_NAME, RTLD_LAZY);
	if (!dl_handle) {
		fprintf(stderr, "dlopen() failes:%s\n", dlerror());
		exit(EXIT_FAILURE);
	}

	dlerror();		/* Clear any existing error. */

	LOOKUP_SYMBOL(__xstat);
	LOOKUP_SYMBOL(__lxstat);
	LOOKUP_SYMBOL(__xstat64);
	LOOKUP_SYMBOL(__lxstat64);
	LOOKUP_SYMBOL(open);
	LOOKUP_SYMBOL(open64);
	LOOKUP_SYMBOL(access);

	homedir = getenv("HOME");
	if (NULL == homedir ||
			(homedirlen = strlen(homedir)) >= MAX_PATH_LEN) {
		fprintf(stderr, "HOME not set or too long!\n");
		exit(EXIT_FAILURE);
	}
	strncat(infifo, homedir, homedirlen);
	strncat(outfifo, homedir, homedirlen);
	strncat(infifo, "/.tex-auto-i", MAX_PATH_LEN - homedirlen - 1);
	strncat(outfifo, "/.tex-auto-o", MAX_PATH_LEN - homedirlen - 1);

	DBG("leave initialize\n");
	return;

ERROR:
	fprintf(stderr, "dlsym() fails:%s\n", error);
	exit(EXIT_FAILURE);
}


__attribute__((destructor)) void finalize(void)
{
	DBG("enter finalize, dl_handle=%p\n", dl_handle);
	dlclose(dl_handle);
	DBG("leave finalize\n");
}


int install_package_for(const char* filename)
{
	char response[256];
	int infd = -1, outfd = -1;

	DBG("open outfifo...\n");
	outfd = real_open(outfifo, O_WRONLY, 0);	/* XXX: need lock? */
	if (-1 == outfd) {
		fprintf(stderr, "can't open %s!", outfifo);
		exit(EXIT_FAILURE);
	}
	if (-1 == write(outfd, filename, strlen(filename)))
		return -1;
	close(outfd);

	DBG("open infifo...\n");
	infd = real_open(infifo, O_RDONLY, 0);		/* XXX: need lock? */
	if (-1 == infd) {
		fprintf(stderr, "can't open %s!", infifo);
		exit(EXIT_FAILURE);
	}
	if (read(infd, response, 256) > 0 && 'o' == response[0]) {
		close(infd);
		return 0;	/* got "ok" */
	}

	close(infd);
	return -1;
}


/* -----------------------system calls --------------------------- */

int __xstat(int __ver, const char *__filename, struct stat *__stat_buf)
{
	int ret;

	DBG("enter: filename=%s\n", __filename);
	ret = real___xstat(__ver, __filename, __stat_buf);
	if (unlikely(-1 == ret)) {
		if (-1 == install_package_for(__filename))
			return -1;
		else
			return real___xstat(__ver, __filename, __stat_buf);
	}

	return ret;
}


int __lxstat(int __ver, const char *__filename, struct stat *__stat_buf)
{
	int ret;

	DBG("enter: filename=%s\n", __filename);
	ret = real___lxstat(__ver, __filename, __stat_buf);
	if (unlikely(-1 == ret)) {
		if (-1 == install_package_for(__filename))
			return -1;
		else
			return real___lxstat(__ver, __filename, __stat_buf);
	}

	return ret;
}


int __xstat64(int __ver, const char *__filename, struct stat64 *__stat_buf)
{
	int ret;

	DBG("enter: filename=%s\n", __filename);
	ret = real___xstat64(__ver, __filename, __stat_buf);
	if (unlikely(-1 == ret)) {
		if (-1 == install_package_for(__filename))
			return -1;
		else
			return real___xstat64(__ver, __filename, __stat_buf);
	}

	return ret;
}


int __lxstat64(int __ver, const char *__filename, struct stat64 *__stat_buf)
{
	int ret;

	DBG("enter: filename=%s\n", __filename);
	ret = real___lxstat64(__ver, __filename, __stat_buf);
	if (unlikely(-1 == ret)) {
		if (-1 == install_package_for(__filename))
			return -1;
		else
			return real___lxstat64(__ver, __filename, __stat_buf);
	}

	return ret;
}


int open(const char *pathname, int flags, mode_t mode)
{
	int ret;

	DBG("enter: filename=%s\n", pathname);
	ret = real_open(pathname, flags, mode);
	if (unlikely(-1 == ret) && !(flags & O_WRONLY)) {
		if (-1 == install_package_for(pathname))
			return -1;
		else
			return real_open(pathname, flags, mode);
	}

	return ret;
}


int open64(const char *pathname, int flags, mode_t mode)
{
	int ret;

	DBG("enter: filename=%s\n", pathname);
	ret = real_open64(pathname, flags, mode);
	if (unlikely(-1 == ret) && !(flags & O_WRONLY)) {
		if (-1 == install_package_for(pathname))
			return -1;
		else
			return real_open64(pathname, flags, mode);
	}

	return ret;
}


int access(const char *pathname, int mode)
{
	int ret;

	DBG("enter: filename=%s\n", pathname);
	ret = real_access(pathname, mode);
	if (unlikely(-1 == ret) && mode == R_OK) {
		if (-1 == install_package_for(pathname))
			return -1;
		else
			return real_access(pathname, mode);
	}

	return ret;
}

