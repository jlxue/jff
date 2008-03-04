/* vi:set tw=72 ts=8 sw=8 noet nowrap ft=c fdm=syntax: */

/*
 * encoding-wrapper.c
 * 	convert file name encoding transparently between application and
 * 	file system by intercepting system calls with LD_PRELOAD.
 *
 * Usage:
 *
 * $ gcc -Wall -fPIC -c encoding-wrapper.c
 * $ gcc -shared -o libew.so encoding-wrapper.o -ldl
 * $ gcc -Wall -o t t.c
 * $ LD_LIBRARY_PATH=. LD_PRELOAD=libew.so PATH_MAX=256 \
 * 	APP_ENCODING=GBK FS_ENCODING=UTF-8 ./t
 *
 * Note:
 * This file doesn't cover all functions that operate on file names.
 *
 * Test with gcc 4.0.3 (Ubuntu 4.0.3-1ubuntu5), glibc 2.3.6, kernel
 * 2.6.15-21-686 on Ubuntu Dapper beta.
 *
 * Author:
 * 	dieken at newsmth BBS <http://newsmth.net>, 2006-05-21
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
#include <linux/limits.h>
#include <iconv.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <dirent.h>
#include <utime.h>
#include <unistd.h>
#include <alloca.h>


#ifndef USE_DEFAULT_ENCODING
#define USE_DEFAULT_ENCODING		1
#endif

#ifndef DEFAULT_APP_ENCODING
#define DEFAULT_APP_ENCODING		"GBK"
#endif

#ifndef DEFAULT_FS_ENCODING
#define DEFAULT_FS_ENCODING		"UTF-8"
#endif

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
char* app_encoding;		/* encoding used by application */
char* fs_encoding;		/* encoding used by file system */
unsigned int path_max;		/* length of path name, including trailing '\0'. */
void* dl_handle = NULL;

/* NOTE: can't use global iconv_t and path name buffer without mutex
 * because there can be many threads calling same function below at the
 * same time.
 */



/*
 * according to linux-2.6/include/linux/syscalls.h
 */

int (*real_truncate)(const char *path, off_t length) = NULL;

/* in /usr/include/sys/stat.h: stat() and lstat() are linked statically
 * into /usr/lib/libc_nonshared.a, so we have to intercept the internal
 * functions __xstat() and __xlstat(), is there any other simple and
 * portable way?
 */
int (*real___xstat)(int __ver, const char *__filename, struct stat *__stat_buf) = NULL;
int (*real___lxstat)(int __ver, const char *__filename, struct stat *__stat_buf) = NULL;
int (*real___xstat64)(int __ver, const char *__filename, struct stat64 *__stat_buf) = NULL;
int (*real___lxstat64)(int __ver, const char *__filename, struct stat64 *__stat_buf) = NULL;

int (*real_link)(const char *oldpath, const char *newpath) 		= NULL;
int (*real_symlink)(const char *oldpath, const char *newpath) 		= NULL;
int (*real_unlink)(const char *pathname) 				= NULL;
int (*real_rename)(const char *oldpath, const char *newpath) 		= NULL;
int (*real_chmod)(const char *path, mode_t mode) 			= NULL;
int (*real_readlink)(const char *path, char *buf, size_t bufsiz) 	= NULL;
int (*real_creat)(const char *pathname, mode_t mode) 			= NULL;
int (*real_open)(const char *pathname, int flags, mode_t mode) 		= NULL;
int (*real_access)(const char *pathname, int mode) 			= NULL;
int (*real_chown)(const char *path, uid_t owner, gid_t group) 		= NULL;
int (*real_lchown)(const char *path, uid_t owner, gid_t group) 		= NULL;
int (*real_utime)(const char *filename, const struct utimbuf *buf) 	= NULL;
int (*real_utimes)(const char *filename, const struct timeval tv[2]) 	= NULL;
char* (*real_getcwd)(char *buf, size_t size) 				= NULL;
int (*real_mkdir)(const char *pathname, mode_t mode) 			= NULL;
int (*real_chdir)(const char *path) 					= NULL;
int (*real_rmdir)(const char *pathname) 				= NULL;

DIR* (*real_opendir)(const char* name) 		= NULL;
struct dirent* (*real_readdir)(DIR* dir) 	= NULL;
int (*real_closedir)(DIR* dir) 			= NULL;
void (*real_rewinddir)(DIR* dir) 		= NULL;
void (*real_seekdir)(DIR* dir, off_t offset) 	= NULL;
off_t (*real_telldir)(DIR* dir) 		= NULL;
int (*real_dirfd)(DIR* dir) 			= NULL;
struct dirent64* (*real_readdir64)(DIR* dir) 	= NULL;

/*---------------------load orignal functions-------------------- */

__attribute__((constructor)) void initialize(void)
{
	char* error;
	char* s_path_max;
	iconv_t conv;

	DBG("enter\n");

	s_path_max = getenv("PATH_MAX");
	if (NULL == s_path_max) path_max = NAME_MAX + 1;	/* 256, PATH_MAX is too long */
	else {
		path_max = atoi(s_path_max);
		if (path_max <= 0 || path_max > PATH_MAX)
			path_max = PATH_MAX;
	}


	app_encoding = getenv("APP_ENCODING");
	fs_encoding = getenv("FS_ENCODING");
	
#if USE_DEFAULT_ENCODING
	if (app_encoding == NULL) app_encoding = DEFAULT_APP_ENCODING;
	if (fs_encoding == NULL) fs_encoding = DEFAULT_FS_ENCODING;
#else
	if (app_encoding == NULL || fs_encoding == NULL) {
		fprintf(stderr, "APP_ENCODING and FS_ENCODING environment variables are not set!\n");
		exit(EXIT_FAILURE);
	}
#endif

	/* test whether the encoding names are valid */
	conv = iconv_open(fs_encoding, app_encoding);
	if ((iconv_t)-1 == conv) {
		perror("Can't create iconv descriptor");
		exit(EXIT_FAILURE);
	}
	iconv_close(conv);

	DBG("fs_encoding=%s, app_encoding=%s, path_max=%u\n", fs_encoding, app_encoding, path_max);

	dl_handle = dlopen(LIBC_NAME, RTLD_LAZY);
	if (!dl_handle) {
		fprintf(stderr, "dlopen() failes:%s\n", dlerror());
		exit(EXIT_FAILURE);
	}

	dlerror();		/* Clear any existing error. */

	LOOKUP_SYMBOL(truncate);
	LOOKUP_SYMBOL(__xstat);
	LOOKUP_SYMBOL(__lxstat);
	LOOKUP_SYMBOL(__xstat64);
	LOOKUP_SYMBOL(__lxstat64);
	LOOKUP_SYMBOL(link);
	LOOKUP_SYMBOL(symlink);
	LOOKUP_SYMBOL(unlink);
	LOOKUP_SYMBOL(rename);
	LOOKUP_SYMBOL(chmod);
	LOOKUP_SYMBOL(readlink);
	LOOKUP_SYMBOL(creat);
	LOOKUP_SYMBOL(open);
	LOOKUP_SYMBOL(access);
	LOOKUP_SYMBOL(chown);
	LOOKUP_SYMBOL(lchown);
	LOOKUP_SYMBOL(utime);
	LOOKUP_SYMBOL(utimes);
	LOOKUP_SYMBOL(getcwd);
	LOOKUP_SYMBOL(mkdir);
	LOOKUP_SYMBOL(chdir);
	LOOKUP_SYMBOL(rmdir); 
	LOOKUP_SYMBOL(opendir);
	LOOKUP_SYMBOL(readdir);
	LOOKUP_SYMBOL(readdir64);
	LOOKUP_SYMBOL(closedir);
	LOOKUP_SYMBOL(rewinddir);
	LOOKUP_SYMBOL(seekdir);
	LOOKUP_SYMBOL(telldir);
	LOOKUP_SYMBOL(dirfd);

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

/* ----------------------encoding convertion --------------------- */
/**
 * convert encoding.
 *
 * @return -1	error
 *         >=0	length of result string, without trailing '\0'
 */
int iconv_wrapper(iconv_t cd,
		const char* inbuf, size_t inbuflen,
		char* outbuf, size_t outbuflen)
{
	int ret = -1;
	char* out = outbuf;

	if (unlikely(NULL == inbuf || NULL == outbuf || outbuflen < 2)) {
		fprintf(stderr, "iconv_wrapper(): illegal parameters: "
				"inbuf=%p, outbuf=%p, outbuflen=%u\n",
				inbuf, outbuf, outbuflen);
		return -1;
	}

	--outbuflen;		/* reserve for trailing '\0' */
	ret = iconv(cd, (char**)&inbuf, &inbuflen, &outbuf, &outbuflen);
	if (unlikely(-1 == ret)) {
		fprintf(stderr, "iconv_wrapper(): fails to convert [%s]: %s",
				inbuf, strerror(errno));
		if (E2BIG == errno) {
			fprintf(stderr, "PATH_MAX(%u) is too small!\n", path_max);
		}
	} else {
		*outbuf = '\0';
		ret = outbuf - out;
	}

	return ret;
}


/* app_encoding to fs_encoding, one path name parameter, return -1 for error. */
#define TEMPLATE_1(p, path, func, ...)	\
	iconv_t conv;				\
	int ret = -1;				\
	char* p = alloca(path_max);	/* XXX: thread local storage is a better solution. */	\
						\
	DBG("enter: " #path "=%s\n", path);	\
	conv = iconv_open(fs_encoding, app_encoding);	\
	if (unlikely(-1 == iconv_wrapper(conv, path, strlen(path), p, path_max)))	\
		goto LEAVE;			\
						\
	ret = real_##func(__VA_ARGS__);		\
						\
	LEAVE:					\
	iconv_close(conv);			\
	DBG("leave: ret=%d\n", ret);		\
	return ret;

/* app_encoding to fs_encoding, two path name parameters, return -1 for error. */
#define TEMPLATE_2(p, p2, path, path2, func, ...)	\
	iconv_t conv;				\
	int ret = -1;				\
	char* p = alloca(path_max);		\
	char* p2 = alloca(path_max);		\
						\
	DBG("enter: " #path "=%s, " #path2 "=%s\n", path, path2);	\
	conv = iconv_open(fs_encoding, app_encoding);			\
	if (unlikely(-1 == iconv_wrapper(conv, path, strlen(path), p, path_max)))	\
		goto LEAVE;			\
	iconv(conv, NULL, NULL, NULL, NULL);	\
	if (unlikely(-1 == iconv_wrapper(conv, path2, strlen(path2), p2, path_max)))	\
		goto LEAVE;			\
						\
	ret = real_##func(__VA_ARGS__);		\
						\
	LEAVE:					\
	iconv_close(conv);			\
	DBG("leave: ret=%d\n", ret);		\
	return ret;

/* -----------------------system calls --------------------------- */
int truncate(const char *path, off_t length)
{
	iconv_t conv;
	int ret = -1;
	char* p = alloca(path_max);	/* XXX: thread local storage is a better solution. */

	conv = iconv_open(fs_encoding, app_encoding);
	if (unlikely(-1 == iconv_wrapper(conv, path, strlen(path), p, path_max)))
		goto LEAVE;

	ret = real_truncate(p, length);

LEAVE:
	iconv_close(conv);
	return ret;
}

int __xstat(int __ver, const char *__filename, struct stat *__stat_buf)
{
	TEMPLATE_1(p, __filename, __xstat, __ver, p, __stat_buf);
}

int __lxstat(int __ver, const char *__filename, struct stat *__stat_buf)
{
	TEMPLATE_1(p, __filename, __lxstat, __ver, p, __stat_buf);
}

int __xstat64(int __ver, const char *__filename, struct stat64 *__stat_buf)
{
	TEMPLATE_1(p, __filename, __xstat64, __ver, p, __stat_buf);
}

int __lxstat64(int __ver, const char *__filename, struct stat64 *__stat_buf)
{
	TEMPLATE_1(p, __filename, __lxstat64, __ver, p, __stat_buf);
}

int link(const char *oldpath, const char *newpath)
{
	TEMPLATE_2(p, p2, oldpath, newpath, link, p, p2);
}

int symlink(const char *oldpath, const char *newpath)
{
	TEMPLATE_2(p, p2, oldpath, newpath, symlink, p, p2);
}

int unlink(const char *pathname)
{
	TEMPLATE_1(p, pathname, unlink, p);
}

int rename(const char *oldpath, const char *newpath)
{
	TEMPLATE_2(p, p2, oldpath, newpath, rename, p, p2);
}

int chmod(const char *path, mode_t mode)
{
	TEMPLATE_1(p, path, chmod, p, mode);
}

int readlink(const char *path, char *buf, size_t bufsiz)
{
	iconv_t conv;
	int ret = -1;
	char* p = alloca(path_max);
	char* p2 = alloca(path_max);

	conv = iconv_open(fs_encoding, app_encoding);
	if (unlikely(-1 == iconv_wrapper(conv, path, strlen(path), p, path_max)))
		goto LEAVE;

	/* `ret` is the length of string in p2[path_max], without trailing '\0'. */
	ret = real_readlink(p, p2, path_max);
	if (errno) goto LEAVE;

	iconv(conv, NULL, NULL, NULL, NULL);
	ret = iconv_wrapper(conv, p2, ret, p, path_max);
	if (unlikely(-1 == ret)) {
		ret = -1;
		goto LEAVE;
	}

	if (ret <= bufsiz) memcpy(buf, p, ret);
	else {
		errno = ENAMETOOLONG;
		ret = -1;
	}

LEAVE:
	iconv_close(conv);
	return ret;
}

int creat(const char *pathname, mode_t mode)
{
	TEMPLATE_1(p, pathname, creat, p, mode);
}

int open(const char *pathname, int flags, mode_t mode)
{
	/* 
	 * man 2 open:
	 * `mode` must be specified when O_CREAT is in the flags, and is
	 * ignored otherwise.
	 */
	TEMPLATE_1(p, pathname, open, p, flags, mode);
}

int access(const char *pathname, int mode)
{
	TEMPLATE_1(p, pathname, access, p, mode);
}

int chown(const char *path, uid_t owner, gid_t group)
{
	TEMPLATE_1(p, path, chown, p, owner, group);
}


int lchown(const char *path, uid_t owner, gid_t group)
{
	TEMPLATE_1(p, path, lchown, p, owner, group);
}

int utime(const char *filename, const struct utimbuf *buf)
{
	TEMPLATE_1(p, filename, utime, p, buf);
}

int utimes(const char *filename, const struct timeval tv[2])
{
	TEMPLATE_1(p, filename, utimes, p, tv);
}

char *getcwd(char *buf, size_t size)
{
	iconv_t conv;
	char* tmp;
	char* cwd;

	/* 
	 * man getcwd:
	 * GNU libc extension in Linux will allocate memory for `buf`, but
	 * this is not standard ;-)
	 */
	if (NULL == buf) return NULL;

	tmp = alloca(path_max);
	cwd = real_getcwd(tmp, path_max);

	if (NULL == cwd) return NULL;

	conv = iconv_open(app_encoding, fs_encoding);
	if (unlikely(-1 == iconv_wrapper(conv, cwd, strlen(cwd), buf, size))) {
		if (errno == E2BIG) errno = ERANGE;
		if (cwd != tmp) free(cwd);
		iconv_close(conv);
		return NULL;
	}

	iconv_close(conv);
	return buf;
}

int mkdir(const char *pathname, mode_t mode)
{
	TEMPLATE_1(p, pathname, mkdir, p, mode);
}

int chdir(const char *path)
{
	TEMPLATE_1(p, path, chdir, p);
}

int rmdir(const char *pathname)
{
	TEMPLATE_1(p, pathname, rmdir, p);
}

/* as dirent.d_name points to a ``dir stream'', the memory occupied by
 * d_name is char[strlen(d_name) + 1] but not char[NAME_MAX + 1], so we
 * need allocate a buffer to return `d_name` converted by iconv().
 */
typedef struct {
	DIR* dir;
	union {
		struct dirent ent;
		struct dirent64 ent64;
	} dentry;
} XDIR;

DIR* opendir(const char* name)
{
	iconv_t conv;
	DIR* dir;
	XDIR* xdir = NULL;
	char* p = alloca(path_max);

	DBG("enter, name=%s\n", name);
	if (NULL == (xdir = malloc(sizeof(XDIR)))) {
		fprintf(stderr, "opendir() wrapper: out of memory!\n");
		return NULL;
	}

	conv = iconv_open(fs_encoding, app_encoding);
	if (unlikely(-1 == iconv_wrapper(conv, name, strlen(name), p, path_max)))
		goto LEAVE;

	dir = real_opendir(p);
	if (NULL == dir) goto LEAVE;

	xdir->dir = dir;

	DBG("leave: xdir=%p\n", xdir);
	return (DIR*)xdir;

LEAVE:
	if (NULL != xdir) free(xdir);
	iconv_close(conv);
	DBG("leave\n");
	return NULL;
}

struct dirent* readdir(DIR* dir)
{
	iconv_t conv;
	struct dirent* de;
	XDIR* xdir = (XDIR*)dir;

	DBG("enter: dir=%p\n", dir);

	de = real_readdir(xdir->dir);
	if (de != NULL) {
		int len = strlen(de->d_name);

		DBG("real_readdir() got d_name=%s\n", de->d_name);

		conv = iconv_open(app_encoding, fs_encoding);
		if (-1 == iconv_wrapper(conv, de->d_name, len, xdir->dentry.ent.d_name, NAME_MAX + 1)) {
			/* just return the garbled entry name */
		} else {
			/* cheat the caller */
			memcpy(&xdir->dentry.ent, de, offsetof(struct dirent, d_name));
			de = &xdir->dentry.ent;
		}
		iconv_close(conv);
		DBG("iconved de->d_name=%s\n", de->d_name);
	}

	return de;
}

struct dirent64* readdir64(DIR* dir)
{
	iconv_t conv;
	struct dirent64* de;
	XDIR* xdir = (XDIR*)dir;

	DBG("enter: dir=%p\n", dir);

	de = real_readdir64(xdir->dir);
	if (de != NULL) {
		int len = strlen(de->d_name);

		DBG("real_readdir64() got d_name=%s\n", de->d_name);

		conv = iconv_open(app_encoding, fs_encoding);
		if (-1 == iconv_wrapper(conv, de->d_name, len, xdir->dentry.ent64.d_name, NAME_MAX + 1)) {
			/* just return the garbled entry name */
		} else {
			/* cheat the caller */
			memcpy(&xdir->dentry.ent64, de, offsetof(struct dirent64, d_name));
			de = &xdir->dentry.ent64;
		}
		iconv_close(conv);
		DBG("iconved de->d_name=%s\n", de->d_name);
	}

	return de;
}

int closedir(DIR* dir) {
	int ret;
	XDIR* xdir = (XDIR*)dir;

	DBG("enter, dir=%p\n", dir);
	ret = real_closedir(xdir->dir);
	free(xdir);

	DBG("leave\n");
	return ret;
}

void rewinddir(DIR* dir)
{
	DBG("dir=%p\n", dir);
	real_rewinddir(((XDIR*)dir)->dir);
}

void seekdir(DIR* dir, off_t offset)
{
	DBG("dir=%p\n", dir);
	real_seekdir(((XDIR*)dir)->dir, offset);
}

off_t telldir(DIR* dir)
{
	DBG("dir=%p\n", dir);
	return real_telldir(((XDIR*)dir)->dir);
}

int dirfd(DIR* dir)
{
	DBG("dir=%p\n", dir);
	return real_dirfd(((XDIR*)dir)->dir);
}

