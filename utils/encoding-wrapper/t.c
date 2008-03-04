#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <assert.h>
#include <dlfcn.h>
#include <linux/limits.h>
#include <iconv.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <dirent.h>
#include <utime.h>
#include <unistd.h>
#include <fcntl.h>


#define TEST_IT(func)	\
	fprintf(stderr, "test " #func "()\t");	\
	if (test_##func()) {			\
		perror("bad");	\
		return EXIT_FAILURE;		\
	} else fputs("ok\n", stderr);

/* GBK encoding */

char* file1 = "==²âÊÔÎÄ¼þ==";
char* file2 = "==²âÊÔÎÄ¼þ-Áí==";
char* symlink1 = "==·ûºÅÁ´½Ó==";
char* link1 = "==Ó²Á´½Ó==";
char* dir1 = "==Ä¿Â¼==";

int test_truncate(void) //const char *path, off_t length)
{
	char* path = file1;
	int length = 0;
	return -1 == truncate(path, length);
}


int test_stat(void) //const char *filename, struct stat *stat_buf)
{
	char* filename = file1;
	struct stat stat_buf;
	return -1 == stat(filename, &stat_buf);
}


int test_lstat(void) //const char *filename, struct stat *stat_buf)
{
	char* filename = symlink1;
	struct stat stat_buf;
	return -1 == lstat(filename, &stat_buf);
}


int test_link(void) //const char *oldpath, const char *newpath)
{
	char* oldpath = file1;
	char* newpath = link1;
	return -1 == link(oldpath, newpath);
}


int test_symlink(void) //const char *oldpath, const char *newpath)
{
	char* oldpath = file1;
	char* newpath = symlink1;
	return -1 == symlink(oldpath, newpath);
}


int test_unlink(void) //const char *pathname)
{
	char* pathname = file1;
	return -1 == unlink(pathname);
}


int test_rename(void) //const char *oldpath, const char *newpath)
{
	char* oldpath = file1;
	char* newpath = file2;
	return -1 == rename(oldpath, newpath);
}


int test_chmod(void) //const char *path, mode_t mode)
{
	char* path = file1;
	mode_t mode = 0666;
	return -1 == chmod(path, mode);
}


int test_readlink(void) //const char *path, char *buf, size_t bufsiz)
{
	char* path = symlink1;
	char buf[NAME_MAX + 1];
	size_t bufsiz = NAME_MAX + 1;
	return -1 == readlink(path, buf, bufsiz);
}


int test_creat(void) //const char *pathname, mode_t mode)
{
	char* pathname = file1;
	mode_t mode = 0666;
	return -1 == creat(pathname, mode);
}


int test_open(void) //const char *pathname, int flags, mode_t mode)
{
	char* pathname = file1;
	int flags = O_RDWR | O_CREAT;
	mode_t mode = 0666;
	return -1 == open(pathname, flags, mode);
}


int test_access(void) //const char *pathname, int mode)
{
	char* pathname = file1;
	int mode = R_OK;
	return -1 == access(pathname, mode);
}


int test_chown(void) //const char *path, uid_t owner, gid_t group)
{
	char* path = file1;
	uid_t owner = UID;
	gid_t group = GID;
	return -1 == chown(path, owner, group);
}


int test_lchown(void) //const char *path, uid_t owner, gid_t group)
{
	char* path = symlink1;
	uid_t owner = UID;
	gid_t group = GID;
	return -1 == lchown(path, owner, group);
}


int test_utime(void) //const char *filename, const struct utimbuf *buf)
{
	char* filename = file1;
	struct utimbuf buf;
	return -1 == utime(filename, &buf);
}


int test_utimes(void) //const char *filename, const struct timeval tv[2])
{
	char* filename = file1;
	struct timeval tv[2];
	return -1 == utimes(filename, tv);
}


int test_getcwd(void) //char *buf, size_t size)
{
	char buf[NAME_MAX + 1];
	size_t size = NAME_MAX + 1;
	return NULL == getcwd(buf, size);
}


int test_mkdir(void) //const char *pathname, mode_t mode)
{
	char* pathname = dir1;
	mode_t mode = 0700;

	return -1 == mkdir(pathname, mode);
}


int test_chdir(void) //const char *path)
{
	char* path = dir1;

	return -1 == chdir(path);
}


int test_rmdir(void) //const char *pathname)
{
	char* pathname = dir1;

	return -1 == rmdir(pathname);
}


int test_opendir(void) //const char* name)
{
	DIR* d;
	char* name = dir1;

	d = opendir(name);
	if (d != NULL) closedir(d);
	else return 1;

	return 0;
}


int test_readdir(void) //DIR* dir)
{
	DIR* d;
	struct dirent* de;
	char* name = ".";
	d = opendir(name);
	if (d == NULL) return 1;

	while (NULL != (de = readdir(d))) {
		if (errno) break;
		printf("t.c: readdir: %s\n", de->d_name);
	}
	closedir(d);

	return errno ? 1 : 0;
}



int main(void)
{
	TEST_IT(creat);
	TEST_IT(truncate);
	TEST_IT(access);
	TEST_IT(utime);
	TEST_IT(utimes);
	TEST_IT(stat);
	TEST_IT(link);
	TEST_IT(symlink);
	TEST_IT(lstat);
	TEST_IT(readlink);
	TEST_IT(chown);
	TEST_IT(lchown);
	TEST_IT(chmod);
	TEST_IT(unlink);
	TEST_IT(open);
	TEST_IT(rename);

	TEST_IT(mkdir);
	TEST_IT(opendir);
	TEST_IT(chdir);
	TEST_IT(getcwd);
	assert (0 == chdir(".."));
	TEST_IT(readdir);
	TEST_IT(rmdir);
	
	return EXIT_SUCCESS;
}

