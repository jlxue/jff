#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(ABBS, 0.0.1, yubao.liu@gmail.com)
AM_INIT_AUTOMAKE([-Wall -Werror foreign dist-bzip2])
AC_CONFIG_SRCDIR([bbs.c])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_LN_S
AC_PROG_LIBTOOL

# Checks for libraries.
AC_CHECK_LIB(readline, readline, [READLINELIB=-lreadline], AC_MSG_ERROR([
*** Unable to find readline library]))
AC_SUBST([READLINELIB])

AC_CHECK_LIB(ev, ev_loop, [EVLIB=-lev], AC_MSG_ERROR([
*** Unable to find libev library]))
AC_SUBST([EVLIB])

# Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([fcntl.h limits.h stddef.h stdint.h stdlib.h string.h sys/file.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_INT64_T
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UID_T

# Checks for library functions.
AC_REPLACE_FNMATCH
AC_FUNC_FORK
AC_FUNC_LSTAT
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AC_FUNC_STAT
AC_FUNC_STRFTIME
AC_CHECK_FUNCS([memset mkdir munmap realpath rmdir strchr strcspn strdup strndup strrchr strspn strtoul])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
