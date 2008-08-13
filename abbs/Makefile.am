SUBDIRS = . test

lib_LTLIBRARIES = libbbs.la libjail.la
bin_PROGRAMS = bbs boardd
#include_HEADERS = apdb.h board.h util.h env.h
noinst_PROGRAMS = test_apdb
EXTRA_DIST = CMakeLists.txt Makefile.am SConstruct design.txt \
			 autogen.sh run_bbs.sh run_boardd.sh test_apdb.sh \
			 test_boardd.sh test_jail.sh

AM_CPPFLAGS = -Wall -Wextra

libbbs_la_SOURCES = \
	apdb.c \
	apdb.h \
	board.c \
	board.h \
	env.c \
	env.h \
	pool.h \
	pool.c \
	user.c \
	user.h \
	util.c \
	util.h

libbbs_la_LDFLAGS = -version-info 0:0:0

libjail_la_SOURCES = jail.c
libjail_la_LIBADD = -ldl

bbs_SOURCES = bbs.c
bbs_LDADD = libbbs.la $(READLINELIB)

boardd_SOURCES = boardd.c
boardd_LDADD = libbbs.la $(EVLIB)

test_apdb_SOURCES = test_apdb.c
test_apdb_LDADD = libbbs.la

