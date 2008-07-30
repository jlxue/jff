lib_LTLIBRARIES = libbbs.la
bin_PROGRAMS = bbs boardd
include_HEADERS = apdb.h board.h util.h env.h

AM_CPPFLAGS = -Wall

libbbs_la_SOURCES = \
	apdb.c \
	apdb.h \
	board.c \
	env.c \
	env.h \
	util.c \
	util.h

libbbs_la_LDFLAGS = -version-info 0:0:0

bbs_SOURCES = bbs.c
bbs_LDADD = libbbs.la $(READLINELIB)

boardd_SOURCES = boardd.c
boardd_LDADD = libbbs.la $(EVLIB)

