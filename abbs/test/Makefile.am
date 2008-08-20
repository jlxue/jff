SUBDIRS = benchmark

noinst_PROGRAMS = test_apdb
EXTRA_DIST = test_apdb.sh test_boardd.sh test_jail.sh

AM_CPPFLAGS = -Wall -Wextra -I$(srcdir)/..

test_apdb_SOURCES = test_apdb.c
test_apdb_LDADD = ../libbbs.la

