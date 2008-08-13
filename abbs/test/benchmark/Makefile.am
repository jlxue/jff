noinst_PROGRAMS = bench_apdb

AM_CPPFLAGS = -Wall -Wextra -I$(srcdir)/../..

common_sources = bench.c bench.h

bench_apdb_SOURCES = $(common_sources) bench_apdb.c
bench_apdb_LDADD = ../../libbbs.la

if HAVE_BDB
noinst_PROGRAMS += bench_bdb
bench_bdb_SOURCES = $(common_sources) bench_bdb.c
bench_bdb_LDADD = $(BDBLIB)
endif

if HAVE_SQLITE3
noinst_PROGRAMS += bench_sqlite3
bench_sqlite3_SOURCES = $(common_sources) bench_sqlite3.c
bench_sqlite3_LDADD = $(SQLITE3LIB)
endif

if HAVE_GDBM
noinst_PROGRAMS += bench_gdbm
bench_gdbm_SOURCES = $(common_sources) bench_gdbm.c
bench_gdbm_LDADD = $(GDBMLIB)
endif

if HAVE_TOKYO_CABINET
noinst_PROGRAMS += bench_tc
bench_tc_SOURCES = $(common_sources) bench_tc.c
bench_tc_LDADD = $(TOKYO_CABINET_LIB)
endif

