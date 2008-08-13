#include "bench.h"

#include <gdbm.h>


void
run_writer(const char* buffer, unsigned size, const char* filename)
{
    GDBM_FILE db;
    datum k, d;
    unsigned i,n, total;
    struct timeval t1, t2;
    int ret;

    unlink(filename);
    db = gdbm_open((char*)filename, 0, GDBM_WRCREAT, 0644, NULL);
    assert(NULL != db);

    gettimeofday(&t1, NULL);

    total = 0;
    n = 200;
    for (i = 1; i <= RECORD_NUM; ++i) {
        n = n + n / 3;
        if (n > size)
            n = 200;
        total += n;

        k.dptr = (char*)&i;
        k.dsize = sizeof(i);

        d.dptr = (char*)buffer;
        d.dsize = n;

        ret = gdbm_store(db, k, d, GDBM_REPLACE);
        assert(0 == ret);
    }

    gettimeofday(&t2, NULL);
    SHOW_TIME("gdbm write", t2, t1, total);

    gdbm_close(db);
}


void
run_reader(const char* buffer, unsigned size, const char* filename)
{
    GDBM_FILE db;
    datum k, d;
    unsigned i,n, total;
    struct timeval t1, t2;

    db = gdbm_open((char*)filename, 0, GDBM_READER, 0644, NULL);
    assert(NULL != db);

    gettimeofday(&t1, NULL);

    k = gdbm_firstkey(db);

    total = 0;
    n = 200;
    for (i = 1; i <= RECORD_NUM; ++i) {
        n = n + n / 3;
        if (n > size)
            n = 200;
        //total += n;

        // the order of key-value pairs changed
        assert(NULL != k.dptr); // && *(int*)(k.dptr) == i);

        d = gdbm_fetch(db, k);
        assert(NULL != d.dptr); // && n == d.dsize);
        assert(0 == memcmp(buffer, d.dptr, d.dsize));

        total += d.dsize;

        // don't free, because gdbm store integer in d.dptr directly
        //free(k.dptr);
        free(d.dptr);

        k = gdbm_nextkey(db, k);
    }
    assert(NULL == k.dptr);

    gettimeofday(&t2, NULL);
    SHOW_TIME("gdbm read", t2, t1, total);

    gdbm_close(db);
}

