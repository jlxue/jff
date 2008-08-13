#include "bench.h"

#include <tcbdb.h>
#include <tcutil.h>


void
run_writer(const char* buffer, unsigned size, const char* filename)
{
    TCBDB* db;
    unsigned i,n, total;
    struct timeval t1, t2;
    int ret;

    db = tcbdbnew();
    assert(NULL != db);

    if (sizeof(unsigned) == 4)
        ret = tcbdbsetcmpfunc(db, tcbdbcmpint32, NULL);
    else if (sizeof(unsigned) == 8)
        ret = tcbdbsetcmpfunc(db, tcbdbcmpint64, NULL);
    else
        abort();
    assert(ret);

    unlink(filename);
    ret = tcbdbopen(db, filename, BDBOWRITER | BDBOCREAT);
    assert(ret);

    gettimeofday(&t1, NULL);

    total = 0;
    n = 200;
    for (i = 1; i <= RECORD_NUM; ++i) {
        n = n + n / 3;
        if (n > size)
            n = 200;
        total += n;

        ret = tcbdbput(db, &i, sizeof(i), buffer, n);
        assert(ret);
    }

    gettimeofday(&t2, NULL);
    SHOW_TIME("tc write", t2, t1, total);

    tcbdbclose(db);
    tcbdbdel(db);
}


void
run_reader(const char* buffer, unsigned size, const char* filename)
{
    TCBDB* db;
    BDBCUR* cursor;
    unsigned* key;
    char* data;
    int m;
    unsigned i,n, total;
    struct timeval t1, t2;
    int ret;

    db = tcbdbnew();
    assert(NULL != db);

    ret = tcbdbopen(db, filename, BDBOREADER | BDBONOLCK);
    assert(ret);

    gettimeofday(&t1, NULL);

    cursor = tcbdbcurnew(db);
    assert(NULL != cursor);

    ret = tcbdbcurfirst(cursor);

    total = 0;
    n = 200;
    for (i = 1; i <= RECORD_NUM; ++i) {
        n = n + n / 3;
        if (n > size)
            n = 200;
        total += n;

        assert(ret);

        key = (unsigned*)tcbdbcurkey(cursor, &m);
        assert(NULL != key);
        assert(m == sizeof(unsigned) && i == *key);

        data = tcbdbcurval(cursor, &m);
        assert(NULL != key);

        assert(n == (unsigned)m);
        assert(0 == memcmp(buffer, data, m));

        free(key);
        free(data);

        ret = tcbdbcurnext(cursor);
    }
    ret = tcbdbcurnext(cursor);
    assert(! ret);

    gettimeofday(&t2, NULL);
    SHOW_TIME("tc read", t2, t1, total);

    tcbdbcurdel(cursor);
    tcbdbclose(db);
    tcbdbdel(db);
}

