#include "bench.h"

#include <db.h>


void
run_writer(const char* buffer, unsigned size, const char* filename)
{
    DB* db;
    DBT key, data;
    unsigned i,n, total;
    struct timeval t1, t2;
    int ret;

    ret = db_create(&db, NULL, 0);
    assert(0 == ret);

    unlink(filename);
    ret = db->open(db, NULL, filename, NULL, DB_RECNO, DB_CREATE, 0);
    assert(0 == ret);

    gettimeofday(&t1, NULL);

    total = 0;
    n = 200;
    for (i = 1; i <= RECORD_NUM; ++i) {
        n = n + n / 3;
        if (n > size)
            n = 200;
        total += n;

        memset(&key, 0, sizeof(DBT));
        memset(&data, 0, sizeof(DBT));
        key.data = &i;
        key.size = sizeof(i);

        data.data = (void*)buffer;
        data.size = n;

        ret = db->put(db, NULL, &key, &data, DB_APPEND);
        assert(0 == ret);
    }

    gettimeofday(&t2, NULL);
    SHOW_TIME("bdb write", t2, t1, total);

    db->close(db, 0);
}


void
run_reader(const char* buffer, unsigned size, const char* filename)
{
    DB* db;
    DBC *cursorp;
    DBT key, data;
    unsigned i,n, total;
    struct timeval t1, t2;
    int ret;

    ret = db_create(&db, NULL, 0);
    assert(0 == ret);

    ret = db->open(db, NULL, filename, NULL, DB_RECNO, DB_RDONLY, 0);
    assert(0 == ret);

    ret = db->cursor(db, NULL, &cursorp, 0);
    assert(0 == ret);

    gettimeofday(&t1, NULL);

    total = 0;
    n = 200;
    for (i = 1; i <= RECORD_NUM; ++i) {
        n = n + n / 3;
        if (n > size)
            n = 200;
        total += n;

        memset(&key, 0, sizeof(DBT));
        memset(&data, 0, sizeof(DBT));

        ret = cursorp->c_get(cursorp, &key, &data, DB_NEXT);
        assert(0 == ret);

        assert(*(unsigned*)(key.data) == i && key.size == sizeof(i));
        assert(n == data.size && 0 == memcmp(buffer, data.data, n));
    }
    ret = cursorp->c_get(cursorp, &key, &data, DB_NEXT);
    assert(DB_NOTFOUND == ret);

    gettimeofday(&t2, NULL);
    SHOW_TIME("bdb read", t2, t1, total);

    cursorp->c_close(cursorp);
    db->close(db, 0);
}

