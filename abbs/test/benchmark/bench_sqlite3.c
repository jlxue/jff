#include "bench.h"

#include <sqlite3.h>


void
run_writer(const char* buffer, unsigned size, const char* filename)
{
    sqlite3* db;
    sqlite3_stmt* stmt;
    unsigned i,n, total;
    struct timeval t1, t2;
    int ret;

    /* init */
    unlink(filename);
    ret = sqlite3_open_v2(filename, &db,
                          SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL);
    assert(SQLITE_OK == ret);

    ret = sqlite3_exec(db, "CREATE TABLE t (id INTEGER PRIMARY KEY AUTOINCREMENT, data BLOB)",
                       NULL, NULL, NULL);
    assert(SQLITE_OK == ret);

    gettimeofday(&t1, NULL);

    /* insert */
    ret = sqlite3_prepare_v2(db, "INSERT INTO t (data) values (?)", -1, &stmt, NULL);
    assert(SQLITE_OK == ret);

    ret = sqlite3_exec(db, "BEGIN TRANSACTION",
                       NULL, NULL, NULL);
    assert(SQLITE_OK == ret);

    total = 0;
    n = 200;
    for (i = 0; i < RECORD_NUM; ++i) {
        n = n + n / 3;
        if (n > size)
            n = 200;
        total += n;

        ret = sqlite3_bind_blob(stmt, 1, buffer, n, SQLITE_STATIC);
        assert(SQLITE_OK == ret);

        ret = sqlite3_step(stmt);
        assert(SQLITE_DONE == ret);

        ret = sqlite3_reset(stmt);
        assert(SQLITE_OK == ret);
    }
    ret = sqlite3_exec(db, "COMMIT",
                       NULL, NULL, NULL);
    assert(SQLITE_OK == ret);

    gettimeofday(&t2, NULL);
    SHOW_TIME("sqlite3 write", t2, t1, total);

    /* finalize */
    sqlite3_finalize(stmt);
    sqlite3_close(db);
}


void
run_reader(const char* buffer, unsigned size, const char* filename)
{
    sqlite3* db;
    sqlite3_stmt* stmt;
    unsigned i,n, total;
    struct timeval t1, t2;
    int ret;

    ret = sqlite3_open_v2(filename, &db,
                          SQLITE_OPEN_READONLY, NULL);
    assert(SQLITE_OK == ret);


    gettimeofday(&t1, NULL);

    ret = sqlite3_prepare_v2(db, "SELECT data FROM t", -1, &stmt, NULL);
    assert(SQLITE_OK == ret);

    total = 0;
    n = 200;
    for (i = 0; i < RECORD_NUM; ++i) {
        n = n + n / 3;
        if (n > size)
            n = 200;
        total += n;

        ret = sqlite3_step(stmt);
        assert(SQLITE_ROW == ret);

        assert(n == (unsigned)sqlite3_column_bytes(stmt, 0));
        assert(0 == memcmp(buffer, sqlite3_column_blob(stmt, 0), n));
    }
    assert(SQLITE_DONE == sqlite3_step(stmt));
    gettimeofday(&t2, NULL);
    SHOW_TIME("sqlite3 read", t2, t1, total);

    sqlite3_finalize(stmt);
    sqlite3_close(db);
}

