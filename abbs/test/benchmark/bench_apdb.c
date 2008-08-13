#include "bench.h"

#include "apdb.h"


void
run_writer(const char* buffer, unsigned size, const char* filename)
{
    apdb_t* db;
    unsigned i,n, total;
    struct timeval t1, t2;
    int ret;
    char* s;

    /* init */
    n = strlen(filename);
    s = (char*)calloc(1, n + 3);
    assert(NULL != s);

    memcpy(s, filename, n);
    s[n++] = '.';
    s[n] = 'd';
    unlink(s);
    s[n] = 'i';
    unlink(s);

    db = apdb_open(filename, 'w', 0);
    assert(NULL != db);

    /* fill db header, this record isn't regarded as first record   */
    assert(-1 != (int)apdb_add_begin(db, 0) &&
           -1 != apdb_append_data(db, NULL, 0) &&
           -1 != apdb_add_end(db, NULL));


    gettimeofday(&t1, NULL);

    /* insert */
    total = 0;
    n = 200;
    for (i = 0; i < RECORD_NUM; ++i) {
        n = n + n / 3;
        if (n > size)
            n = 200;
        total += n;

        ret = apdb_add_begin(db, n);
        assert(-1 != ret);

        ret = apdb_append_data(db, buffer, n);
        assert(-1 != ret);

        ret = apdb_add_end(db, NULL);
        assert(-1 != ret);
    }

    gettimeofday(&t2, NULL);
    SHOW_TIME("apdb write", t2, t1, total);

    /* finalize */
    apdb_close(db);
}


void
run_reader(const char* buffer, unsigned size, const char* filename)
{
    apdb_t* db;
    apdb_record_t r;
    unsigned i,n, total;
    struct timeval t1, t2;

    db = apdb_open(filename, 'r', 0);
    assert(NULL != db);

    gettimeofday(&t1, NULL);

    r = apdb_first(db);

    total = 0;
    n = 200;
    for (i = 0; i < RECORD_NUM; ++i) {
        n = n + n / 3;
        if (n > size)
            n = 200;
        total += n;

        assert(-1 != r);
        assert(n == apdb_record_length(db, r));
        assert(0 == memcmp(buffer, apdb_record_data(db, r), n));

        r= apdb_next(db, r);
    }
    assert(-1 == r);
    gettimeofday(&t2, NULL);
    SHOW_TIME("apdb read", t2, t1, total);


    apdb_close(db);
}

