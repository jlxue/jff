#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "apdb.h"

static void
remove_db(const char* file)
{
    struct stat st;
    char* path;
    int len;

    len = strlen(file);
    path = malloc(len + 3);
    assert(NULL != path);
    strcpy(path, file);
    path[len] = '.';
    path[len + 1] = 'd';
    path[len + 2] = '\0';

    if (0 == lstat(path, &st))
        assert(0 == unlink(path));
    path[len + 1] = 'i';
    if (0 == lstat(path, &st))
        assert(0 == unlink(path));

    free(path);
}

static void
test_apdb_open_close(const char* file)
{
    apdb_t* db;

    remove_db(file);

    db = apdb_open(file, 'r', 0);
    assert(NULL == db);
    fprintf(stderr, "must not open non-exist db: ok\n");

    db = apdb_open(file, 'r', 0);
    assert(NULL == db);
    fprintf(stderr, "must not create db: ok\n");

    db = apdb_open(file, 'w', 0);
    assert(NULL != db);
    apdb_close(db);
    fprintf(stderr, "create db: ok\n");
    
    db = apdb_open(file, 'r', 0);
    assert(NULL != db);
    apdb_close(db);
    fprintf(stderr, "open db to read: ok\n");

    db = apdb_open(file, 'w', 0);
    assert(NULL != db);
    apdb_close(db);
    fprintf(stderr, "open db to write: ok\n");

    db = apdb_open(file, 'r', 0);
    assert(NULL != db);
    apdb_close(db);
    fprintf(stderr, "open db to read: ok\n");
}


static void
test_apdb_get_record(const char* file)
{
    apdb_t* db;
    unsigned id;

    remove_db(file);

    db = apdb_open(file, 'w', 0);
    assert(NULL != db);
    apdb_close(db);

    db = apdb_open(file, 'r', 0);
    assert(NULL != db);

    for (id = 0; id < 1000000; ++id)
        assert(-1 == apdb_get(db, id));
    fprintf(stderr, "get non-exist article: ok\n");

    apdb_close(db);
}


static void
test_apdb_add_record(const char* file)
{
    apdb_t* db;
    apdb_record_t record;
    unsigned  ids[256];
    int i;
    char content[2048];

    for (i = 0; i < sizeof(content); ++i)
        content[i] = (char)i;

    remove_db(file);

    /*
     * add records
     */
    db = apdb_open(file, 'w', 0);
    assert(NULL != db);

    for (i = 0; i < sizeof(ids)/sizeof(unsigned); ++i) {
        assert(i == apdb_add_begin(db, i));
        assert(0 == apdb_append_data(db, content, i));
        record = apdb_add_end(db, NULL);
        assert(-1 != record && i == apdb_record_id(db, record));
        ids[i] = i;
    }

    apdb_close(db);

    fprintf(stderr, "add 256 articles: ok\n");


    /*
     * get records.
     */
    db = apdb_open(file, 'r', 0);
    assert(NULL != db);

    for (i = 0; i < sizeof(ids)/sizeof(unsigned); ++i) {
        record = apdb_get(db, ids[i]);
        assert(-1 != record);
        printf("id = %4d length=%4d\n",
               apdb_record_id(db, record),
               apdb_record_length(db, record));
        assert(apdb_record_id(db, record) == ids[i] &&
               apdb_record_length(db, record) == i &&
               NULL == apdb_record_index(db, record) &&
               0 == memcmp(content, apdb_record_data(db, record), i));
    }
    fprintf(stderr, "get 256 articles: ok\n");

    apdb_close(db);
}


static void
show_usage(const char* progname)
{
    printf("Usage: %s <file>\n", progname);
}


int
main(int argc, char** argv)
{

    if (1 == argc) {
        show_usage(argv[0]);
        return EXIT_FAILURE;
    }


    test_apdb_open_close(argv[1]);

    test_apdb_get_record(argv[1]);

    test_apdb_add_record(argv[1]);

    return EXIT_SUCCESS;
}

