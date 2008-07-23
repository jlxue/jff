/*
 * apdb - append-only record database
 *
 * Copyright (C) 2008 Liu Yubao <yubao.liu@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 *
 * ChangeLog:
 *  2008-07-16  Liu Yubao
 *      * initial version, pass some basic tests
 *
 */

/* #define _FILE_OFFSET_BITS       64 */

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

#include <fcntl.h>
#include <sys/file.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "apdb.h"
#include "util.h"


#define GUARD               0xDEADBEEF
#define INDEX_EXPAND_SIZE   4096
#define DATA_EXPAND_SIZE    (1024 * 1024 * 2)
#define ALIGN_SIZE          8

#ifndef O_NOATIME
#define O_NOATIME           0
#endif

#define RECORD_TO_INDEX(db, record) \
    ((index_t*)((char*)(db)->index_mmap + (record)))

#define INDEX_TO_RECORD(db, index)  \
    ((char*)(index) - (char*)(db)->index_mmap)

#define CAN_WRITE(db)   (PROT_WRITE == (PROT_WRITE & db->prot))


/*
 * actions that can't finish quickly
 */
typedef enum {
    NONE,
    ADD,
    UPDATE
} action_t;


typedef struct {
    unsigned    guard;
    unsigned    flags;
    unsigned    id;
    size_t      length;     /* length of content */
    char        content[];
} data_t;


typedef struct {
    unsigned    guard;
    unsigned    flags;
    unsigned    id;
    size_t      length;     /* length of whole data_t including content */
    off_t       offset;     /* offset of data_t in data file            */
    char        content[];  /* its length is determined in apdb_open()  */
} index_t;


struct apdb_s {
    int         data_fd;
    int         index_fd;
    void*       data_mmap;
    void*       index_mmap;
    size_t      data_mmap_len;
    size_t      index_mmap_len;
    size_t      data_file_len;
    size_t      index_file_len;
    int         prot;

    size_t      index_len;  /* whole length of index_t including content */
    int         error;

    action_t    action;
    unsigned    next_id;        /* id of the new record being added     */
    size_t      next_length;    /* length of data content               */
    size_t      next_offset;    /* offset of data_t in data file        */
    size_t      next_written;   /* how much content has been written    */
    apdb_record_t next_record;  /* the record being updated             */
};


apdb_t*
apdb_open(const char* path, char mode, size_t index_content_len)
{
    int         readonly;
    apdb_t*     db;
    char*       fullpath;
    size_t      len;
    struct stat st;

    assert(NULL != path && index_content_len >= 0);


    /*
     * initialize apdb_t
     */
    db = (apdb_t*)malloc(sizeof(apdb_t));
    if (NULL == db)
        return NULL;

    db->data_fd = db->index_fd = -1;
    db->data_mmap = db->index_mmap = MAP_FAILED;
    db->index_len = sizeof(index_t) + index_content_len;
    db->error = 0;
    db->action = NONE;
    db->next_id = UINT_MAX;
    db->next_length = 0;
    db->next_offset = UINT_MAX;


    /*
     * open data file and index file
     */
    len = strlen(path);
    fullpath = (char*)malloc(len + 3);
    ERRORP_IF(NULL == fullpath, "failed to malloc for fullpath");

    strcpy(fullpath, path);
    fullpath[len++] = '.';
    fullpath[len] = 'd';

    readonly = 'r' == mode;
    if (readonly) {
        int flags = O_RDONLY | O_NOATIME;

        db->data_fd = open(fullpath, flags);
        ERRORP_IF(-1 == db->data_fd, "failed to open %s readonly", fullpath);

        fullpath[len] = 'i';
        db->index_fd = open(fullpath, flags);
        ERRORP_IF(-1 == db->index_fd, "failed to open %s readonly", fullpath);

        db->prot = PROT_READ;
    } else {
        int flags = O_RDWR | O_APPEND | O_CREAT | O_NOATIME;

        db->data_fd = open(fullpath, flags, 0644);
        ERRORP_IF(-1 == db->data_fd, "failed to open %s readwrite", fullpath);
        ERRORP_IF(-1 == flock(db->data_fd, LOCK_EX | LOCK_NB),
               "failed to lock data file");

        fullpath[len] = 'i';
        db->index_fd = open(fullpath, flags, 0644);
        ERRORP_IF(-1 == db->index_fd, "failed to open %s readwrite", fullpath);
        ERRORP_IF(-1 == flock(db->index_fd, LOCK_EX | LOCK_NB),
                  "failed to lock index file");

        db->prot = PROT_READ | PROT_WRITE;
    }

    free(fullpath);


    /*
     * mmap data file
     */
    ERRORP_IF(-1 == fstat(db->data_fd, &st), "failed to stat data file");
    db->data_mmap_len = st.st_size + DATA_EXPAND_SIZE;
    db->data_mmap = mmap(NULL, db->data_mmap_len, PROT_READ, MAP_SHARED,
                         db->data_fd, 0);
    ERRORP_IF(MAP_FAILED == db->data_mmap, "failed to mmap data file");

    db->data_file_len = st.st_size;


    /*
     * check data file
     */
    if (st.st_size > 0) {
        data_t* data;

        ERROR_IF(st.st_size < sizeof(data_t), "corrupted data file");

        data = (data_t*)db->data_mmap;
        ERROR_IF(GUARD != data->guard || 0 != data->id ||
                 st.st_size < ALIGN_UP(sizeof(data_t) + data->length,
                                       ALIGN_SIZE),
                 "corrupted header record");
    }


    /*
     * mmap index file
     */
    ERRORP_IF(-1 == fstat(db->index_fd, &st), "failed to stat index file");
    db->index_mmap_len = st.st_size + INDEX_EXPAND_SIZE;
    db->index_mmap = mmap(NULL, db->index_mmap_len, db->prot, MAP_SHARED,
                          db->index_fd, 0);
    ERRORP_IF(MAP_FAILED == db->index_mmap, "failed to mmap index file");

    db->index_file_len = st.st_size;


    /*
     * check index file
     */
    if (st.st_size > 0) {
        index_t* index;
        unsigned count;

        ERROR_IF(st.st_size < db->index_len, "corrupted index file");

        index = RECORD_TO_INDEX(db, 0);
        ERROR_IF(GUARD != index->guard || 0 != index->id || 0 != index->offset,
                 "corrupted header index");

        /*
         * only one writer process, so there shouldn't be any partially
         * written index
         */
        ERROR_IF(!readonly && (st.st_size % db->index_len) != 0,
                 "found partially written index");

        /*
         * check last index
         */
        count = st.st_size / db->index_len;
        index = RECORD_TO_INDEX(db, (count - 1) * db->index_len);
        ERROR_IF(GUARD != index->guard, "corrupted tailer index");

        ERROR_IF(db->data_file_len < ALIGN_UP(index->offset + index->length,
                                              ALIGN_SIZE),
                 "corrupted tailer record");
    }


    return db;

L_error:
    if (NULL != fullpath)
        free(fullpath);
    apdb_close(db);

    return NULL;
}


int
apdb_close(apdb_t* db)
{
    int ret = 0;

    assert(NULL != db);

    if (MAP_FAILED != db->data_mmap)
        ret = munmap(db->data_mmap, db->data_mmap_len);

    if (MAP_FAILED != db->index_mmap)
        if (-1 == munmap(db->index_mmap, db->index_mmap_len))
            ret = -1;

    if (-1 != db->data_fd)
        if (-1 == close(db->data_fd))
            ret = -1;

    if (-1 != db->index_fd)
        if (-1 == close(db->index_fd))
            ret = -1;

    free(db);

    return ret;
}


static void
remap_data_file(apdb_t* db, off_t to)
{
    struct stat st;

    assert(NULL != db);

    if (to > 0 && to <= db->data_mmap_len)
        return;

    ERRORP_IF(-1 == fstat(db->data_fd, &st),
              "failed to fstat");

    db->data_file_len = st.st_size;

    if (db->data_mmap_len < db->data_file_len) {
        ERRORP_IF(-1 == munmap(db->data_mmap, db->data_mmap_len),
                  "failed to munmap");

        db->data_mmap_len = db->data_file_len + DATA_EXPAND_SIZE;

        /* always mmap to read  */
        db->data_mmap = mmap(NULL, db->data_mmap_len, PROT_READ,
                              MAP_SHARED, db->data_fd, 0);
        ERRORP_IF(MAP_FAILED == db->data_mmap,
                  "failed to mmap");
    }

    return;

L_error:
    db->error = -1;
}


static void
remap_index_file(apdb_t* db, off_t to)
{
    struct stat st;

    assert(NULL != db);

    if (to > 0 && to <= db->index_mmap_len)
        return;

    ERRORP_IF(-1 == fstat(db->index_fd, &st),
              "failed to fstat");

    db->index_file_len = st.st_size;

    if (db->index_mmap_len < db->index_file_len) {
        ERRORP_IF(-1 == munmap(db->index_mmap, db->index_mmap_len),
                  "failed to munmap");

        db->index_mmap_len = db->index_file_len + INDEX_EXPAND_SIZE;

        /* mmap PROT_READ for client and PROT_READ | PROT_WRITE for server */
        db->index_mmap = mmap(NULL, db->index_mmap_len, db->prot,
                              MAP_SHARED, db->index_fd, 0);
        ERRORP_IF(MAP_FAILED == db->index_mmap,
                  "failed to mmap");
    }

    return;

L_error:
    db->error = -1;
}


static unsigned
get_next_id(apdb_t* db)
{
    unsigned count;
    index_t* index;

    assert(NULL != db);

    if (0 == db->index_file_len)
        return 0;

    count = db->index_file_len / db->index_len;
    index = RECORD_TO_INDEX(db, (count - 1) * db->index_len);
    ERROR_IF(GUARD != index->guard || UINT_MAX == index->id, "corrupted index");

    return index->id + 1;

L_error:
    db->error = -1;
    return UINT_MAX;
}


int
apdb_add_begin(apdb_t* db, size_t length)
{
    data_t data;

    assert(NULL != db && NONE == db->action && CAN_WRITE(db));

    if (db->error)
        return -1;

    db->next_id = get_next_id(db);
    if (db->error || UINT_MAX == db->next_id /* avoid overflow */)
        return -1;

    db->next_length = length;
    db->next_offset = db->data_file_len;
    db->next_written = 0;

    data.guard = GUARD;
    data.flags = 0;
    data.id = db->next_id;
    data.length = length;

    ERRORP_IF(sizeof(data_t) != writen(db->data_fd, &data, sizeof(data_t)),
              "failed to write");

    db->data_file_len += sizeof(data_t);
    db->action = ADD;

    return (int)data.id;

L_error:
    db->error = -1;
    return -1;
}


int
apdb_append_data(apdb_t* db, const void* content, size_t length)
{
    assert(NULL != db && (ADD == db->action || UPDATE == db->action) &&
           CAN_WRITE(db) && db->next_length - db->next_written >= length);

    if (db->error) {
        db->action = NONE;
        return -1;
    }

    ERROR_IF(db->next_length - db->next_written < length,
             "write more data than expected");

    ERRORP_IF(length != writen(db->data_fd, content, length),
              "failed to write");

    db->data_file_len += length;
    db->next_written += length;

    return 0;

L_error:
    db->error = -1;
    db->action = NONE;
    return -1;
}


static void
pad_data_file(apdb_t* db)
{
    static char zeros[ALIGN_SIZE] = {'\0'};
    size_t aligned_data_file_len;

    assert(NULL != db && 0 == db->error);

    aligned_data_file_len = ALIGN_UP(db->data_file_len, ALIGN_SIZE);

    if (aligned_data_file_len > db->data_file_len) {
        int len = aligned_data_file_len - db->data_file_len;
        ERRORP_IF(len != writen(db->data_fd, zeros, len),
                  "failed to pad");
        db->data_file_len = aligned_data_file_len;
    }

    return;

L_error:
    db->error = -1;
}


apdb_record_t
apdb_add_end(apdb_t* db, const void* content)
{
    index_t index, *indexp;
    int content_len;

    assert(NULL != db && ADD == db->action && CAN_WRITE(db) &&
           db->next_length == db->next_written);

    db->action = NONE;

    if (db->error)
        return -1;

    ERROR_IF(db->next_length != db->next_written,
             "write less data than expected");

    pad_data_file(db);
    if (db->error)
        return -1;

    index.guard = GUARD;
    index.flags = WRITING;
    index.id = db->next_id;
    index.length = db->next_length + sizeof(data_t);
    index.offset = db->next_offset;

    ERRORP_IF(sizeof(index_t) != writen(db->index_fd, &index, sizeof(index_t)),
              "failed to write");

    content_len = db->index_len - sizeof(index_t);
    ERRORP_IF(content_len != writen(db->index_fd, content, content_len),
              "failed to write index content");

    db->index_file_len += db->index_len;
    remap_index_file(db, db->index_file_len);
    if (db->error)
        return -1;

    indexp = RECORD_TO_INDEX(db, db->index_file_len - db->index_len);

    assert(GUARD == indexp->guard && WRITING == indexp->flags &&
           index.id == indexp->id && index.length == indexp->length &&
           index.offset == indexp->offset);

    indexp->flags = 0;

    return INDEX_TO_RECORD(db, indexp);

L_error:
    db->error = -1;
    return -1;
}


static int
cmp_index_by_id(const void* a, const void* b)
{
    const index_t* k = (index_t*)a;
    const index_t* i = (index_t*)b;

    return (k->id - i->id);
}


apdb_record_t
apdb_get(apdb_t* db, unsigned id)
{
    unsigned count;
    index_t* index;
    index_t  key;

    assert(NULL != db);

    if (db->error)
        return -1;

    remap_index_file(db, 0);
    if (db->error)
        return -1;

    count = db->index_file_len / db->index_len;
    if (0 == count)
        return -1;
    else if (0 == id)
        return 0;

    key.id = id;
    index = bsearch(&key, db->index_mmap, count, db->index_len,
                    cmp_index_by_id);

    if (NULL == index) {
        return -1;
    } else if (GUARD == index->guard) {
        return INDEX_TO_RECORD(db, index);
    } else {
        db->error = -1;
        return -1;
    }
}


void
apdb_delete(apdb_t* db, apdb_record_t record)
{
    index_t* index;

    assert(NULL != db && record >= 0 && CAN_WRITE(db));

    index = RECORD_TO_INDEX(db, record);
    assert(GUARD == index->guard);

    index->flags |= DELETED;
}


int
apdb_update_begin(apdb_t* db, apdb_record_t record, size_t length)
{
    index_t* index;
    data_t data;

    assert(NULL != db && record >= 0 && NONE == db->action && CAN_WRITE(db));

    if (db->error)
        return -1;

    index = RECORD_TO_INDEX(db, record);
    assert(GUARD == index->guard);

    db->next_record = record;
    db->next_length = length;
    db->next_offset = db->data_file_len;
    db->next_written = 0;

    data.guard = GUARD;
    data.flags = index->flags;
    data.id = index->id;
    data.length = length;

    ERRORP_IF(sizeof(data_t) != writen(db->data_fd, &data, sizeof(data_t)),
              "failed to write");

    db->data_file_len += sizeof(data_t);
    db->action = UPDATE;

    return 0;

L_error:
    db->error = -1;
    return -1;
}


int
apdb_update_end(apdb_t* db, void* content)
{
    index_t* index;

    assert(NULL != db && UPDATE == db->action && CAN_WRITE(db));

    db->action = NONE;

    if (db->error)
        return -1;

    ERROR_IF(db->next_length != db->next_written,
             "write less data than expected");

    pad_data_file(db);
    if (db->error)
        return -1;

    index = RECORD_TO_INDEX(db, db->next_record);
    assert(GUARD == index->guard);

    index->flags |= WRITING;

    index->length = sizeof(data_t);  /* make reader processes read nothing */
    index->offset = db->next_offset;
    if (NULL != content)
        memcpy(index->content, content, db->index_len - sizeof(index_t));
    index->length += db->next_length;

    index->flags &= ~WRITING;

    return 0;

L_error:
    db->error = -1;
    return -1;
}


unsigned
apdb_count(apdb_t* db)
{
    assert(NULL != db);

    if (db->error)
        return 0;

    remap_index_file(db, 0);
    if (db->error)
        return 0;

    return (db->index_file_len / db->index_len);
}


apdb_record_t
apdb_first(apdb_t* db)
{
    assert(NULL != db);

    if (db->error)
        return -1;

    remap_index_file(db, 2 * db->index_len);
    if (db->error)
        return -1;

    if (db->index_file_len >= 2 * db->index_len) {
        index_t* index = RECORD_TO_INDEX(db, db->index_len);

        if (GUARD == index->guard) {
            return db->index_len;
        } else {
            db->error = -1;
            return -1;
        }
    } else {
        return -1;
    }
}


apdb_record_t
apdb_last(apdb_t* db)
{
    unsigned count;

    assert(NULL != db);

    if (db->error)
        return -1;

    remap_index_file(db, 0);
    if (db->error)
        return -1;

    /*
     * To avoid partially written index, we can't just return
     * db->index_file_len - db->index_len
     */
    count = db->index_file_len / db->index_len;

    if (count >= 2) {
        apdb_record_t record = (count - 1) * db->index_len;
        index_t* index = RECORD_TO_INDEX(db, record);

        if (GUARD == index->guard) {
            return record;
        } else {
            db->error = -1;
            return -1;
        }
    } else {
        return -1;
    }
}


apdb_record_t
apdb_next(apdb_t* db, apdb_record_t record)
{
    apdb_record_t next;

    assert(NULL != db && record >= 0);

    if (db->error)
        return -1;

    next = record + db->index_len;

    remap_index_file(db, next + db->index_len);
    if (db->error)
        return -1;

    if (db->index_file_len >= next + db->index_len) {
        index_t* index = RECORD_TO_INDEX(db, next);

        if (GUARD == index->guard) {
            return next;
        } else {
            db->error = -1;
            return -1;
        }
    } else {
        return -1;
    }
}


apdb_record_t
apdb_previous(apdb_t* db, apdb_record_t record)
{
    apdb_record_t prev;
    index_t* index;

    assert(NULL != db && record >= 0);

    if (db->error)
        return -1;

    if (record >= 2 * db->index_len) {
        prev = record - db->index_len;
        index = RECORD_TO_INDEX(db, prev);

        if (GUARD == index->guard) {
            return prev;
        } else {
            db->error = -1;
            return -1;
        }
    } else {
        return -1;
    }
}


int
apdb_for_each(apdb_t* db, apdb_record_t from, apdb_record_t to, unsigned count,
              int (*op)(apdb_record_t record, unsigned id, unsigned* flags,
                        size_t length, void* index, void* arg),
              void* arg)
{
    index_t* index;
    unsigned total;
    int step;

    assert(NULL != db && from >= 0 && to >= 0 && NULL != op);

    if (db->error)
        return -1;

    total = from - to;
    if (total > 0) {
        remap_index_file(db, from + db->index_len);

        assert(from + db->index_len <= db->index_file_len);

        step = -db->index_len;      /* backwards */
    } else if (total < 0) {
        remap_index_file(db, to + db->index_len);

        assert(to + db->index_len <= db->index_file_len);

        total = -total;
        step = db->index_len;       /* forwards */
    }

    total += 1;     /* [from, to], not [from, to)   */
    if (count > total)
        count = total;

    if (0 == count)
        return 0;

    index = RECORD_TO_INDEX(db, from);
    do {
        if (GUARD != index->guard) {
            db->error = -1;
            return -1;
        }

        if (op(INDEX_TO_RECORD(db, index), index->id, &index->flags,
                 index->length - sizeof(data_t),
                 db->index_len > sizeof(index_t) ? index->content : NULL,
                 arg))
            break;

        index = (index_t*)((char*)index + step);
    } while (--count > 0);

    return 0;
}


unsigned
apdb_record_id(apdb_t* db, apdb_record_t record)
{
    index_t* index;

    assert(NULL != db && record >= 0);

    index = RECORD_TO_INDEX(db, record);
    assert(GUARD == index->guard);

    return index->id;
}


unsigned
apdb_record_flags(apdb_t* db, apdb_record_t record)
{
    index_t* index;

    assert(NULL != db && record >= 0);

    index = RECORD_TO_INDEX(db, record);
    assert(GUARD == index->guard);

    return index->flags;
}


size_t
apdb_record_length(apdb_t* db, apdb_record_t record)
{
    index_t* index;

    assert(NULL != db && record >= 0);

    index = RECORD_TO_INDEX(db, record);
    assert(GUARD == index->guard);

    return index->length - sizeof(data_t);
}


const void*
apdb_record_data(apdb_t* db, apdb_record_t record)
{
    index_t* index;

    assert(NULL != db && record >= 0);

    if (db->error)
        return NULL;

    index = RECORD_TO_INDEX(db, record);
    assert(GUARD == index->guard && index->offset >= 0);

    if (sizeof(data_t) == index->length)
        return NULL;

    remap_data_file(db, index->offset + index->length);
    if (db->error)
        return NULL;

    return (char*)db->data_mmap + index->offset + sizeof(data_t);
}


void*
apdb_record_index(apdb_t* db, apdb_record_t record)
{
    index_t* index;

    assert(NULL != db && record >= 0);

    index = RECORD_TO_INDEX(db, record);
    assert(GUARD == index->guard);

    if (db->index_len > sizeof(index_t))
        return index->content;
    else
        return NULL;
}


int
apdb_record_set_flags(apdb_t* db, apdb_record_t record, unsigned flags)
{
    index_t* index;

    assert(NULL != db && record >= 0 && NONE == db->action && CAN_WRITE(db));

    if (db->error)
        return -1;

    index = RECORD_TO_INDEX(db, record);
    assert(GUARD == index->guard);

    index->flags = flags & ~(WRITING | DELETED);

    return 0;
}


int
apdb_record_lock(apdb_t* db, apdb_record_t record)
{
    index_t* index;

    assert(NULL != db && record >= 0 &&
           NONE == db->action && CAN_WRITE(db));

    if (db->error)
        return -1;

    index = RECORD_TO_INDEX(db, record);
    assert(GUARD == index->guard && !(WRITING & index->flags));

    index->flags |= WRITING;

    return 0;
}


void
apdb_record_unlock(apdb_t* db, apdb_record_t record)
{
    index_t* index;

    assert(NULL != db && record >= 0 &&
           NONE == db->action && CAN_WRITE(db));

    if (db->error)
        return;

    index = RECORD_TO_INDEX(db, record);
    assert(GUARD == index->guard && (WRITING & index->flags));

    index->flags &= ~WRITING;
}

