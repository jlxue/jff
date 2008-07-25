/*
 * board - append-only database to store articles of a BBS board
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
#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include <fcntl.h>
#include <sys/file.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "apdb.h"
#include "board.h"
#include "util.h"


typedef struct {
    unsigned char   header_t_len;
    unsigned short  index_t_len;
    unsigned short  data_t_len;
    unsigned char   int_len;
    unsigned char   long_len;
    unsigned char   unsigned_len;
    unsigned char   size_t_len;
    unsigned char   off_t_len;
    unsigned char   time_t_len;
    unsigned char   struct_pack;
} header_t;


typedef struct {
    unsigned        pid;        /* id of the replied article                */
    unsigned        tid;        /* id of the first article on this topic    */
    time_t          ctime;      /* time of creation                         */
    time_t          mtime;      /* time of modification                     */
    char            author[AUTHOR_LEN]; /* can't contains '-', see boardd.c */
    char            title[TITLE_LEN];
} index_t;


typedef struct {
    index_t         info;
    char            content[];
} data_t;


struct board_s {
    apdb_t*         db;

    index_t         next_index;
};


board_t*
board_open(const char* path, char mode)
{
    board_t* board;
    header_t h;

    board = (board_t*)malloc(sizeof(board_t));
    if (NULL == board)
        return NULL;

    /* 
     * Because all new articles are added with this temporary index,
     * by setting the last byte to '\0' we can prvent reader processes
     * from accessing memory out of range when writer process is
     * overwriting the author and title fields.
     */
    board->next_index.author[AUTHOR_LEN - 1] = '\0';
    board->next_index.title[TITLE_LEN - 1] = '\0';

    board->db = apdb_open(path, mode, sizeof(index_t));
    ERRORP_IF(NULL == board->db, "can't open board: path=%s, mode=%c",
              path, mode);

    /* byte order has been checked against 'magic' field in apdb_open() */
    h.header_t_len = sizeof(header_t);
    h.index_t_len = sizeof(index_t);
    h.data_t_len = sizeof(data_t);
    h.int_len = sizeof(int);
    h.long_len = sizeof(long);
    h.unsigned_len = sizeof(unsigned);
    h.size_t_len = sizeof(size_t);
    h.off_t_len = sizeof(off_t);
    h.time_t_len = sizeof(time_t);
    h.struct_pack = get_struct_pack();

    if (0 == apdb_count(board->db)) {
        index_t index;

        if ('r' == mode)
            goto L_error;

        memset(&index, 0, sizeof(index_t));
        if (-1 == apdb_add_begin(board->db, sizeof(h)) ||
                -1 == apdb_append_data(board->db, &h, sizeof(h)) ||
                -1 == apdb_add_end(board->db, &index))
            goto L_error;
    } else {
        apdb_record_t r = apdb_get(board->db, 0);
        const void* data = apdb_record_data(board->db, r);
        int len = apdb_record_length(board->db, r);
        if (NULL == data || 0 != memcmp(data, &h, sizeof(h)) ||
                len != sizeof(h))
            goto L_error;
    }

    return board;

L_error:
    board_close(board);
    return NULL;
}


int
board_close(board_t* board)
{
    int ret = 0;

    if (NULL == board)
        return 0;

    ret = apdb_close(board->db);

    free(board);

    return ret;
}


article_t
board_get(board_t* board, unsigned id)
{
    article_t article;
    unsigned flags;

    assert(NULL != board);

    /* header record, hidden to the caller  */
    if (0 == id)
        return -1;

    article = apdb_get(board->db, id);
    if (-1 == article)
        return -1;

    flags = apdb_record_flags(board->db, article);
    if (flags & (DELETED | WRITING))
        return -1;
    else
        return article;
}


article_t
board_first(board_t* board, unsigned mask)
{
    apdb_record_t r;

    assert(NULL != board);

    r = apdb_first(board->db);
    while (-1 != r && (mask & apdb_record_flags(board->db, r)))
        r = apdb_next(board->db, r);

    return r;
}


article_t
board_last(board_t* board, unsigned mask)
{
    apdb_record_t r;

    assert(NULL != board);

    r = apdb_last(board->db);
    while (-1 != r && (mask & apdb_record_flags(board->db, r)))
        r = apdb_previous(board->db, r);

    return r;
}


article_t
board_next(board_t* board, article_t a, unsigned mask)
{
    assert(NULL != board);

    a = apdb_next(board->db, a);
    while (-1 != a && (mask & apdb_record_flags(board->db, a)))
        a = apdb_next(board->db, a);

    return a;
}


article_t
board_previous(board_t* board, article_t a, unsigned mask)
{
    assert(NULL != board);

    a = apdb_previous(board->db, a);
    while (-1 != a && (mask & apdb_record_flags(board->db, a)))
        a = apdb_previous(board->db, a);

    return a;
}


static int
add_begin(board_t* board, unsigned tid, unsigned pid,
          const char* author, const char* title, size_t article_len)
{
    size_t len;
    int id;

    assert(NULL != board && NULL != author && NULL != title);

    len = strlen(author);
    if (len >= AUTHOR_LEN || 0 == len)
        return -1;

    len = strlen(title);
    if (len >= TITLE_LEN || 0 == len)
        return -1;

    if (-1 == (id = apdb_add_begin(board->db, sizeof(data_t) + article_len)))
        return -1;

    if (0 == tid) {
        board->next_index.tid = (unsigned)id;
        board->next_index.pid = (unsigned)id;
    } else {
        board->next_index.tid = tid;
        board->next_index.pid = pid;
    }
    board->next_index.ctime = board->next_index.mtime = time(NULL);
    
    strcpy(board->next_index.author, author);
    strcpy(board->next_index.title, title);

    return apdb_append_data(board->db, &board->next_index, sizeof(index_t));
}


int
board_post_begin(board_t* board, const char* author, const char* title,
                 size_t article_len)
{
    return add_begin(board, 0, 0, author, title, article_len);
}


int
board_append_data(board_t* board, const void* content, size_t length)
{
    assert(NULL != board);

    return apdb_append_data(board->db, content, length);
}


article_t
board_post_end(board_t* board)
{
    assert(NULL != board);

    return apdb_add_end(board->db, &board->next_index);
}


int
board_reply_begin(board_t* board, article_t article,
                  const char* author, const char* title, size_t article_len)
{
    unsigned tid, pid;
    const index_t* indexp;

    assert(NULL != board);

    indexp = (const index_t*)apdb_record_index(board->db, article);

    tid = indexp->tid;
    pid = apdb_record_id(board->db, article);

    return add_begin(board, tid, pid, author, title, article_len);
}


int
board_reply_end(board_t* board)
{
    assert(NULL != board);

    return apdb_add_end(board->db, &board->next_index);
}


int
board_modify_begin(board_t* board, article_t article,
                   const char* title, size_t article_len)
{
    const index_t* indexp;
    size_t len;

    assert(NULL != board && NULL != title);

    len = strlen(title);
    if (len >= TITLE_LEN || 0 == len)
        return -1;

    if (-1 == apdb_update_begin(board->db, article, sizeof(data_t) + article_len))
        return -1;

    indexp = (const index_t*)apdb_record_index(board->db, article);

    board->next_index.tid = indexp->tid;
    board->next_index.pid = indexp->pid;
    board->next_index.ctime = indexp->ctime;
    board->next_index.mtime = time(NULL);
    strcpy(board->next_index.author, indexp->author);
    strcpy(board->next_index.title, title);

    return apdb_append_data(board->db, &board->next_index, sizeof(index_t));
}


int
board_modify_end(board_t* board)
{
    assert(NULL != board);

    return apdb_update_end(board->db, &board->next_index);
}


void
board_delete(board_t* board, article_t article)
{
    assert(NULL != board);

    apdb_delete(board->db, article);
}


unsigned
board_article_id(board_t* board, article_t article)
{
    assert(NULL != board);

    return apdb_record_id(board->db, article);
}


unsigned
board_article_flags(board_t* board, article_t article)
{
    assert(NULL != board);

    return apdb_record_flags(board->db, article);
}


unsigned
board_article_length(board_t* board, article_t article)
{
    assert(NULL != board);

    return apdb_record_length(board->db, article) - sizeof(data_t);
}


const char*
board_article_content(board_t* board, article_t article)
{
    const data_t* data;

    assert(NULL != board);

    data = (const data_t*)apdb_record_data(board->db, article);

    return data->content;
}


unsigned
board_article_tid(board_t* board, article_t article)
{
    const index_t* indexp;

    assert(NULL != board);

    indexp = (const index_t*)apdb_record_index(board->db, article);

    return indexp->tid;
}


unsigned
board_article_pid(board_t* board, article_t article)
{
    const index_t* indexp;

    assert(NULL != board);

    indexp = (const index_t*)apdb_record_index(board->db, article);

    return indexp->pid;
}


time_t
board_article_ctime(board_t* board, article_t article)
{
    const index_t* indexp;

    assert(NULL != board);

    indexp = (const index_t*)apdb_record_index(board->db, article);

    return indexp->ctime;
}


time_t
board_article_mtime(board_t* board, article_t article)
{
    const index_t* indexp;

    assert(NULL != board);

    indexp = (const index_t*)apdb_record_index(board->db, article);

    return indexp->mtime;
}


const char*
board_article_author(board_t* board, article_t article)
{
    const index_t* indexp;

    assert(NULL != board);

    indexp = (const index_t*)apdb_record_index(board->db, article);

    return indexp->author;
}


const char*
board_article_title(board_t* board, article_t article)
{
    const index_t* indexp;

    assert(NULL != board);

    indexp = (const index_t*)apdb_record_index(board->db, article);

    return indexp->title;
}


int
board_article_set_title(board_t* board, article_t article, const char* title)
{
    index_t* indexp;
    size_t len;

    assert(NULL != board && NULL != title);

    len = strlen(title);
    if (len >= TITLE_LEN || 0 == len)
        return -1;

    if (-1 == apdb_record_lock(board->db, article))
        return -1;

    indexp = (index_t*)apdb_record_index(board->db, article);

    strcpy(indexp->title, title);
    indexp->mtime = time(NULL);

    apdb_record_unlock(board->db, article);

    return 0;
}


int
board_article_set_flags(board_t* board, article_t article, unsigned flags)
{
    assert(NULL != board);

    return apdb_record_set_flags(board->db, article, flags);
}

