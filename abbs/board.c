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
#include "board.h"
#include "util.h"


#define AUTHOR_LEN      32
#define TITLE_LEN       128


typedef struct {
    unsigned short  index_t_len;
    unsigned short  data_t_len;
    unsigned char   header_t_len;
    unsigned char   int_len;
    unsigned char   unsigned_len;
    unsigned char   size_t_len;
    unsigned char   off_t_len;
    unsigned char   time_t_len;
    unsigned char   struct_pack;
} header_t;


typedef struct {
    unsigned        tid;        /* id of the first article on this topic    */
    unsigned        pid;        /* id of the replied article                */
    time_t          ctime;      /* time of creation                         */
    time_t          mtime;      /* time of modification                     */
    char            author[AUTHOR_LEN];
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

    board->db = apdb_open(path, mode, sizeof(index_t));
    ERRORP_IF(NULL == board->db, "can't open board: path=%s, mode=%c",
              path, mode);

    /* byte order has been checked against 'magic' field in apdb_open() */
    h.index_t_len = sizeof(index_t);
    h.data_t_len = sizeof(data_t);
    h.header_t_len = sizeof(header_t);
    h.int_len = sizeof(int);
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

    assert(NULL != board);

    ret = apdb_close(board->db);

    free(board);

    return ret;
}


article_t
board_get(board_t* board, unsigned id)
{
    assert(NULL != board);

    /* header record, hidden to the caller  */
    if (0 == id)
        return -1;

    return apdb_get(board->db, id);
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
    pid = board_article_id(board, article);

    return add_begin(board, tid, pid, author, title, article_len);
}


int
board_reply_end(board_t* board)
{
    assert(NULL != board);

    return apdb_add_end(board->db, &board->next_index);
}


int
board_modify_begin(board_t* board, article_t article, unsigned flags,
                   const char* title, size_t article_len)
{
    const index_t* indexp;
    size_t len;

    assert(NULL != board && NULL != title);

    len = strlen(title);
    if (len >= TITLE_LEN || 0 == len)
        return -1;

    board_article_set_flags(board, article, flags);

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

    return apdb_delete(board->db, article);
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
    const char* data;

    assert(NULL != board);

    data = (const char*)apdb_record_data(board->db, article);

    return data + sizeof(data_t);
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
    const index_t* indexp;
    index_t index;
    size_t len;

    assert(NULL != board && NULL != title);

    len = strlen(title);
    if (len >= TITLE_LEN || 0 == len)
        return -1;

    indexp = (const index_t*)apdb_record_index(board->db, article);
    memcpy(&index, indexp, sizeof(index_t));
    index.mtime = time(NULL);
    strcpy(index.title, title);

    return apdb_record_set_index(board->db, article, &index);
}


int
board_article_set_flags(board_t* board, article_t article, unsigned flags)
{
    assert(NULL != board);

    return apdb_record_set_flags(board->db, article, flags);
}

