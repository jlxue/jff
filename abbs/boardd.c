/*
 * boardd - daemon program to manage articles of a BBS board
 *
 * Copyright (C) 2008 Liu Yubao <yubao.liu@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */
#define _GNU_SOURCE         /* for strndup  */

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dirent.h>
#include <fcntl.h>
#include <sys/inotify.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <ev.h>

#include "apdb.h"
#include "board.h"
#include "pool.h"
#include "user.h"
#include "util.h"


#define EVENT_BUF_LEN       (4 * 1024)


typedef struct {
    unsigned        count;
    unsigned short* board_ids;
    char**          db_paths;
    board_t**       boards;

    int             fd;                     /* inotify fd */
    int             wd;                     /* watch fd  */
    char            events[EVENT_BUF_LEN];

    size_t          pool_dir_len;
    char            pool_file[POOL_PATH_MAX];
    char            real_file[POOL_PATH_MAX];
} boardd_t;


static int
boardd_destroy(boardd_t* boardd)
{
    unsigned i;
    int ret = 0;

    if (NULL == boardd)
        return 0;

    if (NULL == boardd->board_ids || NULL == boardd->db_paths ||
            NULL == boardd->boards)
        goto L_leave;


    for (i = 0; i < boardd->count; ++i) {
        if (NULL != boardd->db_paths[i])
            free(boardd->db_paths[i]);
        if (NULL != boardd->boards[i])
            ret |= board_close(boardd->boards[i]);
    }

L_leave:
    if (NULL != boardd->board_ids)
        free(boardd->board_ids);
    if (NULL != boardd->db_paths)
        free(boardd->db_paths);
    if (NULL != boardd->boards)
        free(boardd->boards);

    if (-1 != boardd->fd)
        ret |= close(boardd->fd);

    free(boardd);

    return ret;
}


static boardd_t*
boardd_create(char* pool_dir, unsigned count, unsigned short* ids, char** paths)
{
    boardd_t* boardd;
    struct stat st;
    unsigned i;
    long flags;

    assert(NULL != pool_dir && count > 0);

    boardd = (boardd_t*)calloc(1, sizeof(boardd_t));
    if (NULL == boardd)
        return NULL;

    boardd->fd = -1;

    boardd->board_ids = (unsigned short*)calloc(count, sizeof(unsigned short));
    boardd->db_paths = (char**)calloc(count, sizeof(char*));
    boardd->boards = (board_t**)calloc(count, sizeof(board_t*));
    ERRORP_IF(NULL == boardd->board_ids || NULL == boardd->db_paths ||
              NULL == boardd->boards, "out of memory");


    boardd->pool_dir_len = strlen(pool_dir);
    ERROR_IF(boardd->pool_dir_len < 2 || boardd->pool_dir_len >= POOL_PATH_MAX - 1,
             "invalid pool dir");
    ERRORP_IF(-1 == stat(pool_dir, &st), "can't stat(%s)", pool_dir);
    ERROR_IF(! S_ISDIR(st.st_mode), "%s isn't a directory", pool_dir);

    memcpy(boardd->pool_file, pool_dir, boardd->pool_dir_len);
    if ('/' != pool_dir[boardd->pool_dir_len - 1]) {
        boardd->pool_file[boardd->pool_dir_len] = '/';
        ++boardd->pool_dir_len;
    }


    boardd->fd = inotify_init();
    ERRORP_IF(-1 == boardd->fd, "inotify_init() failed");

    flags = fcntl(boardd->fd, F_GETFL);
    flags |= O_NONBLOCK;
    ERROR_IF(-1 == fcntl(boardd->fd, F_SETFL, flags),
             "can't set inotify fd to nonblock mode");
    boardd->wd = inotify_add_watch(boardd->fd, pool_dir,
                                   IN_CREATE | IN_DONT_FOLLOW);
    ERRORP_IF(-1 == boardd->wd, "can't watch %s with inotify",
              pool_dir);


    for (i = 0; i < count; ++i) {
        ++boardd->count;

        boardd->board_ids[i] = ids[i];
        boardd->db_paths[i] = strdup(paths[i]);
        ERRORP_IF(NULL == boardd->db_paths[i],
                  "out of memory");

        boardd->boards[i] = board_open(paths[i], 'w');
        ERRORP_IF(NULL == boardd->boards[i], "can't open board %s to write",
                  paths[i]);
    }

    return boardd;

L_error:
    boardd_destroy(boardd);
    return NULL;
}


static void
usage(void)
{
    fprintf(stderr, "Usage: boardd <pool_dir> <board_id1> <board_db1> ...\n");
}


static void
signal_cb(EV_P_ struct ev_signal* w, int revents)
{
    fprintf(stderr, "got signal: %d, revents=%d\n", w->signum, revents);
    ev_unloop(EV_A_ EVUNLOOP_ALL);
}


size_t
get_title(const char* content, size_t len, char* title, size_t size)
{
    const char* p = content;
    size_t n = 0;

    assert(NULL != content && len > 0 && NULL != title && size > 0);

    while (len > 0 && isspace(*p)) {
        --len;
        ++p;
    }

    while (len > 0 && n < size && ('\r' != *p && '\n' != *p)) {
        title[n] = *p;
        --len;
        ++p;
        ++n;
    }

    title[n] = '\0';

    return n;
}


static int
post_article(board_t* board, const char* author, const char* title,
             const char* content, size_t len)
{
    if (-1 == board_post_begin(board, author, title, len) ||
            -1 == board_append_data(board, content, len) ||
            -1 == board_post_end(board))
        return -1;

    return 0;
}


static int
reply_article(board_t* board, article_t article, const char* author,
              const char* title, const char* content, size_t len)
{
    if (-1 == board_reply_begin(board, article, author, title, len) ||
            -1 == board_append_data(board, content, len) ||
            -1 == board_reply_end(board))
        return -1;

    return 0;
}


static int
modify_article(board_t* board, article_t article, const char* title,
              const char* content, size_t len)
{
    if (-1 == board_modify_begin(board, article, title, len) ||
            -1 == board_append_data(board, content, len) ||
            -1 == board_modify_end(board))
        return -1;

    return 0;
}


static int
delete_article(board_t* board, article_t article)
{
    return -1;
}


static int
delete_article_range(board_t* board, article_t from, article_t to)
{
    return -1;
}


static void
process_pool_file(boardd_t* boardd, const char* filename)
{
    board_op_t op;
    unsigned id1, id2, i;
    unsigned short bid;
    article_t article1, article2;
    struct stat st;
    size_t len;
    int fd = -1, ret = -1;
    const char* content = MAP_FAILED;

    assert(NULL != boardd && NULL != filename);

    len = strlen(filename) + 1;
    ERROR_IF(len + boardd->pool_dir_len >= POOL_PATH_MAX,
             "file path too long: %s\n", filename);
    memcpy(boardd->pool_file + boardd->pool_dir_len, filename, len);

    ERRORP_IF(-1 == lstat(boardd->pool_file, &st), "lstat() failed");

    ERROR_IF(st.st_uid < 1000,
             "uid(%d) < 1000 is not allowed to post/reply/modify/delete"
             " article\n", st.st_uid);

    op = parse_pool_file_name(filename, &bid, &id1, &id2);
    ERROR_IF(OP_INVALID == op,
             "unkown file %s in pool directory\n", filename);

    for (i = 0; i < boardd->count; ++i) {
        if (bid == boardd->board_ids[i])
            break;
    }
    ERROR_IF(i == boardd->count,
             "unkown board id in file name: %s\n", filename);


    if (OP_POST != op) {
        article1 = board_get(boardd->boards[i], id1);
        ERROR_IF(-1 == article1, "can't find article %u in board %hu", id1, bid);
    }

    if (OP_DELETE_RANGE == op) {
        article2 = board_get(boardd->boards[i], id2);
        ERROR_IF(-1 == article2, "can't find article %u in board %hu",
                 id1, bid);
        ret = delete_article_range(boardd->boards[i], article1, article2);
    } else if (OP_DELETE == op) {
        ret = delete_article(boardd->boards[i], article1);
    } else {
        ssize_t n;
        const char *author;
        char title[TITLE_LEN];
        uid_t uid;

        ERROR_IF(! S_ISLNK(st.st_mode),
                 "%s should be symlink for post/reply/modify", filename);
        n = readlink(boardd->pool_file, boardd->real_file, POOL_PATH_MAX);
        ERROR_IF(-1 == n || POOL_PATH_MAX == n, "readlink(%s) failed",
                 boardd->pool_file);

        fd = open(boardd->real_file, O_RDONLY);
        ERRORP_IF(-1 == fd, "can't read %s", boardd->real_file);

        uid = st.st_uid;
        ERRORP_IF(-1 == fstat(fd, &st), "can't stat(%s)", boardd->real_file);
        ERROR_IF(uid != st.st_uid, "%s and %s must have same uid",
                 boardd->pool_file, boardd->real_file);
        ERROR_IF(0 == st.st_size, "%s is empty", boardd->real_file);

        content = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
        ERRORP_IF(MAP_FAILED == content, "mmap(%s) failed", boardd->real_file);

        ERROR_IF(0 == get_title(content, st.st_size, title, TITLE_LEN),
                 "title too short");

        author = get_user_name(uid);
        ERROR_IF(NULL == author, "can't find username for uid(%u)", uid);

        switch (op) {
        case OP_POST:
            ret = post_article(boardd->boards[i], author, title,
                               content, st.st_size);
            break;
        case OP_REPLY:
            ret = reply_article(boardd->boards[i], article1, author, title,
                                content, st.st_size);
            break;
        case OP_MODIFY:
            ret = modify_article(boardd->boards[i], article1, title,
                                 content, st.st_size);
            break;
        default:
            break;
        }


        if (0 == ret)
            unlink(boardd->real_file);
        else
            ERRORP_IF(1, "failed to post/reply/modify %s", boardd->real_file);
    }

    if (0 == ret)
        unlink(boardd->pool_file);
    else
        ERRORP_IF(1, "failed to process %s", boardd->pool_file);

L_error:
    if (MAP_FAILED != content)
        munmap((void*)content, st.st_size);
    if (-1 != fd)
        close(fd);
    return;
}


static void
process_inotify_events(boardd_t* boardd, ssize_t bytes)
{
    struct inotify_event* event;

    event = (struct inotify_event*)boardd->events;
    while ((char*)event - boardd->events < bytes) {
        fprintf(stderr, "got event in pool dir: file %s, wd %d: ",
                event->name, event->wd);
        if (IN_IGNORED == (IN_IGNORED & event->mask) ||
                IN_UNMOUNT == (IN_IGNORED & event->mask)) {
            fprintf(stderr, "file removed or fs unmounted\n");
        } else if (IN_CREATE == (IN_CREATE & event->mask)) {
            fprintf(stderr, "file created\n");
            process_pool_file(boardd, event->name);
        }

        event = (struct inotify_event*)((char*)event +
                                        sizeof(struct inotify_event) +
                                        event->len);
    }
}


static void
pool_dir_watcher_cb(EV_P_ struct ev_io* w, int revents)
{
    ssize_t bytes;
    boardd_t* boardd = (boardd_t*)w->data;

    assert(NULL != boardd);

    fprintf(stderr, "got inotify event: revents=%d (EV_READ=%d)\n", revents,
            EV_READ);

    while (1) {
        bytes = read(boardd->fd, boardd->events, EVENT_BUF_LEN);
        if (bytes > 0) {
            process_inotify_events(boardd, bytes);
        } else if (0 == bytes) {
            /* kernel before 2.6.21, see inotify(7) */
            perror("event buf too small or reach end of file");
            ev_unloop(EV_A_ EVUNLOOP_ALL);
            break;
        } else if (EINTR == errno) {
            continue;
        } else if (EAGAIN == errno) {
            break;
        } else {
            perror("error happended when read inotify events");
            ev_unloop(EV_A_ EVUNLOOP_ALL);
            break;
        }
    }
}


int
main(int argc, char** argv)
{
    unsigned count, i;
    int id;
    boardd_t* boardd;
    unsigned short* ids;
    char** paths;
    struct ev_loop* loop;
    struct ev_signal sig_hup_watcher, sig_int_watcher,
                     sig_quit_watcher, sig_term_watcher;
    struct ev_io pool_dir_watcher;

    if (argc < 4 || (argc % 2) == 1) {
        usage();
        return EXIT_FAILURE;
    }

    loop = ev_default_loop(EVFLAG_AUTO);
    if (NULL == loop) {
        fprintf(stderr, "could not initialize libev, bad $LIBEV_FLAGS"
                " in environment?\n");
        return EXIT_FAILURE;
    }

    ev_signal_init(&sig_hup_watcher, signal_cb, SIGHUP);
    ev_signal_init(&sig_int_watcher, signal_cb, SIGINT);
    ev_signal_init(&sig_quit_watcher, signal_cb, SIGQUIT);
    ev_signal_init(&sig_term_watcher, signal_cb, SIGTERM);

    ev_signal_start(EV_A_ &sig_hup_watcher);
    ev_signal_start(EV_A_ &sig_int_watcher);
    ev_signal_start(EV_A_ &sig_quit_watcher);
    ev_signal_start(EV_A_ &sig_term_watcher);

    count = (argc - 2) / 2;
    ids = (unsigned short*)malloc(count * sizeof(unsigned short));
    if (NULL == ids)
        return EXIT_FAILURE;
    paths = (char**)malloc(count * sizeof(char*));
    if (NULL == paths) {
        free(ids);
        return EXIT_FAILURE;
    }
    for (i = 0; i < count; ++i) {
        id = atoi(argv[2 + 2 * i]);
        if (! IS_VALID_BOARD_ID(id))
            break;
        ids[i] = (unsigned short)id;
        paths[i] = argv[2 + 2 * i + 1];
    }
    if (i < count) {
        free(ids);
        free(paths);
        return EXIT_FAILURE;
    }

    boardd = boardd_create(argv[1], count, ids, paths);
    if (NULL == boardd)
        return EXIT_FAILURE;

    ev_io_init(&pool_dir_watcher, pool_dir_watcher_cb, boardd->fd, EV_READ);
    pool_dir_watcher.data = boardd;
    ev_io_start(EV_A_ &pool_dir_watcher);

    ev_loop(EV_A_ 0);

    if (boardd_destroy(boardd))
        return EXIT_FAILURE;
    else
        return EXIT_SUCCESS;
}

