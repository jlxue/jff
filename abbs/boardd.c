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
 *
 *
 * ChangeLog:
 *  2008-07-21  Liu Yubao
 *      * initial version
 *
 */
#define _GNU_SOURCE         /* for strndup  */

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dirent.h>
#include <fcntl.h>
#include <pwd.h>
#include <sys/inotify.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <ev.h>

#include "apdb.h"
#include "board.h"
#include "util.h"


#define EVENT_BUF_LEN       (4 * 1024)


typedef struct {
    unsigned        count;
    char**          db_paths;
    char**          pool_dirs;
    board_t**       boards;

    int             fd;        /* inotify fd */
    int*            wds;       /* watch fd  */
    char            events[EVENT_BUF_LEN];
    char            pool_file[PATH_MAX];
    char            real_file[PATH_MAX];
} boardd_t;


int
boardd_close(boardd_t* boardd)
{
    unsigned i;
    int ret = 0;

    if (NULL == boardd)
        return 0;

    if (NULL == boardd->db_paths || NULL == boardd->pool_dirs ||
            NULL == boardd->boards || NULL == boardd->wds)
        goto L_leave;


    for (i = 0; i < boardd->count; ++i) {
        if (NULL != boardd->db_paths[i])
            free(boardd->db_paths[i]);
        if (NULL != boardd->pool_dirs[i])
            free(boardd->pool_dirs[i]);
        if (NULL != boardd->boards[i]);
            board_close(boardd->boards[i]);
    }

L_leave:
    if (NULL != boardd->db_paths)
        free(boardd->db_paths);
    if (NULL != boardd->pool_dirs)
        free(boardd->pool_dirs);
    if (NULL != boardd->boards)
        free(boardd->boards);
    if (NULL != boardd->wds)
        free(boardd->wds);

    if (-1 != boardd->fd)
        ret = close(boardd->fd);

    free(boardd);

    return ret;
}


boardd_t*
boardd_open(unsigned count, char* paths[])
{
    boardd_t* boardd;
    unsigned i;
    struct stat st;

    assert(count > 0);

    boardd = (boardd_t*)calloc(1, sizeof(boardd_t));
    if (NULL == boardd)
        return NULL;

    boardd->fd = -1;

    boardd->db_paths = (char**)calloc(count, sizeof(char*));
    boardd->pool_dirs = (char**)calloc(count, sizeof(char*));
    boardd->boards = (board_t**)calloc(count, sizeof(board_t*));
    boardd->wds = (int*)calloc(count, sizeof(int));
    ERRORP_IF(NULL == boardd->db_paths || NULL == boardd->pool_dirs ||
            NULL == boardd->boards || NULL == boardd->wds,
            "out of memory");

    boardd->fd = inotify_init();
    ERRORP_IF(-1 == boardd->fd, "inotify_init() failed");
    ERROR_IF(-1 == fcntl(boardd->fd, F_SETFL, O_NONBLOCK),
             "can't set inotify fd to nonblock mode");

    for (i = 0; i < count; ++i) {
        ++boardd->count;
        boardd->db_paths[i] = strdup(paths[2 * i]);
        boardd->pool_dirs[i] = strdup(paths[2 * i + 1]);
        ERRORP_IF(NULL == boardd->db_paths[i] || NULL == boardd->pool_dirs[i],
                  "out of memory");

        ERRORP_IF(-1 == stat(boardd->pool_dirs[i], &st),
                  "can't stat(%s)", boardd->pool_dirs[i]);
        ERROR_IF(! S_ISDIR(st.st_mode), "%s isn't a directory",
                 boardd->pool_dirs[i]);

        boardd->boards[i] = board_open(boardd->db_paths[i], 'w');
        ERRORP_IF(NULL == boardd->boards[i], "can't open board %s to write",
                  boardd->db_paths[i]);

        boardd->wds[i] = inotify_add_watch(boardd->fd, boardd->pool_dirs[i],
                                           IN_CREATE | IN_DONT_FOLLOW);
        ERRORP_IF(-1 == boardd->wds[i], "can't watch %s with inotify",
                  boardd->pool_dirs[i]);
    }

    return boardd;

L_error:
    boardd_close(boardd);
    return NULL;
}


static void
usage(void)
{
    fprintf(stderr, "Usage: boardd <board_db1> <pooldir1> ...\n");
}


static void
signal_cb(EV_P_ struct ev_signal* w, int revents)
{
    fprintf(stderr, "got signal: %d, revents=%d\n", w->signum, revents);
    ev_unloop(EV_A_ EVUNLOOP_ALL);
}


/*
 * filename template:
 *      d-yyyymmddHHMMSS-author-id.xxxxxx
 *      d-yyyymmddHHMMSS-author-id1-id2.xxxxxx
 */
static int
delete_articles(boardd_t* boardd, unsigned i, const char* filename,
                uid_t uid)
{
    article_t article;
    unsigned long id1, id2;
    const char* p;
    char* end;

    assert(NULL != boardd && i < boardd->count && NULL != filename &&
           uid >= 1000);

    ERROR_IF(FLAG_DELETE_CHAR != *filename || '-' != *(filename + 1),
             "invalid file name");

    p = strrchr(filename, '-');
    if (NULL == p)
        return -1;
    
    p = strrchr(p, '-');
    if (NULL == p)
        return -1;

    p = strrchr(p, '-');
    if (NULL == p)
        return -1;

    if ('\0' == *++p)
        return -1;

    id1 = strtoul(p, &end, 10);
    if (0 == id1 || id1 >= UINT_MAX)
        return -1;
    article = board_get(boardd->boards[i], (unsigned)id1);
    ERROR_IF(-1 == article, "article %u not found", (unsigned)id1);

    if ('\0' == *++end)
        return -1;
    id2 = strtoul(end, NULL, 10);
    if (0 == id2 || (id2 >= UINT_MAX && ULONG_MAX != id2)) {
        return -1;
    } else {
        id2 = 0;
        /* TODO: delete a range */
        return -1;
    }

    board_delete(boardd->boards[i], article);

    return 0;

L_error:
    return -1;
}


/* 
 * filename template:
 *      post:   p-yyyymmddHHMMSS-author.xxxxxx
 *      reply:  r-yyyymmddHHMMSS-author-id.xxxxxx
 *      modify: m-yyyymmddHHMMSS-author-id.xxxxxx
 *
 *  where xxxxxx are random strings generated by mkstemp()
 */
static int
post_reply_modify_article(boardd_t* boardd, unsigned i, const char* filename,
                          uid_t uid, const char* real_fullpath)
{
    struct stat st;
    int fd = -1;
    void* data = MAP_FAILED;

    article_t article;

    struct passwd* pwd;         /* for author name  */
    const char *title = NULL;


    assert(NULL != boardd && i < boardd->count && NULL != filename &&
           uid >= 1000 && NULL != real_fullpath);

    ERROR_IF('-' != *(filename + 1), "invalid file name");

    /* get article */
    if (FLAG_MODIFY_CHAR == *filename || FLAG_REPLY_CHAR == *filename) {
        unsigned long id;
        const char* p = strrchr(filename, '-');
        char* end;

        ERROR_IF(NULL == p || '\0' == *++p, "invalid filename: %s",
                 filename);

        id = strtoul(p, &end, 10);
        ERROR_IF(0 == id || id >= UINT_MAX, "invalid id: %ld", id);

        article = board_get(boardd->boards[i], (unsigned)id);
        ERROR_IF(-1 == article, "article not found");
    }

    /* get author name, don't trust the file name   */
    pwd = getpwuid(uid);
    ERRORP_IF(NULL == pwd || NULL == pwd->pw_name,
              "can't find passwd entry for uid=%d", uid);


    /* TODO: check permission                       */


    /* get content  */
    fd = open(real_fullpath, O_RDONLY);
    ERRORP_IF(-1 == fd, "can't open %s to read", real_fullpath);

    ERRORP_IF(-1 == fstat(fd, &st), "can't stat %s", real_fullpath);

    ERROR_IF(st.st_uid != uid, "uid of %s and %s are different",
             filename, real_fullpath);

    ERROR_IF(0 == st.st_size, "can't post empty article: %s", real_fullpath);

    data = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
    ERRORP_IF(MAP_FAILED == data, "mmap failed");


    /* get title    */
    title = strndup((const char*)data,
                    MIN(TITLE_LEN - 1, strcspn((const char*)data, "\r\n")));

    switch (*filename) {
    case FLAG_MODIFY_CHAR:
        ERRORP_IF(-1 == board_modify_begin(boardd->boards[i], article, title,
                                     st.st_size) ||
                  -1 == board_append_data(boardd->boards[i], data,
                                          st.st_size) ||
                  -1 == board_modify_end(boardd->boards[i]),
                  "failed to modify %s, %s", filename, real_fullpath);
        break;
    case FLAG_POST_CHAR:
        ERRORP_IF(-1 == board_post_begin(boardd->boards[i], pwd->pw_name, title,
                                         st.st_size) ||
                  -1 == board_append_data(boardd->boards[i], data,
                                          st.st_size) ||
                  -1  == board_post_end(boardd->boards[i]),
                  "failed to post %s, %s", filename, real_fullpath);
        break;
    case FLAG_REPLY_CHAR:
        ERRORP_IF(-1 == board_reply_begin(boardd->boards[i], article,
                                          pwd->pw_name, title, st.st_size) ||
                  -1 == board_append_data(boardd->boards[i], data,
                                          st.st_size) ||
                  -1 == board_reply_end(boardd->boards[i]),
                  "failed to reply %s, %s", filename, real_fullpath);
        break;
    default:
        fprintf(stderr, "invalid file name: %s\n", filename);
        goto L_error;
    }

    return 0;

L_error:
    if (NULL != title)
        free((void*)title);
    if (data != MAP_FAILED)
        munmap(data, st.st_size);
    if (-1 != fd)
        close(fd);

    return -1;
}


static void
process_pool_file(boardd_t* boardd, unsigned i, const char* filename)
{
    struct stat st;
    size_t len;

    assert(NULL != boardd && i < boardd->count && NULL != filename);

    len = strlen(boardd->pool_dirs[i]);
    if (len + strlen(filename) + 1 >= PATH_MAX) {
        fprintf(stderr, "file path too long: %s/%s\n", boardd->pool_dirs[i],
                filename);
        return;
    }
    strcpy(boardd->pool_file, boardd->pool_dirs[i]);
    boardd->pool_file[len] = '/';
    strcpy(boardd->pool_file + len + 1, filename);

    ERRORP_IF(-1 == lstat(boardd->pool_file, &st),
             "lstat() failed");

    ERROR_IF(st.st_uid < 1000,
             "uid(%d) < 1000 is not allowed to post/reply/modify/delete"
             " article\n", st.st_uid);

    if (S_ISREG(st.st_mode)) {
        /* delete */
        delete_articles(boardd, i, filename, st.st_uid);

        /* always delete the pool file as we won't retry    */
        ERRORP_IF(-1 == unlink(boardd->pool_file),
                  "failed to unlink %s", boardd->pool_file);
    } else if (S_ISLNK(st.st_mode)) {
        /* post, reply modify */
        ERRORP_IF(NULL == realpath(boardd->pool_file, boardd->real_file),
                  "realpath(%s) failed", boardd->pool_file);
        if (0 == post_reply_modify_article(boardd, i, filename, st.st_uid,
                                           boardd->real_file)) {
            ERRORP_IF(-1 == unlink(boardd->pool_file),
                      "failed to unlink %s", boardd->pool_file);
            ERRORP_IF(-1 == unlink(boardd->real_file),
                      "failed to unlink %s", boardd->real_file);
        }
    } else {
        fprintf(stderr, "unkown file in pool directory\n");
    }

L_error:
    return;
}


static void
process_inotify_events(boardd_t* boardd, ssize_t bytes)
{
    struct inotify_event* event;
    unsigned i;

    event = (struct inotify_event*)boardd->events;
    while ((char*)event - boardd->events < bytes) {
        for (i = 0; i < boardd->count; ++i)
            if (event->wd == boardd->wds[i])
                break;
        if (i == boardd->count) {
            fprintf(stderr, "unkown wd: %d, name=%s\n", event->wd, event->name);
        } else {
            fprintf(stderr, "got event in pool dir %s, file %s, wd %d: ",
                    boardd->pool_dirs[i], event->name, event->wd);
            if (IN_IGNORED == (IN_IGNORED & event->mask) ||
                    IN_UNMOUNT == (IN_IGNORED & event->mask)) {
                fprintf(stderr, "file removed or fs unmounted\n");
            } else if (IN_CREATE == (IN_CREATE & event->mask)) {
                fprintf(stderr, "file created\n");
                process_pool_file(boardd, i, event->name);
            }
        }

        event = (struct inotify_event*)((char*)event +
                                        sizeof(struct inotify_event) +
                                        event->len);
    }
}


static void
pool_dirs_watcher_cb(EV_P_ struct ev_io* w, int revents)
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
    unsigned count;
    boardd_t* boardd;
    struct ev_loop* loop;
    struct ev_signal sig_hup_watcher, sig_int_watcher,
                     sig_quit_watcher, sig_term_watcher;
    struct ev_io pool_dirs_watcher;

    if (argc < 3 || (argc % 2) == 0) {
        usage();
        return EXIT_FAILURE;
    }

    loop = ev_default_loop(EVFLAG_AUTO);
    if (NULL == loop) {
        fprintf(stderr, "could not initialize libev, bad $LIBEV_FLAGS"
                " in environment?\n");
        return EXIT_FAILURE;
    }

    count = (argc - 1) / 2;
    boardd = boardd_open(count, &argv[1]);
    if (NULL == boardd)
        return EXIT_FAILURE;

    ev_signal_init(&sig_hup_watcher, signal_cb, SIGHUP);
    ev_signal_init(&sig_int_watcher, signal_cb, SIGINT);
    ev_signal_init(&sig_quit_watcher, signal_cb, SIGQUIT);
    ev_signal_init(&sig_term_watcher, signal_cb, SIGTERM);

    ev_signal_start(EV_A_ &sig_hup_watcher);
    ev_signal_start(EV_A_ &sig_int_watcher);
    ev_signal_start(EV_A_ &sig_quit_watcher);
    ev_signal_start(EV_A_ &sig_term_watcher);

    ev_io_init(&pool_dirs_watcher, pool_dirs_watcher_cb, boardd->fd, EV_READ);
    pool_dirs_watcher.data = boardd;
    ev_io_start(EV_A_ &pool_dirs_watcher);

    ev_loop(EV_A_ 0);

    if (boardd_close(boardd))
        return EXIT_FAILURE;
    else
        return EXIT_SUCCESS;
}

