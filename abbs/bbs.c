/*
 * bbs - bbs client for ABBS
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
 */

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dirent.h>
#include <fcntl.h>
#include <pwd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <readline/history.h>
#include <readline/readline.h>

#include "apdb.h"
#include "board.h"
#include "util.h"


#define MAX_NUM     20
#define DELIM_CHARS " \t,"


typedef enum {
    NONE,
    VIEW,
    DELETE,
    EDIT,
    TITLE,
    LIST,
    NEXT,
    PREV,
    POST,
    REPLY,
    HELP
} cmd_t;


typedef struct {
    board_t*        board;
    char*           db_path;
    char*           pool_dir;
    char*           tmp_dir;
    article_t*      articles;
    unsigned        count;
    cmd_t           cmd;
    unsigned        argc,argv[2];
} client_t;


int
client_close(client_t* client)
{
    if (NULL == client)
        return 0;

    if (NULL != client->db_path)
        free(client->db_path);
    if (NULL != client->pool_dir)
        free(client->pool_dir);
    if (NULL != client->tmp_dir)
        free(client->tmp_dir);

    if (NULL != client->board)
        board_close(client->board);

    free(client);

    return 0;
}


client_t*
client_open(const char* db_path, const char* pool_dir, const char* tmp_dir)
{
    client_t* client;

    assert(NULL != db_path && NULL != pool_dir && NULL != tmp_dir);

    client = (client_t*)calloc(1, sizeof(client_t));
    if (NULL == client)
        return NULL;

    client->db_path = strdup(db_path);
    client->pool_dir = strdup(pool_dir);
    client->tmp_dir = strdup(tmp_dir);

    if (NULL == client->db_path || NULL == client->pool_dir ||
            NULL == client->tmp_dir)
        goto L_error;

    client->board = board_open(db_path, 'r');
    if (NULL == client->board)
        goto L_error;

    return client;

L_error:
    client_close(client);
    return NULL;
}


/* Read a string, and return a pointer to it. Returns NULL on EOF. */
char *
rl_gets (char* line_read)
{
    if (line_read) {
        free(line_read);
    }

    line_read = readline ("& ");

    if (line_read && *line_read && strlen(line_read) > strspn(line_read, " \t")) {
        add_history (line_read);
    }

    return (line_read);
}


static int
cmd_view(client_t* client, unsigned argc, unsigned arg1, unsigned arg2)
{
    FILE* fp;
    article_t article;
    time_t t;

    if (argc != 1) {
        fprintf(stderr, "wrong argument");
        return -1;
    }

    article = board_get(client->board, arg1);
    if (-1 == article) {
        fprintf(stderr, "article %u not found\n", arg1);
        return -1;
    }

    fflush(stdout);
    fflush(stderr);

    fp = popen("/usr/bin/less", "w");
    if (NULL == fp) {
        perror("");
        fprintf(stderr, "can't launch /usr/bin/less\n");
        return -1;
    }

    fprintf(fp, "id     : %u\n", board_article_id(client->board, article));
    fprintf(fp, "pid    : %u\n", board_article_pid(client->board, article));
    fprintf(fp, "tid    : %u\n", board_article_tid(client->board, article));
    fprintf(fp, "flags  : %u\n", board_article_flags(client->board, article));
    fprintf(fp, "title  : %s\n", board_article_title(client->board, article));
    fprintf(fp, "author : %s\n", board_article_author(client->board, article));
    t = board_article_ctime(client->board, article);
    fprintf(fp, "ctime  : %s", ctime(&t));
    t = board_article_mtime(client->board, article);
    fprintf(fp, "mtime  : %s\n", ctime(&t));

    fflush(fp);

    writen(fileno(fp), board_article_content(client->board, article),
           board_article_length(client->board, article));

    pclose(fp);

    return 0;
}


static int
cmd_delete(client_t* client, unsigned argc, unsigned arg1, unsigned arg2)
{
    return 0;
}


static int
cmd_edit(client_t* client, unsigned argc, unsigned arg1, unsigned arg2)
{
    return 0;
}


static int
cmd_title(client_t* client, unsigned argc, unsigned arg1, unsigned arg2)
{
    return 0;
}


static int
cmd_list(client_t* client, unsigned argc, unsigned arg1, unsigned arg2)
{
    article_t from;
    int count = 20;
    char timestamp[17];   /* YYYY-mm-dd HH:MM */
    time_t t;

    if (0 == argc) {
        from = board_first(client->board, (WRITING | DELETED));
        if (-1 == from) {
            fprintf(stderr, "no article found\n");
            return -1;
        }
    } else {
        from = board_get(client->board, arg1);
        if (-1 == from) {
            fprintf(stderr, "article %u not found\n", arg1);
            return -1;
        }
    }

    if (argc > 1)
        count = (int)arg2;

    if (count > 0) {
        do {
            t = board_article_ctime(client->board, from);

            if (sizeof(timestamp) - 1 != strftime(timestamp, sizeof(timestamp),
                                                  "%Y-%m-%d %H:%M",
                                                  localtime(&t)))
                break;

            printf("%4u/%4u/%4u/    %s  %10s    %s (%d)\n",
                   board_article_id(client->board, from),
                   board_article_pid(client->board, from),
                   board_article_tid(client->board, from),
                   timestamp,
                   board_article_author(client->board, from),
                   board_article_title(client->board, from),
                   board_article_length(client->board, from));

            from = board_next(client->board, from, (WRITING | DELETED));

        } while (--count > 0 && -1 != from);
    } else if (count < 0) {
        do {
            t = board_article_ctime(client->board, from);

            if (sizeof(timestamp) - 1 != strftime(timestamp, sizeof(timestamp),
                                                  "%Y-%m-%d %H:%M",
                                                  localtime(&t)))
                break;

            printf("%4u/%4u/%4u/    %s  %10s    %s (%d)\n",
                   board_article_id(client->board, from),
                   board_article_pid(client->board, from),
                   board_article_tid(client->board, from),
                   timestamp,
                   board_article_author(client->board, from),
                   board_article_title(client->board, from),
                   board_article_length(client->board, from));

            from = board_previous(client->board, from, (WRITING | DELETED));

        } while (++count < 0 && -1 != from);
    }

    return 0;
}


static int
cmd_next(client_t* client, unsigned argc, unsigned arg1, unsigned arg2)
{
    return 0;
}


static int
cmd_prev(client_t* client, unsigned argc, unsigned arg1, unsigned arg2)
{
    return 0;
}


/*
 * p-yyyymmddHHMMSS-author.xxxxxx
 */
static int
cmd_post(client_t* client, unsigned argc, unsigned arg1, unsigned arg2)
{
    char realpath[PATH_MAX];
    char poolpath[PATH_MAX];
    time_t ctime;
    char date[15];
    struct passwd* pwd;
    int fd;
    pid_t pid;

    if (argc != 0) {
        fprintf(stderr, "too many arguments\n");
        return -1;
    }

    ctime = time(NULL);
    if (sizeof(date) - 1 != strftime(date, sizeof(date), "%Y%m%d%H%M%S",
                                     localtime(&ctime))) {
        fprintf(stderr, "strftime failed\n");
            return -1;
    }

    pwd = getpwuid(geteuid());
    if (NULL == pwd || NULL == pwd->pw_name) {
        perror("can't find current user name\n");
        return -1;
    }
    if (strchr(pwd->pw_name, '-')) {
        fprintf(stderr, "usename can't contains '-'\n");
        return -1;
    }

    realpath[PATH_MAX - 1] = '\0';
    if (snprintf(realpath, PATH_MAX, "%s/%c-%s-%s.XXXXXX",
                 client->tmp_dir, FLAG_POST_CHAR, date, pwd->pw_name)
            >= PATH_MAX) {
        fprintf(stderr, "username too long\n");
        return -1;
    }

    poolpath[PATH_MAX - 1] = '\0';
    if (snprintf(poolpath, PATH_MAX, "%s/%c-%s-%s.XXXXXX",
                 client->pool_dir, FLAG_POST_CHAR, date, pwd->pw_name)
            >= PATH_MAX) {
        fprintf(stderr, "username too long\n");
        return -1;
    }

    fd = mkstemp(realpath);
    if (-1 == fd) {
        perror(realpath);
        return -1;
    }
    close(fd);

    pid = fork();
    if (pid < 0) {
        perror("Can't lauch /usr/bin/rvim");
        goto L_error;
    }

    if (pid > 0) {
        for (;;) {
            int status;
            pid_t pid2 = waitpid(pid, &status, 0);
            if (pid2 < 0 && EINTR == errno)
                continue;
            if (pid2 != pid)
                goto L_error;
            if (WIFEXITED(status)) {
                if (EXIT_SUCCESS == WEXITSTATUS(status))
                    break;
                else
                    goto L_error;
            }
        }
    } else {
        execl("/usr/bin/rvim", "/usr/bin/rvim", realpath, NULL);
        exit(-1);
    }

    {
        char* line;
        line = readline("Sure to post?[y/N]");
        if (NULL == line)
            goto L_error;
        if ('y' != *line && 'Y' != *line) {
            free(line);
            goto L_error;
        }
    }
    
    if (0 == symlink(realpath, poolpath))
        return 0;

L_error:
    unlink(realpath);
    return -1;
}


static int
cmd_reply(client_t* client, unsigned argc, unsigned arg1, unsigned arg2)
{
    return 0;
}


static void
help(void)
{
    puts("Available commands\n"
         "  NNNN                view specified article\n"
         "  view NNNN           view specified article\n"
         "  delete NNNN         delete specified article\n"
         "  delete NNNN,MMMM    delete range\n"
         "  edit NNNN           edit specified article\n"
         "  title NNNN         rename title of specified article\n"
         "  list NNNN           list range [NNNN, NNNN + 20)\n"
         "  list NNNN,N         list range [NNNN, NNNN + N)\n"
         "  next                next 20 articles\n"
         "  next N              next N articles\n"             
         "  prev                previous 20 articles\n"
         "  prev N              previous N articles\n"
         "  post                post new article\n"
         "  reply NNNN          reply specified article\n"
         "  help                show this help\n"
         "  ?                   show this help\n"
         "  quit                quit\n"
         "  exit                quit\n"
         "  ctrl-d              quit\n"
         "  <enter>             repeat last command\n"
         );
}


static unsigned
get_unsigned_int(const char* s)
{
    char* end;
    unsigned long a;

    assert(NULL != s);

    a = strtoul(s, &end, 10);
    if (a >= UINT_MAX) {
        fprintf(stderr, "out of range\n");
        return UINT_MAX;
    } else if (s == end || '\0' != *end) {
        return UINT_MAX;
    } else
        return (unsigned)a;
}


static int
process_input_line(client_t* client, char* line)
{
    char *token;
    unsigned argv[2];
    unsigned argc;
    cmd_t cmd;
    int ret;

    assert(NULL != client && NULL != line);

    token = strtok(line, DELIM_CHARS);
    if (NULL != token) {
        argv[0] = get_unsigned_int(token);
        if (UINT_MAX != argv[0]) {
            cmd = VIEW;
            argc = 1;
        } else {
            argc = 0;

            switch (*token++) {
            case 'v':       /* view */
                cmd = VIEW;
                break;
            case 'd':       /* delete */
                cmd = DELETE;
                break;
            case 'e':       /* edit, exit   */
                if ('x' == *token)
                    return -1;
                cmd = EDIT;
                break;
            case 'E':       /* edit */
                cmd = EDIT;
                break;
            case 't':       /* title    */
                cmd = TITLE;
                break;
            case 'l':       /* list     */
                cmd = LIST;
                break;
            case 'n':       /* next     */
                cmd = NEXT;
                break;
            case 'p':       /* prev, post   */
                if ('o' == *token)
                    cmd = POST;
                else
                    cmd = PREV;
                break;
            case 'P':       /* post     */
                cmd = POST;
                break;
            case 'r':       /* reply    */
                cmd = REPLY;
                break;
            case 'h':       /* help     */
                cmd = HELP;
                break;
            case '?':       /* help     */
                cmd = HELP;
                break;
            case 'q':       /* quit     */
                return -1;
                break;
            default:
                fprintf(stderr, "unkown command, type ? or help for help.\n");
                return 0;
            }
        }

        token = strtok(NULL, DELIM_CHARS);
        if (NULL != token) {
            argv[argc] = get_unsigned_int(token);
            if (UINT_MAX == argv[argc]) {
                fprintf(stderr, "Bad command, type ? or help for help.\n");
                return 0;
            }
            if (++argc < 2) {
                token = strtok(NULL, DELIM_CHARS);
                if (NULL != token) {
                    argv[argc] = get_unsigned_int(token);
                    if (UINT_MAX == argv[argc]) {
                        fprintf(stderr, "Bad command, "
                                "type ? or help for help.\n");
                        return 0;
                    } else {
                        ++argc;
                    }
                }
            }
        }
    } else {    /* empty input, execute last successful command */
        cmd = client->cmd;
        argc = client->argc;
        argv[0] = client->argv[0];
        argv[1] = client->argv[1];
    }


    switch (cmd) {
    case VIEW:
        ret = cmd_view(client, argc, argv[0], argv[1]);
        break;
    case DELETE:
        ret = cmd_delete(client, argc, argv[0], argv[1]);
        break;
    case EDIT:
        ret = cmd_edit(client, argc, argv[0], argv[1]);
        break;
    case TITLE:
        ret = cmd_title(client, argc, argv[0], argv[1]);
        break;
    case LIST:
        ret = cmd_list(client, argc, argv[0], argv[1]);
        break;
    case NEXT:
        ret = cmd_next(client, argc, argv[0], argv[1]);
        break;
    case PREV:
        ret = cmd_prev(client, argc, argv[0], argv[1]);
        break;
    case POST:
        ret = cmd_post(client, argc, argv[0], argv[1]);
        break;
    case REPLY:
        ret = cmd_reply(client, argc, argv[0], argv[1]);
        break;
    case HELP:
        help();
        ret = 0;
        break;
    default:        /* NONE */
        ret = -1;
        break;
    }


    /* record last successful command   */
    if (0 == ret) {
        client->cmd = cmd;
        client->argc = argc;
        client->argv[0] = argv[0];
        client->argv[1] = argv[1];
    } 

    return 0;
}


static void
usage(void)
{
    fprintf(stderr, "Usage: bbs db_path pool_dir tmp_dir\n");
}


int main(int argc, char** argv)
{
    client_t* client;
    char* line_read;

    if (argc < 4) {
        usage();
        return EXIT_FAILURE;
    }

    umask(077);     /* make mkstemp() secure    */

    client = client_open(argv[1], argv[2], argv[3]);
    if (NULL == client) {
        fprintf(stderr, "Failed to open board %s\n", argv[1]);
        return EXIT_FAILURE;
    }

    puts("ABBS client v0.0.1, readline supported. Type ? or help for help");

    line_read = NULL;
    for (;;) {
        if (NULL == (line_read = rl_gets(line_read))) {
            fputc('\n', stdout);
            break;
        }
        if (process_input_line(client, line_read))
            break;
    }
    
    client_close(client);
    if (line_read)
        free(line_read);

    return EXIT_SUCCESS;
}

