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
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <readline/history.h>
#include <readline/readline.h>

#include "board.h"
#include "pool.h"
#include "user.h"
#include "util.h"


#define ARTICLES_PER_SCREEN     20
#define DELIM_CHARS             " \t,"
#define CLIENT_CMD_NUM          (sizeof(cmd_table)/sizeof(cmd_t))


struct client_s;

typedef void (*cmd_func_t)(struct client_s* client);

typedef struct {
    char        c;
    cmd_func_t  func;
    int         argc;
} cmd_t;


typedef struct client_s {
    board_t*        board;
    char*           db_path;
    char            pool_file[POOL_PATH_MAX];
    char            tmp_file[POOL_PATH_MAX];
    size_t          pool_dir_len;
    size_t          tmp_dir_len;
    unsigned short  board_id;

    article_t       articles[ARTICLES_PER_SCREEN];
    unsigned        article_ids[ARTICLES_PER_SCREEN];
    unsigned        count;

    cmd_func_t      cmd;
    unsigned        arg1, arg2;
} client_t;


static void
client_cmd_view(client_t* client);

static void
client_cmd_delete(client_t* client);

static void
client_cmd_delete_range(client_t* client);

static void
client_cmd_edit(client_t* client);

static void
client_cmd_change_title(client_t* client);

static void
client_cmd_list(client_t* client);

static void
client_cmd_next_page(client_t* client);

static void
client_cmd_prev_page(client_t* client);

static void
client_cmd_post(client_t* client);

static void
client_cmd_reply(client_t* client);


cmd_t cmd_table[] = {
    {'v',   client_cmd_view,            1},
    {'d',   client_cmd_delete,          1},
    {'D',   client_cmd_delete_range,    2},
    {'E',   client_cmd_edit,            1},
    {'T',   client_cmd_change_title,    1},
    {'l',   client_cmd_list,            1},
    {'n',   client_cmd_next_page,       0},
    {'p',   client_cmd_prev_page,       0},
    {'P',   client_cmd_post,            0},
    {'r',   client_cmd_reply,           1}
};


static int
client_destroy(client_t* client)
{
    int ret = 0;

    if (NULL == client)
        return 0;

    if (NULL != client->db_path)
        free(client->db_path);

    if (NULL != client->board)
        ret = board_close(client->board);

    free(client);

    return ret;
}


static client_t*
client_create(unsigned short bid, const char* db_path,
              const char* pool_dir, const char* tmp_dir)
{
    client_t* client;

    assert(NULL != db_path && NULL != pool_dir && NULL != tmp_dir);

    client = (client_t*)calloc(1, sizeof(client_t));
    if (NULL == client)
        return NULL;

    client->db_path = strdup(db_path);
    if (NULL == client->db_path)
        goto L_error;


    client->pool_dir_len = strlen(pool_dir);
    client->tmp_dir_len = strlen(tmp_dir);

    if (client->pool_dir_len >= POOL_PATH_MAX - 2 ||
            client->tmp_dir_len >= POOL_PATH_MAX - 2) {
        fprintf(stderr, "pool_dir and tmp_dir can't be longer than %d bytes\n",
                (POOL_PATH_MAX - 3));
        goto L_error;
    }

    memcpy(client->pool_file, pool_dir, client->pool_dir_len);
    memcpy(client->tmp_file, tmp_dir, client->tmp_dir_len);

    if ('/' != client->pool_file[client->pool_dir_len - 1])
        client->pool_file[client->pool_dir_len++] = '/';

    if ('/' != client->tmp_file[client->tmp_dir_len - 1])
        client->tmp_file[client->tmp_dir_len++] = '/';


    client->board = board_open(db_path, 'r');
    if (NULL == client->board)
        goto L_error;

    client->board_id = bid;

    return client;

L_error:
    client_destroy(client);
    return NULL;
}


/* Read a string, and return a pointer to it. Returns NULL on EOF. */
static char *
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


static void
client_cmd_view(client_t* client)
{
    FILE* fp;
    article_t article;
    time_t t;

    article = board_get(client->board, client->arg1, (WRITING | DELETED));
    if (-1 == article) {
        fprintf(stderr, "article %u not found\n", client->arg1);
        return;
    }

    fflush(stdout);
    fflush(stderr);

    fp = popen("/usr/bin/less", "w");
    if (NULL == fp) {
        perror("can't launch /usr/bin/less");
        return;
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
}


static void
client_cmd_delete(client_t* client)
{
    char filename[POOL_FILENAME_MAX];
    time_t curtime;
    const char* author;
    size_t len;
    int fd;

    assert(NULL != client);

    curtime = time(NULL);

    author = get_user_name(geteuid());
    if (NULL == author) {
        perror("can't find current user name");
        return;
    }

    len = generate_pool_file_name(filename, POOL_FILENAME_MAX,
                OP_DELETE, &curtime, author, client->board_id, client->arg1, 0);
    if (0 == len) {
        fprintf(stderr, "pool file name too long\n");
        return;
    }

    if (len + client->pool_dir_len >= POOL_PATH_MAX) {
        fprintf(stderr, "user name too long\n");
        return;
    }
    memcpy(client->pool_file + client->pool_dir_len, filename, len);

    fd = mkstemp(client->pool_file);
    if (-1 == fd) {
        perror(client->pool_file);
        return;
    }
    close(fd);
}


static void
client_cmd_delete_range(client_t* client)
{
    char filename[POOL_FILENAME_MAX];
    time_t curtime;
    const char* author;
    size_t len;
    int fd;

    assert(NULL != client);

    curtime = time(NULL);

    author = get_user_name(geteuid());
    if (NULL == author) {
        perror("can't find current user name");
        return;
    }

    len = generate_pool_file_name(filename, POOL_FILENAME_MAX,
                OP_DELETE_RANGE, &curtime, author, client->board_id,
                client->arg1, client->arg2);
    if (0 == len) {
        fprintf(stderr, "pool file name too long\n");
        return;
    }

    if (len + client->pool_dir_len >= POOL_PATH_MAX) {
        fprintf(stderr, "user name too long\n");
        return;
    }
    memcpy(client->pool_file + client->pool_dir_len, filename, len);

    fd = mkstemp(client->pool_file);
    if (-1 == fd) {
        perror(client->pool_file);
        return;
    }
    close(fd);
}


static void
client_cmd_edit(client_t* client)
{
}


static void
client_cmd_change_title(client_t* client)
{
}


static void
client_cmd_list(client_t* client)
{
    article_t from;
    char timestamp[17];   /* YYYY-mm-dd HH:MM */
    time_t t;

    if (1 == client->arg1) {
        from = board_first(client->board, (WRITING | DELETED));
        if (-1 == from) {
            fprintf(stderr, "no article found\n");
            return;
        }
    } else {
        unsigned flags;

        from = board_get(client->board, client->arg1, 0);
        if (-1 == from) {
            fprintf(stderr, "article %u not found\n", client->arg1);
            return;
        }

        flags = board_article_flags(client->board, from);
        if (flags & (WRITING | DELETED)) {
            from = board_next(client->board, from, (WRITING | DELETED));
            if (-1 == from) {
                fprintf(stderr, "no article found\n");
                return;
            }
        }
    }

    client->count = 0;

    puts("  id/ pid/ tid/    time                  author    title (length)");
    puts("-----------------------------------------------------------------");
    do {
        if ((WRITING | DELETED) & board_article_flags(client->board, from))
            continue;

        t = board_article_ctime(client->board, from);

        if (sizeof(timestamp) - 1 != strftime(timestamp, sizeof(timestamp),
                                              "%Y-%m-%d %H:%M",
                                              localtime(&t)))
            break;

        client->articles[client->count] = from;
        client->article_ids[client->count] = board_article_id(client->board,
                                                              from);

        printf("%4u/%4u/%4u/    %s  %10s    %s (%d)\n",
               client->article_ids[client->count],
               board_article_pid(client->board, from),
               board_article_tid(client->board, from),
               timestamp,
               board_article_author(client->board, from),
               board_article_title(client->board, from),
               board_article_length(client->board, from));

        from = board_next(client->board, from, 0);

    } while (++client->count < ARTICLES_PER_SCREEN && -1 != from);
}


static void
client_cmd_next_page(client_t* client)
{
}


static void
client_cmd_prev_page(client_t* client)
{
}


static void
client_cmd_post(client_t* client)
{
    char filename[POOL_FILENAME_MAX];
    time_t curtime;
    const char* author;
    size_t len;
    int fd;
    pid_t pid;
    struct stat st;

    assert(NULL != client);

    curtime = time(NULL);

    author = get_user_name(geteuid());
    if (NULL == author) {
        perror("can't find current user name");
        return;
    }

    len = generate_pool_file_name(filename, POOL_FILENAME_MAX,
                OP_POST, &curtime, author, client->board_id, 0, 0);
    if (0 == len) {
        fprintf(stderr, "pool file name too long\n");
        return;
    }

    if (len + client->tmp_dir_len >= POOL_PATH_MAX) {
        fprintf(stderr, "user name too long\n");
        return;
    }

    if (len + client->pool_dir_len >= POOL_PATH_MAX) {
        fprintf(stderr, "user name too long\n");
        return;
    }
    memcpy(client->tmp_file + client->tmp_dir_len, filename, len);

    fd = mkstemp(client->tmp_file);
    if (-1 == fd) {
        perror(client->tmp_file);
        return;
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
        execl("/usr/bin/rvim", "/usr/bin/rvim", client->tmp_file, NULL);
        exit(-1);
    }

    if (-1 == stat(client->tmp_file, &st) || 0 == st.st_size)
        return;

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
    
    memcpy(client->pool_file + client->pool_dir_len,
           client->tmp_file + client->tmp_dir_len, len);
    if (0 == symlink(client->tmp_file, client->pool_file))
        return;

L_error:
    unlink(client->tmp_file);
    return;
}


static void
client_cmd_reply(client_t* client)
{
}


static void
help(void)
{
    puts("Available commands\n"
         "  NNNN                view specified article\n"
         "  v NNNN              view specified article\n"
         "  d NNNN              delete specified article\n"
         "  D NNNN,MMMM         delete range\n"
         "  E NNNN              edit specified article\n"
         "  T NNNN              rename title of specified article\n"
         "  l NNNN              list range [NNNN, NNNN + 20)\n"
         "  n                   next 20 articles\n"
         "  p                   previous 20 articles\n"
         "  P                   post new article\n"
         "  r NNNN              reply specified article\n"
         "  h or ?              show this help\n"
         "  q                   quit\n"
         "  <enter>             repeat last command"
         );
}


static int
str_to_uint(const char* s, unsigned* i)
{
    unsigned long n;
    char* end;

    assert(NULL != s && NULL != i);

    if ('-' == *s)
        return -1;

    n = strtoul(s, &end, 10);
    if ('\0' != *end)
        return -1;
    if (n >= UINT_MAX)
        return -1;

    *i = (unsigned)n;
    return 0;
}


/*
 * @retval
 * -1   exit
 *  0   go on
 */
static int
process_input_line(client_t* client, char* line)
{
    char *token;
    cmd_func_t cmd;
    unsigned arg1, arg2;

    token = strtok(line, DELIM_CHARS);
    if (NULL != token) {
        if (0 == str_to_uint(token, &arg1)) {       /* special view command */
            if (NULL == strtok(NULL, DELIM_CHARS))
                cmd = client_cmd_view;
            else
                goto L_bad;
        } else {
            unsigned i;
            char c;

            /* get cmd  */
            c = token[0];
            for (i = 0; i < CLIENT_CMD_NUM; ++i) {
                if (cmd_table[i].c == c)
                    break;
            }
            if (CLIENT_CMD_NUM == i) {
                if ('h' == c || '?' == c) {
                    help();
                    return 0;
                } else if ('q' == c)
                    return -1;
                else
                    goto L_bad;
            }

            /* get arg 1    */
            token = strtok(NULL, DELIM_CHARS);
            if (NULL == token) {
                if (cmd_table[i].argc > 0)
                    goto L_bad;
            } else {
                if (cmd_table[i].argc == 0 || -1 == str_to_uint(token, &arg1))
                    goto L_bad;

                /* get arg2 */
                token = strtok(NULL, DELIM_CHARS);
                if (NULL == token) {
                    if (cmd_table[i].argc > 1)
                        goto L_bad;
                } else if (cmd_table[i].argc == 1 || -1 == str_to_uint(token, &arg2))
                    goto L_bad;
            }

            cmd = cmd_table[i].func;
        }
        client->cmd = cmd;
        client->arg1 = arg1;
        client->arg2 = arg2;
    }

    if (NULL != client->cmd)
        client->cmd(client);
    return 0;

L_bad:
    fprintf(stderr, "Bad command, type ? or h for help.\n");
    return 0;
}


static void
usage(void)
{
    fprintf(stderr, "Usage: bbs board_id db_path pool_dir tmp_dir\n");
}


int main(int argc, char** argv)
{
    client_t* client;
    char* line_read;
    int i;

    if (argc < 5) {
        usage();
        return EXIT_FAILURE;
    }

    umask(077);     /* make mkstemp() secure    */

    i = atoi(argv[1]);
    if (! IS_VALID_BOARD_ID(i)) {
        fprintf(stderr, "invalid board id");
        usage();
        return EXIT_FAILURE;
    }

    client = client_create((unsigned short)i, argv[2], argv[3], argv[4]);
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
    
    client_destroy(client);
    if (line_read)
        free(line_read);

    return EXIT_SUCCESS;
}

