#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "board.h"

static void
remove_board(const char* file)
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
test_board_open_close(const char* file)
{
    board_t* board;

    remove_board(file);

    board = board_open(file, 'r');
    assert(NULL == board);
    fprintf(stderr, "must not open non-exist board: ok\n");

    board = board_open(file, 'r');
    assert(NULL == board);
    fprintf(stderr, "must not create board: ok\n");

    board = board_open(file, 'w');
    assert(NULL != board);
    board_close(board);
    fprintf(stderr, "create board: ok\n");
    
    board = board_open(file, 'r');
    assert(NULL != board);
    board_close(board);
    fprintf(stderr, "open board to read: ok\n");

    board = board_open(file, 'w');
    assert(NULL != board);
    board_close(board);
    fprintf(stderr, "open board to write: ok\n");

    board = board_open(file, 'r');
    assert(NULL != board);
    board_close(board);
    fprintf(stderr, "open board to read: ok\n");
}


static void
test_board_get_article(const char* file)
{
    board_t* board;
    uint32_t id;

    remove_board(file);

    board = board_open(file, 'w');
    assert(NULL != board);
    board_close(board);

    board = board_open(file, 'r');
    assert(NULL != board);

    for (id = 0; id < 1000000; ++id)
        assert(NULL == board_get_article(board, id));
    fprintf(stderr, "get non-exist article: ok\n");

    board_close(board);
}


static void
test_board_add_article(const char* file)
{
    board_t* board;
    char* author = "dieken";
    char* title = "hello world!";
    char* filename = "mypost";
    struct stat st;
    uint32_t ids[256];
    int i;
    article_t* article;

    if (-1 == stat(filename, &st)) {
        fprintf(stderr, "ERROR: please write some text to `mypost' file first!\n");
        return;
    }

    remove_board(file);

    /*
     * add articles.
     */
    board = board_open(file, 'w');
    assert(NULL != board);

    for (i = 0; i < sizeof(ids)/sizeof(uint32_t); ++i) {
        article = board_add_article(board, 0, 0, author, title, filename);
        assert(NULL != article && 0 != article_get_id(article));
        ids[i] = article_get_id(article);
    }

    board_close(board);

    fprintf(stderr, "add 256 articles: ok\n");


    /*
     * get articles.
     */
    board = board_open(file, 'r');
    assert(NULL != board);

    for (i = 0; i < sizeof(ids)/sizeof(uint32_t); ++i) {
        article = board_get_article(board, ids[i]);
        assert(0 != article);
        printf("offset=%8d (i %4d t %4d p %4d) size=%4d, %8s %s\n",
               article_get_offset(article), article_get_id(article),
               article_get_topic_id(article), article_get_parent_id(article),
               article_get_size(article), article_get_author(article),
               article_get_title(article));
        assert(article_get_id(article) == ids[i]);
    }
    fprintf(stderr, "get 256 articles: ok\n");

    board_close(board);
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

    bbsdb_init(LIB_BBSDB_VER);

    test_board_open_close(argv[1]);

    test_board_get_article(argv[1]);

    test_board_add_article(argv[1]);

    return EXIT_SUCCESS;
}

