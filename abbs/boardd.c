#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "board.h"


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

    return EXIT_SUCCESS;
}

#if 0
        /*
         * mmap 'board.s' to read and see whether boardd daemon is ready,
         */
        board->stat_fd = open(path, O_RDONLY);
        ERRORVP(-1 == board->stat_fd, "Can't read %s", path);
        ERRORVP(-1 == fstat(board->stat_fd, &st), "Can't fstat %s", path);
        ERRORV(st.st_size != sizeof(boardd_info_t), "%s is incomplete!\n", path);

        board->boardd_info = mmap(NULL, sizeof(boardd_info_t), PROT_READ,
                                  MAP_SHARED, board->stat_fd, 0);
        ERRORVP(NULL == board->boardd_info, "Can't mmap %s", path);

        ERROR(BOARDD_STARTED != board->boardd_info->status,
              "The boardd daemon isn't ready!\n");

#endif

#if 0
        /*
         * mmap 'board.s' to write stat info about boardd daemon.
         */
        board->stat_fd = open(path, O_RDWR);
        ERRORVP(-1 == board->stat_fd, "Can't read and write %s", path);

        board->boardd_info = mmap(NULL, sizeof(boardd_info_t), PROT_WRITE,
                                  MAP_SHARED, board->stat_fd, 0);
        ERRORVP(NULL == board->boardd_info, "Can't mmap %s", path);

        memset(board->boardd_info, 0, sizeof(boardd_info_t));
        board->boardd_info->status = BOARDD_STARTING;
        msync(board->boardd_info, sizeof(boardd_info_t),
              MS_ASYNC | MS_INVALIDATE);
#endif

