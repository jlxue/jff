#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dirent.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "apdb.h"
#include "util.h"

#define TITLE_LEN       64

typedef int (*cmp_func_t)(const void*, const void*);

static void
usage(void)
{
    fprintf(stderr, "Usage: boardd <board_db>\n");
}


int cmp_by_mtime(const struct dirent** a, const struct dirent** b)
{
    struct stat sta, stb;

    if (-1 == stat((*a)->d_name, &sta))
        return 0;
    if (-1 == stat((*b)->d_name, &stb))
        return 0;

    return sta.st_mtime - stb.st_mtime;
}


int
main(int argc, char** argv)
{
    char*       dbname;
    char*       pooldir;
    apdb_t*     db;
    struct stat st;
    DIR*        dir;
    struct dirent*     ent;
    int         ret;
    time_t      old_mtime;

    if (argc < 3) {
        usage();
        return EXIT_FAILURE;
    }

    dbname = argv[1];
    pooldir = argv[2];

    db = apdb_open(dbname, 'w', TITLE_LEN);
    if (NULL == db) {
        fprintf(stderr, "failed to open %s to write.\n", dbname);
        return EXIT_FAILURE;
    }

    ret = chdir(pooldir);
    assert(0 == ret);
    dir = opendir(".");
    assert(NULL != dir);
    /* make sure it's empty */
    while (NULL != (ent = readdir(dir))) {
        if (0 == strcmp(".", ent->d_name) ||
                0 == strcmp("..", ent->d_name))
            continue;
        assert(0);
    }
    closedir(dir);

    old_mtime = 0;
    while (-1 != stat(".", &st)) {
        int n;
        struct dirent** namelist;

        if (old_mtime == st.st_mtime) {
            sleep(2);
            continue;
        } else
            old_mtime = st.st_mtime;

        n = scandir(".", &namelist, NULL, (cmp_func_t)cmp_by_mtime);
        if (n < 0) {
            perror("");
            break;
        } else if (0 == n) {
            sleep(2);
        } else {
            int i, fd, len;
            char title[TITLE_LEN];
            char* data;

            for (i = 0; i < n; ++i) {
                if (-1 == stat(namelist[i]->d_name, &st))
                    break;
                if (! S_ISREG(st.st_mode))
                    continue;

                fd = open(namelist[i]->d_name, O_RDONLY);
                if (-1 == fd)
                    break;
                data = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
                if (MAP_FAILED == data) {
                    close(fd);
                    break;
                }

                len = strcspn(data, "\r\n");
                len = MIN(st.st_size, len);
                len = MIN(len, TITLE_LEN - 4);
                strncpy(title, data, len);
                memset(title + len, 0, TITLE_LEN - len);

                if (0 != apdb_add_begin(db, st.st_size))
                    break;
                if (0 != apdb_append_data(db, data, st.st_size))
                    break;
                if (-1 == apdb_add_end(db, title))
                    break;

                munmap(data, st.st_size);
                unlink(namelist[i]->d_name);
            }

            if (i < n)
                break;

            for (i = 0; i < n; ++i)
                free(namelist[i]);
            free(namelist);
        }
    }

    perror("out of while loop");
    apdb_close(db);

    return EXIT_SUCCESS;
}

