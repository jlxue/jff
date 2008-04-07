/*
 *  An append-only database to store articles of a BBS board.
 *  
 *  Copyright (C) 2008 Liu Yubao
 * 
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * ChangeLog:
 *
 *  2008-04-07  Liu Yubao <yubao.liu@gmail.com>
 *      * initial version.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <time.h>

#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <search.h>

#include "board.h"
#include "util.h"


#define     DATA_HEADER_MAGIC           "BBSDBDAT"
#define     INDEX_HEADER_MAGIC          "BBSDBIDX"

#define     DATA_HEADER_VERSION         1
#define     INDEX_HEADER_VERSION        1

#define     DEFAULT_DATA_MMAP_WIN       (32 * 1024 * 1024)  /* 32 MB    */
#define     DEFAULT_INDEX_MMAP_WIN      (10 * 1024 * 1024)  /* 10 MB    */

#define     DEFAULT_DATA_EXPAND_SIZE    (8 * 1024)          /* 8 KB     */
#define     DEFAULT_INDEX_EXPAND_SIZE   (4 * 1024)          /* 4 KB     */


#define     MAX_TITLE_LEN               128
#define     MAX_AUTHOR_LEN              32


/* ----------------------------------------------------------------------- */

typedef struct {
    char        a;
    int64_t     b;
} dummy_t;                  /* used to check struct pack            */


/*
 *                                 used_size        st_size
 *                                    \|/             \|/
 * file: ------------------------------.................
 *                 /|\                             '---'
 *                offset                             \_____ stat_info_t in index
 * mmap:            -------------------........
 *                 /|\                /|\    /|\
 *              content              length  window
 *
 *
 * the purpose of [used_size, st_size) is to avoid frequent calls to mmap()
 * when new article is added.
 *
 * assert:
 *      offset <= st_size
 *      0 <= length <= used_size - offset
 *      length <= window
 *      window <= st_size - offset
 */
typedef struct {
    int         fd;         /* file descriptor mapped with mmap()   */
    off_t       st_size;    /* size of file                         */
    off_t       used_size;  /* length of valid data in the file     */
    char*       content;    /* data pointer returned from mmap()    */
    size_t      window;     /* current size of the mapping window   */
    size_t      length;     /* length of valid data in the mmap window      */
    off_t       offset;     /* offset in file, the last parameter of mmap() */
    int         prot;       /* the third parameter of mmap()        */
    int         flags;      /* the fourth parameter of mmap()       */
    size_t      expand_size;/* size of file expansion each time     */
    size_t      default_win;/* default size of the mapping window   */
} mmap_info_t;


/*
 * this structure is read in when open then index and written out when
 * close or expand the index.
 */
typedef struct {
    int32_t     magic;              /* 0xBEEFDEAD                   */
    uint32_t    length;             /* length of this structure     */
    uint32_t    count;              /* count of all articles        */
    uint32_t    first_id;           /* id of first article          */
    uint32_t    last_id;            /* id of last article           */
    uint32_t    alive_count;        /* count of all alive articles  */
    uint32_t    first_alive_id;     /* id of first alive article    */
    uint32_t    last_alive_id;      /* id of last alive article     */
} stat_info_t;


typedef struct {
    char        magic[8];           /* "BBSDBDAT" or "BBSDBIDX"             */
    uint32_t    length;             /* length of this header                */
    int8_t      format;             /* data file format                     */
    int8_t      flags;
    char        padding[2];
} file_header_t;                    /* the header of 'board.d' or 'board.i' */


struct board_s {
    mmap_info_t         data;       /* used to read/write 'board.d'     */
    mmap_info_t         index;      /* used to read/write 'board.i'     */
    stat_info_t         stat;       /* statistics information           */
};


struct article_s {
    int32_t             magic;      /* 0xDEADBEEF                       */
    union {
        uint32_t        offset;     /* offset in 'board.d'              */
        uint32_t        length;     /* length of the record in 'board.d'*/
    };
    uint32_t            id;         /* id of this article               */
    uint32_t            topic_id;   /* the first article of this topic  */
    uint32_t            parent_id;  /* the article to be replied        */
    uint32_t            flags;      /* flags like g, m, @ in NewSMTH    */
    uint32_t            ctime;      /* time of creation                 */
    uint32_t            mtime;      /* time of last modification        */
    uint32_t            size;       /* size of this article, in bytes   */
    char                author[MAX_AUTHOR_LEN];
    char                title[MAX_TITLE_LEN];
};


/* ----------------------------------------------------------------------- */

static inline int
is_little_endian(void)
{
    int n = 1;
    
    return (1 == *((char*)&n));
}


static inline int8_t
get_struct_pack(void)
{
    switch (sizeof(dummy_t)) {
    case 9:
        return 0;       /* pack(1)                  */
    case 10:
        return 1;       /* pack(2)                  */
    case 12:
        return 2;       /* pack(4)                  */
    case 16:
        return 3;       /* pack(8)                  */
    default:
        abort();        /* should never reach here  */
        return 3;
    }
}


static int
write_header(int fd, int type)
{
    file_header_t header;
    stat_info_t stat;
    const char* magic;
    int format;

    assert(-1 != fd && ('d' == type || 'i' == type));

    memset(&header, 0, sizeof(header));
    magic = 'd' == type ? DATA_HEADER_MAGIC : INDEX_HEADER_MAGIC;
    memcpy(header.magic, magic, sizeof(header.magic));
    header.length = sizeof(header);

    format = 'd' == type ? DATA_HEADER_VERSION : INDEX_HEADER_VERSION;
    if (is_little_endian())
        format |= 0x80;
    header.format = (int8_t)format;

    header.flags = get_struct_pack();

    if (0 != lseek(fd, 0, SEEK_SET))
        return -1;
    if (sizeof(header) != writen(fd, &header, sizeof(header)))
        return -1;

    if ('i' == type) {
        memset(&stat, 0, sizeof(stat));
        stat.magic = 0xBEEFDEAD;
        stat.length = sizeof(stat);
        if (sizeof(stat) != writen(fd, &stat, sizeof(stat)))
            return -1;
    }

    return 0;
}


static int
check_header(const file_header_t* header, int type)
{
    const char* magic;
    int format;

    assert(NULL != header && ('d' == type || 'i' == type));

    magic = 'd' == type ? DATA_HEADER_MAGIC : INDEX_HEADER_MAGIC;
    format = 'd' == type ? DATA_HEADER_VERSION : INDEX_HEADER_VERSION;
    if (is_little_endian())
        format |= 0x80;

    if (0 != memcmp(header->magic, magic, sizeof(header->magic)) ||
            sizeof(*header) != header->length ||
            (int8_t)format != header->format ||
            get_struct_pack() != header->flags)
        return -1;

    return 0;
}


static int
check_and_write_header(int fd, int type, int readonly)
{
    file_header_t header;
    ssize_t bytes;
    char* s;

    assert(-1 != fd && ('d' == type || 'i' == type));


    s = 'd' == type ? "data" : "index";
    memset(&header, 0, sizeof(header));
    bytes = readn(fd, &header, sizeof(header));
    if (bytes != sizeof(header)) {
        ERRORV(readonly || bytes != 0, "Incomplete %s file header", s);
        if (! readonly) {
            ERRORVP(0 != write_header(fd, type),
                    "Can't write %s file header", s);
        }
    } else {
        ERRORV(0 != check_header(&header, type), "Corrupted %s file", s);
    }


    return 0;

L_error:
    return -1;
}


static int
check_stat_info(board_t* board)
{
    struct stat st;

    /*
     * initialize st_size fields
     */
    ERRORP(-1 == fstat(board->data.fd, &st), "Can't fstat data file.");
    board->data.st_size = st.st_size;
    ERRORP(-1 == fstat(board->index.fd, &st), "Can't fstat index file.");
    board->index.st_size = st.st_size;


    /*
     * check statistics information
     */
    if (st.st_size > sizeof(file_header_t) + sizeof(stat_info_t)) {
        ssize_t bytes;
        off_t offset;
        article_t article;

        /* read the stat_info_t structure in the end of index file. */
        offset = lseek(board->index.fd, -sizeof(stat_info_t), SEEK_END);
        ERRORP(-1 == offset, "Can't lseek");
        bytes = readn(board->index.fd, &board->stat, sizeof(stat_info_t));
        ERRORP(sizeof(stat_info_t) != bytes, "Can't read stat info.");
        ERRORP(board->stat.magic != 0xBEEFDEAD ||
               sizeof(stat_info_t) != board->stat.length,
               "Corrupted stat info.");

        /* calculate used_size of index file and check it.  */
        board->index.used_size = sizeof(file_header_t) + board->stat.count *
            sizeof(article_t);
        offset = board->index.used_size - sizeof(article_t);
        offset = lseek(board->index.fd, offset, SEEK_SET);
        ERRORP(-1 == offset, "Can't seek in index file.");
        bytes = readn(board->index.fd, &article, sizeof(article_t));
        ERRORP(sizeof(article_t) != bytes, "Can't read the last index item.");
        ERRORP(0xDEADBEEF != article.magic, "Corrupted index file.");

        /* calculate used_size of data file and check it.   */
        offset = article.offset;
        offset = lseek(board->data.fd, offset, SEEK_SET);
        ERRORP(-1 == offset, "Can't seek in data file.");
        bytes = readn(board->data.fd, &article, sizeof(article_t));
        ERRORP(sizeof(article_t) != bytes, "Can't read the last article.");
        ERRORP(0xDEADBEEF != article.magic, "Corrupted data file.");
        board->data.used_size = offset + article.length;

        /* check whether the article is the last one in data file.  */
        if (board->data.used_size + sizeof(article_t) <= board->data.st_size) {
            bytes = readn(board->data.fd, &article, sizeof(article_t));
            ERRORP(sizeof(article_t) != bytes, "Can't read data file.");
            ERRORP(0xDEADBEEF == article.magic, "Corrupted index file.");
        }
    } else {
        assert(sizeof(file_header_t) == board->data.st_size);
        board->data.used_size = sizeof(file_header_t);
        board->index.used_size = sizeof(file_header_t);
        memset(&board->stat, 0, sizeof(stat_info_t));
        board->stat.magic = 0xBEEFDEAD;
        board->stat.length = sizeof(stat_info_t);
    }


    return 0;

L_error:
    return -1;
}


static void
board_destroy(board_t* board)
{
    if (NULL != board) {
        if (MAP_FAILED != board->data.content)
            munmap(board->data.content, board->data.length);
        if (MAP_FAILED != board->index.content)
            munmap(board->index.content, board->index.length);
        if (-1 != board->data.fd)
            close(board->data.fd);
        if (-1 != board->index.fd)
            close(board->index.fd);
    }
}


/*
 * if [begin, begin + size) overlaps the file content, then slide
 * memory mapping window to cover [begin, begin + size) but never
 * exceed the file ending.
 *
 * returns:
 *  -1  error happened.
 *  0   requested range is mapped already or doesn't overlap the file content.
 *  1   [begin, min(begin + size, info->st_size)) is mapped.
 *
 *  notice:
 *      if successfully mapped, info->offset must not be greater than `begin'
 *      as info->offset must be page aligned, and  info->offset + info->length
 *      must be not less than begin + size. if info->length is greater than
 *      info->window, then info->window is adjusted to be equal to info->length.
 */
static int
slide_mmap_window(mmap_info_t* info, off_t begin, size_t size)
{
    off_t pa_offset, end;
    size_t length;
    size_t window;
    char* content;
    long pagesize;
    struct stat st;

    assert(NULL != info && begin >= 0);


    /*
     * adjust `end' and `size' to make sure [begin, end) doesn't exceed
     * the file ending.
     */
    ADD_TO_MAX_NO_WRAP(end, begin, size, UINT32_MAX);
    if (info->st_size < end) {
        ERRORP(-1 == fstat(info->fd, &st), "Can't fstat");
        info->st_size = st.st_size;
        if (info->st_size < end) {
            end = info->st_size;
            SUB_TO_MIN_NO_WRAP(size, end, begin, 0);
        }
    }
    

    /*
     * Is [begin, end) contains valid range?
     */
    if (begin >= end)
        return -1;


    /*
     * Is [begin, end) fully covered by [info->offset, info->window) ?
     */
    if (begin >= info->offset &&  end <= info->offset + info->window) {
        DEBUGV("slide_mmap_window(): cached: [%4ld, %4ld) < [%4ld, %4ld).\n",
               begin, end, info->offset, info->offset + info->window);
        return 0;
    }


    /*
     * make sure [begin, end) can be covered by memory mapping window.
     */
    pagesize = sysconf(_SC_PAGESIZE);
    pa_offset = ALIGN_DOWN(begin, pagesize);
    assert(pa_offset >=0 && pa_offset < info->st_size);

    /* mmap() as much as possible */
    window = MIN(info->default_win, info->st_size);
    if (end - pa_offset > window) {
        window = end - pa_offset;
    } else if (info->st_size - pa_offset < window) {
        SUB_TO_MIN_NO_WRAP(pa_offset, info->st_size, window, 0);
        pa_offset = ALIGN_DOWN(begin, pagesize);
        window = info->st_size - pa_offset;
    }


    /*
     * slide memory mapping window.
     */
    content = mmap(NULL, window, info->prot, info->flags, info->fd, pa_offset);
    ERRORVP(MAP_FAILED == content, "Can't mmap(offset=%ld, window=%d) to %s",
            pa_offset, window, info->prot == PROT_READ ? "read" : "read/write");
    DEBUGV("slide_mmap_window(offset, window): (%8ld, %8d) => (%8ld, %8d).",
           info->offset, info->window, pa_offset, window);


    /*
     * calculate length of valid data in mapping window.
     */
    length = 0;
    if (pa_offset < info->used_size)
        length = MIN(window, info->used_size - pa_offset);
    DEBUGV("slide_mmap_window(): old length -> new length: %d -> %d.",
           info->length, length);


    /*
     * update mmap info.
     */
    if (MAP_FAILED != info->content)
        munmap(info->content, info->length);
    info->content = content;
    info->offset = pa_offset;
    info->length = length;
    info->window = window;


    return 1;

L_error:
    return -1;
}


#if 0
/*
 * slide towards to the file ending.
 */
static inline int
slide_mmap_window_forwards(mmap_info_t* info)
{
}


/*
 * slide towards to the file beginning.
 */
static inline int
slide_mmap_window_backwards(mmap_info_t* info)
{
}
#endif


#if 0
/*
 * fill memory mapping window with file content in [offset, offset + window).
 */
static inline int
fill_mmap_window(mmap_info_t* info)
{
    return slide_mmap_window(info, info->offset, info->window);
}
#endif


static int
expand_file(const mmap_info_t* info)
{
    char buf[256];
    size_t n;
    ssize_t bytes;

    assert(NULL != info && -1 != info->fd && info->expand_size > 0);

    if (-1 == lseek(info->fd, 0, SEEK_END))
        return -1;

    /* to avoid sparse file, don't fill with zero */
    memset(buf, 'z', sizeof(buf));  
    n = info->expand_size;
    while (n > 0) {
        if (n > sizeof(buf))
            bytes = writen(info->fd, buf, sizeof(buf));
        else
            bytes = writen(info->fd, buf, n);
        if (bytes <= 0)
            return -1;
        n -= bytes;
    }

    return 0;
}


static int
write_stat_info(int fd, const stat_info_t* stat)
{
    assert(-1 != fd && NULL != stat);

    if (-1 == lseek(fd, 0, SEEK_END))
        return -1;

    if (sizeof(stat_info_t) != writen(fd, stat, sizeof(stat_info_t)))
        return -1;
    else
        return 0;
}


/*
 * get the range of articles contained completely in memory mapping window. 
 * The out parameters `first' and `last' indict the range [first,start).
 */
static void
get_article_range_in_mmap_window(mmap_info_t* index,
                                 article_t** first, article_t** last,
                                 off_t* first_off, off_t* last_off)
{
    off_t first_offset, last_offset, offset;

    assert(NULL != index && MAP_FAILED != index->content &&
            ((NULL != first && NULL != last) ||
             (NULL != first_off && NULL != last_off)));


    /*
     * does [index->offset, index->length) contain valid data?
     */
    if (index->length < sizeof(article_t)) {
        if (NULL != first)
            *first = *last = NULL;
        if (NULL != first_off)
            *first_off = *last_off = sizeof(file_header_t);
        return;
    }


    /*
     * get relative offset to the first article index.
     */
    if (index->offset < sizeof(file_header_t)) {
        first_offset = 0;
    } else {
        offset = index->offset - sizeof(file_header_t);
        first_offset = ALIGN_UP(offset, sizeof(article_t));
    }

    offset = index->offset + index->length;
    if (offset < sizeof(file_header_t) + sizeof(article_t)) {
        last_offset = 0;
    } else {
        offset -= sizeof(file_header_t);
        last_offset = ALIGN_DOWN(offset, sizeof(article_t));
    }


    /*
     * get absolute offset to the beginning of index file.
     */
    first_offset += sizeof(file_header_t);
    last_offset += sizeof(file_header_t);

    assert(first_offset <= last_offset && first_offset >= index->offset &&
           last_offset <= index->offset + index->length &&
           last_offset + sizeof(article_t) > index->offset + index->length);

    if (NULL != first_off && NULL != last_off) {
        *first_off = first_offset;
        *last_off = last_offset;
    }


    /*
     * translate to memory pointer in index->content.
     */
    if (NULL != first && NULL != last) {
        *first = (article_t*)(index->content + (first_offset - index->offset));
        *last = (article_t*)(index->content + (last_offset - index->offset));
    }
}


static int
cmp_article_by_id(const void* a, const void* b)
{
    article_t* aa = (article_t*)a;
    article_t* bb = (article_t*)b;

    if (aa->id < bb->id)
        return -1;
    else if (aa->id == bb->id)
        return 0;
    else
        return 1;
}


/* ----------------------------------------------------------------------- */


int
bbsdb_init(const char* version)
{
    if (NULL == version || 0 != strcmp(version, LIB_BBSDB_VER))
        return -1;
    else
        return 0;
}


board_t*
board_open(const char* file, char mode)
{
    board_t* board = NULL;
    char* path = NULL;
    int readonly;
    off_t begin;
    size_t len;
    long pagesize;
    
    assert(NULL != file && ('r' == mode || 'w' == mode));


    /*
     * prepare board_t* pointer to return.
     */
    board = malloc(sizeof(board_t));
    ERRORP(NULL == board, "Can't allocate memory for board.");

    memset(board, 0, sizeof(board_t));
    board->data.fd = board->index.fd = -1;
    board->data.content = board->index.content = MAP_FAILED;

    board->data.flags = MAP_SHARED;
    board->index.flags = MAP_SHARED;

    pagesize = sysconf(_SC_PAGESIZE);
    board->data.default_win = ALIGN_UP(DEFAULT_DATA_MMAP_WIN, pagesize);
    board->index.default_win = ALIGN_UP(DEFAULT_INDEX_MMAP_WIN, pagesize);

    board->data.expand_size = DEFAULT_DATA_EXPAND_SIZE;
    board->index.expand_size = DEFAULT_INDEX_EXPAND_SIZE;


    /*
     * prepare file path for 'board.d'.
     */
    len = strlen(file);
    assert(len > 0);
    path = malloc(len + 3);
    ERRORP(NULL == path, "Can't allocate memory for path.");
    strcpy(path, file);
    path[len] = '.';
    path[len + 1] = 'd';
    path[len + 2] = '\0';


    /*
     * open 'board.d' and 'board.i'
     */
    readonly = 'r' == mode ? 1 : 0;
    if (readonly) {
        board->data.fd = open(path, O_RDONLY);
        ERRORVP(-1 == board->data.fd, "Can't open %s to read", path);
        board->data.prot = PROT_READ;

        path[len + 1] = 'i';
        board->index.fd = open(path, O_RDONLY);
        ERRORVP(-1 == board->data.fd, "Can't open %s to read", path);
        board->index.prot = PROT_READ;
    } else {
        board->data.fd = open(path, O_RDWR | O_CREAT, 0644);
        ERRORVP(-1 == board->data.fd, "Can't open %s to read and write", path);
        board->data.prot = PROT_WRITE;

        path[len + 1] = 'i';
        board->index.fd = open(path, O_RDWR | O_CREAT, 0644);
        ERRORVP(-1 == board->data.fd, "Can't open %s to read and write", path);
        board->index.prot = PROT_WRITE;
    }


    /*
     * check data file header and index file header, write if neccessary.
     */
    if (0 != check_and_write_header(board->data.fd, 'd', readonly))
        goto L_error;
    if (0 != check_and_write_header(board->index.fd, 'i', readonly))
        goto L_error;


    /*
     * check statistics information in the end of index file.
     */
    if (0 != check_stat_info(board))
        goto L_error;


    /*
     * do memory mapping for data file and index file.
     */
    begin = 0;
    if (board->data.used_size > board->data.default_win)
        begin = board->data.used_size - board->data.default_win;
    if (-1 == slide_mmap_window(&board->data, begin, board->data.default_win))
        goto L_error;
    
    begin = 0;
    if (board->index.used_size > board->index.default_win)
        begin = board->index.used_size - board->index.default_win;
    if (-1 == slide_mmap_window(&board->index, begin, board->index.default_win))
        goto L_error;


    return board;

L_error:
    board_destroy(board);
    if (NULL != path)
        free(path);
    return NULL;
}


int
board_close(board_t* board)
{
    int ret = 0;

    /*
     * write statistics information to the end of index file.
     */
    if (PROT_WRITE == board->index.prot) {
        struct stat st;
        if ((-1 != fstat(board->index.fd, &st) &&
                board->index.used_size + sizeof(stat_info_t) <= st.st_size &&
                -1 != lseek(board->index.fd, -sizeof(stat_info_t), SEEK_END)) ||
                -1 != lseek(board->index.fd, 0, SEEK_END)) {
            writen(board->index.fd, &board->stat, sizeof(stat_info_t));
        } else {
            ret = -1;
        }
    }

    board_destroy(board);

    return ret;
}


int
board_fsck(board_t* board)
{
    return -1;
}


/*
 * count all articles including deleted.
 */
uint32_t
board_count_all_articles(board_t* board)
{
    assert(NULL != board);

    return board->stat.count;
}


uint32_t
board_count_alive_articles(board_t* board)
{
    assert(NULL != board);

    return board->stat.alive_count;
}


uint32_t
board_get_first_article_id(board_t* board)
{
    assert(NULL != board);

    return board->stat.first_id;
}


uint32_t
board_get_last_article_id(board_t* board)
{
    assert(NULL != board);

    return board->stat.last_id;
}


uint32_t
board_get_first_alive_article_id(board_t* board)
{
    assert(NULL != board);

    return board->stat.first_alive_id;
}


uint32_t
board_get_last_alive_article_id(board_t* board)
{
    assert(NULL != board);

    return board->stat.last_alive_id;
}


article_t*
board_add_article(board_t* board, uint32_t topic_id, uint32_t parent_id,
                  const char* author, const char* title, const char* filename)
{
    struct stat st;
    article_t* article;
    uint32_t length;
    int fd = -1, author_len, title_len;
    char* p;

    assert(NULL != board && NULL != author && NULL != title && NULL != filename);


    author_len = strlen(author);
    title_len = strlen(title);
    if (author_len >= MAX_AUTHOR_LEN || title_len >= MAX_TITLE_LEN)
        goto L_error;


    /*
     * prepare room for the new article.
     */
    ERRORP(-1 == fstat(board->index.fd, &st), "Can't fstat index file.");
    board->index.st_size = st.st_size;
    ERRORP(-1 == fstat(board->data.fd, &st), "Can't fstat data file.");
    board->data.st_size = st.st_size;

    fd = open(filename, O_RDONLY);
    ERRORVP(-1 == fd, "Can't open %s to read.", filename);
    ERRORVP(-1 == fstat(fd, &st), "Can't fstat: %s", filename);
    ADD_TO_MAX_NO_WRAP(length, st.st_size, 10 * sizeof(article_t), UINT32_MAX);
    if (board->data.st_size - board->data.used_size < length)
        ERRORP(-1 == expand_file(&board->data),
               "Error happened when expand data file.");
    if (board->index.st_size - board->index.used_size < 10 * sizeof(article_t)) {
        ERRORP(-1 == expand_file(&board->index),
               "Error happened when expand index file.");
        ERRORP(-1 == write_stat_info(board->index.fd, &board->stat),
               "Error happend when write stat info.");
    }


    /*
     * write data file.
     */
    length = sizeof(article_t) + st.st_size;
    if (-1 == slide_mmap_window(&board->data, board->data.used_size, length))
        goto L_error;
    p = board->data.content + board->data.length + sizeof(article_t);
    if (p < board->data.content + board->data.length)
        goto L_error;
    if (st.st_size != readn(fd, p, st.st_size))
        goto L_error;
    article = (article_t*)(board->data.content + board->data.length);
    article->length = length;
    article->id = board->stat.last_id + 1;
    if (0 == topic_id)
        topic_id = parent_id = article->id;     /* new article, not reply   */
    article->topic_id = topic_id;
    article->parent_id = parent_id;
    article->flags = 0;
    article->ctime = article->mtime = time(NULL);
    article->size = st.st_size;
    memset(article->author, 0, MAX_AUTHOR_LEN);
    memset(article->title, 0, MAX_TITLE_LEN);
    memcpy(article->author, author, author_len);
    memcpy(article->title, title, title_len);
    article->magic = 0xDEADBEEF;


    /*
     * write index file.
     */
    if (-1 == slide_mmap_window(&board->index, board->index.used_size,
                                sizeof(article_t)))
        goto L_error;
    memcpy(board->index.content + board->index.length, article,
           sizeof(article_t));
    article = (article_t*)(board->index.content + board->index.length);
    article->offset = board->data.used_size;


    /*
     * update mmap_info.
     */
    board->index.used_size += sizeof(article_t);
    board->index.length += sizeof(article_t);
    board->data.used_size += length;
    board->data.length += length;


    /*
     * update stat_info.
     */
    board->stat.last_id++;
    board->stat.last_alive_id = board->stat.last_id;
    board->stat.count++;
    board->stat.alive_count++;


    close(fd);
    return article;

L_error:
    if (-1 != fd)
        close(fd);
    return NULL;
}


article_t*
board_get_article(board_t* board, uint32_t id)
{
    mmap_info_t* index;

    assert(NULL != board);


    /*
     * basic range check
     */
    if (0 == id || id < board->stat.first_id || id > board->stat.last_id)
        return NULL;

    
    /*
     * search the article specified by ID, slide memory mapping window
     * if neccessary.
     */
    index = (mmap_info_t*)&board->index;
    do {
        article_t *first_article, *last_article;
        off_t first_offset, last_offset;

        get_article_range_in_mmap_window(index, &first_article, &last_article,
                                         &first_offset, &last_offset);

        if (first_article >= last_article)
            return NULL;

        if (id < first_article->id) {
            off_t new_offset;

            if (index->offset <= sizeof(file_header_t))
                return NULL;

            SUB_TO_MIN_NO_WRAP(new_offset, first_offset, index->window, 0);
            /* slide towards to the file beginning  */
            if (1 != slide_mmap_window(index, new_offset, index->window))
                return NULL;

        } else if (id > (last_article - 1)->id) {
            /* slide towards to the file ending     */
            if (1 != slide_mmap_window(index, last_offset, index->window))
                return NULL;

        } else {
            article_t article;
            article.id = id;
            return bsearch(&article, first_article,
                           last_article - first_article,
                           sizeof(article_t), cmp_article_by_id);
        }
    } while (1);
}


uint32_t
board_get_article_range(board_t* board, uint32_t from,
                        article_t** articles, uint32_t count)
{
    return 0;
}


uint32_t
board_delete_article(board_t* board, article_t* article)
{
    assert(NULL != board && NULL != article && 0xDEADBEEF == article->magic);

    article->flags = 1;

    return 1;
}


uint32_t
board_delete_article_range(board_t* board, article_t** articles,
                           uint32_t count)
{
    uint32_t i;
    uint32_t n;

    assert(NULL != board && NULL != articles);

    n = 0;
    for (i = 0; i < count; ++i) {
        if (NULL == articles[i] || 0xDEADBEEF != articles[i]->magic)
            continue;
        articles[i]->flags = 1;
        ++n;
    }

    return n;
}


article_t*
board_update_article(board_t* board, article_t* article,
                     const char* title, const char* filename)
{
    assert(NULL != board && NULL != article && 0xDEADBEEF == article->magic &&
           NULL != filename);

    return board_add_article(board, article->topic_id, article->parent_id,
                             article->author,
                             NULL == title ? article->title : title,
                             filename);
}


article_t*
board_reply_article(board_t* board, article_t* article, const char* author,
                    const char* title, const char* filename)
{
    assert(NULL != board && NULL != article && 0xDEADBEEF == article->magic &&
           NULL != filename);

    return board_add_article(board, article->topic_id, article->id, author,
                             NULL == title ? article->title : title,
                             filename);
}


uint32_t
article_get_offset(article_t* article)
{
    assert(NULL != article);

    return article->offset;
}


uint32_t
article_get_id(article_t* article)
{
    assert(NULL != article);

    return article->id;
}


uint32_t
article_get_topic_id(article_t* article)
{
    assert(NULL != article);

    return article->topic_id;
}


uint32_t
article_get_parent_id(article_t* article)
{
    assert(NULL != article);

    return article->parent_id;
}


uint32_t
article_get_flags(article_t* article)
{
    assert(NULL != article);

    return article->flags;
}


uint32_t
article_get_ctime(article_t* article)
{
    assert(NULL != article);

    return article->ctime;
}


uint32_t
article_get_mtime(article_t* article)
{
    assert(NULL != article);

    return article->mtime;
}


uint32_t
article_get_size(article_t* article)
{
    assert(NULL != article);

    return article->size;
}


const char*
article_get_author(article_t* article)
{
    assert(NULL != article);

    return article->author;
}


const char*
article_get_title(article_t* article)
{
    assert(NULL != article);

    return article->title;
}

