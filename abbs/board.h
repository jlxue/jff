#ifndef BOARD_H__
#define BOARD_H__

#ifdef __cplusplus
extern "C" {
#endif

typedef struct board_s board_t;

typedef off_t article_t;


board_t*
board_open(const char* path, char mode);


int
board_close(board_t* board);


article_t
board_get(board_t* board, unsigned id);


int
board_post_begin(board_t* board, const char* author, const char* title,
                 size_t article_len);


int
board_append_data(board_t* board, const void* content, size_t length);


article_t
board_post_end(board_t* board);


int
board_reply_begin(board_t* board, article_t article,
                  const char* author, const char* title, size_t article_len);


int
board_reply_end(board_t* board);


int
board_modify_begin(board_t* board, article_t article, unsigned flags,
                   const char* title, size_t article_len);


int
board_modify_end(board_t* board);


void
board_delete(board_t* board, article_t article);


unsigned
board_article_id(board_t* board, article_t article);


unsigned
board_article_flags(board_t* board, article_t article);


unsigned
board_article_length(board_t* board, article_t article);


const char*
board_article_content(board_t* board, article_t article);


unsigned
board_article_tid(board_t* board, article_t article);


unsigned
board_article_pid(board_t* board, article_t article);


time_t
board_article_ctime(board_t* board, article_t article);


time_t
board_article_mtime(board_t* board, article_t article);


const char*
board_article_author(board_t* board, article_t article);


const char*
board_article_title(board_t* board, article_t article);


int
board_article_set_title(board_t* board, article_t article, const char* title);


int
board_article_set_flags(board_t* board, article_t article, unsigned flags);


#ifdef __cplusplus
}
#endif

#endif /* BOARD_H__ */

