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
 */

#ifndef BOARD_H__
#define BOARD_H__

#include <stdint.h>

#define     LIB_BBSDB_VER       "0.1"

#ifdef __cplusplus
extern "C" {
#endif


typedef struct board_s board_t;

typedef struct article_s article_t;


int
bbsdb_init(const char* version);

board_t*
board_open(const char* file, char mode);

int
board_close(board_t* board);

int
board_fsck(board_t* board);

uint32_t
board_count_all_articles(board_t* board);

uint32_t
board_count_alive_articles(board_t* board);

uint32_t
board_get_first_article_id(board_t* board);

uint32_t
board_get_last_article_id(board_t* board);

uint32_t
board_get_first_alive_article_id(board_t* board);

uint32_t
board_get_last_alive_article_id(board_t* board);

article_t*
board_add_article(board_t* board, uint32_t topic_id, uint32_t parent_id,
                  const char* author, const char* title, const char* filename);

article_t*
board_get_article(board_t* board, uint32_t id);


uint32_t
board_get_article_range(board_t* board, uint32_t from,
                        article_t** articles, uint32_t count);

uint32_t
board_delete_article(board_t* board, article_t* article);

uint32_t
board_delete_article_range(board_t* board, article_t** articles,
                           uint32_t count);

article_t*
board_update_article(board_t* board, article_t* article,
                     const char* title, const char* filename);

article_t*
board_reply_article(board_t* board, article_t* article, const char* author,
                     const char* title, const char* filename);

uint32_t
article_get_offset(article_t* article);

uint32_t
article_get_id(article_t* article);

uint32_t
article_get_topic_id(article_t* article);

uint32_t
article_get_parent_id(article_t* article);

uint32_t
article_get_flags(article_t* article);

uint32_t
article_get_ctime(article_t* article);

uint32_t
article_get_mtime(article_t* article);

uint32_t
article_get_size(article_t* article);

const char*
article_get_author(article_t* article);

const char*
article_get_title(article_t* article);


#ifdef __cplusplus
}
#endif

#endif /* BOARD_H__ */ 

