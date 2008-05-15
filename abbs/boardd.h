#ifndef BOARDD_H__
#define BOARDD_H__

#ifdef __cplusplus
extern "C" {
#endif


#define     BOARDD_STOPPED      0
#define     BOARDD_STARTING     1
#define     BOARDD_STARTED      2
#define     BOARDD_STOPPING     3


typedef struct {
    unsigned char       status;
    pid_t               pid;
    uint32_t            alive_article_count;
    uint32_t            last_article_id;
    off_t               index_begin;
    off_t               index_end;
} boardd_info_t;


#ifdef __cplusplus
}
#endif

#endif /* BOARDD_H__ */

