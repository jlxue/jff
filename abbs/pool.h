#ifndef POOL_H__
#define POOL_H__

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define POOL_FILENAME_MAX   256
#define POOL_PATH_MAX       512


typedef enum {
    OP_INVALID,
    OP_POST,
    OP_REPLY,
    OP_MODIFY,
    OP_DELETE,
    OP_DELETE_RANGE
} board_op_t;


board_op_t
parse_pool_file_name(const char* filename, unsigned short* bid,
                     unsigned* id1, unsigned* id2);

size_t
generate_pool_file_name(char* filename, size_t len, board_op_t op,
                        time_t* t, const char* author, unsigned short bid,
                        unsigned id1, unsigned id2);


#ifdef __cplusplus
}
#endif

#endif /* POOL_H__ */

