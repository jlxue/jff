#ifndef ENV_H__
#define ENV_H__

#ifdef __cplusplus
extern "C" {
#endif


#define     RESTRICTED_ALLOW_OPEN_READ_ENV      "_R_ALLOW_OPEN_READ"
#define     RESTRICTED_ALLOW_OPEN_WRITE_ENV     "_R_ALLOW_OPEN_WRITE"
#define     RESTRICTED_ALLOW_EXEC_ENV           "_R_ALLOW_EXEC"
#define     RESTRICTED_ALLOW_OPENDIR_ENV        "_R_ALLOW_OPENDIR"
#define     RESTRICTED_ALLOW_MKDIR_ENV          "_R_ALLOW_MKDIR"
#define     RESTRICTED_ALLOW_RMDIR_ENV          "_R_ALLOW_RMDIR"

char* const* add_env(char *const envp[], char *const extra[]);

void dump_env(void);


#ifdef __cplusplus
}
#endif

#endif /* ENV_H__ */

