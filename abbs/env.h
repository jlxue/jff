/*
 * Copyright (C) 2008 Liu Yubao <yubao.liu@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 */
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

