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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>

#include "env.h"

extern char** environ;

char* const* add_env(char *const envp[], char *const extra[])
{
    char* const *new_envp;
    char* const *p;
    int n, n2;

    p = envp;
    while (NULL != *p)
        ++p;
    n = p - envp;

    p = extra;
    while (NULL != *p)
        ++p;
    n2 = p - extra;

    new_envp = (char**)malloc(sizeof(char*) * (n + n2 + 1));
    if (NULL == new_envp)
        return NULL;

    memcpy((void*)new_envp, envp, sizeof(char*) * n);
    memcpy((char*)(new_envp + n), extra, sizeof(char*) * (n2 + 1));

    return new_envp;
}


void dump_env(void)
{
    char** p = environ;
    while (*p != NULL) {
        puts(*p);
        ++p;
    }
}

