/*
 *  util - some useful macros and functions
 *
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
#include <unistd.h>

#include "util.h"


ssize_t
readn(int fd, void* buf, size_t n)
{
    size_t left;
    ssize_t len;
    char* buffer;

    buffer = buf;
    left = n;

    while (left > 0) {
        if ((len = xread(fd, buffer, left)) < 0)
            return len;
        else if (0 == len)
            break;

        left -= len;
        buffer += len;
    }

    return (n - left);
}



ssize_t
writen(int fd, const void* buf, size_t n)
{
    size_t left;
    ssize_t len;
    const char* buffer;

    buffer = buf;
    left = n;

    while (left > 0) {
        if ((len = xwrite(fd, buffer, left)) <= 0)
            return len;

        left -= len;
        buffer += len;
    }

    return n;
}


