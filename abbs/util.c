#include <unistd.h>

#include "util.h"


ssize_t
readn(int fd, void* buf, size_t n)
{
    size_t left, len;
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
    size_t left, len;
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


