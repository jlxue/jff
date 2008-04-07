#ifndef UTIL_H__
#define UTIL_H__

#include <errno.h>
#include <stddef.h>
#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif


#define     ERROR(cond, s)          \
    do {                            \
        if (cond) {                 \
            fprintf(stderr, "[%s:%d] " s "\n", __FILE__, __LINE__);     \
            goto L_error;           \
        }                           \
    } while (0)


#define     ERRORP(cond, s)         \
    do {                            \
        if (cond) {                 \
            fprintf(stderr, "[%s:%d] " s ": %s\n", __FILE__, __LINE__,  \
                    strerror(errno));   \
            goto L_error;           \
        }                           \
    } while (0)


#define     ERRORV(cond, s, ...)    \
    do {                            \
        if (cond) {                 \
            fprintf(stderr, "[%s:%d] " s "\n", __FILE__, __LINE__,      \
                    __VA_ARGS__);   \
            goto L_error;           \
        }                           \
    } while (0)


#define     ERRORVP(cond, s, ...)   \
    do {                            \
        if (cond) {                 \
            fprintf(stderr, "[%s:%d] " s ": %s\n", __FILE__, __LINE__,  \
                    __VA_ARGS__, strerror(errno));                      \
            goto L_error;           \
        }                           \
    } while (0)


#define     DEBUGV(s, ...)      \
    fprintf(stderr, "[%s:%d] " s "\n", __FILE__, __LINE__, __VA_ARGS__)


#define     ALIGN_UP(n, a)      (((n) + (a) - 1) & ~((size_t)(a) - 1))
#define     ALIGN_DOWN(n, a)    ((n) & ~((size_t)(a) - 1))


#define     MAX(a, b)   ((a) > (b) ? (a) : (b))
#define     MIN(a, b)   ((a) > (b) ? (b) : (a))


/*
 * a = b + c, set a to maximum if wrap around.
 */
#define     ADD_TO_MAX_NO_WRAP(a, b, c, m)  \
    do {    \
        if ((m) - (b) > (c)) (a) = (b) + (c); else (a) = (m);   \
    } while (0)

/*
 * a = b - c, set a to minimum if wrap around.
 */
#define     SUB_TO_MIN_NO_WRAP(a, b, c, m)  \
    do {    \
        if ((b) - (m) > (c)) (a) = (b) - (c); else (a) = (m);   \
    } while (0)


/*
 * copied from git/git-compat-util.h
 */
static inline ssize_t xread(int fd, void *buf, size_t len)
{
    ssize_t nr;
    while (1) {
        nr = read(fd, buf, len);
        if ((nr < 0) && (errno == EAGAIN || errno == EINTR))
            continue;
        return nr;
    }
}


/*
 * copied from git/git-compat-util.h
 */
static inline ssize_t xwrite(int fd, const void *buf, size_t len)
{
    ssize_t nr;
    while (1) {
        nr = write(fd, buf, len);
        if ((nr < 0) && (errno == EAGAIN || errno == EINTR))
            continue;
        return nr;
    }
}


ssize_t
readn(int fd, void* buf, size_t n);

ssize_t
writen(int fd, const void* buf, size_t n);


#ifdef __cplusplus
}
#endif


#endif /* UTIL_H__ */

