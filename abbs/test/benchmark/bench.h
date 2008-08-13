#ifndef BENCH_H__
#define BENCH_H__


#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <sys/time.h>
#include <unistd.h>

#define     RECORD_NUM      50000

#define     SHOW_TIME(s, t2, t1, total)    \
    printf("%-16s time=%6ld ms, total=%d bytes\n", (s), \
           ((t2).tv_sec - (t1).tv_sec) * 1000 + ((t2).tv_usec - (t1).tv_usec) / 1000,   \
           (total))


#ifdef __cplusplus
extern "C" {
#endif

void
run_writer(const char* buffer, unsigned size, const char* filename);


void
run_reader(const char* buffer, unsigned size, const char* filename);


#ifdef __cplusplus
}
#endif

#endif /* BENCH_H__ */

