#include "bench.h"

char    buffer[4096];

int main(int argc, char** argv) {
    char* p;
    unsigned i;

    assert(3 == argc);

    for (i = 0; i < sizeof(buffer); ++i)
        buffer[i] = (char)i;

    p = argv[2];

    if ('w' == *p || 'w' == *(p+1))
        run_writer(buffer, sizeof(buffer), argv[1]);

    if ('r' == *p || 'r' == *(p+1))
        run_reader(buffer, sizeof(buffer), argv[1]);

    return 0;
}

