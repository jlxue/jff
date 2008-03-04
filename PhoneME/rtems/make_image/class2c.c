#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main(int argc, char** argv)
{
    FILE* fp;
    int c;
    int n;

    assert(2 == argc);

    fp = fopen(argv[1], "rb");
    assert(NULL != fp);

    puts("static char classdata [] = {");
    n = 0;
    while (EOF != (c = fgetc(fp))) {
        ++n;
        printf("(char)%d, ", c);
        if (0 == (n % 10))
            puts("");
    }
    puts("};");

    fclose(fp);

    return 0;
}


