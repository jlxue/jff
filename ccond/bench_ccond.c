#include <assert.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "ccond.h"

char* begin = NULL;
char* end = NULL;

int fa_c(int);
int fa_cpp(int);
int fa_ccond(int);

int fb_c(int a)
{
    if (a >= 0)
        return 3 * a + fa_c(a-1);

    end = (char*)&a;
    return 0;
}

int fa_c(int a)
{
    if (a < 1) {
        end = (char*)&a;
        return 0;
    }

    return 2 * a + fb_c(a-1);
}
/* -------------------------------------------------- */
#ifdef __cplusplus
int fb_cpp(int a)
{
    if (a < 0) {
        end = (char*)&a;
        throw 1;
    }

    return 3 * a + fa_cpp(a-1);
}

int fa_cpp(int a)
{
    try {
        return 2 * a + fb_cpp(a-1);
    } catch (...) {
        return 0;
    }
}
#endif

/* -------------------------------------------------- */
const char* NEGATIVE_ERROR = "negative-error";
const char* RETURN_ZERO = "return-zero";

void return_zero(Condition* c)
{
    invoke_restart(find_restart(RETURN_ZERO), 0);
}

int fb_ccond(int a)
{
    if (a < 0) {
        Condition c;
        c._name = NEGATIVE_ERROR;
        ccond_error(&c);
    }

    return 3 * a + fa_ccond(a-1);
}

int fa_ccond(int a)
{
    int n;

    HANDLER_BIND_BEGIN({NEGATIVE_ERROR, return_zero}) {
        RESTART_CASE_BEGIN(RETURN_ZERO) {
#ifdef __cplusplus
            n = 2 * a + fb_ccond(a-1);
        } RESTART_CATCH {
#else
        case 0:
            n = 2 * a + fb_ccond(a-1);
            break;
#endif
        case 1:
            n = 0;
            break;
        default:
            assert(0);
        } RESTART_CASE_END;
    } HANDLER_BIND_END;

    return n;
}

void run(int (*f)(int), int n, int m, const char* message)
{
    int r;
    clock_t t1, t2;

    t1 = clock();
    while (m-- > 0)
        r = f(n);
    t2 = clock();

    printf("%-30s - got %d. Stack occupied: %u, time spent: %lf\n",
            message, r, begin - end, ((double)(t2 - t1)) / CLOCKS_PER_SEC);
}

int f_setjmp(int a)
{
    int r;
    jmp_buf env;

    if (a < 0)
        return 0;

    if (r = setjmp(env)) {
        r = a + r + f_setjmp(a-1);
    } else {
        r = a - r;
    }

    return r;
}

int main(int argc, char** argv)
{
    int n = 1000;
    int m = 1000;

    begin = (char*)&argc;

    if (argc > 1)
        n = atoi(argv[1]);
    assert(n > 0);

    if (argc > 2)
        m = atoi(argv[2]);
    assert(m > 0);

    printf("recursion count=%d loop count=%d\n", n, m);

    // C
    run(fa_c, n, m, "C, no exception");
    // C++
#ifdef __cplusplus
    run(fa_cpp, n, m, "C++ with try/catch/throw");

    // ccond with try/catch/throw
    run(fa_ccond, n, m, "ccond with try/catch/throw");
#else
    // ccond with setjmp/longjmp
    run(fa_ccond, n, m, "ccond with setjmp/longjmp");
    run(f_setjmp, n, m, "benchmark setjmp");
#endif

    return 0;
}

