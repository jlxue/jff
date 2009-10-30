/********************************************************************
 * File: instrument.c
 *
 * Instrumentation source -- link this with your application, and
 *  then execute to build trace data file (trace.dat).
 *
 * Author: M. Tim Jones <mtj@mtjones.com>
 *
 * Modified by: Yin AiHua, Liu Yubao    2009-10-30
 *
 * Usage:
 *  gcc -g -finstrument-functions xxx.c instrument.c
 *  ./xxx
 */

#include <stdio.h>
#include <stdlib.h>

extern "C"
void main_constructor(void)
    __attribute__ ((no_instrument_function, constructor));

extern "C"
void main_destructor(void)
    __attribute__ ((no_instrument_function, destructor));

extern "C"
void __cyg_profile_func_enter(void *, void *)
    __attribute__ ((no_instrument_function));

extern "C"
void __cyg_profile_func_exit(void *, void *)
    __attribute__ ((no_instrument_function));

extern "C"
void enable_fp_record(void)
    __attribute__ ((no_instrument_function));

extern "C"
void disable_fp_record(void)
    __attribute__ ((no_instrument_function));


static FILE *fp = 0;
static int r = 1;


void enable_fp_record(void)
{
    r = 1;
}

void disable_fp_record(void)
{
    r = 0;
}

void main_constructor(void)
{
    if (fp)
        return;
    fp = fopen("trace.dat", "wb");
    if (fp == NULL)
        exit(EXIT_FAILURE);
}


void main_deconstructor(void)
{
    fclose( fp );
}


void __cyg_profile_func_enter(void *ithis, void *callsite)
{
    if (__builtin_expect (!(fp), 1))
        main_constructor();

    if (r) {
        fputc(0, fp);
        fwrite(&ithis, sizeof(ithis), 1, fp);
    }
}


void __cyg_profile_func_exit(void *ithis, void *callsite)
{
    if (r) {
        fputc(1, fp);
        fwrite(&ithis, sizeof(ithis), 1, fp);
    }
}

