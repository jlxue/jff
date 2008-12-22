#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ccond.h"

static const char* EMPTY_LINE_ERROR             = "empty-line-error";
static const char* MALFORMED_LOG_ENTRY_ERROR    = "malformed-log-entry-error";
static const char* SKIP_LOG_ENTRY               = "skip-log-entry";

typedef struct {
    EXTENDS_CONDITION;
    char*   cause;
} MyCondition;


void skip_log_entry(Condition* c)
{
    MyCondition* c2 = (MyCondition*)c;

    invoke_restart(find_restart(SKIP_LOG_ENTRY), c2->cause);
}

size_t parse_log_entry(char* line)
{
    MyCondition c;
    size_t n;

    if (' ' == line[0]) {
        c._name = MALFORMED_LOG_ENTRY_ERROR;
        c.cause = (char*)"leading space";
        ccond_error((Condition*)&c);
    }

    if ('\n' == line[0]) {
        c._name = EMPTY_LINE_ERROR;
        c.cause = (char*)"empty line";
        ccond_error((Condition*)&c);
    }

    if ('\0' == line[0]) {
        c._name = EMPTY_LINE_ERROR;
        c.cause = (char*)"invalid line";
        ccond_error((Condition*)&c);
    }

    n = strlen(line);
    if ('\n' == line[n - 1])
        line[--n] = '\0';

    return n;
}

size_t parse_log_file(FILE* fp, char* entry, size_t size)
{
    size_t len;

    assert(NULL != fp && NULL != entry && size > 0);

    if (NULL == fgets(entry, size, fp)) {
        entry[0] = '\0';
        return 0;
    }

    RESTART_CASE_BEGIN(SKIP_LOG_ENTRY) {
#ifdef __cplusplus
        len = parse_log_entry(entry);
    } RESTART_CATCH {
#else
    case 0:
        len = parse_log_entry(entry);
        break;
#endif
    case 1:
        {
            strcpy(entry, "skip it: ");
            strcat(entry, RESTART_ARG(char*));
            len = strlen(entry);
        }
        break;
    default:
        assert(0);
    } RESTART_CASE_END;

    return len;
}

void analyze_entry(char* entry, size_t len)
{
    printf("Got [%s], len=%d\n", entry, len);
}

void analyze_log(FILE* fp)
{
    char entry[80];
    size_t len;

    assert(NULL != fp);

    HANDLER_BIND_BEGIN({MALFORMED_LOG_ENTRY_ERROR, skip_log_entry},
                       {EMPTY_LINE_ERROR, skip_log_entry}) {
        while ((len = parse_log_file(fp, entry, 80)) > 0) {
            analyze_entry(entry, len);
        }
    } HANDLER_BIND_END;
}

int main(int argc, char** argv)
{
    FILE* fp;

    ccond_init();

    if (argc > 1) {
        fp = fopen(argv[1], "r");
        assert(NULL != fp);
    } else {
        fp = stdin;
    }

    analyze_log(fp);

    return 0;
}

