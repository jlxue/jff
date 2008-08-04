#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>

#include "user.h"

const char*
get_user_name(uid_t uid)
{
    struct passwd* pwd;

    pwd = getpwuid(uid);
    if (NULL == pwd)
        return NULL;
    return pwd->pw_name;
}

