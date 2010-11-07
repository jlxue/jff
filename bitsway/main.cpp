#include <iostream>
#include <string>
#include <boost/asio.hpp>

#include "client/linux/handler/exception_handler.h"


static bool dumpCallback(const char* dump_path,
                         const char* minidump_id,
                         void* context,
                         bool succeeded)
{
    fprintf(stderr, "Minidump file created: %s/%s.dmp\n", dump_path, minidump_id);
    return succeeded;
}


int main(int argc, char* argv[])
{
    google_breakpad::ExceptionHandler eh(".", NULL, dumpCallback, NULL, true);

    boost::asio::io_service io_service;
    io_service.run();

    return 0;
}

