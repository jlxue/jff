#include <iostream>
#include <string>
#include <boost/asio.hpp>

#include "client/linux/handler/exception_handler.h"
#include "upnp.hpp"


static bool dumpCallback(const char* dump_path,
                         const char* minidump_id,
                         void* context,
                         bool succeeded)
{
    std::cerr << "Minidump file created: " << dump_path << "/" << minidump_id << ".dmp\n";
    return succeeded;
}


int main(int argc, char* argv[])
{
    google_breakpad::ExceptionHandler eh(".", NULL, dumpCallback, NULL, true);

    boost::asio::io_service io_service;

    bitsway::UPnPIGDClient upnp_igd_client(io_service);

    io_service.run();

    return 0;
}

