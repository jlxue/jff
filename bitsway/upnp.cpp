#include <iostream>
#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>

#include "upnp.hpp"

namespace bitsway {

using namespace boost::asio;

static const char*      UPNP_MULTI_CAST_ADDR = "239.255.255.250";
static unsigned short   UPNP_MULTI_CAST_PORT = 1900;
static const char*      UPNP_IGD_SEARCH_REQUEST =
    "M-SEARCH * HTTP/1.1\r\n"
    "Host: 239.255.255.250:1900\r\n"
    "Man: \"ssdp:discover\"\r\n"
    "ST: upnp:rootdevice\r\n"
    "MX: 5\r\n"
    "\r\n";

UPnPIGDClient::UPnPIGDClient(io_service& io_svc)
    : multicast_address_(ip::address::from_string(UPNP_MULTI_CAST_ADDR)),
      multicast_endpoint_(multicast_address_, UPNP_MULTI_CAST_PORT),
      socket_(io_svc, multicast_endpoint_.protocol()),
      timer_(io_svc)
{
    socket_.set_option(ip::multicast::hops(4));       // default is 1

    startSend();
    startReceive();
    startTimer();
}

void UPnPIGDClient::handleSendTo(const boost::system::error_code& ec,
                                 std::size_t bytes_transferred)
{
    std::cerr << "UPnPIGDClient::handleSendTo(ec=" << ec.message()
        << ", bytes=" << bytes_transferred << ")\n";

    if (! ec) {
    }
}

void UPnPIGDClient::handleReceiveFrom(const boost::system::error_code& ec,
        std::size_t bytes_transferred)
{
    std::cerr << "UPnPIGDClient::handleReceiveFrom(ec=" << ec.message()
        << ", bytes=" << bytes_transferred << ")\n";

    if (! ec) {
        timer_.cancel();
        std::cerr.write(data_, bytes_transferred);

        startReceive();
    }
}

void UPnPIGDClient::handleTimeout(const boost::system::error_code& ec)
{
    std::cerr << "UPnPIGDClient::handleTimeout(ec=" << ec.message() << ")\n";

    if (!ec) {
        startSend();
    }
}

void UPnPIGDClient::startSend()
{
    socket_.async_send_to(buffer(UPNP_IGD_SEARCH_REQUEST),
            multicast_endpoint_,
            boost::bind(&UPnPIGDClient::handleSendTo,
                this,
                placeholders::error,
                placeholders::bytes_transferred));
}

void UPnPIGDClient::startReceive()
{
    socket_.async_receive_from(buffer(data_, max_length),
            server_endpoint_,
            boost::bind(&UPnPIGDClient::handleReceiveFrom,
                this,
                placeholders::error,
                placeholders::bytes_transferred));
}

void UPnPIGDClient::startTimer()
{
    timer_.expires_from_now(boost::posix_time::seconds(5));
    timer_.async_wait(
            boost::bind(&UPnPIGDClient::handleTimeout,
                this,
                placeholders::error));
}

UPnPIGDClient::~UPnPIGDClient()
{
}

} /* end namespace bitsway */

