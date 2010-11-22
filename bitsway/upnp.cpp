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
    //socket_.set_option(socket_base::broadcast(true));
    //socket_.set_option(ip::udp::socket::reuse_address(true));
    socket_.set_option(ip::multicast::hops(4));
    //unsigned char ttl = 4;
    //setsockopt(socket_.native(), IPPROTO_IP, IP_MULTICAST_TTL, &ttl, sizeof(ttl));
    //socket_.set_option(ip::multicast::join_group(multicast_address_));
    //socket_.bind(ip::udp::endpoint(ip::address::from_string("10.0.2.15"), 36997));
    socket_.bind(ip::udp::endpoint());

    socket_.async_send_to(buffer(UPNP_IGD_SEARCH_REQUEST),
            multicast_endpoint_,
            boost::bind(&UPnPIGDClient::handleSendTo,
                this,
                placeholders::error,
                placeholders::bytes_transferred));

    socket_.async_receive_from(buffer(data_, max_length),
            server_endpoint_,
            boost::bind(&UPnPIGDClient::handleReceiveFrom,
                this,
                placeholders::error,
                placeholders::bytes_transferred));

}

void UPnPIGDClient::handleSendTo(const boost::system::error_code& ec,
                                 std::size_t bytes_transferred)
{
    std::cerr << "sendto: ec=" << ec << "\n";
    if (!ec) {
        /*
        timer_.expires_from_now(boost::posix_time::seconds(3));
        timer_.async_wait(
                boost::bind(&UPnPIGDClient::handleTimeout,
                    this,
                    placeholders::error));
        */
    }
}

void UPnPIGDClient::handleReceiveFrom(const boost::system::error_code& ec,
        std::size_t bytes_transferred)
{
    std::cerr << "error_code " << ec << "\n";

    if (! ec) {
        std::cerr << "got " << bytes_transferred << "\n";
    }
}

void UPnPIGDClient::handleTimeout(const boost::system::error_code& ec)
{
    if (!ec) {
        socket_.async_send_to(buffer(UPNP_IGD_SEARCH_REQUEST),
                multicast_endpoint_,
                boost::bind(&UPnPIGDClient::handleSendTo,
                    this,
                    placeholders::error,
                    placeholders::bytes_transferred));
    }
}

UPnPIGDClient::~UPnPIGDClient()
{
}

} /* end namespace bitsway */

