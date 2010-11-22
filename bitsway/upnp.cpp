#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include "upnp.hpp"

namespace bitsway {

using namespace boost::asio;

static const char*      UPNP_MULTI_CAST_ADDR = "239.255.255.250";
static unsigned short   UPNP_MULTI_CAST_PORT = 1900;
static const char*      UPNP_IGD_SEARCH_REQUEST =
    ""
    ""
    "";

UPnPIGDClient::UPnPIGDClient(io_service& io_svc)
    : multicast_address_(ip::address::from_string(UPNP_MULTI_CAST_ADDR)),
      multicast_endpoint_(multicast_address_, UPNP_MULTI_CAST_PORT),
      socket_(io_svc, multicast_endpoint_.protocol()),
      timer_(io_svc)
{
    socket_.set_option(ip::udp::socket::reuse_address(true));
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
    if (! ec) {
    }
}

void UPnPIGDClient::handleReceiveFrom(const boost::system::error_code& ec,
        std::size_t bytes_transferred)
{
    if (! ec) {
    }
}

UPnPIGDClient::~UPnPIGDClient()
{
}

} /* end namespace bitsway */

