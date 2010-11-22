#ifndef BITSWAY_UPNP_HPP_
#define BITSWAY_UPNP_HPP_

#include <boost/asio.hpp>

namespace bitsway {

class UPnPIGDClient {
public:
    UPnPIGDClient(boost::asio::io_service& io_service);
    ~UPnPIGDClient();
    void handleSendTo(const boost::system::error_code& ec,
            std::size_t bytes_transferred);
    void handleReceiveFrom(const boost::system::error_code& ec,
            std::size_t bytes_transferred);

private:
    boost::asio::ip::address        multicast_address_;
    boost::asio::ip::udp::endpoint  multicast_endpoint_;
    boost::asio::ip::udp::endpoint  server_endpoint_;
    boost::asio::ip::udp::socket    socket_;
    boost::asio::deadline_timer     timer_;
    enum { max_length = 1024 };
    char                            data_[max_length];
};

} /* end namespace bitsway */

#endif /* BITSWAY_UPNP_HPP_ */

