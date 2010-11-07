#ifndef BITSWAY_UPNP_HPP_
#define BITSWAY_UPNP_HPP_

#include <boost/asio.hpp>

namespace bitsway {

class UPnPIGDClient {
public:
    UPnPIGDClient(boost::asio::io_service& io_service);
    ~UPnPIGDClient();
}

}

#endif /* BITSWAY_UPNP_HPP_ */

