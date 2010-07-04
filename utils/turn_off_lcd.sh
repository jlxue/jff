#!/bin/sh

while :; do
    xset q | grep -q "Monitor is On" && xset dpms force off
    sleep 60
done
