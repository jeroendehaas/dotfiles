#!/bin/sh
killall -q polybar
while pgrep -u $UID -x polybar > /dev/null; do sleep 1; done
polybar top 2>&1 > /dev/null &
