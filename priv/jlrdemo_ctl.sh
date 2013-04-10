#!/bin/sh -e
### BEGIN INIT INFO
# Provides:          jlrdemo
# Required-Start:    
# Required-Stop:
# Default-Start:     1 2 3 4 5
# Default-Stop:
# Short-Description: JLR demo launch script
### END INIT INFO

case "$1" in
  start) 
	# Load the pcan driver with the correct 125Kbit rate.
	if [  "$(lsmod | grep pcan | cut -d' ' -f 1)" != "pcan" ]
	then
	    insmod /usr/lib/erlang/lib/jlrdemo*/priv/pcan.ko  bitrate=0x031C;
        fi
	ifconfig can0 up
	# Start the demo.
	sh /usr/lib/erlang/lib/jlrdemo*/priv/start_demo.sh > /tmp/jlrdemo.log 2>&1 &
	echo $! > /tmp/jlrdemo.pid ;;
  stop) kill $(cat /tmp/jlrdemo.pid); exit 0 ;;
  *) echo "Usage: $0 {start|stop}" >&2; exit 1 ;;
esac
