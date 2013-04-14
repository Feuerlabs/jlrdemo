#!/bin/sh -e
### BEGIN INIT INFO
# Provides:          tizen
# Required-Start:    
# Required-Stop:
# Default-Start:     1 2 3 4 5
# Default-Stop:
# Short-Description: JLR demo tizen launch script
### END INIT INFO
# exec > /tmp/sysd.log 2>&1
export PATH="/bin:/usr/bin:/sbin:/usr/sbin"
export HOME=/root
case "$1" in
  start) 
	cd /home
	/usr/sbin/uxlaunch &
	/usr/bin/npm start intel-tizen-ivi-engine 
	/bin/echo $! > /tmp/npm.pid
	/bin/sleep 20 
	/usr/bin/nohup /usr/bin/webskeleton -f http://127.0.0.1:8088 --width=720 --height=1280 &
	/bin/echo $! > /tmp/webskeleton.pid
	/bin/sleep 2
	/bin/sh /usr/sbin/jlrdemo_ctl.sh start

	;;
  stop) 
	kill $(cat /tmp/npm.pid); 
	kill $(cat /tmp/webskeleton.pid);
	pkill node
	pkill -9 WebProcess
	sh /usr/sbin/jlrdemo_ctl.sh stop
	pkill -9 uxlaunch 
	pkill Xorg
	exit 0 ;;

  *) echo "Usage: $0 {start|stop}" >&2; exit 1 ;;
esac
