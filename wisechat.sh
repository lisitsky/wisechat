#!/bin/sh

##
## usage web_server.sh {start|stop|debug}
##

##   PA   = path to the web server
##   PORT  = port to run as

PA="ebin deps/*/ebin"  # $HOME/tutorials/dev/web_server
PORT=8008
ERL=erl
HOSTNAME=`hostname`
export HEART_COMMAND="./web_server.sh start"

case $1 in

  start)
    echo  "Starting WiseChat"
    $ERL -boot start_sasl -name wisechat001@$HOSTNAME -pa $PA \
         -heart -detached -s wisechat start_work $PORT
    ;;

  debug)
    echo  "Starting WiseChat in console mode"
    $ERL -name  wisechat001$@HOSTNAME -pa $PA \
         -noshell -s wisechat start_work $PORT
    ;;

  stop)
    echo "Stopping WiseChat"
    $ERL -noshell -name wisechat001_stopper@$HOSTNAME -pa $PA \
           -s wisechat stop_work wisechat001@$HOSTNAME
    ;;

  *)
    echo "Usage: $0 {start|stop|debug}"
    exit 1
esac

exit 0
