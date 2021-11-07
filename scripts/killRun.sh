#!/usr/bin/env bash
APP=${1:-}

if [ -z "$APP" ]; then
   echo "APP is required!"
   exit 1
fi

pid="$(pgrep "${APP}")"

if [ -n "$pid" ]; then
   echo "Previous app running with pid $pid. So killing it..."
   kill -9 "$pid"
fi

stack exec "${APP}" &
sleep 0.6
pid="$(pgrep "${APP}")"
echo -e "server running with PID={$pid} (if that's empty, then the server is not running)"