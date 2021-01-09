#!/bin/bash
kill -9 `pgrep ApiTemplate-exe`
stack exec ApiTemplate-exe &
sleep 0.6
echo -e "server running with PID={`pgrep ApiTemplate-exe`} (if that's empty, then the server is not running)"