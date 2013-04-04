#!/bin/sh
#  Create new accounts, like the ga account
#  But other accounts may be create
. $HOME/.exodmrc

if [ $# != 2 ]
then
    echo "Usage: $0 device-id speed(0-10)"
    exit 255
fi
# the password (actually erlang node cookie) must be 100% hidden
# so this is only for testing!!!!!
curl -k -X POST http://localhost:8800 -d @- << EOF
{
    "jsonrpc": "2.0",
    "method": "jlrdemo:set-fan-speed-request",
    "id": "1",
    "params":
    {
      "device-id": "$1",
      "fan-speed": $2

    }
}
EOF
