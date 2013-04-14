#!/bin/sh

. $HOME/.exodmrc

if [ $# != 1 ]
then
    echo "Usage: $0 device-id"
    exit 255
fi
# the password (actually erlang node cookie) must be 100% hidden
# so this is only for testing!!!!!
curl -u $USER_AUTH -k -X POST  $URL -d @- << EOF
{
    "jsonrpc": "2.0",
    "method": "jlrdemo:get-left-temperature-request",
    "id": "1",
    "params":
    {
      "device-id": "$1"
    }
}
EOF
