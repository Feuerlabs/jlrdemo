#!/bin/sh

if [ $# != 1 ]
then
    echo "Usage: $0 device-id"
    exit 255
fi

curl -k -X POST http://localhost:8800 -d @- << EOF
{
    "jsonrpc": "2.0",
    "method": "jlrdemo:get-right-temperature-request",
    "id": "1",
    "params":
    {
      "device-id": "$1"
    }
}
EOF
