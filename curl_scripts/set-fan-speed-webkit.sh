#!/bin/sh
#  Create new accounts, like the ga account
#  But other accounts may be create
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
