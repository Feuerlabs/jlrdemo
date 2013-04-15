#!/bin/sh
#  Create new accounts, like the ga account
#  But other accounts may be create
# the password (actually erlang node cookie) must be 100% hidden
# so this is only for testing!!!!!
curl -k -X POST http://localhost:8800 -d @- << EOF
{
    "jsonrpc": "2.0",
    "method": "jlrdemo:set-airflow-direction-request",
    "id": "1",
    "params":
    {
      "device-id": "$1",
      "direction": $2

    }
}
EOF
