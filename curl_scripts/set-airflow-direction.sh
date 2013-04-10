#!/bin/sh
#  Create new accounts, like the ga account
#  But other accounts may be create
. $HOME/.exodmrc

if [ $# != 2 ]
then
    echo "Usage: $0 device-id direction(0-7)"
    exit 255
fi
# the password (actually erlang node cookie) must be 100% hidden
# so this is only for testing!!!!!
curl -u $USER_AUTH -k -X POST  $URL -d @- << EOF
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
