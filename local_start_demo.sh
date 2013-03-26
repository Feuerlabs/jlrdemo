#!/bin/sh
## ---- BEGIN COPYRIGHT -------------------------------------------------------
##
## Copyright © 2012 Feuerlabs, Inc. All rights reserved.
##
## This Source Code Form is subject to the terms of the Mozilla Public
## License, v. 2.0. If a copy of the MPL was not distributed with this
## file, You can obtain one at http://mozilla.org/MPL/2.0/.
##
## ---- END COPYRIGHT ---------------------------------------------------------

# Boot script for the exodemo system.


##
export KVDB_BACKENDS=ets

erl -boot $PWD/setup/start -config $PWD/setup/sys

# This needs to be setup correctl
# {exodm_host, "vps.ulf.wiger.net"}.
# {'device-id', "*account*2"}.
# {'ckey', 4}.
# {'skey', 3}.
