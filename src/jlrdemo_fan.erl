%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(jlrdemo_fan).

-export(['set-fan-speed-request'/1]).

-include_lib("lager/include/log.hrl").

'set-fan-speed-request'(Speed) ->
    ?debug("set-fan-speed-request(~p)~n", [Speed]),
    ok.
