%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(jlrdemo_fan).

-export(['set-fan-speed-request'/1]).

'set-fan-speed-request'(Speed) ->
    io:format("set-fan-speed-request(~p)~n", [Speed]),
    jlrdemo_can:set_fan_speed(Speed),
    ok.
