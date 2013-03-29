-module(jlrdemo_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 start_phase/3,
	 stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    jlrdemo_sup:start_link().

start_phase(ping, _, _) ->
    exoport:ping(),
    ok;
start_phase(can, _, _) ->
    jlrdemo_can:start_can(),
    ok;
start_phase(waypoints, _, _) ->
    jlrdemo_waypoints:start_waypoints("/dev/ttySAC1"),
    ok.

stop(_State) ->
    ok.
