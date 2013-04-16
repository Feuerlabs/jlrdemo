-module(jlrdemo_rpc).


-export(['set-airflow-direction-request'/1,
	 'get-airflow-direction-request'/1,
	 'set-fan-speed-request'/1,
	 'get-fan-speed-request'/1,
	 'set-left-temperature-request'/1,
	 'get-left-temperature-request'/1,
	 'set-right-temperature-request'/1,
	 'get-right-temperature-request'/1]).

-export([handle_rpc/4]).

-define(COMPLETE   , <<"1">>).
-define(VALUE_ERROR, <<"7">>).

-define(URL, <<"http://localhost:8086">>).

-include_lib("lhttpc/include/lhttpc.hrl").
-include_lib("lager/include/log.hrl").

to_string(Int) ->
    lists:flatten(io_lib:format("~p", [ Int ])).

'set-airflow-direction-request'(Args) ->
    io:format("jlrdemo_rpc:set-airflow-direction-request(): Args(~p) ~n", [ Args ]),
    'set-airflow-direction-request_'(Args),
    send_http_request(<<"jlrdemo">>, <<"set-airflow-direction-request">>, Args),
    ok(?COMPLETE).
%%    notify("jlrdemo:get-airflow-direction-notification", ?COMPLETE,
%%	   [ {'direction',  jlrdemo_can:get_airflow_direction() } ]).


'set-airflow-direction-request_'(Args) ->
    io:format("jlrdemo_rpc:set-airflow-direction-request_(): Args(~p) ~n", [ Args ]),
    case lists:keyfind('direction', 1, Args) of
	Found when is_tuple(Found) ->
	    Value = element(2, Found),
	    jlrdemo_can:set_airflow_direction(Value),
	    ok(?COMPLETE);
	false ->
	    ok(?VALUE_ERROR)
    end.

'get-airflow-direction-request'(Args) ->
    Direction = jlrdemo_can:get_airflow_direction(),
    io:format("jlrdemo_rpc:get-airflow-direction-request(): Args(~p) ~p ~n", [ Args, Direction ]),
    notify("jlrdemo:get-airflow-direction-notification", ?COMPLETE,
	   [ {'direction', Direction } ]).


'set-fan-speed-request'(Args) ->
    io:format("jlrdemo_rpc:set-fan-speed-request(): Args(~p) ~n", [ Args ]),
    'set-fan-speed-request_'(Args),
    send_http_request(<<"jlrdemo">>, <<"set-fan-speed-request">>, Args),
    ok(?COMPLETE).
%%    notify( "jlrdemo:get-fan-speed-notification", ?COMPLETE, [ {'fan-speed',  jlrdemo_can:get_fan_speed() } ]).



'get-fan-speed-request'(Args) ->
    io:format("jlrdemo_rpc:get-fan-speed-request(): Args(~p) ~n", [ Args ]),
    Speed = jlrdemo_can:get_fan_speed(),
%%    spawn(fun() -> exoport:rpc(exodm_rpc, rpc, [<<"jlrdemo">>, <<"set-fan-speed-request">>, [{'fan-speed', Speed}]]) end),
    notify( "jlrdemo:get-fan-speed-notification", ?COMPLETE, [ {'fan-speed', Speed } ]).

'set-fan-speed-request_'(Args) ->
    io:format("jlrdemo_rpc:set-fan-speed-request_(): Args(~p) ~n", [ Args ]),
    case lists:keyfind('fan-speed', 1, Args) of
	Found when is_tuple(Found) ->
	    Value = element(2, Found),
	    jlrdemo_can:set_fan_speed(Value),
	    ok(?COMPLETE);
	false ->
	    ok(?VALUE_ERROR)
    end.

'set-left-temperature-request'(Args) ->
    'set-left-temperature-request_'(Args),
    send_http_request(<<"jlrdemo">>, <<"set-left-temperature-request">>, Args),
%%    notify("jlrdemo:get-left-temperature-notification", ?COMPLETE, [ {'temperature', jlrdemo_can:get_left_temperature() } ]).
    ok(?COMPLETE).


'get-left-temperature-request'(Args) ->
    Temp = jlrdemo_can:get_left_temperature(),
    io:format("jlrdemo_rpc:get-left-temperature-request(): Args(~p) ~p~n", [ Args, Temp ]),
%%    spawn(fun() -> exoport:rpc(exodm_rpc, rpc, [<<"jlrdemo">>, <<"set-left-temperature-request">>,
%%						[{temperature, Temp}]]) end),
    notify("jlrdemo:get-left-temperature-notification", ?COMPLETE, [ {'temperature', Temp } ]).

'set-left-temperature-request_'(Args) ->
    case lists:keyfind('temperature', 1, Args) of
	false ->
	    ok(?VALUE_ERROR);
	Found when is_tuple(Found) ->
	    Value = element(2, Found),
	    ok = jlrdemo_can:set_left_temperature(Value),
	    ok(?COMPLETE)
    end.


'set-right-temperature-request'(Args) ->
    'set-right-temperature-request_'(Args),
    send_http_request(<<"jlrdemo">>, <<"set-right-temperature-request">>, Args),
    ok(?COMPLETE).
%%    notify("jlrdemo:get-right-temperature-notification", ?COMPLETE, [ {'temperature', jlrdemo_can:get_right_temperature() } ]).

'set-right-temperature-request_'(Args) ->
    case lists:keyfind('temperature', 1, Args) of
	false ->
	    ok(?VALUE_ERROR);
	Found when is_tuple(Found) ->
	    Value = element(2, Found),
	    ok = jlrdemo_can:set_right_temperature(Value),
	    ok(?COMPLETE)
    end.


'get-right-temperature-request'(Args) ->
    Temp = jlrdemo_can:get_right_temperature(),
    io:format("jlrdemo_rpc:get-right-temperature-request(): Args(~p) ~p~n", [ Args, Temp ]),
    notify( "jlrdemo:get-right-temperature-notification", ?COMPLETE, [ {'temperature', Temp } ]).



%% JSON-RPC entry point
%% CAlled by local exo http server
handle_rpc(<<"jlrdemo">>, Method, Args, _Meta) ->
    case Method of
	<<"set-airflow-direction-request">> ->
	    Res = 'set-airflow-direction-request_'(Args),
	    exoport:rpc(
	      exodm_rpc, rpc,
	      [<<"jlrdemo">>, <<"set-airflow-direction-request">>, Args]),
	    io:format("jlrdemo_rpc:handle_rpc(set-airflow-direction-request): Res(~p)", [ Res ]),
	    Res;

	<<"get-airflow-direction-request">> ->
	    Res = 'get-airflow-direction-request'(Args),
	    io:format("jlrdemo_rpc:handle_rpc(get-airflow-direction-request): Res(~p)", [ Res ]),
	    Res;

	<<"set-fan-speed-request">> ->
	    Res = 'set-fan-speed-request_'(Args),
	    exoport:rpc(
	      exodm_rpc, rpc,
	      [<<"jlrdemo">>, <<"set-fan-speed-request">>, Args]),
	    io:format("jlrdemo_rpc:handle_rpc(set-fan-speed-request): Res(~p)~n", [ Res ]),
	    Res;

	<<"get-fan-speed-request">> ->
	    io:format("jlrdemo_rpc:handle_rpc(get-fan-speed-request)~n", [ ]),
	    ok(?COMPLETE, [{ speed, jlrdemo_can:get_fan_speed() }]);

	<<"set-left-temperature-request">> ->
	    io:format("jlrdemo_rpc:handle_rpc(set-left-temperature-request)~n", []),
	    Res = 'set-left-temperature-request_'(Args),
	    exoport:rpc(
	      exodm_rpc, rpc,
	      [<<"jlrdemo">>, <<"set-left-temperature-request">>, Args]),
	    Res;

	<<"get-left-temperature-request">> ->
	    Res = 'get-left-temperature-request'(Args),
	    io:format("jlrdemo_rpc:handle_rpc(get-left-temperature-request): Res(~p)", [ Res ]),
	    Res;

	<<"set-right-temperature-request">> ->
	    io:format("jlrdemo_rpc:handle_rpc(set-right-temperature-request)~n", []),
	    Res = 'set-right-temperature-request_'(Args),
	    exoport:rpc(
	      exodm_rpc, rpc,
	      [<<"jlrdemo">>, <<"set-right-temperature-request">>, Args]),
	    Res;

	<<"get-right-temperature-request">> ->
	    Res = 'get-right-temperature-request'(Args),
	    io:format("jlrdemo_rpc:handle_rpc(get-right-temperature-request): Res(~p)", [ Res ]),
	    Res

    end.


ok(Status) ->
    {ok, [{'rpc-status', Status},
	  {'final', true}]}.

ok(Status, Extra) ->
    X = { ok, [{'rpc-status', Status},
	       {'final', true}] ++ Extra },
    io:format("ok/2:  ~p~n", [X]),
    X.

notify(RPC, Status, Extra) ->
     { notify,
       RPC,
       [{'rpc-status', Status},
	{'final', true}] ++ Extra }.

notify(Callback, Status) ->
    notify(Callback, Status, []).

send_http_request(Mod, Method, Args) ->
    YangMod = list_to_existing_atom("yang_spec_" ++ binary_to_list(Mod)),
    {rpc,_,_,Spec} = YangMod:rpc(Method),
    {input,_,_,InputSpec} = lists:keyfind(input, 1, Spec),
    JSON = exoport_exo_http:data_to_json(InputSpec, [], Args),
    Req = {struct, [{"jsonrpc", "2.0"},
		    {"id", 1},
		    {"method", binary_to_list(
				 <<Mod/binary, ":", Method/binary>>)},
		    {"params", {struct, JSON}}]},
    {Body, Hdrs} = encode_request(Req),
    post_request(?URL, Hdrs, Body).


encode_request(JSON) ->
    Body = exo_json:encode(JSON),
    Hdrs = [
            {"Content-Length", integer_to_list(iolist_size(Body))},
            {"Content-Type", "application/json"},
            {"Host", "localhost"}  % will probably be replaced before sending
           ],
    {Body, Hdrs}.


post_request(URL, Hdrs, Body) ->
    try
        Host = get_host_part(URL),
        Hdrs1 = lists:keystore("Host", 1, Hdrs, {"Host", Host}),
        Res =
            lhttpc:request(
              binary_to_list(URL), "POST", Hdrs1, Body, 1000),
        ?debug("post_request(~p, ...) ->~n  ~p~n", [URL, Res]),
        Res
    catch
        Type:Reason ->
            ?error("post_request(~p, ~p, ~p) CRASHED~n"
                   "~p:~p; ~p~n",
                   [URL, Hdrs, Body, Type, Reason, erlang:get_stacktrace()]),
            error
    end.

get_host_part(URL0) ->
    URL = if is_list(URL0) -> URL0;
             is_binary(URL0) -> binary_to_list(URL0)
          end,
    #lhttpc_url{host = Host} = lhttpc_lib:parse_url(URL),
    Host.
