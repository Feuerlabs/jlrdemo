-module(jlrdemo_rpc).

-export(['set-fan-speed-request'/1,
	 'set-left-temperature-request'/1,
	 'set-right-temperature-request'/1]).

-export([handle_rpc/4]).

-define(COMPLETE   , <<"1">>).
-define(VALUE_ERROR, <<"7">>).

-define(URL, <<"http://localhost:8086">>).

-include_lib("lhttpc/include/lhttpc.hrl").
-include_lib("lager/include/log.hrl").

'set-fan-speed-request'(Args) ->
    io:format("jlrdemo_rpc:set-fan-speed-request(): Args(~p) ~n", [ Args ]),
    'set-fan-speed-request_'(Args),
    send_http_request(<<"jlrdemo">>, <<"set-fan-speed-request">>, Args),
    ok.

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
     ok.

'set-left-temperature-request_'(Args) ->
    case lists:keyfind('temperature', 1, Args) of
	false ->
	    ok(?VALUE_ERROR);
	Found when is_tuple(Found) ->
	    Value = element(2, Found),
	    ok = jlrdemo_can:'set-left-temperature-request'(Value),
	    ok(?COMPLETE)
    end.


'set-right-temperature-request'(Args) ->
    'set-right-temperature-request_'(Args),
    send_http_request(<<"jlrdemo">>, <<"set-right-temperature-request">>, Args),
     ok.

'set-right-temperature-request_'(Args) ->
    case lists:keyfind('temperature', 1, Args) of
	false ->
	    ok(?VALUE_ERROR);
	Found when is_tuple(Found) ->
	    Value = element(2, Found),
	    ok = jlrdemo_can:'set-right-temperature-request'(Value),
	    ok(?COMPLETE)
    end.



%% JSON-RPC entry point
handle_rpc(<<"jlrdemo">>, Method, Args, _Meta) ->
    case Method of
	<<"set-fan-speed-request">> ->
	    Res = 'set-fan-speed-request_'(Args),
	    exoport:rpc(
	      exodm_rpc, rpc,
	      [<<"jlrdemo">>, <<"set-fan-speed-request">>, Args]),
	    io:format("jlrdemo_rpc:handle_rpc(): Res(~p)", [ Res ]),
	    Res;

	<<"set-left-temperature-request">> ->
	    Res = 'set-left-temperature-request_'(Args),
	    exoport:rpc(
	      exodm_rpc, rpc,
	      [<<"jlrdemo">>, <<"set-left-temperature-request">>, Args]),
	    Res;

	<<"set-right-temperature-request">> ->
	    Res = 'set-right-temperature-request_'(Args),
	    exoport:rpc(
	      exodm_rpc, rpc,
	      [<<"jlrdemo">>, <<"set-right-temperature-request">>, Args]),
	    Res

    end.


ok(Status) ->
    {ok, [{'rpc-status', Status},
	  {'final', true}]}.


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
