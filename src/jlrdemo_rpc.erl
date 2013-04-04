-module(jlrdemo_rpc).

-export(['set-fan-speed-request'/1,
	 'set-temperature-request'/1]).
-export([handle_rpc/4]).

-define(COMPLETE   , <<"1">>).
-define(VALUE_ERROR, <<"7">>).

-define(URL, <<"http://localhost:8086">>).

-include_lib("lhttpc/include/lhttpc.hrl").
-include_lib("lager/include/log.hrl").

'set-fan-speed-request'(Args) ->
    'set-fan-speed-request_'(Args),
    send_http_request(<<"jlrdemo">>, <<"set-fan-speed-request">>, Args),
    ok.

'set-fan-speed-request_'(Args) ->
    case lists:keyfind('fan-speed', 1, Args) of
	Found when is_tuple(Found) ->
	    Value = element(2, Found),
	    ok = jlrdemo_fan:'set-fan-speed-request'(Value),
	    ok(?COMPLETE);
	false ->
	    ok(?VALUE_ERROR)
    end.

'set-temperature-request'(Args) ->
    'set-temperature-request_'(Args),
     ok.

'set-temperature-request_'(Args) ->
    case lists:keyfind('fan-speed', 1, Args) of
	false ->
	    ok(?VALUE_ERROR);
	Found when is_tuple(Found) ->
	    Value = element(2, Found),
	    ok = jlrdemo_fan:'set-fan-speed-request'(Value),
	    ok(?COMPLETE)
    end.



%% JSON-RPC entry point
handle_rpc(<<"jlrdemo">>, Method, Args, Meta) ->
    case Method of
	<<"set-fan-speed-request">> ->
	    Res = 'set-fan-speed-request_'(Args),
	    exoport:rpc(
	      exodm_rpc, rpc,
	      [<<"jlrdemo">>, <<"set-fan-speed-request">>, Args]),
	    Res;
	<<"set-temperature-request">> -> 'set-temperature-request_'(Args)
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
