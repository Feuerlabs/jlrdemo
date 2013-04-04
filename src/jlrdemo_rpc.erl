-module(jlrdemo_rpc).

-export(['set-fan-speed-request'/1,
	 'set-temperature-request'/1]).
-export([handle_rpc/4]).

-define(COMPLETE   , <<"1">>).
-define(VALUE_ERROR, <<"7">>).

'set-fan-speed-request'(Args) ->
    'set-fan-speed-request_'(Args),
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
	<<"set-fan-speed-request">> -> 'set-fan-speed-request_'(Args);
	<<"set-temperature-request">> -> 'set-temperature-request_'(Args)
    end.


ok(Status) ->
    {ok, [{'rpc-status', Status},
	  {'final', true}]}.
