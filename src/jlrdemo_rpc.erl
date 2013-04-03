-module(jlrdemo_rpc).

-export([handle_rpc/4]).

-define(COMPLETE   , <<"1">>).
-define(VALUE_ERROR, <<"7">>).

handle_rpc(<<"jlrdemo">>, Method, Args, Meta) ->
    case Method of
	<<"set-fan-speed-request">> ->
	    case lists:keyfind('fan-speed', 1, Args) of
		{_, Value, _} ->
		    ok = jlrdemo_fan:'set-fan-speed-request'(Value),
		    ok(?COMPLETE);
		false ->
		    ok(?VALUE_ERROR)
	    end
    end.


ok(Status) ->
    {ok, [{'rpc-status', Status},
	  {'final', true}]}.
