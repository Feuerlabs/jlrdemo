%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
-module(jlrdemo_config).

-export(['update-config-entry-request'/1]).

-include_lib("lager/include/log.hrl").

root() ->
    <<"jlrdemo">>.

status(complete) -> 1;
status(_) -> 5. % device-error


'update-config-entry-request'(Elems) ->
    ?debug("update-config-entry-request(~p)~n", [Elems]),
    Status = process_elems(Elems),
    TransactionID = 1,
    {notify, "demo:update-config-entry-callback",
     [{'transaction-id', TransactionID},
      {'status-code', status(Status)},
      {'final', true}]}.

%% Example:
%% [{'device-id', <<"2">>},
%%  {'config-entries',
%%   [[{'name', <<"alarm">>},
%%     {'val', {array, [{struct, [{"can_frame_id", "1001"},
%%                                {"trigger_threshold", "10"},
%%                                {"reset_threshold", "7"}]}]}}
%%     ]}
%%   ]}
%% ].

process_elems(Elems) ->
    case lists:keyfind('config-entries', 1, Elems) of
        {_, CfgElems} ->
            ?debug("CfgElems = ~p~n", [CfgElems]),
            Data =
                lists:foldl(
                  fun([{'name', Name},
                       {'val', {array, [{struct,_}|_] = Structs}}], Acc) ->
                       %% {'val', {array,[{struct, Values}]}}], Acc) ->
                          ?debug("Structs = ~p~n", [Structs]),
                          Objs = lists:flatmap(
                                   fun({struct, Values}) ->
                                           conf_objs(Name, Values)
                                   end, Structs),
                          ?debug("conf_objs() -> ~p~n", [Objs]),
                          Objs ++ Acc;
                     (_Other, Acc) ->
                          ?debug("_Other = ~p~n", [_Other]),
                          Acc
                  end, [], CfgElems),
            if Data =/= [] ->
                    kvdb_conf:in_transaction(
                      fun(_) ->
                              [kvdb_conf:write(Obj) || Obj <- Data]
                      end),
                    jlrdemo_log:config_update(),
                    jlrdemo_alarms:config_update();
               true ->
                    ok
            end,
            complete;
        false ->
            ?debug("No config-entries (~p)~n", [Elems]),
            error
    end.

conf_objs(Name, Values) ->
    Root = root(),
    case lists:keytake("can_frame_id", 1, Values) of
        {value, {_, ID}, Rest} ->
            ?debug("ID = ~p; Rest = ~p~n", [ID, Rest]),
            Node = kvdb_conf:raw_join_key([Root,<<"config">>,
                                           to_bin(Name), to_bin(ID)]),
            [{kvdb_conf:raw_join_key(Node, to_bin(Key)), [], to_bin(Value)}
             || {Key, Value} <- Rest];
        false ->
            []
    end.

to_bin(L) when is_list(L) ->
    list_to_binary(L);
to_bin(B) when is_binary(B) ->
    B.
