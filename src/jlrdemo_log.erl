-module(jlrdemo_log).
-behavior(gen_server).

-export([log_can/3,
	 read_config/0,
	 config_update/0]).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include_lib("lager/include/log.hrl").
-include_lib("kvdb/include/kvdb_conf.hrl").

-record(st, {cfg = orddict:new()}).
-record(i, {buf = new_buf(), int = 500, max = 50, tref,
	    last = jlrdemo_lib:timestamp()}).

log_can(FrameID, DataLen, Data) ->
    TS = jlrdemo_lib:make_decimal(jlrdemo_lib:timestamp()),
    io:format("log_can(): FrameID ~p Data ~p DataLen ~p ~n", [ FrameID, Data, DataLen ]),
    %% FIXME: ms_timestamp() should be millisec_timestamp() (ms since epoch).
    gen_server:cast(?MODULE, {log_can, TS, FrameID, DataLen, Data}).

read_config() ->
    gen_server:call(?MODULE, read_config).

config_update() ->
    gen_server:cast(?MODULE, config_update).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #st{}}.

handle_cast({log_can, TS, FrameID, DataLen, Data}, #st{cfg = Cfg} = S) ->
    case orddict:find(list_to_binary(integer_to_list(FrameID)), Cfg) of
	{ok, #i{buf = B, max = Max} = I} ->
	    io:format("jlrdemo_log(): Will log Frame ~p  Max ~p DataLen ~p Data ~p~n",
		      [FrameID, Max, DataLen, Data]),
	    B1 = log_item({TS,DataLen,Data}, B, Max),
	    I1 = I#i{buf = B1},
	    Cfg1 = orddict:store(list_to_binary(integer_to_list(FrameID)), I1, Cfg),
	    {noreply, S#st{cfg = Cfg1}};
	error ->
	    {noreply, S}
    end;
handle_cast(config_update, S) ->
    S1 = read_config(S),
    ?debug("read_config() -> ~p~n", [S1#st.cfg]),
    {noreply, S1};
handle_cast(_, S) ->
    {noreply, S}.

handle_call(read_config, _From, S) ->
    {reply, ok, read_config(S)};
handle_call(_Msg, _From, S) ->
    {reply, error, S}.

handle_info({timeout, _, {send, FrameID}}, #st{cfg = Cfg} = S) ->
    case orddict:find(FrameID, Cfg) of
	{ok, #i{buf = B, int = Int} = I} ->
	    B1 = send_buf(B, FrameID),
	    TRef = start_timer(Int, FrameID),
	    Cfg1 = orddict:store(FrameID, I#i{buf = B1, tref = TRef}, Cfg),
	    {noreply, S#st{cfg = Cfg1}};
	error ->
	    io:format("jlrdemo_log(timeout): Nope ~p ~p~n", [FrameID, Cfg ]),
	    {noreply, S}
    end;
handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% helper functions
%% FIXME ms since epoch
%% ms_timestamp() ->

%%     DT = erlang:universaltime(),
%%     calendar:datetime_to_gregorian_seconds(DT).

new_buf() ->
    {0, gb_trees:empty()}.

log_item(Item, {Last, T}, Max) ->
    NewLast = Last+1,
    T1 = case gb_trees:size(T) of
	     Sz when Sz >= Max ->
		 {_, _, TAdj} = gb_trees:take_smallest(T),
		 TAdj;
	     _ ->
		 T
	 end,
    {NewLast, gb_trees:insert(NewLast, Item, T1)}.

send_buf({_,B}, ID) ->
    case gb_trees:to_list(B) of
	[] ->
	    %% don't send?
	    new_buf();
	[_|_] = List ->
	    io:fwrite("List = ~p~n", [List]),
	    LogData = [{struct, [{ts, unix_time()},
				 {'can-frame-id', list_to_integer(binary_to_list(ID)) },
				 {'can-value',
				  Data
				  %%list_to_binary(integer_to_list(Data))}]}
				  %% jlrdemo_lib:can_data_value(Len, binary:encode_unsigned(Data))
							    }]}
		       || {_, {TS, Len, Data}} <- List],
	    io:fwrite("LogData = ~p~n", [LogData]),
	    exoport:rpc(
	      exodm_rpc, rpc, [<<"demo">>, <<"process-logdata">>,
			       [{'logdata', {array, LogData}}]]),
	    new_buf()
    end.

read_config(#st{cfg = Cfg} = S) ->
    case read_tree() of
	[] ->
	    S#st{cfg = orddict:new()};
	#conf_tree{tree = T} ->
	    Cfg1 =
		lists:foldl(
		  fun({ID, Attrs}, Acc) ->
			  Sz = to_int(
				 jlrdemo_lib:find_val(
				   <<"buffer_size">>, Attrs, infinity)),
			  Int = to_int(
				  jlrdemo_lib:find_val(
				    <<"sample_interval">>, Attrs, 1000)),
			  io:format("jlrdemo_log(): Starting timer for Frame ~p Int ~p~n",
				    [ID, Int]),
			  TRef = start_timer(Int, ID),
			  I = #i{max = Sz, int = Int, tref = TRef},
			  orddict:store(ID, I, Acc)
		  end, Cfg, T),
	    S#st{cfg = Cfg1}
    end.

start_timer(Int, ID) ->
    erlang:start_timer(Int, self(), {send, ID}).

read_tree() ->
    case kvdb_conf:read_tree(<<"jlrdemo*config*logging">>) of
	[] -> [];
	T  -> right_tree(T)
    end.

right_tree(#conf_tree{root = Root} = Tree) ->
    case kvdb_conf:unescape_key(Root) of
	<<"jlrdemo*config*logging">> ->
	    Tree;
	<<"jlrdemo*config*logging", _/binary>> ->
	    right_tree(kvdb_conf:shift_root(up, Tree))
    end.

to_int(infinity) ->
    infinity;
to_int(I) when is_integer(I) ->
    I;
to_int(B) when is_binary(B) ->
    list_to_integer(binary_to_list(B)).

unix_time() ->
     calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now()))-719528*24*3600.
