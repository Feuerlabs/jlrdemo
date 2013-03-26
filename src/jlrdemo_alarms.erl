-module(jlrdemo_alarms).
-behavior(gen_server).

-export([
	 %% set/2,
	 %% clear/1,
	 check_alarm/3]).
-export([read_config/0,
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

-record(st, {alarms = orddict:new()}).
-record(alarm, {status = clear, set, reset, ts, value}).

-define(TWO_HOURS, 2*60*60).  % in seconds

%% set(FrameID, Value) ->
%%     gen_server:cast(?MODULE, {set, timestamp(), FrameID, Value}).

check_alarm(FrameID, DataLen, Data) ->
    gen_server:cast(?MODULE, {check_alarm, timestamp(), FrameID, Data, DataLen}).

config_update() ->
    gen_server:cast(?MODULE, config_update).

read_config() ->
    gen_server:call(?MODULE, read_config).

%% clear(FrameID) ->
%%     gen_server:cast(?MODULE, {clear, FrameID}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #st{}}.


handle_cast({check_alarm, TS, FrameID, Data, _DataLen},
	    #st{alarms = As} = S) ->
    Key = list_to_binary(integer_to_list(FrameID)),
    S1 = case orddict:find(Key, As) of
	     {ok, #alarm{set = SThr, reset = CThr} = Alarm} ->
		 case Data of
		     I when is_integer(I) ->
			 if I > SThr -> set_alarm(TS, Key, I, Alarm, S);
			    I < CThr -> clear_alarm(TS, Key, I, Alarm, S);
			    true -> S
			 end;
		     _ ->
			 S
		 end;
	     _ ->
		 S
	 end,
    {noreply, S1};

handle_cast(config_update, S) ->
    S1 = read_alarms(S),
    ?debug("read_alarms() -> ~p~n", [S1#st.alarms]),
    {noreply, S1};

handle_cast(_, S) ->
    {noreply, S}.

handle_call(read_config, _From, S) ->
    {reply, ok, read_alarms(S)};
handle_call(_Msg, _From, S) ->
    {reply, error, S}.

handle_info(send_alarms, #st{alarms = As} = S) ->
    flush_send_msgs(),
    NewAlarms = rpc(As),
    {noreply, S#st{alarms = NewAlarms}}.

terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% helper functions

set_alarm(TS, FrameID, Value, Alarm, #st{alarms = As} = S) ->
    case Alarm of
	#alarm{status = sent} ->
	    S;
	_ ->
	    %% send alarm...
	    self() ! send_alarms,
	    NewAs = orddict:store(
		      FrameID, Alarm#alarm{status = set,
					   ts = TS,
					   value = Value}, As),
	    ?debug("OldAs = ~p; NewAs = ~p~n", [As, NewAs]),
	    S#st{alarms = NewAs}
    end.

clear_alarm(TS, FrameID, Value, Alarm, #st{alarms = As} = S) ->
    case Alarm of
	#alarm{status = clear} ->
	    S;
	_ ->
	    NewAs = orddict:store(FrameID, Alarm#alarm{status = clear,
						       ts = TS,
						       value = Value}, As),
	    S#st{alarms = NewAs}
    end.

timestamp() ->
    %% DT = erlang:universaltime(),
    jlrdemo_lib:make_decimal(jlrdemo_lib:timestamp()).
    %% calendar:datetime_to_gregorian_seconds(DT) - ?TWO_HOURS.  % GMT

flush_send_msgs() ->
    receive
	send_alarms ->
	    flush_send_msgs()
    after 0 ->
	    ok
    end.

rpc(Alarms) ->
    {ToSend, NewAlarms} =
	orddict:fold(
	  fun(FrameID, #alarm{status = set,
			      ts = TS,
			      value = Value} = A, {Send, Update}) ->
		  {[{struct, [{'ts', TS},
			      {'can-frame-id', FrameID},
			      {'can-value', Value}]} | Send],
		   orddict:store(FrameID, A#alarm{status = sent}, Update)};
	     (_Frame, _Alarm, Acc) ->
		  Acc
	  end, {[], Alarms}, Alarms),
    %% {ToSend, NewAlarms} =
    %% 	lists:mapfoldr(
    %% 	  fun({FrameID, #alarm{status = set,
    %% 			       ts = TS,
    %% 			       value = Value} = A}, Acc) ->
    %% 		  {[{struct, [{'ts', TS},
    %% 			      {'can-frame-id', FrameID},
    %% 			      {'can-value', Value}]} | Acc],
    %% 		   {FrameID, orddict:store(
    %% 			       FrameID, A#alarm{status = sent}, Acc)}};
    %% 	     (Entry, Acc) ->
    %% 		  {Entry, Acc}
    %% 	  end, orddict:new(), orddict:to_list(Alarms)),
    exoport:rpc(exodm_rpc, rpc, [<<"demo">>, <<"process-alarms">>,
				 [{'alarms', {array, ToSend}}]]),
    NewAlarms.

read_alarms(#st{alarms = As} = S) ->
    case read_tree() of
	[] ->
	    S#st{alarms = orddict:new()};
	#conf_tree{tree = T} ->
	    As1 = lists:foldl(
		    fun({ID, Attrs}, Acc) ->
			    SThr = jlrdemo_lib:find_val(
				     <<"trigger_threshold">>, Attrs, infinity),
			    CThr = jlrdemo_lib:find_val(
				     <<"reset_threshold">>, Attrs, 0),
			    orddict:store(ID, #alarm{set = SThr,
						     reset = CThr}, Acc)
		    end, As, T),
	    S#st{alarms = As1}
    end.

read_tree() ->
    case kvdb_conf:read_tree(<<"jlrdemo*config*alarm">>) of
	[] -> [];
	T  -> right_tree(T)
    end.

right_tree(#conf_tree{root = Root} = Tree) ->
    case kvdb_conf:unescape_key(Root) of
	<<"jlrdemo*config*alarm">> ->
	    Tree;
	<<"jlrdemo*config*alarm", _/binary>> ->
	    right_tree(kvdb_conf:shift_root(up, Tree))
    end.

