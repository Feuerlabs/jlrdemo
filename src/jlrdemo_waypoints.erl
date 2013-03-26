-module(jlrdemo_waypoints).
-behavior(gen_server).

-export([start_waypoints/1]).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(st, {nmea}).

start_waypoints(Device) ->
    gen_server:call(?MODULE, {start_waypoints, Device}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #st{nmea = false}}.


handle_cast(_, S) ->
    {noreply, S}.

handle_call({start_waypoints, Device}, _From, _S) ->
    {ok, Pid } = nmea_0183_srv:start(Device),
    nmea_0183_srv:subscribe(Pid, 3000),
    {reply, ok, #st { nmea = Pid }};

handle_call(_Msg, _From, S) ->
    {reply, error, S}.


handle_info({nmea_log, _NmeaPid, Tab, Pos, Len, Size}, State) ->
    Wpts = read_wpts(Tab, Pos, Len, Size, []),
    send_waypoints(Wpts),
    %% Ulf Wiger. Add waypoint logging here.
%%    case do_some_waypoint_stuff_here({waypoint,Wpts}, nmea_0183_srv, State) of
%%	{noreply,State1} ->
%%	    {noreply, State1};
%%	{reply, What, State1} ->
%%	    {noreply, State1}
%%    end;
     {noreply, State};

handle_info(_Msg, S) ->
    {noreply, S}.

unix_time() ->
     calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now()))-719528*24*3600.

send_waypoints([]) ->
    true;

send_waypoints(Wpts) ->
    [H|T] = Wpts,
    case H of
	{ position, Lat, Long } ->
	    exoport:rpc(exodm_rpc, rpc,
			[<<"demo">>, <<"process-waypoints">>,
			 [
			  {'waypoints',
			   { array,
			     [
			       { struct,
				 [
				  {ts, unix_time()},
				  {'lat', Lat},
				  {'lon', Long }
				 ]
			       }
			     ]
			   }
			  }
			 ]
			]
		       );
	_ ->
		true
    end,
    send_waypoints(T).



read_wpts(_Tab, _Pos, 0, _Size, Acc) ->
    lists:reverse(Acc);


read_wpts(Tab, Pos, Len, Size, Acc) ->
    case ets:lookup(Tab, Pos) of
	[{_,{position,Lat,Long}}] ->
	    read_wpts(Tab, (Pos+1) rem Size, Len-1, Size,
		      [{position, Lat, Long}|Acc]);
	[{_,{timestamp, Ts}}] ->
	    read_wpts(Tab, (Pos+1) rem Size, Len-1, Size,
		      [{timestamp, Ts}|Acc]);
	X ->
	    io:format("Uncaught waypoint: ~p~n", [X])
    end.


terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% helper functions
timestamp() ->
    DT = erlang:universaltime(),
    calendar:datetime_to_gregorian_seconds(DT).
