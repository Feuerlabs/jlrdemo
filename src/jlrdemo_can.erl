-module(jlrdemo_can).
-behavior(gen_server).

-export([start_can/0]).

-include_lib("can/include/can.hrl").


-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(st, {
	  iface = undefined  %% CAN Interface
	 }).

start_can() ->
    io:format("jlrdemo_can:start_can()~n", []),
    gen_server:call(?MODULE, {start_can, "can0", "can_sock_drv"}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    io:format("jlrdemo_can:init()~n"),
    can_router:start(),
    {ok, #st{}}.

handle_cast(_, S) ->
    {noreply, S}.


handle_call({start_can, Interface, Driver}, _From, #st { iface = OldInterface } = _St) ->
    io:format("jlrdemo_can:handle_call(start_can, ~p, ~p, ~p)~n", [Interface, Driver, OldInterface]),
    case OldInterface of
	undefined ->
	    true;
	_ ->
	    can_sock:stop(OldInterface)
    end,
    io:format("jlrdemo_can:handle_call(start_can, ~p, ~p): Starting router~n", [Interface, Driver]),

    can_sock:start(Interface, Driver, []),
    can_router:attach(),
    {reply, ok, #st { iface = Interface }};


handle_call({send_set_fan_speed_frame, Speed}, _F, #st { iface = Iface } = St) ->
    can_router:send(#can_frame {
		       intf = Iface,
		       id = 1234,
		       data = Speed
		      }),
    St;

handle_call(Msg, From, S) ->
    io:format("jlrdemo_can:handle_call(~p, ~p, ~p)~n", [Msg, From, S]),
    {reply, error, S}.


handle_info({can_frame, FrameID, _DataLen, Data, _A, _B}, St) ->
    io:format("Got CAN frame id(~p) Data(~p)~n", [ FrameID, Data]),
    { noreply, St };


handle_info(Msg,  S) ->
    io:format("jlrdemo_can:handle_info(?? ~p, ~p)~n", [Msg, S]),
    {noreply, S}.

terminate(_Reason, _S) ->
    can_router:stop(),
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.
