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


-export([set_fan_speed_frame/1]).

-record(st, {
	  iface = undefined,
	  unknown1 = 0,
	  air_distribution = 0,
	  unknown2 = 0,
	  fan_blower_speed = 0,
	  unknown3 = 0,
	  left_temp = 0,
	  unknown4 = 0,
	  right_temp = 0,
	  system_on = 0,
	  ac_on = 0,
	  unknown5 = 0,
 	  recirc = 0,
	  unknown6 = 0,
	  heated_rear_window = 0,
	  heated_front_screen = 0,
	  unknown7 = 0
	 }).

-define(FCIM_FACP_A, 16#240).
-define(SERVER, jlrdemo_can).

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


handle_call({send_set_fan_speed_frame, Speed}, _F, St) ->
    Frame =
	<< (St#st.unknown1):16 ,
	   (St#st.air_distribution):3 ,
	   (St#st.unknown2):5 ,
	   Speed:5 ,
	   (St#st.unknown3):3 ,
	   (St#st.left_temp):6 ,
	   (St#st.unknown4):2 ,
	   (St#st.right_temp):6 ,
	   (St#st.system_on):1 ,
	   (St#st.ac_on):1 ,
	   (St#st.unknown5):8 ,
	   (St#st.recirc):2 ,
	   (St#st.unknown6):2 ,
	   (St#st.heated_rear_window):1 ,
	   (St#st.heated_front_screen):1,
	   (St#st.unknown7):2 >>,
    can_router:send(#can_frame {
		       intf = St#st.iface,
		       id = ?FCIM_FACP_A,
		       data = Frame,
		       len = 8}),
    { reply, St#st { fan_blower_speed = Speed }};

handle_call(Msg, From, S) ->
    io:format("jlrdemo_can:handle_call(~p, ~p, ~p)~n", [Msg, From, S]),
    {reply, error, S}.


handle_info({can_frame, FrameID, DataLen, Data, _A, _B}, St) ->
    case FrameID of
	?FCIM_FACP_A when DataLen =:= 8->
	    <<Unknown1:16,        %% 00-15
	      FLHSDistrCmd:3,     %% 16-18  Air Distribution command
	      Unknown2:5,         %% 19-23
	      FanBlwrSpeedCmd:5,  %% 24-28  Fan Blower speed
	      Unknown3:3,         %% 29-31
	      FrontSetLeftCmd:6,  %% 32-37  Set left temp (C)
	      Unknown4:2,         %% 38-39
	      FrontSetRightCmd:6, %% 40-45  Set right temp (C)
	      FrontSystemOnCmd:1, %% 46     Front system On/off
	      ACCommand:1,        %% 47     AC On/Off
	      Unknown5:8,         %% 48-55
	      RecircReq:2,        %% 56-57  Recirculation
	      Unknown6:2,         %% 58-59
	      HRWCommand:1,       %% 60     Heated rear window
	      HFSCommand:1,       %% 61     Heated front screen
	      Unknown7:2          %% 62-63
	    >> = Data,

	NSt = #st {
	  iface = St#st.iface,
	  unknown1 = Unknown1,
	  air_distribution = FLHSDistrCmd,
	  unknown2 = Unknown2,
	  fan_blower_speed = FanBlwrSpeedCmd,
	  unknown3 = Unknown3,
	  left_temp = FrontSetLeftCmd,
	  unknown4 = Unknown4,
	  right_temp = FrontSetRightCmd,
	  system_on = FrontSystemOnCmd,
	  ac_on = ACCommand,
	  unknown5 = Unknown5,
	  recirc = RecircReq,
	  unknown6 = Unknown6,
	  heated_rear_window = HRWCommand,
	  heated_front_screen = HFSCommand,
	  unknown7 = Unknown7
	 },

	io:format("jlrdemo_can: Got valid CAN frame Record(~p)~n", [ NSt]),

	{ noreply, NSt };

	_ ->
	    io:format("jlrdemo_can: Ignored unknown CAN frame ID(~p) Length(~p)~n", [ FrameID, DataLen])
    end,
    {noreply, St};



handle_info(Msg,  S) ->
    io:format("jlrdemo_can:handle_info(?? ~p, ~p)~n", [Msg, S]),
    {noreply, S}.

terminate(_Reason, _S) ->
    can_router:stop(),
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

set_fan_speed_frame(Speed) ->
    gen_server:call(?SERVER, { set_fan_speed_frame, Speed }).
