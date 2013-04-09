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


-export([set_fan_speed/1]).

-record(st, {
	  iface = 0,
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
    {ok, #st {}}.

handle_cast(_, S) ->
    {noreply, S}.


handle_call({start_can, Interface, Driver}, _From, #st { iface = OldInterface } = _St) ->
    io:format("jlrdemo_can:handle_call(start_can, ~p, ~p, ~p)~n",
	      [Interface, Driver, OldInterface]),
    case OldInterface of
	0 ->
	    true;
	_ ->
	    can_sock:stop(OldInterface)
    end,
    io:format("jlrdemo_can:handle_call(start_can, ~p, ~p): Starting router~n", [Interface, Driver]),

    can_sock:start(Interface, Driver, []),
    can_router:attach(),

   <<
      Unknown1:16,        %% Byte[0:0-7] Byte[1:0-7]

      Unknown2:5,         %% Byte[2:3-7]
      FLHSDistrCmd:3,     %% Byte[2:0-2] Air Distribution command

      Unknown3:4,         %% Byte[3:0-3]
      FanBlwrSpeedCmd:4,  %% Byte[3:4-7] Fan Blower speed

      Unknown4:2,         %% Byte[4:6-7]
      FrontSetLeftCmd:6,  %% Byte[4:0-5] 32-37  Set left temp (C)

      ACCommand:1,        %% Byte[5:7]
      FrontSystemOnCmd:1, %% Byte[5:6]
      FrontSetRightCmd:6, %% Byte[5:0-5]  Set right temp (C)

      Unknown5:8,         %% Byte[6:0-7]

      Unknown6:2,         %% Byte[7:6-7]
      HFSCommand:1,       %% Byte[7:5]    Heated front screen
      HRWCommand:1,       %% Byte[7:4]    Heated rear window
      Unknown7:2,         %% Byte[7:2-3]
      RecircReq:2         %% Byte[7:0-1]  Recirculation
    >> = <<0,24,192,1,36,228,192,192>>,

    NSt = #st {
      iface = 0,
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
    io:format("NST(~p)~n", [ NSt ]),
    {reply, ok, NSt};


handle_call({set_fan_speed, Speed}, _F, St) ->
    Frame =
	<< (St#st.unknown1):16,
	   (St#st.unknown2):5,
	   (St#st.air_distribution):3,
	   (St#st.unknown3):4,
	   Speed:4,
	   (St#st.unknown4):2,
	   (St#st.left_temp):6,
	   (St#st.ac_on):1,
	   (St#st.system_on):1,
	   (St#st.right_temp):6,
	   (St#st.unknown5):8,
	   (St#st.unknown6):2,
	   (St#st.heated_front_screen):1,
	   (St#st.heated_rear_window):1,
	   (St#st.unknown7):2,
	   (St#st.recirc):2 >>,

    CanFrame = #can_frame {
      intf = 0,
      id = ?FCIM_FACP_A,
      data = Frame,
      len = 8,
      ts = 0
     },
    io:format("jlrdemo_can:handle_call({set_fan_speed} ~p) ~p~n",
	      [Speed, CanFrame]),

    can:send(CanFrame),
    { reply, ok, St#st { fan_blower_speed = Speed }};

handle_call(Msg, From, S) ->
    io:format("jlrdemo_can:handle_call(~p, ~p, ~p)~n", [Msg, From, S]),
    {reply, error, S}.


handle_info({can_frame, FrameID, DataLen, Data, _A, _B}, St) ->
    case FrameID of
	?FCIM_FACP_A when DataLen =:= 8 ->
	    <<Unknown1:16,        %% Byte[0:0-7] Byte[1:0-7]

	      Unknown2:5,         %% Byte[2:3-7]
	      FLHSDistrCmd:3,     %% Byte[2:0-2] Air Distribution command

	      Unknown3:4,         %% Byte[3:0-3]
	      FanBlwrSpeedCmd:4,  %% Byte[3:4-7] Fan Blower speed

	      Unknown4:2,         %% Byte[4:6-7]
	      FrontSetLeftCmd:6,  %% Byte[4:0-5] 32-37  Set left temp (C)

	      ACCommand:1,        %% Byte[5:7]
	      FrontSystemOnCmd:1, %% Byte[5:6]
	      FrontSetRightCmd:6, %% Byte[5:0-5]  Set right temp (C)

	      Unknown5:8,         %% Byte[6:0-7]

              Unknown6:2,         %% Byte[7:6-7]
	      HFSCommand:1,       %% Byte[7:5]    Heated front screen
	      HRWCommand:1,       %% Byte[7:4]    Heated rear window
	      Unknown7:2,         %% Byte[7:2-3]
	      RecircReq:2         %% Byte[7:0-1]  Recirculation
	    >> = Data,

      %% io:format("u1(~p) Air(~p) u2(~p) Fan(~p) u3(~p) LTemp(~p) u4(~p) RTemp(~p) SysOn(~p), ACOn(~p) u5(~p) Recirc(~p) u6(~p) RearWin(~p), FrontWin(~p) u7(~p)~n",
      %% 	      [ Unknown1,         %% 00-15
      %% 		FLHSDistrCmd,     %% 16-18  Air Distribution command
      %% 		Unknown2,         %% 19-23
      %% 		FanBlwrSpeedCmd,  %% 24-28  Fan Blower speed
      %% 		Unknown3,         %% 29-31
      %% 		FrontSetLeftCmd,  %% 32-37  Set left temp (C)
      %% 		Unknown4,         %% 38-39
      %% 		FrontSetRightCmd, %% 40-45  Set right temp (C)
      %% 		FrontSystemOnCmd, %% 46     Front system On/off
      %% 		ACCommand,        %% 47     AC On/Off
      %% 		Unknown5,         %% 48-55
      %% 		RecircReq,        %% 56-57  Recirculation
      %% 		Unknown6,         %% 58-59
      %% 		HRWCommand,       %% 60     Heated rear window
      %% 		HFSCommand,       %% 61     Heated front screen
      %% 		Unknown7 ]),      %% 62-63

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
%%	    io:format("jlrdemo_can:get_data(~p)~n", [ NSt ]),
	    {noreply, NSt};

	_ ->
	    {noreply, St}
    end;




handle_info(Msg,  S) ->
    io:format("jlrdemo_can:handle_info(?? ~p, ~p)~n", [Msg, S]),
    {noreply, S}.

terminate(_Reason, _S) ->
    can_router:stop(),
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

set_fan_speed(Speed) ->
    gen_server:call(?SERVER, { set_fan_speed, Speed }).



