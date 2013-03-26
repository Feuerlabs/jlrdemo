-module(jlrdemo_lib).

-export([timestamp/0,
	 ts_to_datetime/1,
	 make_decimal/1,
	 can_data_value/2,
	 find_val/3]).

-define(EPOCH, 62167219200).

timestamp() ->
    {_,_,US} = Now = os:timestamp(),
    MS = round(US/1000),
    %% ?EPOCH is 1970-1-1, 0:0:0 in gregorian seconds
    (calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time(Now)) - ?EPOCH) * 1000 + MS.

ts_to_datetime(TS) ->
    MS = TS rem 1000,
    S = TS div 1000,
    {calendar:gregorian_seconds_to_datetime(S + ?EPOCH), MS}.


make_decimal(MS) ->
    S = MS div 1000,
    Rem = MS rem 1000,
    integer_to_list(S) ++ "." ++ integer_to_list(Rem).

can_data_value(Len, Bin) ->
    <<Val:Len/integer-unit:8>> = Bin,
    Val.

find_val(K, [{K,_,V}|_], _) ->
    list_to_integer(binary_to_list(V));
find_val(K, [_|T], Default) ->
    find_val(K, T, Default);
find_val(_, [], Default) ->
    Default.
