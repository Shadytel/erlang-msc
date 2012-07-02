-module(dtap_codec).
-author("Duncan Smith <Duncan@xrtc.net>").
-include_lib("emsc/include/dtap.hrl").

-export([parse_message/2, encode_message/1]).
-compile(export_all).

% This file is designed to encode and decode DTAP messages between the
% binary wire format and an Erlang tuple format.

encode_message(_Msg) ->
    ok.

parse_message(DLCI, <<Trans:4, ?GSM48_PDISC_RR:4, Type:8, Remain/bytes>>) ->
    {ok, {dtap_rr, Trans, Type, parse_rr_msg(Type, Remain)}};
parse_message(DLCI, <<Trans:4, ?GSM48_PDISC_MM:4, Type:8, Remain/bytes>>) ->
    {ok, {dtap_mm, Trans, Type, parse_mm_msg(Type, Remain)}};
parse_message(DLCI, <<Trans:4, ?GSM48_PDISC_CC:4, Type:8, Remain/bytes>>) ->
    {ok, {dtap_cc, Trans, Type, parse_cc_msg(Type, Remain)}};
parse_message(DLCI, <<Trans:4, Proto:4, Type:8, Remain/bytes>>) ->
    {unknown, {Proto, Trans, Type, Remain}}.


parse_rr_msg(?GSM48_MT_RR_INIT_REQ, _Msg) ->
    ok.

parse_cc_msg(?GSM48_MT_CC_ALERTING, _Msg) ->
    ok.

parse_mm_msg(?GSM48_MT_MM_IMSI_DETACH_IND, _Msg) ->
    ok.

parse_el_list(DataBin) ->
    lists:reverse(parse_el_list(DataBin, [])).

parse_el_list(<<>>, List) ->
    List;
parse_el_list(DataBin, List) ->
    {ok, Length, Element} = parse_el(DataBin),
    parse_el_list(binary:part(DataBin, Length, byte_size(DataBin) - Length), [Element|List]).

% 10.5.1.4
% XXX The identity number isn't properly digit-swapped.
parse_el(<<?GSM48_IE_MOBILE_ID:8, Length:8, Msg:Length/bytes, _/binary>>) ->
    <<Digit1:4, Odd:1, TypeId:3, Digits/bytes>> = Msg,
    Type = case TypeId of
	       2#001 -> imsi;
	       2#010 -> imei;
	       2#011 -> imeisv;
	       2#100 -> tmsi;
	       2#000 -> none;
	       _ -> undefined
	   end,
    {ok, Length+2, {mobile_id, {Type,
     case Type of
	 tmsi ->
	     [ hd(erlang:integer_to_list(Nibble, 16)) || <<Nibble:4>> <= Digits ];
	 _ ->
	     [ hd(erlang:integer_to_list(Nibble, 16)) || <<Nibble:4>> <= <<Digit1:4, Digits/binary>>]
     end}}};

% 10.5.3.5a
parse_el(<<?GSM48_IE_NAME_LONG:8, Length:8, Msg:Length/bytes, _/binary>>) ->
    <<1:1, Coding:3, AddCi:1, Spare:3, Text/binary>> = Msg,
    2#000 = Coding,
    % unpack 7-bit bytes.  etsi are composed of maniacs
    %%% XXX actually unpack
    {ok, Length+2, {name, { Text }}};

% 10.5.3.8
parse_el(<<?GSM48_IE_UTC, Sign:1, Zone:7, _/binary>>) ->
    {ok, 2, {time_zone,
	     {minuteswest,
	      case Sign of
		  1 -> -15 * Zone;
		  0 -> 15 * Zone
	      end}
	    }};

% 10.5.3.9
parse_el(<<?GSM48_IE_NET_TIME_TZ,
	   YearT:4, YearU:4,
	   MonthT:4, MonthU:4,
	   DayT:4, DayU:4,
	   HourT:4, HourU:4,
	   MinuteT:4, MinuteU:4,
	   SecondT:4, SecondU:4,
	   ZSign:1, Zone:7,
	   _/binary>>) ->
    {ok, 8, {time,
	     [{year, YearT*10 + YearU},
	      {month, MonthT*10 + MonthU},
	      {day, DayT*10 + DayU},
	      {hour, HourT*10 + HourU},
	      {minute, MinuteT*10 + MinuteU},
	      {second, SecondT*10 + SecondU},
	      {time_zone,
	       {minuteswest,
		case ZSign of
		    1 -> -15 * Zone;
		    2 -> 15 * Zone
		end}}
	     ]}};


parse_el(Binary) ->
    Binary.

