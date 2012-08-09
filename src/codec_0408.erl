-module(codec_0408).
-author('Duncan Smith <Duncan@xrtc.net>').

-include_lib("emsc/include/dtap.hrl").

-export([parse_message/1, encode_message/1]).
-compile(export_all).

parse_message(<<Skip:4, Discrim:4, Msg/binary>>) ->
    case Skip of
	0 ->
	    parse_message(Discrim, Msg);
	_ ->
	    {}
    end.

parse_message(?GSM48_PDISC_CC, <<_:1, Seq:1, Type:6, Msg/binary>>) ->
    parse_cc_msg(Type, Seq, Msg);
parse_message(?GSM48_PDISC_MM, <<Type:8, Msg/binary>>) ->
    parse_mm_msg(Type, Msg);
parse_message(?GSM48_PDISC_RR, <<Type:8, Msg/binary>>) ->
    {dtap_rr, Type, Msg};
parse_message(Discrim, <<Type:8, Msg/binary>>) ->
    {dtap_unknown, Discrim, {Type, Msg}}.

encode_message({dtap_mm, Type, Msg}) ->
%    io:format("Encoding MM ~p message ~p~n", [Type, Msg]),
    {0, encode_mm_msg(Type, Msg)};
encode_message({dtap_cc, Type, Msg}) ->
    {0, encode_cc_msg(Type, Msg)}.


% This file only contains routines to parse messages and elements that
% are defined to come from mobile stations, and generation routines
% will only handle messages and elements that are sent to the mobile
% station.  That is, this file is only designed for land-side use.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CC message parsing and generation


message_from_mt_cc(Type) ->
    case Type of
	% Type -> {[mandatory], [optional]}
	?GSM48_MT_CC_ALERTING -> {[], [?GSM48_IE_FACILITY, ?GSM48_IE_PROGR_IND, ?GSM48_IE_USER_USER, ?GSM48_IE_SS_VERS]};
	% XXX: MT_CC_CALL_CONF has zero or one or two ?GSM48_IE_BEARER_CAP
	?GSM48_MT_CC_CALL_CONF -> {[], [?GSM48_IE_BEARER_CAP, ?GSM48_IE_CAUSE, ?GSM48_IE_CC_CAP]};
	?GSM48_MT_CC_CALL_PROC -> {[], [?GSM48_IE_BEARER_CAP, ?GSM48_IE_FACILITY, ?GSM48_IE_PROGR_IND]};
	?GSM48_MT_CC_CONG_CTRL -> {[cong_lev, spare_half], [?GSM48_IE_CAUSE]};
	?GSM48_MT_CC_CONNECT -> {[], [?GSM48_IE_FACILITY, ?GSM48_IE_PROGR_IND, ?GSM48_IE_CONN_BCD, ?GSM48_IE_CONN_SUB, ?GSM48_IE_USER_USER]};
	?GSM48_MT_CC_CONNECT_ACK -> {[], []};
	?GSM48_MT_CC_DISCONNECT -> {[?GSM48_IE_CAUSE], [?GSM48_IE_FACILITY, ?GSM48_IE_PROGR_IND, ?GSM48_IE_USER_USER]};
	?GSM48_MT_CC_EMERG_SETUP -> {[], [?GSM48_IE_BEARER_CAP]};
	?GSM48_MT_CC_FACILITY -> {[?GSM48_IE_FACILITY], []};
	?GSM48_MT_CC_HOLD -> {[], []};
	?GSM48_MT_CC_HOLD_ACK -> {[], []};
	?GSM48_MT_CC_HOLD_REJ -> {[?GSM48_IE_CAUSE], []};
	?GSM48_MT_CC_MODIFY -> {[?GSM48_IE_BEARER_CAP], [?GSM48_IE_LOWL_COMPAT, ?GSM48_IE_HIGHL_COMPAT, ?GSM48_IE_REV_C_SETUP]};
	?GSM48_MT_CC_MODIFY_COMPL -> {[?GSM48_IE_BEARER_CAP], [?GSM48_IE_LOWL_COMPAT, ?GSM48_IE_HIGHL_COMPAT, ?GSM48_IE_REV_C_SETUP]};
	?GSM48_MT_CC_MODIFY_REJECT -> {[?GSM48_IE_BEARER_CAP, ?GSM48_IE_CAUSE], [?GSM48_IE_LOWL_COMPAT, ?GSM48_IE_HIGHL_COMPAT]};
	?GSM48_MT_CC_NOTIFY -> {[notif_ind], []};
	?GSM48_MT_CC_PROGRESS -> {[?GSM48_IE_PROGR_IND], [?GSM48_IE_USER_USER]};
	% XXX: MT_CC_RELEASE may have a second ?GSM48_IE_CAUSE
	?GSM48_MT_CC_RELEASE -> {[], [?GSM48_IE_CAUSE, ?GSM48_IE_FACILITY, ?GSM48_IE_USER_USER]};
	% XXX: MT_CC_RELEASE_COMPL may have a second ?GSM48_IE_CAUSE
	?GSM48_MT_CC_RELEASE_COMPL -> {[], [?GSM48_IE_CAUSE, ?GSM48_IE_FACILITY, ?GSM48_IE_USER_USER]};
	?GSM48_MT_CC_RETR -> {[], []};
	?GSM48_MT_CC_RETR_ACK -> {[], []};
	?GSM48_MT_CC_RETR_REJ -> {[?GSM48_IE_CAUSE], []};
	?GSM48_MT_CC_SETUP -> {[], [?GSM48_IE_BEARER_CAP, ?GSM48_IE_FACILITY, ?GSM48_IE_PROGR_IND, ?GSM48_IE_SIGNAL, ?GSM48_IE_CALLING_BCD, ?GSM48_IE_CALLING_SUB, ?GSM48_IE_CALLED_BCD, ?GSM48_IE_CALLED_SUB, ?GSM48_IE_USER_USER]};
	?GSM48_MT_CC_START_DTMF -> {[], [?GSM48_IE_KPD_FACILITY]};
	?GSM48_MT_CC_START_DTMF_ACK -> {[], [?GSM48_IE_KPD_FACILITY]};
	?GSM48_MT_CC_START_DTMF_REJ -> {[?GSM48_IE_CAUSE], []};
	?GSM48_MT_CC_STATUS -> {[?GSM48_IE_CAUSE, call_state], [?GSM48_IE_AUX_STATUS]};
	?GSM48_MT_CC_STATUS_ENQ -> {[], []};
	?GSM48_MT_CC_STOP_DTMF -> {[], []};
	?GSM48_MT_CC_STOP_DTMF_ACK -> {[], []};
	?GSM48_MT_CC_USER_INFO -> {[?GSM48_IE_USER_USER], [?GSM48_IE_MORE_DATA]};
	_ -> {[], []}
    end.

parse_cc_msg(Type, Seq, Msg) ->
    {Mand, _Opt} = message_from_mt_cc(Type),
    {dtap_cc, Type, [{sequence, Seq} | parse_cc_ies(Mand, Msg)]}.

encode_cc_msg(Type, Msg) ->
    encode_cc_msg(Type, proplists:get_value(sequence, Msg), Msg).

encode_cc_msg(Type, Seq, Msg) ->
    {Mand, Opt} = message_from_mt_cc(Type),
    encode_cc_msg(Type, Seq, Mand, Opt, Msg).

encode_cc_msg(Type, Seq, [Next|Mand], Opt, Msg) ->
    [].

parse_cc_ies(Expect, Msg) ->
    parse_cc_ies(Expect, Msg, []).

parse_cc_ies(_Expect, _Msg, []) ->
    [];

% The bearer capability is a type 4 information element with a minimum
% length of 3 octets and a maximum length of 10 octets.  NOTE: The
% coding of the octets of the bearer capability information element is
% not conforming to TS CCITT Q.931.
%
% This is a 10-octet IE where every bit means something different.
%
% 10.5.4.5
parse_cc_ies([bearer_cap|T], <<Length:8, Bearer:Length/bytes, Rest/bytes>>, SoFar) ->
    parse_cc_ies(T, Rest, [{bearer_cap, {unparsed, Bearer}} | SoFar]);
parse_cc_ies([], <<?GSM48_IE_BEARER_CAP:8, Rest/bytes>>, SoFar) ->
    parse_cc_ies([bearer_cap], Rest, SoFar).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MM message parsing and generation


message_from_mt_mm(Type) ->
    case Type of
	% Type -> {[mandatory], [optional]}
	?GSM48_MT_MM_AUTH_REJ -> {[], []};
	?GSM48_MT_MM_AUTH_REQ -> {[cipher_key_seq, spare_half, rand], []};
	?GSM48_MT_MM_AUTH_RESP -> {[auth_param_sres], []};
	?GSM48_MT_MM_CM_REEST_REQ -> {[cipher_key_seq, spare_half, classmark_2, ?GSM48_IE_MOBILE_ID], [?GSM48_IE_LOCATION_AREA]};
	?GSM48_MT_MM_CM_SERV_ACC -> {[], []};
	?GSM48_MT_MM_CM_SERV_REJ -> {[rej_cause], []};
	?GSM48_MT_MM_CM_SERV_ABORT -> {[], []};
	?GSM48_MT_MM_ABORT -> {[rej_cause], []};
	?GSM48_MT_MM_CM_SERV_REQ -> {[cm_serv_type, cipher_key_seq, classmark_2, ?GSM48_IE_MOBILE_ID], []};
	?GSM48_MT_MM_ID_REQ -> {[id_type, spare_half], []};
	?GSM48_MT_MM_ID_RESP -> {[?GSM48_IE_MOBILE_ID], []};
	?GSM48_MT_MM_IMSI_DETACH_IND -> {[classmark_1, ?GSM48_IE_MOBILE_ID], []};
	?GSM48_MT_MM_LOC_UPD_ACCEPT -> {[?GSM48_IE_LOCATION_AREA], [?GSM48_IE_MOBILE_ID, ?GSM48_IE_FOLLOW_ON_PROC]};
	?GSM48_MT_MM_LOC_UPD_REJECT -> {[rej_cause], []};
	?GSM48_MT_MM_LOC_UPD_REQUEST -> {[loc_upd_type, cipher_key_seq, ?GSM48_IE_LOCATION_AREA, classmark_1, ?GSM48_IE_MOBILE_ID], []};
	?GSM48_MT_MM_INFO -> {[], [?GSM48_IE_NAME_LONG, ?GSM48_IE_NAME_SHORT, ?GSM48_IE_UTC, ?GSM48_IE_NET_TIME_TZ]};
	?GSM48_MT_MM_STATUS -> {[rej_cause], []};
	?GSM48_MT_MM_TMSI_REALL_CMD -> {[?GSM48_IE_LOCATION_AREA, ?GSM48_IE_MOBILE_ID], []};
	?GSM48_MT_MM_TMSI_REALL_COMPL -> {[], []};
	?GSM48_MT_MM_NULL -> {[], []};
	_ -> {[], []}
    end.

parse_mm_msg(Type, Msg) ->
    {Mand, _Opt} = message_from_mt_mm(Type),
    {dtap_mm, Type, parse_mm_ies(Mand, Msg)}.

encode_mm_msg(Type, Msg) ->
    {Mand, Opt} = message_from_mt_mm(Type),
    erlang:list_to_binary([<< 0:4, ?GSM48_PDISC_MM:4, Type:8 >>, encode_mm_msg(Type, Mand, Opt, Msg)]).

encode_mm_msg(Type, [], [], Msg) ->
    [];

encode_mm_msg(Type, [], [Next|Opt], Msg) ->
    [enc_mm_ie(opt, Next, Msg) | encode_mm_msg(Type, [], Opt, Msg)];

encode_mm_msg(Type, [Next|Mand], Opt, Msg) ->
    [enc_mm_ie(mand, Next, Msg) | encode_mm_msg(Type, Mand, Opt, Msg)].


parse_mm_ies(Expect, Msg) ->
    parse_mm_ies(Expect, Msg, []).

% first argument: expected non-type-tagged IEs
% second argument: remaining unparsed message
% third: list of IEs parsed so far
% returns: list of IEs
parse_mm_ies(_, <<>>, SoFar) ->
    SoFar;

% 10.5.1.2
parse_mm_ies([cipher_key_seq|T], <<_:1, Seq:3, Rest/bits>>, SoFar) ->
    parse_mm_ies(T, Rest, [{cipher_key_seq, Seq} | SoFar]);
% 10.5.3.3
parse_mm_ies([cm_serv_type|T], <<TypeInt:4, Rest/bits>>, SoFar) ->
    Type = case TypeInt of
	       2#0001 -> mo_call;
	       2#0010 -> emerg_call;
	       2#0100 -> sms;
	       2#1000 -> suppl_serv;
	       2#1001 -> v_group_est;
	       2#1010 -> v_broadcast_est;
	       _ -> TypeInt
	   end,
    parse_mm_ies(T, Rest, [{cm_serv_type, Type} | SoFar]);
% 10.5.1.3
parse_mm_ies([], <<?GSM48_IE_LOCATION_AREA:8, Rest/binary>>, SoFar) ->
    parse_mm_ies([lai], Rest, SoFar);
parse_mm_ies([?GSM48_IE_LOCATION_AREA|T], <<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4, LAC:16/big, Rest/bytes>>, SoFar) ->
    MCC = MCC3 + MCC2*10 + MCC1*100,
    MNC = if (MNC3 == 2#1111) -> MNC2 + MNC1*10;
       true -> MNC2 + MNC1*10 + MNC3*100 end,
    parse_mm_ies(T, Rest, [{lai, {MCC, MNC, LAC}} | SoFar]);
% 10.5.1.4
parse_mm_ies([], <<?GSM48_IE_MOBILE_ID:8, Rest/binary>>, SoFar) ->
    parse_mm_ies([?GSM48_IE_MOBILE_ID], Rest, SoFar);
parse_mm_ies([?GSM48_IE_MOBILE_ID|T], <<Length:8, Message:Length/bytes, Rest/bytes>>, SoFar) ->
    Ident = common_0408:parse_mobile_id(Message),
    parse_mm_ies(T, Rest, [{mobile_id, Ident} | SoFar]);
% 10.5.1.5
parse_mm_ies([classmark_1|T], <<CM1:8/bits, Rest/bytes>>, SoFar) ->
    parse_mm_ies(T, Rest, [{classmark_1, common_0408:parse_classmark_1(CM1)} | SoFar]);
% 10.5.1.6
parse_mm_ies([classmark_2|T], <<Length:8, CM2:Length/bytes, Rest/binary>>, SoFar) ->
    parse_mm_ies(T, Rest, [{classmark_2, common_0408:parse_classmark_2(CM2)} | SoFar]);
% 10.5.1.8
parse_mm_ies([spare_half|T], <<_Spare:4/bits, Msg/bits>>, SoFar) ->
    parse_mm_ies(T, Msg, SoFar);
% 10.5.3.5
parse_mm_ies([loc_upd_type|T], <<Follow:1, _:1, TypeBin:2, Rest/bits>>, SoFar) ->
    Type = case TypeBin of
               2#00 -> normal;
               2#01 -> periodic;
               2#10 -> imsi_attach;
               2#11 -> undefined
           end,
    parse_mm_ies(T, Rest, [{loc_upd_type, [{type, Type}, {follow_on, Follow}]} | SoFar]);
% 10.5.3.6
parse_mm_ies([rej_cause|T], <<Cause:8, Rest/bytes>>, SoFar) ->
    parse_mm_ies(T, Rest, [{rej_cause, Cause} | SoFar]);
% 10.5.4.12
parse_mm_ies([cong_lev|T], <<Cong:4, Rest/bits>>, SoFar) ->
    case Cong of
	2#0000 ->
	    parse_mm_ies(T, Rest, [{congestion, ready} | SoFar]);
	2#1111 ->
	    parse_mm_ies(T, Rest, [{congestion, not_ready} | SoFar])
    end;
% 10.5.4.11
parse_mm_ies([?GSM48_IE_CAUSE|T], <<Length:8, Cause:Length/binary, Rest/bytes>>, SoFar) ->
    parse_mm_ies(T, Rest, [{cause, {unparsed, Cause}} | SoFar]);
parse_mm_ies([], <<?GSM48_IE_CAUSE:8, Msg/bytes>>, SoFar) ->
    parse_mm_ies([?GSM48_IE_CAUSE], Msg, SoFar);
% 10.5.4.15
parse_mm_ies([?GSM48_IE_FACILITY|T], <<Length:8, Facility:Length/binary, Rest/bytes>>, SoFar) ->
    parse_mm_ies(T, Rest, [{facility, {unparsed, Facility}} | SoFar]);
parse_mm_ies([], <<?GSM48_IE_FACILITY:8, Msg/bytes>>, SoFar) ->
    parse_mm_ies([?GSM48_IE_FACILITY], Msg, SoFar);
% 10.5.4.17
parse_mm_ies([?GSM48_IE_KPD_FACILITY|T], <<Char:8, Rest/bytes>>, SoFar) ->
    parse_mm_ies(T, Rest, [{keypad, Char} | SoFar]);
parse_mm_ies([], <<?GSM48_IE_KPD_FACILITY:8, Msg/bytes>>, SoFar) ->
    parse_mm_ies([?GSM48_IE_KPD_FACILITY], Msg, SoFar);
% 10.5.4.25
parse_mm_ies([?GSM48_IE_USER_USER|T], <<Length:8, Data:Length/binary, Rest/bytes>>, SoFar) ->
    parse_mm_ies(T, Rest, [{user_user, {unparsed, Data}}|SoFar]);
parse_mm_ies([], <<?GSM48_IE_USER_USER:8, Msg/bytes>>, SoFar) ->
    parse_mm_ies([?GSM48_IE_USER_USER], Msg, SoFar).

% need to encode:
% cipher_key_seq	10.5.1.2 ok
% spare_half		10.5.1.8 ok
% rand			10.5.3.1 ok
% rej_cause		10.5.3.6 ok
% id_type		10.5.3.4 ok
% IE_MOBILE_ID		10.5.1.4 ok
% IE_FOLLOW_ON_PROC	10.5.3.7 ok
% IE_NAME_LONG		10.5.3.5a ok
% IE_NAME_SHORT		10.5.3.5a ok
% IE_UTC		10.5.3.8 
% IE_NET_TIME_TZ	10.5.3.9 

% (mand/opt, type, data) -> <<IE>>/bytes with type-tag if necessary
enc_mm_ie(mand, spare_half, D) ->
    << 0:4 >>;

enc_mm_ie(mand, rand, D) ->
    Rand = proplists:get_value(rand, D),
    << Rand:16/bytes >>;

enc_mm_ie(mand, cipher_key_seq, D) ->
    Seq = proplists:get_value(cipher_key_seq, D),
    << 0:1, Seq:3 >>;

enc_mm_ie(mand, rej_cause, D) ->
    Cause = proplists:get_value(rej_cause, D),
    << Cause:8 >>;

enc_mm_ie(mand, id_type, D) ->
    Type = proplists:get_value(id_type, D),
    << 0:1, (case Type of
	       imsi -> 2#001;
	       imei -> 2#010;
	       imeisv -> 2#011;
	       tmsi -> 2#100
	   end):3 >>;

enc_mm_ie(mand, T = ?GSM48_IE_MOBILE_ID, D) ->
    enc_ie(T, D);
enc_mm_ie(opt, T = ?GSM48_IE_MOBILE_ID, D) ->
    IE = enc_mm_ie(mand, T, D),
    case IE of
	<< >> -> << >>;
	_ -> << T:8, IE/bytes >>
    end;

enc_mm_ie(mand, T = ?GSM48_IE_LOCATION_AREA, D) ->
    enc_ie(T, D);
enc_mm_ie(opt, T = ?GSM48_IE_LOCATION_AREA, D) ->
    case proplists:is_defined(lai, D) of
	true -> << T:8, (enc_mm_ie(mand, T, D))/bytes >>;
	_ -> << >>
    end;

enc_mm_ie(mand, T = ?GSM48_IE_NAME_LONG, D) ->
    {AddCI, Text} = proplists:get_value(name_long, D),
    Scheme = 2#000,
    {_, _, Spare, TEnc} = common_0408:encode_0338_ascii(Text),
    Encoded = << 1:1, Scheme:3, (common_0408:binarize(AddCI)):1, Spare:3, TEnc/binary >>,
    Len = byte_size(Encoded),
    << Len:8, Encoded/binary >>;
enc_mm_ie(opt, T = ?GSM48_IE_NAME_LONG, D) ->
    case proplists:is_defined(name_long, D) of
	true -> << T:8, (enc_mm_ie(mand, T, D))/binary >>;
	_ -> << >>
    end;

enc_mm_ie(mand, ?GSM48_IE_NAME_SHORT, D) ->
    {AddCI, Text} = proplists:get_value(name_short, D),
    Scheme = 2#000,
    {_, _, Spare, TEnc} = common_0408:encode_0338_ascii(Text),
    Encoded = <<1:1, Scheme:3, (common_0408:binarize(AddCI)):1, Spare:3, TEnc/binary>>,
    Len = byte_size(Encoded),
    << Len:8, Encoded/binary >>;
enc_mm_ie(opt, T = ?GSM48_IE_NAME_SHORT, D) ->
    case proplists:is_defined(name_short, D) of
	true -> << T:8, (enc_mm_ie(mand, T, D))/binary >>;
	_ -> << >>
    end;

enc_mm_ie(opt, T = ?GSM48_IE_FOLLOW_ON_PROC, D) ->
    case proplists:is_defined(follow_on_proc, D) of
	true -> << T:8 >>;
	_ -> << >>
    end;

enc_mm_ie(opt, _T, _D) ->
    << >>;

enc_mm_ie(mand, ?GSM48_IE_UTC, D) ->
    error.


enc_ie(?GSM48_IE_MOBILE_ID, D) ->
    case proplists:is_defined(mobile_id, D) of
	true ->
	    {Type, Id} = proplists:get_value(mobile_id, D),
	    MID = common_0408:encode_mobile_id(Type, Id),
	    << (byte_size(MID)):8, MID/bytes >>;
	_ -> << >>
    end;

% 10.5.1.3
enc_ie(?GSM48_IE_LOCATION_AREA, D) ->
    {MCC, MNC, LAC} = proplists:get_value(lai, D),
    [MCC1, MCC2, MCC3] = lists:map((fun (E) -> E - $0 end), erlang:integer_to_list(MCC)),
    [MNC1, MNC2|_] = lists:map((fun (E) -> E - $0 end), erlang:integer_to_list(MNC)),
    MNC3 = 16#f,
    <<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4, LAC:16/big>>.
