-module(codec_0408).
-author('Duncan Smith <Duncan@xrtc.net>').

-include_lib("emsc/include/dtap.hrl").

-export([parse_message/1]).
-compile(export_all).

parse_message(<<Skip:4, Discrim:4, Msg/binary>>) ->
    case Skip of
	0 ->
	    parse_message(Discrim, Msg);
	_ ->
	    {}
    end.

parse_message(2#0011, <<0:1, Seq:1, Type:6, Msg/binary>>) ->
    parse_cc_msg(Type, Seq, Msg);
parse_message(2#0101, <<Type:8, Msg/binary>>) ->
    parse_mm_msg(Type, Msg);
parse_message(2#0110, <<Type:8, Msg/binary>>) ->
    parse_rr_msg(Type, Msg);
parse_message(Discrim, <<Type:8, Msg/binary>>) ->
    {dtap_unknown, Discrim, Type, Msg}.

% This file only contains routines to parse messages and elements that
% are defined to come from mobile stations, and generation routines
% will only handle messages and elements that are sent to the mobile
% station.  That is, this file is only designed for land-side use.

% 9.3.1
parse_cc_msg(?GSM48_MT_CC_ALERTING, Seq, Msg) ->
    {dtap_cc, alerting, [{sequence, Seq} | parse_ies(Msg)]};
% 9.3.2
parse_cc_msg(?GSM48_MT_CC_CALL_CONF, Seq, Msg) ->
    {dtap_cc, call_confirm, [{sequence, Seq} | parse_ies(Msg)]};
% 9.3.4
parse_cc_msg(?GSM48_MT_CC_CONG_CTRL, Seq, Msg) ->
    {dtap_cc, congestion_ctrl, [{sequence, Seq} | parse_ies([cong_lev, spare_half], Msg)]};
% 9.3.5
parse_cc_msg(?GSM48_MT_CC_CONNECT, Seq, Msg) ->
    {dtap_cc, connect, [{sequence, Seq} | parse_ies(Msg)]};
% 9.3.6
parse_cc_msg(?GSM48_MT_CC_CONNECT_ACK, Seq, _Msg) ->
    {dtap_cc, connect_ack, [{sequence, Seq}]};
% 9.3.7
parse_cc_msg(?GSM48_MT_CC_DISCONNECT, Seq, Msg) ->
    {dtap_cc, disconnect, [{sequence, Seq} | parse_ies([cause], Msg)]};
% 9.3.8
parse_cc_msg(?GSM48_MT_CC_EMERG_SETUP, Seq, Msg) ->
    {dtap_cc, emerg_setup, [{sequence, Seq} | parse_ies(Msg)]};
% 9.3.9.2
parse_cc_msg(?GSM48_MT_CC_FACILITY, Seq, Msg) ->
    {dtap_cc, facility, [{sequence, Seq} | parse_ies([facility], Msg)]};
% 9.3.10
parse_cc_msg(?GSM48_MT_CC_HOLD, Seq, _Msg) ->
    {dtap_cc, hold, [{sequence, Seq}]};
% 9.3.13
parse_cc_msg(?GSM48_MT_CC_MODIFY, Seq, Msg) ->
    {dtap_cc, modify, [{sequence, Seq} | parse_ies([bearer_cap], Msg)]};
% 9.3.14
parse_cc_msg(?GSM48_MT_CC_MODIFY_COMPL, Seq, Msg) ->
    {dtap_cc, modify_compl, [{sequence, Seq} | parse_ies([bearer_cap], Msg)]};
% 9.3.15
parse_cc_msg(?GSM48_MT_CC_MODIFY_REJECT, Seq, Msg) ->
    {dtap_cc, modify_rej, [{sequence, Seq} | parse_ies([bearer_cap, cause], Msg)]};
% 9.3.16
parse_cc_msg(?GSM48_MT_CC_NOTIFY, Seq, Msg) ->
    {dtap_cc, notify, [{sequence, Seq} | parse_ies([notif_ind], Msg)]};
% 9.3.18
parse_cc_msg(?GSM48_MT_CC_RELEASE, Seq, Msg) ->
    {dtap_cc, release, [{sequence, Seq} | parse_ies(Msg)]};
% 9.3.19
parse_cc_msg(?GSM48_MT_CC_RELEASE_COMPL, Seq, Msg) ->
    {dtap_cc, release_compl, [{sequence, Seq} | parse_ies(Msg)]};
% 9.3.20
parse_cc_msg(?GSM48_MT_CC_RETR, Seq, _Msg) ->
    {dtap_cc, retrieve, [{sequence, Seq}]};
% 9.3.23.2
parse_cc_msg(?GSM48_MT_CC_SETUP, Seq, Msg) ->
    % there are mandatory IEs, but they are all type-tagged.
    {dtap_cc, setup, [{sequence, Seq} | parse_ies(Msg)]};
% 9.3.24
parse_cc_msg(?GSM48_MT_CC_START_DTMF, Seq, Msg) ->
    {dtap_cc, dtmf_start, [{sequence, Seq} | parse_ies([keypad_fac], Msg)]};
% 9.3.27
parse_cc_msg(?GSM48_MT_CC_STATUS, Seq, Msg) ->
    {dtap_cc, status, [{sequence, Seq} | parse_ies([cause, call_state], Msg)]};
% 9.3.28
parse_cc_msg(?GSM48_MT_CC_STATUS_ENQ, Seq, _Msg) ->
    {dtap_cc, status_enq, [{sequence, Seq}]};
% 9.3.29
parse_cc_msg(?GSM48_MT_CC_STOP_DTMF, Seq, _Msg) ->
    {dtap_cc, dtmf_stop, [{sequence, Seq}]};
% 9.3.31
parse_cc_msg(?GSM48_MT_CC_USER_INFO, Seq, Msg) ->
    {dtap_cc, user_info, [{sequence, Seq} | parse_ies([user_user], Msg)]}.



% 9.2.3
parse_mm_msg(?GSM48_MT_MM_AUTH_RESP, Msg) ->
    {dtap_mm, auth_resp, parse_ies([auth_param_sres], Msg)};
% 9.2.4
parse_mm_msg(?GSM48_MT_MM_CM_REEST_REQ, Msg) ->
    {dtap_mm, reest_req, parse_ies([cipher_key_seq, spare_half, classmark_2, mobile_id, lai], Msg)};
% 9.2.7
parse_mm_msg(?GSM48_MT_MM_CM_SERV_ABORT, _Msg) ->
    {dtap_mm, serv_abort, []};
% 9.2.9
parse_mm_msg(?GSM48_MT_MM_CM_SERV_REQ, Msg) ->
    {dtap_mm, serv_req, parse_ies([cm_serv_type, cipher_key_seq, classmark_2, mobile_id], Msg)};
% 9.2.11
parse_mm_msg(?GSM48_MT_MM_ID_RESP, Msg) ->
    {dtap_mm, ident_resp, parse_ies([mobile_id], Msg)};
% 9.2.12
parse_mm_msg(?GSM48_MT_MM_IMSI_DETACH_IND, Msg) ->
    {dtap_mm, detach_ind, parse_ies([classmark_1, mobile_id], Msg)};
% 9.2.15
parse_mm_msg(?GSM48_MT_MM_LOC_UPD_REQUEST, Msg) ->
    {dtap_mm, loc_update_req, parse_ies([loc_upd_type, cipher_key_seq, lai, classmark_1, mobile_id], Msg)};
% 9.2.16
parse_mm_msg(?GSM48_MT_MM_STATUS, Msg) ->
    {dtap_mm, mm_status, parse_ies([rej_cause], Msg)};
% 9.2.18
parse_mm_msg(?GSM48_MT_MM_TMSI_REALL_COMPL, _Msg) ->
    {dtap_mm, tmsi_realloc_compl, []}.


parse_rr_msg(Type, Msg) ->
    {dtap_rr, Type, Msg}.


parse_ies(Msg) ->
    parse_ies([], Msg).

parse_ies(Expect, Msg) ->
    parse_ies(Expect, Msg, []).

% first argument: expected non-type-tagged IEs
% second argument: remaining unparsed message
% third: list of IEs parsed so far
% returns: list of IEs
parse_ies(_, <<>>, SoFar) ->
    SoFar;

% 10.5.1.2
parse_ies([cipher_key_seq|T], <<0:1, Seq:3, Rest/bits>>, SoFar) ->
    parse_ies(T, Rest, [{cipher_key_seq, Seq} | SoFar]);
% 10.5.1.3
parse_ies([lai|T], <<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4, LAC:16/big, Rest/bits>>, SoFar) ->
    MCC = MCC3 + MCC2*10 + MCC1*100,
    MNC = MNC2 + MNC1*10,
    parse_ies(T, Rest, [{lai, {MCC, MNC, LAC}} | SoFar]);
% 10.5.1.4
parse_ies([mobile_id|T], <<Length:8, Message:Length/bytes, Rest/bits>>, SoFar) ->
    Ident = common_0408:parse_mobile_id(Message),
    parse_ies(T, Rest, [{mobile_id, Ident} | SoFar]);
% 10.5.1.5
parse_ies([classmark_1|T], <<_:1, Rev:2, Early:1, A51:1, Power:3, Rest/bits>>, SoFar) ->
    parse_ies(T, Rest, [{classmark_1, [{revision, Rev},
				       {early_classmark, Early},
				       {a5_1, A51},
				       {power_class, Power}]}]);
% 10.5.1.8
parse_ies([spare_half|T], <<_Spare:4/bits, Msg/bits>>, SoFar) ->
    parse_ies(T, Msg, SoFar);
% XXX don't yet handle type-tag version
% 10.5.3.5
parse_ies([loc_upd_type|T], <<Follow:1, _:1, TypeBin:2, Rest/bits>>, SoFar) ->
    Type = case TypeBin of
               2#00 -> normal;
               2#01 -> periodic;
               2#10 -> imsi_attach;
               2#11 -> undefined
           end,
    parse_ies(T, Rest, [{loc_upd_type, [{type, Type}, {follow_on, Follow}]} | SoFar]);
% The bearer capability is a type 4 information element with a minimum
% length of 3 octets and a maximum length of 10 octets.  NOTE: The
% coding of the octets of the bearer capability information element is
% not conforming to TS CCITT Q.931.
%
% This is a 10-octet IE where every bit means something different.
%
% 10.5.4.5
parse_ies([bearer_cap|T], <<Length:8, Bearer:Length/binary, Rest/bits>>, SoFar) ->
    parse_ies(T, Rest, [{bearer_cap, {unparsed, Bearer}} | SoFar]);
parse_ies([], <<?GSM48_IE_BEARER_CAP:8, Rest/bits>>, SoFar) ->
    parse_ies([bearer_cap], Rest, SoFar);
% 10.5.4.12
parse_ies([cong_lev|T], <<Cong:4, Rest/bits>>, SoFar) ->
    case Cong of
	2#0000 ->
	    parse_ies(T, Rest, [{congestion, ready} | SoFar]);
	2#1111 ->
	    parse_ies(T, Rest, [{congestion, not_ready} | SoFar])
    end;
% 10.5.4.11
parse_ies([cause|T], <<Length:8, Cause:Length/binary, Rest/bits>>, SoFar) ->
    parse_ies(T, Rest, [{cause, {unparsed, Cause}} | SoFar]);
parse_ies([], <<?GSM48_IE_CAUSE:8, Msg/bits>>, SoFar) ->
    parse_ies([cause], Msg, SoFar);
% 10.5.4.15
parse_ies([facility|T], <<Length:8, Facility:Length/binary, Rest/bits>>, SoFar) ->
    parse_ies(T, Rest, [{facility, {unparsed, Facility}} | SoFar]);
parse_ies([], <<?GSM48_IE_FACILITY:8, Msg/bits>>, SoFar) ->
    parse_ies([facility], Msg, SoFar);
% 10.5.4.17
parse_ies([keypad_fac|T], <<Char:8, Rest/bits>>, SoFar) ->
    parse_ies(T, Rest, [{keypad, Char} | SoFar]);
parse_ies([], <<?GSM48_IE_KPD_FACILITY:8, Msg/bits>>, SoFar) ->
    parse_ies([keypad_fac], Msg, SoFar);
% 10.5.4.25
parse_ies([user_user|T], <<Length:8, Data:Length/binary, Rest/bits>>, SoFar) ->
    parse_ies(T, Rest, [{user_user, {unparsed, Data}}|SoFar]);
parse_ies([], <<?GSM48_IE_USER_USER:8, Msg/bits>>, SoFar) ->
    parse_ies([user_user], Msg, SoFar).


