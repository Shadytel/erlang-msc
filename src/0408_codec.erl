-module(0408_codec).
-author('Duncan Smith <Duncan@xrtc.net>').

parse_msg(<<Skip:4/bits, Discrim:4/bits, Type:8/bits, Msg/binary>>) ->
    case Skip of
	0 ->
	    {};
	_ ->
	    parse_msg(Discrim, Type, Msg)
    end.

parse_msg(2#0011, Type, Msg) ->
    parse_cc_msg(Type, Msg);
parse_msg(2#0101, Type, Msg) ->
    parse_mm_msg(Type, Msg);
parse_msg(2#0110, Type, Msg) ->
    parse_rr_msg(Type, Msg);
parse_msg(Discrim, Type, Msg) ->
    {dtap_unknown, Discrim, Type, Msg}.

% This file only contains routines to parse messages and elements that
% are defined to come from mobile stations, and generation routines
% will only handle messages and elements that are sent to the mobile
% station.  That is, this file is only designed for land-side use.

% 9.3.1
parse_cc_msg(?GSM48_MT_CC_ALERTING, Msg) ->
    {dtap_cc, alerting, parse_ies(Msg)};
% 9.3.2
parse_cc_msg(?GSM48_MT_CC_CALL_CONF, Msg) ->
    {dtap_cc, call_confirm, parse_ies(Msg)};
% 9.3.4
parse_cc_msg(?GSM48_MT_CC_CONG_CTRL, Msg) ->
    {dtap_cc, congestion_ctrl, parse_ies([cong_lev, spare_half], Msg)};
% 9.3.5
parse_cc_msg(?GSM48_MT_CC_CONNECT, Msg) ->
    {dtap_cc, connect, parse_ies(Msg)};
% 9.3.6
parse_cc_msg(?GSM48_MT_CC_CONNECT_ACK, Msg) ->
    {dtap_cc, connect_ack, []};
% 9.3.7
parse_cc_msg(?GSM48_MT_CC_DISCONNECT, Msg) ->
    % XXX it seems that the Cause in this case is sent as a LV IE
    % rather than a TLV IE as defined in 10.5.4.11.  Investigate this.
    {dtap_cc, disconnect, parse_ies([cause], Msg)};
% 9.3.8
parse_cc_msg(?GSM48_MT_CC_EMERG_SETUP, Msg) ->
    {dtap_cc, emerg_setup, parse_ies()};
% 9.3.9.2
parse_cc_msg(?GSM48_MT_CC_FACILITY, Msg) ->
    {dtap_cc, facility, parse_ies([facility], Msg)};
% 9.3.10
parse_cc_msg(?GSM48_MT_CC_HOLD, Msg) ->
    {dtap_cc, hold, []};
% 9.3.13
parse_cc_msg(?GSM48_MT_CC_MODIFY, Msg) ->
    % XXX bearer_cap here is another LV/TLV mismatch.
    {dtap_cc, modify, parse_ies([bearer_cap], Msg)};
% 9.3.14
parse_cc_msg(?GSM48_MT_CC_MODIFY_COMPL, Msg) ->
    % XXX bearer_cap here is another LV/TLV mismatch.
    {dtap_cc, modify_compl, parse_ies([bearer_cap], Msg)};
% 9.3.15
parse_cc_msg(?GSM48_MT_CC_MODIFY_REJECT, Msg) ->
    % XXX bearer_cap and cause here are another LV/TLV mismatch.
    {dtap_cc, modify_rej, parse_ies([bearer_cap, cause], Msg)};
% 9.3.16
parse_cc_msg(?GSM48_MT_CC_NOTIFY, Msg) ->
    {dtap_cc, notify, parse_ies([notif_ind], Msg)};
% 9.3.18
parse_cc_msg(?GSM48_MT_CC_RELEASE, Msg) ->
    {dtap_cc, release, parse_ies(Msg)};
% 9.3.19
parse_cc_msg(?GSM48_MT_CC_RELEASE_COMPL, Msg) ->
    {dtap_cc, release_compl, parse_ies(Msg)};
% 9.3.20
parse_cc_msg(?GSM48_MT_CC_RETR, Msg) ->
    {dtap_cc, retrieve, []};
% 9.3.23.2
parse_cc_msg(?GSM48_MT_CC_SETUP, Msg) ->
    % there are mandatory IEs, but they are all type-tagged.
    {dtap_cc, setup, parse_ies(Msg)};
% 9.3.24
parse_cc_msg(?GSM48_MT_CC_START_DTMF, Msg) ->
    {dtap_cc, dtmf_start, parse_ies([keypad_fac], Msg)};
% 9.3.27
parse_cc_msg(?GSM48_MT_CC_STATUS, Msg) ->
    {dtap_cc, status, parse_ies([cause, call_state], Msg)};
% 9.3.28
parse_cc_msg(?GSM48_MT_CC_STATUS_ENQ, Msg) ->
    {dtap_cc, status_enq, []};
% 9.3.29
parse_cc_msg(?GSM48_MT_CC_STOP_DTMF, Msg) ->
    {dtap_cc, dtmf_stop, []};
% 9.3.31
parse_cc_msg(?GSM48_MT_CC_USER_INFO, Msg) ->
    {dtap_cc, user_info, parse_ies([user_user], Msg)}.



% 9.2.3
parse_mm_msg(?GSM48_MT_MM_AUTH_RESP, Msg) ->
    {dtap_mm, auth_resp, parse_ies([auth_param_sres], Msg)};
% 9.2.4
parse_mm_msg(?GSM48_MT_MM_CM_REEST_REQ, Msg) ->
    {dtap_mm, reest_req, parse_ies([cipher_key_seq, spare_half, classmark_2, mobile_ident, lai], Msg)};
% 9.2.7
parse_mm_msg(?GSM48_MT_MM_CM_SERV_ABORT, Msg) ->
    {dtap_mm, serv_abort, []};
% 9.2.9
parse_mm_msg(?GSM48_MT_MM_CM_SERV_REQ, Msg) ->
    {dtap_mm, serv_req, parse_ies([cm_serv_type, cipher_key_seq, classmark_2, mobile_ident], Msg)};
% 9.2.11
parse_mm_msg(?GSM48_MT_MM_ID_RESP, Msg) ->
    {dtap_mm, ident_resp, parse_ies([mobile_ident], Msg)};
% 9.2.12
parse_mm_msg(?GSM48_MT_MM_IMSI_DETACH_IND, Msg) ->
    {dtap_mm, detach_ind, parse_ies([classmark_1, mobile_ident], Msg)};
% 9.2.15
parse_mm_msg(?GSM48_MT_MM_LOC_UPD_REQUEST, Msg) ->
    {dtap_mm, loc_update_req, parse_ies([loc_upd_type, cipher_key_seq, lai, classmark_1, mobile_ident], Msg)};
% 9.2.16
parse_mm_msg(?GSM48_MT_MM_STATUS, Msg) ->
    {dtap_mm, mm_status, parse_ies([rej_cause], Msg)};
% 9.2.18
parse_mm_msg(?GSM48_MT_MM_TMSI_REALL_COMPL, Msg) ->
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
parse_ies([], <<>>, SoFar) ->
    SoFar;

% 10.5.1.8
parse_ies([spare_half|T], <<Spare:4/bits, Msg/binary>>, SoFar) ->
    parse_ies(T, Msg, SoFar);
% The bearer capability is a type 4 information element with a minimum
% length of 3 octets and a maximum length of 10 octets.  NOTE: The
% coding of the octets of the bearer capability information element is
% not conforming to TS CCITT Q.931.
%
% This is a 10-octet IE where every bit means something different.
%
% 10.5.4.5
parse_ies([bearer_cap|T], <<Length:8, Bearer:Length/binary, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{bearer_cap, {unparsed, Bearer}} | SoFar]);
parse_ies([], <<?GSM48_IE_BEARER_CAP:8, Rest/binary>>, SoFar) ->
    parse_ies([bearer_cap], Rest, SoFar);
% 10.5.4.12
parse_ies([cong_lev|T], <<Cong:4/bits, Msg/binary>>, SoFar) ->
    case Cong of
	2#0000 ->
	    parse_ies(T, Msg, [{congestion, ready} | SoFar]);
	2#1111 ->
	    parse_ies(T, Msg, [{congestion, not_ready} | SoFar])
    end;
% 10.5.4.11
parse_ies([cause|T], <<Length:8, Cause:Length/binary, Rest/binary>>, SoFar) ->
    parse_ies(T, Msg, [{cause, {unparsed, Cause}} | SoFar]);
parse_ies([], <<?GSM48_IE_CAUSE:8, Msg/binary>>, SoFar) ->
    parse_ies([cause], Msg, SoFar);
% 10.5.4.15
parse_ies([facility|T], <<Length:8, Fac:Length/binary, Rest/binary>>, SoFar) ->
    parse_ies(T, Msg, [{facility, {unparsed, Facility}} | SoFar]);
parse_ies([], <<?GSM48_IE_FACILITY:8, Msg/binary>>, SoFar) ->
    parse_ies([facility], Msg, SoFar);
% 10.5.4.17
parse_ies([keypad_fac|T], <<Char:8, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{keypad, Char} | SoFar]);
parse_ies([], <<?GSM48_IE_KPD_FACILITY:8, Rest/binary>>, Sofar) ->
    parse_ies([keypad_fac], Rest, SoFar);
% 10.5.4.25
parse_ies([user_user|T], <<Length:8, Data:Length/binary, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{user_user, {unparsed, Data}}|SoFar]);
parse_ies([], <<?GTM48_IE_USER_USER:8, Msg/binary>>, SoFar) ->
    parse_ies([user_user], Msg, SoFar);
parse_ies() ->



