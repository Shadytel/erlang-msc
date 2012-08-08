-module(codec_sm_rp).
-author("Duncan Smith <Duncan@xrtc.net>").

-define(RP_DATA_MO, 2#000).
-define(RP_DATA_MT, 2#001).
-define(RP_ACK_MO, 2#010).
-define(RP_ACK_MT, 2#011).
-define(RP_ERROR_MO, 2#100).
-define(RP_ERROR_MT, 2#101).
-define(RP_SMMA_MO, 2#110).


parse_message(mt, <<?RP_DATA_MT:8, Rest/bytes>>) ->
    parse_message(mo, <<?RP_DATA_MO:8, Rest/bytes>>);
parse_message(mo, <<?RP_DATA_MO:8, Rest/bytes>>) ->
    {rp_data, [parse_ies([orig_addr, dest_addr, user_data], Rest)]};

parse_message(mt, <<?RP_ACK_MT:8, Rest/bytes>>) ->
    parse_message(mo, <<?RP_ACK_MO:8, Rest/bytes>>);
parse_message(mo, <<?RP_ACK_MO:8, Rest/bytes>>) ->
    {rp_ack, []};

parse_message(mt, <<?RP_ERROR_MT:8, Rest/bytes>>) ->
    parse_message(mo, <<?RP_ERROR_MO:8, Rest/bytes>>);
parse_message(mo, <<?RP_ERROR_MO:8, Rest/bytes>>) ->
    % RP-ERROR
    {rp_error, parse_ies([cause, user_data], Rest)};

% this one is MO-only
parse_message(mo, <<?RP_SMMA_MO:8, Rest/bytes>>) ->
    {rp_smma, []}.


parse_ies([orig_addr|T], <<Length:8, Addr:Length/bytes, Rest/bytes>>) ->
    case Length of
	0 -> parse_ies(T, Rest);
	_ -> [{orig_addr, common_0408:parse_dest_number(Addr)} | parse_ies(T, Rest)]
    end;

parse_ies([dest_addr|T], <<Length:8, Addr:Length/bytes, Rest/bytes>>) ->
    case Length of
	0 -> parse_ies(T, Rest);
	_ -> [{dest_addr, common_0408:parse_dest_number(Addr)} | parse_ies(T, Rest)]
    end;

parse_ies([user_data|T], <<Length:8, User:Length/bytes, Rest/bytes>>) ->
    [{user_data, User} | parse_ies(T, Rest)];

parse_ies([cause|T], <<Length:8, Msg:Length/bytes, Rest/bytes>>) ->
    <<Cause:8, _Diag/binary>> = Msg,
    [{cause, Cause} | parse_ies(T, Rest)].


