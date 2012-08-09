-module(bssap).
-author("Duncan Smith <Duncan@xrtc.net>").

% State machines for BSSAP sublayer dispatch (picking between BSSMAP
% and DTAP)

% sccp discriminator types
-define(SCCP_DISCRIM_DTAP,	2#00000001).
-define(SCCP_DISCRIM_BSSMAP,	2#00000000).

-export([parse_message/1, encode_message/1]).

% BSSMAP message
parse_message(<<?SCCP_DISCRIM_BSSMAP:8, Length:8, Message:Length/binary, _/binary>>) ->
    bssmap_codec:parse_message(Message);

% DTAP message
parse_message(<<?SCCP_DISCRIM_DTAP:8, DLCI:8, Length:8, Message:Length/binary, _Rest/binary>>) ->
    codec_0408:parse_message(Message);

% Something else entirely
parse_message(<<Discrim:8, Bin/binary>>) ->
    {unknown, Discrim, Bin}.


encode_message({bssmap, Type, Msg}) ->
    MsgBin = bssmap_codec:encode_message({Type, Msg}),
    Len = byte_size(MsgBin),
    <<?SCCP_DISCRIM_BSSMAP:8, Len:8, MsgBin/binary>>;

encode_message({dtap, Msg}) ->
    {DLCI, MsgBin} = codec_0408:encode_message(Msg),
    Len = byte_size(MsgBin),
    <<?SCCP_DISCRIM_DTAP:8, DLCI:8, Len:8, MsgBin/binary>>.



