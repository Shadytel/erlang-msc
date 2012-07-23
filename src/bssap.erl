-module(bssap).
-author("Duncan Smith <Duncan@xrtc.net>").

% State machines for BSSAP sublayer dispatch (picking between BSSMAP
% and DTAP)

% sccp discriminator types
-define(SCCP_DISCRIM_DTAP,	1).
-define(SCCP_DISCRIM_BSSMAP,	0).

-export([parse_message/1, encode_message/1]).

% BSSMAP message
parse_message(<<?SCCP_DISCRIM_BSSMAP:8, Length:8, Message:Length/binary>>) ->
    {bssmap, bssmap_codec:parse_bssmap(Message)};

% DTAP message
parse_message(<<?SCCP_DISCRIM_DTAP:8, DLCI:8, Length:8, Message:Length/binary>>) ->
    {dtap, codec_0408:parse_message(DLCI, Message)};

% Something else entirely
parse_message(<<Discrim:8, Bin/binary>>) ->
    {unknown, Discrim, Bin}.


encode_message({bssmap, Msg}) ->
    MsgBin = bssmap_codec:encode_bssmap(Msg),
    Len = byte_size(MsgBin),
    <<?SCCP_DISCRIM_BSSMAP:8, Len:8, MsgBin/binary>>;

encode_message({dtap, Msg}) ->
    {DLCI, MsgBin} = dtap_codec:encode_message(Msg),
    Len = byte_size(MsgBin),
    <<?SCCP_DISCRIM_DTAP:8, DLCI:8, Len:8, MsgBin/binary>>.



