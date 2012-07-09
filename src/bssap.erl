-module(bssap).
-author("Duncan Smith <Duncan@xrtc.net>").

% State machines for BSSAP sublayer dispatch (picking between BSSMAP
% and DTAP)

% sccp discriminator types
-define(SCCP_DISCRIM_DTAP,	1).
-define(SCCP_DISCRIM_BSSMAP,	0).

-export([parse_message/1]).

% BSSMAP message
parse_message(<<?SCCP_DISCRIM_BSSMAP:8, Length:8, Message:Length/binary>>) ->
    {ok, bssmap, bssmap_codec:parse_bssmap_msg(Message)};

% DTAP message
parse_message(<<?SCCP_DISCRIM_DTAP:8, DLCI:8, Length:8, Message:Length/binary>>) ->
    {ok, dtap, dtap_codec:parse_message(DLCI, Message)};

% Something else entirely
parse_message(<<Discrim:8, Bin/binary>>) ->
    {ok, unknown, Discrim, Bin}.


