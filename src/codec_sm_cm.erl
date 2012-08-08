-module(codec_sm_cm).
-author("Duncan Smith <Duncan@xrtc.net>").

-export([parse_message/1]).

parse_message(<<2#00000001:8, Rest/bytes>>) ->
    % CP-DATA
    {cp_data, [parse_ie(user_data, Rest)]};
parse_message(<<2#00000100:8, Rest/bytes>>) ->
    % CP-ACK
    {cp_ack, []};
parse_message(<<2#00010000:8, Rest/bytes>>) ->
    % CP-ERROR
    {cp_error, [parse_ie(cause, Rest)]}.

parse_ie(user_data, << Length:8, User:Length/bytes >>) ->
    {user_data, User};
parse_ie(cause, <<Cause:8>>) ->
    {cause, Cause}.


