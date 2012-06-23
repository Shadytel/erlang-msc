-module(sccp_machine).
-author('Duncan Smith <Duncan@xrtc.net').
-include_lib("osmo_ss7/include/osmo_ss7.hrl").
-include_lib("emsc/include/ipaccess.hrl").

-export([boot_link/1]).

boot_link(Socket) ->
    ipa_proto:register_stream(Socket, ?IPAC_PROTO_SCCP, {callback_fn, fun rx_sccp/4, [nil]}),
    io:format("Sending hello~n", []),
    ipa_proto:send(Socket, 254, << 6 >>),
    ok.

handle_message(Socket, StreamID, DataBin, Args) ->
    ok.

rx_sccp(_Socket, Port, Data, [Args]) ->
    io:format("Got ~p message ~p~n", [Port, sccp_codec:parse_sccp_msg(Data)]).


%% hello, msc:
% Got 253 message {ok,{sccp_msg,9,
%                         [{protocol_class,{0,0}},
%                          {called_party_addr,
%                              {sccp_addr,0,1,undefined,254,undefined}},
%                          {calling_party_addr,
%                              {sccp_addr,0,1,undefined,254,undefined}},
%                          {user_data,<<0,4,48,4,1,32>>}]}}

%% roam request:
% Got 253 message {ok,{sccp_msg,1,
%                         [{src_local_ref,131075},
%                          {protocol_class,{2,0}},
%                          {called_party_addr,
%                              {sccp_addr,0,1,undefined,254,undefined}},
%                          {user_data,
%                              <<0,31,87,5,8,0,19,3,115,0,1,0,1,23,18,5,8,
%                                112,19,3,115,255,254,48,8,57,49,115,0,0,0,2,
%                                32>>}]}}

% GSM 08.06, sec 6.3:
%
% The user data field is a mandatory parameter of the Data frames
% (DT); the user data field always contains either a DTAP or a BSSMAP
% message.

% todo:
% 1) support SCCP connections (Q.713, I think)
% 2) pass along user data to DTAP/BSSMAP parsers -> handlers
