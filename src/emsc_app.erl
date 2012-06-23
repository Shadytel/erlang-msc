-module(emsc_app).
-behaviour(application).
-author('Duncan Smith <Duncan@xrtc.net>').

-include_lib("osmo_ss7/include/osmo_util.hrl").
%-include_lib("osmo_ss7/include/ipa.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/osmo_ss7.hrl").
-include_lib("osmo_ss7/include/mtp3.hrl").

-include_lib("emsc/include/ipaccess.hrl").


-export([start/2, stop/1, link_state_handler/0]).

start(_App, _Type) ->
    code:load_file(ipa_proto),
    ipa_proto:init(),
    ipa_proto:start_listen(6666, 1, [], spawn(emsc_app, link_state_handler, [])),

    % well, that's a start, I guess.

    % TODO:

% register SCCP streams with OpenBSC:
% <000b> osmo_bsc_sccp.c:205 Not connected to a MSC. Not forwarding data.

% in this file, register and handle streams for A-protocols

    ok.

stop(_State) ->
    ok.

link_state_handler() ->
    receive
	{ipa_tcp_accept, Socket} ->
	    io:format("Got accept ~w~n", [Socket]),
	    sccp_machine:boot_link(Socket),
	    ok;
	{link_down, _Socket} ->
	    ok
    end,
    emsc_app:link_state_handler().


