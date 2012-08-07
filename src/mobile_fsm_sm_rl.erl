-module(mobile_fsm_sm_rl).
-author('Duncan Smith <Duncan@xrtc.net>').
-behaviour(gen_fsm).

-include_lib("emsc/include/bssmap.hrl").
-include_lib("emsc/include/dtap.hrl").
-include_lib("emsc/include/timers.hrl").

% api
-export([start_link/1]).

% external interfaces
-export([incoming/2]).

% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

% state callbacks
-export([
	]).

-define(SERVER, mobile_cc_fsm).

start_link(Downlink) ->
    io:format("Starting mobile_cc_fsm~n", []),
    gen_fsm:start_link(mobile_cc_fsm, [Downlink], []).

init([Downlink]) ->
    {ok, st_idle, [{downlink, Downlink}]}.

st_idle(Foo, Data) ->
    io:format("CC in idle got message ~p~n", [Foo]),
    {ok, st_idle, Data}.

handle_event(_,_,_) ->
    ok.

handle_sync_event(_,_,_,_) ->
    ok.

handle_info(_,_,_) ->
    ok.

terminate(_,_,_) ->
    ok.

code_change(_,_,_,_) ->
    ok.

% message from lower layer
incoming(_,_) ->
    ok.
