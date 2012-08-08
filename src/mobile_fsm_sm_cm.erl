-module(mobile_fsm_sm_cm).
-author('Duncan Smith <Duncan@xrtc.net>').
-behaviour(gen_fsm).

-include_lib("emsc/include/bssmap.hrl").
-include_lib("emsc/include/dtap.hrl").
-include_lib("emsc/include/timers.hrl").

% api
-export([start_link/2]).

% external interfaces
-export([incoming/2, send/2]).

% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

% state callbacks
-export([st_idle/2,
	 st_conn_pend/2,
	 st_conn_act/2,
	 st_wait_for_ack/2
	]).

-define(SERVER, mobile_fsm_sm_cm).

start_link(from_below, Downlink) ->
    gen_fsm:start_link(mobile_fsm_sm_cm, [below, Downlink], []);

start_link(from_above, Uplink) ->
    gen_fsm:start_link(mobile_fsm_sm_cm, [above, Uplink], []).

% ============================================================
% External messages

% got a message from lower layer
incoming(FsmRef, Message) ->
    ok.

% got a message from upper layer
send(FsmRef, Message) ->
    ok.

% ============================================================
% State handlers

init([Downlink]) ->
    {ok, st_conn_act, [{downlink, Downlink}]}.

st_idle(Msg, Data) ->
    io:format("CM in idle got message ~p~n", [Msg]),
    {ok, st_idle, Data}.

st_conn_pend(Msg, Data) ->
    io:format("CM waiting for connection got ~p~n", [Msg]),
    {ok, st_conn_pend, Data}.

st_conn_act(Msg, Data) ->
    io:format("CM in connection got ~p~n", [Msg]),
    {ok, st_conn_act, Data}.

st_wait_for_ack(Msg, Data) ->
    io:format("CM in ack-wait got ~p~n", [Msg]),
    {ok, st_wait_for_ack, Data}.

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
