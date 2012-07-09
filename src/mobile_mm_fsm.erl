-module(mobile_mm_fsm).
-author('Duncan Smith <Duncan@xrtc.net>').
-behaviour(gen_fsm).

% api
-export([rr_est_ind/2, rr_rel_ind/1, rr_est_cnf/1]).

% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

% state callbacks
-export([st_idle/2,
	 st_wait_for_rr/2,
	 st_mm_conn_act/2,
	 st_ident_inited/2,
	 st_auth_inited/2,
	 st_tmsi_inited/2,
	 st_cipher_inited/2,
	 st_wait_mo_mm/2,
	 st_wait_reest/2
	]).

% This module defines the MSC's model of a mobile station's behavior.
%
% A link to the running mobile_mm_fsm for that mobile is located in the
% VLR.
%
% A mobile_mm_fsm is created upon each establishment of a layer 2
% connection.  If this turns out to be a duplicate, the new mobile_mm_fsm
% is responsible for hunting down the elder mobile_mm_fsm for the same
% handset, informing it of the new session, and destroying itself.

-define(SERVER, ?MODULE).

start_link(LocalRef, Sender) ->
    io:format("Starting mobile_mm_fsm link ...~n", []),
    gen_fsm:start_link(?MODULE, [LocalRef, Sender], []).

%% ============================================================
%% gen_fsm interworking

init([LocalRef, Sender]) ->
    {ok, st_idle, [{localref, LocalRef}, {downlink, Sender}]}.

%% StateName is an atom
%%
%% Returns: {next_state, NextName, NewData} |
%%          {next_state, NextName, NewData, Timeout} |
%%          {next_state, NextName, NewData, hibernate} |
%%          {stop, Reason, NewData}
handle_event(Event, StateName, Data) ->
    io:format("Mobile got ~p while in ~p~n", [Event, StateName]),
    {next_state, StateName, Data}.

handle_sync_event(Event, _From, StateName, Data) ->
    case Event of
	{kill, Cause} ->
	    {stop, {cause, Cause}, Data};
	_ ->
	    {reply, {error, invalid_sync_event}, StateName, Data}
    end.

handle_info(Info, StateName, Data) ->
    {next_state, StateName, Data}.

terminate(Reason, StateName, Data) ->
    ok.

code_change(Old, StateName, Data, Extra) ->
    {ok, StateName, Data}.

%% ============================================================
%% external api

% Accept a message from the mobile station
incoming(FsmRef, Message) ->
    gen_fsm:send_event(FsmRef, {in, Message}).

terminate(FsmRef, Cause) ->
    gen_fsm:send_sync_event(FsmRef, {kill, Message}).

% messages from the RR layer
rr_est_ind(_, _) ->
    io:format("rr_est_ind from rr to mm~n").

rr_rel_ind(_) ->
    io:format("rr_rel_ind from rr to mm~n").


rr_est_cnf(_) ->
    io:format("rr_est_cnf from rr to mm~n").

%% ============================================================
%% States

% IDLE
st_idle(Event, Data) ->
    io:format("Mobile got message ~p~n", [Event]),
    {next_state, st_idle, Data}.

% WAIT FOR RR CONNECTION
st_wait_for_rr(Event, Data) ->
    {next_state, st_wait_for_rr, Data}.

% MM CONNECTION ACTIVE
st_mm_conn_act(Event, Data) ->
    {next_state, st_mm_conn_act, Data}.

% IDENTIFICATION INITIATED
st_ident_inited(Event, Data) ->
    {next_state, st_mm_ident_initd, Data}.

% AUTHENTICATION INITIATED
st_auth_inited(Event, Data) ->
    {next_state, st_auth_inited, Data}.

% TMSI REALLOCATION INITIATED
st_tmsi_inited(Event, Data) ->
    {next_state, st_tmsi_inited, Data}.

% CIPHERING MODE INITIATED
st_cipher_inited(Event, Data) ->
    {next_state, st_cipher_inited, Data}.

% WAIT FOR MOBILE ORIGINATED MM CONNECTION
st_wait_mo_mm(Event, Data) ->
    {next_state, st_wait_mo_mm, Data}.

% WAIT FOR REESTABLISHMENT
st_wait_reest(Event, Data) ->
    {next_state, st_wait_reest, Data}.

