-module(mobile_mm_fsm).
-author('Duncan Smith <Duncan@xrtc.net>').
-behaviour(gen_fsm).

-include_lib("emsc/include/bssmap.hrl").
-include_lib("emsc/include/dtap.hrl").
-include_lib("emsc/include/timers.hrl").

% api
-export([rr_est_ind/2, rr_rel_ind/1, rr_est_cnf/1, start_link/3]).

% external interfaces
-export([incoming/2, assign/3]).

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
	 st_wait_mo_mm/2
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

-define(SERVER, mobile_mm_fsm).

start_link(LocalRef, RemoteRef, Sender) ->
    io:format("Starting mobile_mm_fsm link ...~n", []),
    gen_fsm:start_link(mobile_mm_fsm, [LocalRef, RemoteRef, Sender], []).

%% ============================================================
%% gen_fsm interworking

init([LocalRef, RemoteRef, Sender]) ->
    {ok, st_idle, [{localref, LocalRef}, {remoteref, RemoteRef}, {downlink, Sender}]}.

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

% Now I know where my downlink is
assign(FsmRef, Downlink, Message) ->
    ok.

% Accept a message from the mobile station
incoming(FsmRef, Message) ->
    gen_fsm:send_event(FsmRef, bssap:parse_message(Message)).

terminate(FsmRef, Cause) ->
    gen_fsm:send_sync_event(FsmRef, {kill, Cause}).

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
st_idle({bssmap, {?BSSMAP_CLASSMARK_UPD, Args}}, Data) ->
    io:format("Mobile in idle got classmark~n"),
    Cm2 = proplists:get_value(classmark2, Args),
    Cm3 = proplists:get_value(classmark3, Args),
    Dlci = proplists:get_value(dlci, Data),
    Dlci = 0,
    proplists:get_value(downlink, Data) !
	{sccp_data_out,
	 proplists:get_value(localref, Data),
	 proplists:get_value(remoteref, Data),
	 bssap:encode_message({bssmap, {?BSSMAP_CLASSMARK_UPD, 0, [{mobile_id, imsi}]}})
	 % FIXME transaction probably ought not be 0
	},
    {next_state, st_idle, [{classmark2, Cm2}, {classmark3, Cm3} | Data]};
st_idle({bssmap, {Type, Args}}, Data) ->
    io:format("Mobile in idle got unk BSSMAP ~p message ~p~n", [Type, Args]),
    {next_state, st_idle, Data};
st_idle({dtap, {Type, Args}}, Data) ->
    io:format("Mobile in idle got unk DTAP ~p message ~p~n", [Type, Args]),
    {next_state, st_idle, Data}.

% WAIT FOR RR CONNECTION
st_wait_for_rr(Event, Data) ->
    {next_state, st_wait_for_rr, Data}.

% MM CONNECTION ACTIVE
st_mm_conn_act(Event, Data) ->
    {next_state, st_mm_conn_act, Data}.

% IDENTIFICATION INITIATED
st_ident_inited(timeout, Data) ->
    ok;
st_ident_inited({dtap, {?GSM48_MT_MM_ID_RESP, Args}}, Data) ->
    io:format("Mobile got id response ~p~n", [Args]),
    ok;
st_ident_inited({dtap, {Type, Args}}, Data) ->
    io:format("Mobile got unknown ~p response ~p~n", [Type, Args]),
    {next_state, st_mm_ident_inited, Data}.

% AUTHENTICATION INITIATED
st_auth_inited(timeout, Data) ->
    ok;
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
