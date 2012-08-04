-module(mobile_mm_fsm).
-author('Duncan Smith <Duncan@xrtc.net>').
-behaviour(gen_fsm).

-include_lib("emsc/include/bssmap.hrl").
-include_lib("emsc/include/dtap.hrl").
-include_lib("emsc/include/timers.hrl").

% api
-export([start_link/3]).

% external interfaces to the downward layer
-export([rr_est_ind/2, rr_rel_ind/1, rr_est_cnf/1, incoming/2]).

% external interfaces to the upward layer
-export([txn_gimme/1, txn_begin/2, txn_end/2, outgoing/2]).

% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

% state callbacks
-export([st_idle_offl/2,
	 st_idle/2,
	 st_wait_for_rr/2,
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

-define(SERVER, mobile_mm_fsm).

start_link(LocalRef, RemoteRef, Sender) ->
    io:format("Starting mobile_mm_fsm link ...~n", []),
    gen_fsm:start_link(mobile_mm_fsm, [LocalRef, RemoteRef, Sender], []).

%% ============================================================
%% gen_fsm interworking

init([LocalRef, RemoteRef, Sender]) ->
    {ok, st_idle_offl, [{localref, LocalRef}, {remoteref, RemoteRef}, {downlink, Sender}]}.

%% StateName is an atom
%%
%% Returns: {next_state, NextName, NewData} |
%%          {next_state, NextName, NewData, Timeout} |
%%          {next_state, NextName, NewData, hibernate} |
%%          {stop, Reason, NewData}

%handle_event(Event, StateName, Data) ->
%    io:format("Mobile got ~p while in ~p~n", [Event, StateName]),
%    {next_state, StateName, Data}.

handle_event(Event, StateName, Data) ->
    case Event of
	{kill, Cause} ->
	    {stop, {cause, Cause}, Data};
	{rr_est_ind, Downlink} ->
	    {next_state, st_idle, [{downlink, Downlink}]};
	{rr_rel_ind} ->
	    {next_state, st_idle, Data};
	_ ->
	    {reply, {error, invalid_all_state_event}, StateName, Data}
    end.

handle_sync_event(_Event, _From, StateName, Data) ->
    {next_state, StateName, Data}.

handle_info(Info, StateName, Data) ->
    {next_state, StateName, Data}.

terminate(Reason, StateName, Data) ->
    ok.

code_change(Old, StateName, Data, Extra) ->
    {ok, StateName, Data}.


send_to_mobile(Data, Message) ->
    proplists:get_value(downlink, Data) !
	{sccp_data_out,
	 proplists:get_value(localref, Data),
	 proplists:get_value(remoteref, Data),
	 bssap:encode_message(Message)}.

replace_data(Data, {Type, Value}) ->
    [{Type, Value} | proplists:delete(Type, Data)];
replace_data(Data, []) ->
    Data;
replace_data(Data, [{Type, Value}|T]) ->
    replace_data(replace_data(Data, {Type, Value}), T).


%% ============================================================
%% external interfaces to the downward layer

% Accept a message from the mobile station
incoming(FsmRef, Message) ->
    gen_fsm:send_event(FsmRef, bssap:parse_message(Message)).
% function used internally only, put here for consistency
incoming_0408(FsmRef, Message) ->
    gen_fsm:send_event(FsmRef, codec_0408:parse_message(Message)).

terminate(FsmRef, Cause) ->
    gen_fsm:send_all_state_event(FsmRef, {kill, Cause}).

% messages from the RR layer

% Now I know where my downlink is
rr_est_ind(FsmRef, Downlink) ->
    io:format("rr_est_ind from rr to mm~n"),
    gen_fsm:send_event(FsmRef, {rr_est_ind, Downlink}).

rr_rel_ind(FsmRef) ->
    io:format("rr_rel_ind from rr to mm~n"),
    gen_fsm:send_all_state_event(FsmRef, {rr_rel_ind}).


rr_est_cnf(FsmRef) ->
    io:format("rr_est_cnf from rr to mm~n"),
    gen_fsm:send_all_state_event(FsmRef, {rr_est_cnf}).

%% ============================================================
%% external interfaces to the upward layer

txn_gimme(FsmRef) ->
    gen_fsm:send_all_state_sync_event(FsmRef, {txn_gimme}).

txn_begin(FsmRef, TxnId) ->
    gen_fsm:send_all_state_event(FsmRef, {txn_begin, TxnId, self()}).

txn_end(FsmRef, TxnId) ->
    gen_fsm:send_all_state_event(FsmRef, {txn_end, TxnId, self()}).

outgoing(FsmRef, Message) ->
    gen_fsm:send_event(FsmRef, {msg_out, Message}).


%% ============================================================
%% States

% IDLE
% No RR connection exists, and no MM procedures are running.
st_idle_offl({rr_est_ind, Downlink}, Data) ->
    NewData = replace_data(Data, {downlink, Downlink}),
    {next_state, st_idle, NewData}.

% An RR connection exists, but no MM procedures are running.
st_idle({rr_est_ind, _}, Data) ->
    {next_state, st_idle, Data};
st_idle({dtap_mm, ?GSM48_MT_MM_LOC_UPD_REQUEST, Args}, Data) ->
    io:format("Permitting LU of ~p by default~n", [proplists:get_value(mobile_id, Args)]),
    {ID_t, ID_value} = proplists:get_value(mobile_id, Args),
    NewData = replace_data(Data, [{classmark_1, proplists:get_value(classmark_1, Args)},
				  {ID_t, ID_value}]),
    send_to_mobile(NewData, {dtap, {dtap_mm,
				    ?GSM48_MT_MM_LOC_UPD_ACCEPT,
				    [{lai, {313, 37, 1}}]}}),
    {next_state, st_idle, NewData};
st_idle({dtap_mm, ?GSM48_MT_MM_CM_REEST_REQ, Args}, Data) ->
    send_to_mobile(Data, {dtap, {dtap_mm,
				 ?GSM48_MT_MM_CM_SERV_REJ,
				 [{rej_cause, 2#00100000} % service option not supported, 10.5.3.6
				  ]}}),
    {next_state, st_idle, Data};
st_idle({dtap_mm, ?GSM48_MT_MM_CM_SERV_REQ, Args}, Data) ->
    NewData = lists:append([{classmark2, proplists:get_value(classmark_2, Args)}],
			   proplists:delete(classmark_2, Data)),
    case proplists:get_value(cm_serv_type, Args) of
	mo_call ->
	    mobile_cc_fsm:start_link(self()),
	    send_to_mobile(NewData, {dtap, {dtap_mm,
					    ?GSM48_MT_MM_CM_SERV_ACC,
					    []}}),
	    {next_state, st_mm_conn_act, NewData};
	sms ->
	    mobile_sms_fsm:start_link(self()),
	    send_to_mobile(NewData, {dtap, {dtap_mm,
					    ?GSM48_MT_MM_CM_SERV_ACC,
					    []}}),
	    {next_state, st_mm_conn_act, NewData};
	_ -> % add USSD state machine call here
	    send_to_mobile(NewData, {dtap, {dtap_mm,
					    ?GSM48_MT_MM_CM_SERV_REJ,
					    [{rej_cause, 2#00100000} % service option not supported, 10.5.3.6
					    ]}}),
	    {next_state, st_idle, NewData}
    end;
st_idle({bssmap, ?BSSMAP_CLASSMARK_UPD, Args}, Data) ->
    io:format("Mobile in idle got classmark~n"),
    Cm2 = proplists:get_value(classmark2, Args),
    Cm3 = proplists:get_value(classmark3, Args),
    Dlci = proplists:get_value(dlci, Data),
%    send_to_mobile(Data, {bssmap, ?BSSMAP_CLASSMARK_UPD, [{mobile_id, imsi}]}),
						% FIXME transaction probably ought not be 0
    {next_state, st_idle, [{classmark2, Cm2}, {classmark3, Cm3} | Data]};
st_idle({bssmap, ?BSSMAP_COMPL_L3_INF, Params}, Data) ->
    {unparsed, MsgBin} = proplists:get_value(l3_message, Params),
    incoming_0408(self(), MsgBin),
    {next_state, st_idle, Data};
st_idle({bssmap, Type, Params}, Data) ->
    io:format("Mobile in conn got unk BSSMAP ~p message ~p~n", [Type, Params]),
    {next_state, st_idle, Data};
st_idle({Tag, Type, Params}, Data) ->
    io:format("Mobile in idle got unk ~p:~p message ~p~n", [Tag, Type, Params]),
    {next_state, st_idle, Data}.


% MM CONNECTION ACTIVE
st_mm_conn_act({dtap_mm, T = ?GSM48_MT_MM_CM_SERV_REQ, Args}, Data) ->
    % Request for an additional connection.  Follow the same procedure
    % as in idle state, but don't return to idle state if it fails.
    {next_state, State, NewData} = st_idle({dtap_mm, T, Args}, Data),
    {next_state, st_mm_conn_act, NewData};
st_mm_conn_act({Tag, Type, Params}, Data) ->
    io:format("Mobile in conn got unk ~p:~p message ~p~n", [Tag, Type, Params]),
    {next_state, st_mm_conn_act, Data}.


% WAIT FOR RR CONNECTION
st_wait_for_rr(Event, Data) ->
    {next_state, st_wait_for_rr, Data}.


% IDENTIFICATION INITIATED
st_ident_inited(timeout, Data) ->
    {next_state, st_mm_ident_inited, Data};
st_ident_inited({dtap_mm, ?GSM48_MT_MM_ID_RESP, Args}, Data) ->
    io:format("Mobile got id response ~p~n", [Args]),
    {next_state, st_mm_ident_inited, Data};
st_ident_inited({T, Type, Args}, Data) ->
    io:format("Mobile got unknown ~p:~p response ~p~n", [T, Type, Args]),
    {next_state, st_mm_ident_inited, Data}.

% AUTHENTICATION INITIATED
st_auth_inited(timeout, Data) ->
    {next_state, st_mm_ident_inited, Data};
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
