-module(mobile_rr_fsm).
-author('Duncan Smith <Duncan@xrtc.net>').
-behaviour(gen_fsm).

% api
-export([start_link/2, incoming/2, assign/3]).

% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

% state callbacks
-export([st_idle/2, st_con_pend/2, st_dt1/2, st_dt2/2]).

% This module defines the MSC's model of a mobile station's behavior,
% Radio Resource Management sublayer.
%
% A mobile_rr_fsm is created upon each establishment of a layer 2
% connection.  If this turns out to be a duplicate, the new mobile_rr_fsm
% is responsible for hunting down the elder mobile_rr_fsm for the same
% handset, informing it of the new session, and destroying itself.

-define(SERVER, ?MODULE).
-define(TIMEOUT_, 8*1000). % milliseconds to wait before closing an idle connection

start_link(LocalRef, Sender) ->
    io:format("Starting mobile_rr_fsm link ...~n", []),
    gen_fsm:start_link(?MODULE, [LocalRef, Sender], []).

%% ============================================================
%% gen_fsm interworking

init([LocalRef, Sender]) ->
    {ok, st_idle, [{localref, LocalRef}]}.

%% handle_event is only used for events that ought to be handled the
%% same in every state.  RR sublayer has as yet no such events.

%% StateName is an atom
%%
%% Returns: {next_state, NextName, NewData} |
%%          {next_state, NextName, NewData, Timeout} |
%%          {next_state, NextName, NewData, hibernate} |
%%          {stop, Reason, NewData}
handle_event(Event, StateName, Data) ->
    io:format("Mobile-RR got ~p while in ~p~n", [Event, StateName]),
    {next_state, StateName, Data}.

handle_sync_event(_Event, _From, StateName, Data) ->
    {reply, {error, "No sync events for mobiles"}, StateName, Data}.

handle_info(Info, StateName, Data) ->
    {next_state, StateName, Data}.

terminate(Reason, StateName, Data) ->
    ok.

code_change(Old, StateName, Data, Extra) ->
    {ok, StateName, Data}.

%% ============================================================
%% external api

% Accept a message from the mobile station (BSS direction)
incoming(FsmRef, Message) ->
    gen_fsm:send_event(FsmRef, {in, Message}).

assign(FsmRef, Downlink, Message) ->
    gen_fsm:send_event(FsmRef, {assign, Downlink, Message}).


%% ============================================================
%% States

% no dedicated channel established
st_idle({rr_est_req, Msg}, Data) ->
    io:format("Mobile got establish request ~p~n", [Msg]),
    %% XXX
    proplists:get_value(downlink, Data) ! {establish, "give me an rr"},
    {next_state, st_con_pend, Data};
st_idle({assign, Downlink, Msg}, Data) ->
    % this message comes from the down-link side
    io:format("Mobile got RR channel assignment~n", []),
    mobile_mm_fsm:rr_est_ind(proplists:get_value(uplink, Data), Msg),
    {next_state, st_dt1, [ {downlink, Downlink} | Data ]};
st_idle({rr_unit_data_req, Msg}, Data) ->
    % ignore unacknowledged data sent when there is no connection
    {next_state, st_idle, Data};
st_idle({rr_rel_req, _Cause}, Data) ->
    % nothing to release, so no worries mate
    {next_state, st_idle, Data}.

% connection pending
st_con_pend({rr_unit_data_req, Msg}, Data) ->
    % connection's not up yet, can't send unitdata, no worries
    {next_state, st_con_pend, Data};
st_con_pend({rr_rel_req, Cause}, Data) ->
    mobile_mm_fsm:rr_rel_ind(Cause),
    {next_state, st_idle, Data};
st_con_pend({connect}, Data) ->
    % this message comes from the down-link side
    mobile_mm_fsm:rr_est_cnf(proplists:get_value(uplink, Data)),
    {next_state, st_dt1, Data}.

% dedicated channel established
st_dt1({rr_data_req, Msg}, Data) ->
    proplists:get_value(downlink, Data) ! {sccp_message, undefined, proplists:get_value(localref, Data), Msg},
    {next_state, st_dt1, Data};
st_dt1({rr_unit_data_req, Msg}, Data) ->
    % XXX do I need to do something special here
    proplists:get_value(downlink, Data) ! {sccp_message, undefined, proplists:get_value(localref, Data), Msg},
    {next_state, st_dt1, Data};
st_dt1({sccp_message_in, Msg}, Data) ->
    % message passed up from lower layer
    Type = foo,
    % determine whether it's acknowledged or not, relay appropriately
    % as rr_data_ind or rr_unit_data_ind
    proplists:get_value(uplink, Data) ! foo;




st_dt1(Event, Data) ->
    {next_state, st_dt1, Data}.

% dedicated channel established, with ciphering
st_dt2(Event, Data) ->
    {next_state, st_dt2, Data}.
