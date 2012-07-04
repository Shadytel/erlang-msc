-module(mobile_fsm).
-author('Duncan Smith <Duncan@xrtc.net>').
-behaviour(gen_fsm).

% api
-export([start_link/2, incoming/2]).

% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

% state callbacks
-export([st_idle/2]).

% This module defines the MSC's model of a mobile station's behavior.
%
% A link to the running mobile_fsm for that mobile is located in the
% VLR.
%
% A mobile_fsm is created upon each establishment of a layer 2
% connection.  If this turns out to be a duplicate, the new mobile_fsm
% is responsible for hunting down the elder mobile_fsm for the same
% handset, informing it of the new session, and destroying itself.

-define(SERVER, ?MODULE).

start_link(LocalRef, Sender) ->
    io:format("Starting mobile_fsm link ...~n", []),
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
    {reply, {error, no_sync_events_for_mobiles}, StateName, Data}.

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


%% ============================================================
%% States
st_idle(Event, Data) ->
    io:format("Mobile got message ~p~n", [Event]),
    {next_state, idle, Data}.
