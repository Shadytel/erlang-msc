-module(hlr_server).
-author('Duncan Smith <Duncan@xrtc.net>').
-behaviour(gen_server).

-define(SERVER, hlr).

-compile(export_all).

-export([
	 start_link/0,
	 stop/0,
	 get/2,
	 find_imsi/1
	]).

% Home location register for Erlang MSC - uses DETS.

% Data stored:
%
% Main data table
% - IMSI (key)
% - Directory number
% - Key Ki
% and some other stuff, all in a proplist
%
% Reverse table
% - Directory number (key)
% - IMSI

% internal exports ( ... )
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Starts the HLR server.
%% @spec start_link() -> ok
%% @end
start_link() ->
    io:format("Starting hlr_server~n", []),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stops the HLR server.
%% @spec stop() -> ok
%% @end
stop() ->
    gen_server:cast(?SERVER, stop).

%% @doc Fetch an attribute of a mobile station given its IMSI
%% <pre>
%% Types:
%%  Station = string()
%%  Attr = atom()
%% </pre>
%% @spec get(Station, Attr) -> {ok, Value} | {error, Reason} | EXIT
%% @end
get(Station, Attr) ->
    gen_server:call(?SERVER, {get, Station, Attr}).

%% @doc Find the IMSI of a mobile station given its DN
%% <pre>
%% Types:
%%  IMSI = string()
%% </pre>
%% @spec get(Station, Attr) -> {ok, IMSI} | {error, Reason} | EXIT
%% @end
find_imsi(DN) ->
    gen_server:call(?SERVER, {find_imsi, DN}).


code_change(_Old, State, _Extra) ->
    {ok, State}.

%% ============================================================
%% Server functions

%% Function: init/1
%% Description: Initiates the HLR server
%% Returns: {ok, State} |
%%          {ok, State, Timeout} |
%%          ignore |
%%          {stop, Reason}
init(_Args) ->
    dets:open_file(hlr, [{access, read}, {file, "/var/emsc/hlr.dets"}]),
    Hlr = hlr,
    io:format("Starting vlr_server~n", []),
    {ok, Hlr}.

%% Function: handle_call, 3
%% Description: Handle incoming calls
%% Returns: {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, Reply, State} |   (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
handle_call({find_imsi, DN}, _From, Data) ->
    case dets:lookup(Data, {dn, DN}) of
	[{{dn, DN}, Imsi}|_] ->
	    {reply, {ok, Imsi}, Data};
	_ ->
	    {reply, {error, no_such_dn}, Data}
    end;
handle_call({get, Imsi, Attr}, _From, Data) ->
    case dets:lookup(Data, {imsi, Imsi}) of
	[{{imsi, Imsi}, Plist}|_] ->
	    {reply, {ok, proplists:get_value(Attr, Plist)}, Data};
	_Foo ->
	    {reply, {error, {no_such_imsi, _Foo}}, Data}
    end;
handle_call(_Message, _From, State) ->
    {reply, error, State}.


%% Function: handle_cast, 3
%% Description: Handle incoming casts
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, Reply, State} |   (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
handle_cast(stop, State) ->
    dets:close(hlr_tab),
    {stop, simon_says, State};
handle_cast(_Message, State) ->
    {noreply, State}.



handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
