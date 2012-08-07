-module(vlr_server).
-author('Duncan Smith <Duncan@xrtc.net>').
-behaviour(gen_server).

-define(SERVER, vlr).

-compile(export_all).

-export([
	 start_link/0,
	 stop/0,
	 put/3,
	 get/2,
	 get/1,
	 find_tmsi/1,
	 add_station/1,
	 add_station/2,
	 drop_station/1
	]).

% Visitor location register for Erlang MSC.

% Data stored:
%
% Main data table
% - TMSI (primary key)
% - IMSI
% - pid for handling the mobile station
% - Authentication and Ciphering options
% - - ???
% - - Key Kc
%
% Reverse table
% - IMSI (primary key)
% - TMSI

% internal exports ( ... )
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Starts the VLR server.
%% @spec start_link() -> ok
%% @end
start_link() ->
    io:format("Starting vlr_server~n", []),
    Res = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stops the VLR server.
%% @spec stop() -> ok
%% @end
stop() ->
    gen_server:cast(?SERVER, stop).

%% @doc Write the given attribute of a mobile station
%% <pre>
%% Types:
%%  Station = string()
%%  Attr = atom()
%%  Val = anything
%% </pre>
%% @spec put(Station, Attr, Val) -> ok
%% @end
put(Station, Attr, Val)
when is_list(Station), is_atom(Attr) ->
    gen_server:call(?SERVER, {put, Station, Attr, Val}).

%% @doc Make a slot for the given mobile station
%% <pre>
%% Types:
%%  Station = string()
%%  Tmsi = string()
%% </pre>
%% @spec put(Station, Tmsi) -> ok
%% @end
add_station(Station, Imsi)
when is_list(Station), is_list(Imsi) ->
    gen_server:cast(?SERVER, {add_station, Station, Imsi}).

add_station(Imsi)
when is_list(Imsi) ->
    Station = gen_server:call(?SERVER, {pick_tmsi, Imsi}),
    add_station(Imsi, Station),
    Station.

%% @doc Remove the given mobile station
%% <pre>
%% Types:
%%  Station = string()
%% </pre>
%% @spec put(Station) -> ok
%% @end
drop_station(Station)
when is_list(Station) ->
    gen_server:cast(?SERVER, {drop_station, Station}).

%% @doc Fetch an attribute of a mobile station given its TMSI
%% <pre>
%% Types:
%%  Station = string()
%%  Attr = atom()
%% </pre>
%% @spec get(Station, Attr) -> {ok, Value} | {error, Reason} | EXIT
%% @end
get(Station, Attr) ->
    gen_server:call(?SERVER, {get, Station, Attr}).

get(Station) ->
    gen_server:call(?SERVER, {get, Station}).

%% @doc Find the TMSI of a mobile station given its IMSI
%% <pre>
%% Types:
%%  IMSI = string()
%% </pre>
%% @spec get(Station, Attr) -> {ok, TMSI} | {error, Reason} | EXIT
%% @end
find_tmsi(IMSI) ->
    gen_server:call(?SERVER, {find_tmsi, IMSI}).


code_change(_Old, State, _Extra) ->
    {ok, State}.

%% ============================================================
%% Server functions

%% Function: init/1
%% Description: Initiates the VLR server
%% Returns: {ok, State} |
%%          {ok, State, Timeout} |
%%          ignore |
%%          {stop, Reason}
init(_Args) ->
    {ok, {dict:new(), dict:new()}}.

%% Function: handle_call, 3
%% Description: Handle incoming calls
%% Returns: {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, Reply, State} |   (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
handle_call({find_tmsi, Imsi}, _From, {Data, Temps}) ->
    case dict:find(Imsi, Temps) of
	{ok, Tmsi} -> {reply, {ok, Tmsi}, {Data, Temps}};
	error      -> {reply, {error, no_such_imsi}, {Data, Temps}}
    end;
handle_call({get, Tmsi, Attr}, _From, {Data, Temps}) ->
    case dict:find(Tmsi, Data) of
	error       -> {reply, {error, no_such_tmsi}, {Data, Temps}};
	{ok, Plist} -> {reply, {ok, proplists:get_value(Attr, Plist)}, {Data, Temps}}
    end;
handle_call({get, Tmsi}, _From, {Data, Temps}) ->
    case dict:find(Tmsi, Data) of
	error       -> {reply, {error, no_such_tmsi}, {Data, Temps}};
	{ok, Plist} -> {reply, Plist, {Data, Temps}}
    end;
handle_call({put, Station, Attr, Val}, _From, {Data, Temps}) ->
    case dict:find(Station, Data) of
	error ->
	    {reply, {error, no_such_tmsi}, {Data, Temps}};
	_ ->
	    case Attr of
		tmsi ->
		    case dict:find(Val, Data) of
			error -> % if the tmsi isn't allocated already, return okay
			    {NewData, NewTemps} = change_tmsi(Station, Val, {Data, Temps}),
			    {reply, ok, {NewData, NewTemps}};
			_ -> % That TMSI is already allocated, you boor
			    {reply, {error, tmsi_already_allocated}, {Data, Temps}}
		    end;
		_ -> % most common case: update to parameter of existing station
		    NewData = put_attr(Station, Attr, Val, Data),
		    {reply, ok, {NewData, Temps}}
	    end
    end;
handle_call({pick_tmsi, Imsi}, _From, {Data, Temps}) ->
    Station = generate_tmsi(),
    case dict:find(Station, Data) of
	error -> % tmsi not used yet, is good choice yes
	    {reply, Station, {Data, Temps}};
	_ ->
	    handle_call({pick_tmsi, Imsi}, _From, {Data, Temps})
    end;
handle_call(_Message, _From, State) ->
    {reply, error, State}.

% a tmsi is 4 bytes long
generate_tmsi() ->
    [ lists:nth(I, "0123456789abcdef") || I <- [random:uniform(16), random:uniform(16),
						random:uniform(16), random:uniform(16),
						random:uniform(16), random:uniform(16),
						random:uniform(16), random:uniform(16)] ].


%% Function: handle_cast, 3
%% Description: Handle incoming casts
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, Reply, State} |   (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
handle_cast({add_station, Imsi, Tmsi}, {Data, Temps}) ->
    {noreply, {dict:store(Tmsi, [{imsi, Imsi}], Data), dict:store(Imsi, Tmsi, Temps)}};
handle_cast({drop_station, Tmsi}, {Data, Temps}) ->
    case dict:find(Tmsi, Data) of
	error     -> {noreply, {Data, Temps}};
	{ok, Sub} -> Imsi = proplists:get_value(imsi, Sub),
		     {noreply, {dict:erase(Tmsi, Data), dict:erase(Imsi, Temps)}}
    end;
handle_cast(stop, State) ->
    {stop, simon_says, State};
handle_cast(_Message, State) ->
    {noreply, State}.



handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


% Do the jiggity-swap to change a TMSI for a mobile station,
% identified only by its TMSI.
%
% Assumes that the old TMSI exists and the new TMSI is unallocated.
change_tmsi(OldTmsi, NewTmsi, {DataList, TmsiList}) ->
    Data = dict:fetch(OldTmsi, DataList),
    Imsi = proplists:lookup(imsi, Data),
    NewDataList = dict:store(NewTmsi, Data, dict:erase(OldTmsi, DataList)),
    NewTmsiList = dict:store(Imsi, NewTmsi, dict:erase(Imsi, TmsiList)),
    {NewDataList, NewTmsiList}.

put_attr(Tmsi, Attr, Val, DataList) ->
    OldData = dict:fetch(Tmsi, DataList),
    NewData = [ {Attr, Val} | proplists:delete(Attr, OldData) ],
    NewDataList = dict:store(Tmsi, NewData, DataList),
    NewDataList.
