-module(sccp_machine).
-author('Duncan Smith <Duncan@xrtc.net>').
-include_lib("osmo_ss7/include/osmo_ss7.hrl").
-include_lib("emsc/include/ipaccess.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").

-export([boot_link/1, reg_connect_callback/1, reg_dgram_callback/1, connect/1]).

-export([sccp_loop/0, sccp_loop/1]).

boot_link(Socket) ->
    Looper = spawn(fun sccp_loop/0),
    Looper ! {socket, Socket},
    register(sccp_loop, Looper),
    ipa_proto:register_stream(Socket, ?IPAC_PROTO_SCCP, {callback_fn, fun rx_message/4, []}),
    io:format("Sending ip.access hello~n", []),
    ipa_proto:send(Socket, 254, << 6 >>),
    ok.

% This routine accepts a fun, which is stored for later use.  When an
% incoming connection is created, a new process is spawned starting at
% Fun.  It is sent the message {kill} when the connection is closed.
reg_connect_callback(Fun) ->
    Looper = whereis(sccp_loop),
    Looper ! {reg_connect_callback, Fun}.

% Much like reg_connect_callback, but Fun is spawned for every
% datagram reception.  It is sent exactly one message: {sccp_message,
% Msg}, where Msg is a binary.  It is expected that Fun will die when
% its task has been completed.
reg_dgram_callback(Fun) ->
    Looper = whereis(sccp_loop),
    Looper ! {reg_dgram_callback, Fun}.

% This routine initiates a connection, spawns Fun, and sends it
% messages from that connection exactly like reg_connect_callback.
connect(Fun) ->
    Looper = whereis(sccp_loop),
    Looper ! {connect, Fun}.


rx_message(_Socket, _Port, Data, []) ->
    Looper = whereis(sccp_loop),
    {ok, Msg} = sccp_codec:parse_sccp_msg(Data),
    io:format("Got SCCP message~n ->~p~n ->~p~n", [Data, Msg]),
    Looper ! Msg.

sccp_loop() ->
    receive
	{socket, Socket} ->
	    sccp_machine:sccp_loop(Socket)
    end.

sccp_loop(Socket) ->
    receive
	% change socket message
	{socket, NewSocket} ->
	    sccp_machine:sccp_loop(NewSocket);
	{reg_dgram_callback, Fun} ->
	    io:format("Registering datagram handler ~p for SCCP~n", [Fun]),
	    put(dgram_callback, Fun),
    	    sccp_machine:sccp_loop(Socket);
	{connect, Fun} ->
	    io:format("SCCP connecting using handler ~p~n", [Fun]),
	    Self = self(),
	    LocalRef = get_cur_local_ref(),
	    Controller = spawn(fun() -> sccp_socket_loop(outgoing, LocalRef, undefined, Self, spawn(Fun)) end),
	    % race condition exists
	    put({sccp_local_ref, LocalRef}, Controller),
	    sccp_machine:sccp_loop(Socket);

	{sccp_msg, Type, Params} ->
	    % Find a receiver for this message.  Messages without a
	    % destination local reference that is in the table
	    % (e.g. messages from dead connections, connectionless
	    % messages) won't have one and so Controller will be
	    % undefined.  sccp_receive_dispatch should only throw
	    % errors in case of this being a genuine problem.
	    Class = proplists:get_value(protocol_class, Params),
	    LocalRef = proplists:get_value(dst_local_ref, Params),
	    RemoteRef = proplists:get_value(src_local_ref, Params),
	    Controller = get({sccp_local_ref, LocalRef}),
%	    io:format("Dispatching T=~p C=~p LR=~p P=~p~n", [Type, Class, LocalRef, Params]),
	    try sccp_receive_dispatch(Type, Class, Params, Controller)
	    catch
		X:Y ->
		    io:format("XXXX====XXXX Catching ~p (~p:~p)~n", [Type, X, Y]),
		    case LocalRef of
			% connection-oriented messages always have a
			% destination reference, but may not have a
			% protocol class element (wtf).
			undefined -> ok; % discard erroneous connectionless message
			_ when is_integer(LocalRef), is_integer(RemoteRef) ->
				% not sure if this is technically the
				% right cause code, but it'll do.
			    self() ! {sccp_released,
				      LocalRef,
				      RemoteRef,
				      ?SCCP_CAUSE_REL_SCCP_FAILURE};
			_ -> ok
			    % discard connectionful messages that
			    % don't have enough information to make
			    % into a RELEASED message.
		    end
	    end,
	    sccp_machine:sccp_loop(Socket);
	{sccp_data_out, LocalRef, RemoteRef, Data} ->
	    % send out a message
	    Msg = #sccp_msg{msg_type=?SCCP_MSGT_DT1,
			    parameters=[{dst_local_ref, RemoteRef},
					{src_local_ref, LocalRef},
					{protocol_class, {2,0}},
					{segm_reass, 0},
					{user_data, Data}]},
	    self() ! {sccp_message_out, LocalRef, RemoteRef, Msg},
	    sccp_machine:sccp_loop(Socket);
	{sccp_message_out, LocalRef, RemoteRef, Msg} ->
	    % send out a message
	    io:format("About to encode & send ~p~n", [Msg]),
	    ipa_proto:send(Socket, ?IPAC_PROTO_SCCP, sccp_codec:encode_sccp_msg(Msg)),
	    sccp_machine:sccp_loop(Socket);
	{sccp_connect_confirm, LocalRef, RemoteRef} ->
	    % Connection confirm from MSC direction

	    Msg = {sccp_msg, ?SCCP_MSGT_CC, [{dst_local_ref, RemoteRef},
					     {src_local_ref, LocalRef},
					     {protocol_class, {2,0}}]},
	    ipa_proto:send(Socket, ?IPAC_PROTO_SCCP, sccp_codec:encode_sccp_msg(Msg)),
	    sccp_machine:sccp_loop(Socket);
	{sccp_released, LocalRef, RemoteRef, Cause} ->
	    % Release from MSC direction
	    Msg = {sccp_msg, ?SCCP_MSGT_RLSD, [{src_local_ref, LocalRef},
					       {dst_local_ref, RemoteRef},
					       {release_cause, Cause}]},
	    io:format("Sccp releasing ref=~p/~p: ~p~n", [LocalRef, RemoteRef, Msg]),
	    ipa_proto:send(Socket, ?IPAC_PROTO_SCCP, sccp_codec:encode_sccp_msg(Msg)),
	    sccp_machine:sccp_loop(Socket);
	{sccp_release_compl, LocalRef, RemoteRef} ->
	    % Release from BSS direction
	    io:format("Sccp release complete ref=~p/~p~n", [LocalRef, RemoteRef]),
	    Msg = {sccp_msg, ?SCCP_MSGT_RLC, [{src_local_ref, LocalRef},
					      {dst_local_ref, RemoteRef}]},
	    erase({sccp_local_ref, LocalRef}),
	    ipa_proto:send(Socket, ?IPAC_PROTO_SCCP, sccp_codec:encode_sccp_msg(Msg)),
	    sccp_machine:sccp_loop(Socket);
	{sccp_ping, To, From} ->
	    Msg = {sccp_msg, ?SCCP_MSGT_IT, [{dst_local_ref, To},
					     {src_local_ref, From},
					     {protocol_class, {2,0}},
					     {seq_segm, 0},
					     {credit, 0}]},
	    ipa_proto:send(Socket, ?IPAC_PROTO_SCCP, sccp_codec:encode_sccp_msg(Msg)),
	    sccp_machine:sccp_loop(Socket);
	{killed, LocalRef} ->
	    % one of my workers has killed himself
	    io:format("Removing entry ~p from local worker table~n", [LocalRef]),
	    erase({sccp_local_ref, LocalRef}),
	    sccp_machine:sccp_loop(Socket)
    end.

% primitive state machine for SCCP connections

    % Connection request from BSS direction
sccp_receive_dispatch(?SCCP_MSGT_CR, {2,0}, Params, _) ->
    RemoteRef = proplists:get_value(src_local_ref, Params),
    LocalRef = get_cur_local_ref(),
    {ok, Pid} = mobile_mm_fsm:start_link(LocalRef, RemoteRef, self()),
    Self = self(),
    Controller = spawn(fun () -> sccp_socket_loop(incoming, LocalRef, undefined, Self, Pid) end),
%    io:format("Link started using ~p as uplink~n", [Controller]),
    put({sccp_local_ref, LocalRef}, Controller),
    UserData = proplists:get_value(user_data, Params),
    Controller ! {sccp_connect_request, LocalRef, RemoteRef, UserData};

    % Connection confirm from BSS direction
sccp_receive_dispatch(?SCCP_MSGT_CC, {2,0}, Params, Controller) ->
    RemoteRef = proplists:get_value(src_local_ref, Params),
    LocalRef = proplists:get_value(dst_local_ref, Params),
%    io:format("Connect confirm ~p~n", [RemoteRef]),
    Controller ! {sccp_connect_confirm, LocalRef, RemoteRef, proplists:get_value(user_data, Params)};

    % First phase of disconnect
sccp_receive_dispatch(?SCCP_MSGT_RLSD, _, Params, Controller) ->
    RemoteRef = proplists:get_value(src_local_ref, Params),
    LocalRef = proplists:get_value(dst_local_ref, Params),
    Msg = proplists:get_value(user_data, Params),
    Cause = proplists:get_value(release_cause, Params),
%    io:format("Sccp releasing ref=~p/~p~n", [LocalRef, RemoteRef]),
    Controller ! {sccp_message, LocalRef, RemoteRef, Msg},
    Controller ! {sccp_released, LocalRef, RemoteRef, Cause};

    % Second and final phase of disconnect
sccp_receive_dispatch(?SCCP_MSGT_RLC, _, Params, Controller) ->
    RemoteRef = proplists:get_value(src_local_ref, Params),
    LocalRef = proplists:get_value(dst_local_ref, Params),
    Controller ! {sccp_release_compl, LocalRef, RemoteRef};

    % Connection-oriented dataframe type 1 from BSS direction
sccp_receive_dispatch(?SCCP_MSGT_DT1, _, Params, Controller) ->
    LocalRef = proplists:get_value(dst_local_ref, Params),
    MsgBin = proplists:get_value(user_data, Params),
%    io:format("Sccp ref=~p/~p DT1=~p~n", [LocalRef, undefined, MsgBin]),
    Controller ! {sccp_message, LocalRef, undefined, MsgBin};

    % Connectionless datagram from BSS direction
sccp_receive_dispatch(?SCCP_MSGT_UDT, {0,0}, Params, _) ->
    MsgBin = proplists:get_value(user_data, Params),
    Fun = get(dgram_callback),
%    if (is_function(Fun)) ->
%       {ok, Pid} = spawn(Fun),
%       Pid ! {sccp_message, MsgBin}
%    end,
    ok;

    % "Ping?"
sccp_receive_dispatch(?SCCP_MSGT_IT, {2,0}, Params, Controller) ->
    RemoteRef = proplists:get_value(src_local_ref, Params),
    LocalRef = proplists:get_value(dst_local_ref, Params),
%    Controller ! {sccp_ping, LocalRef, RemoteRef};
    ok;

% *UDTS messages are error responses.  This code is perfect and never
% generates erroneous messages.  Also, it's not interested in hearing
% about how much it sucks, so it sets the "don't tell me about errors"
% flag.  Thus, I don't even bother to parse *UDTS messages

    % Extended unitdata
sccp_receive_dispatch(?SCCP_MSGT_XUDT, {2,0}, Params, Controller) ->
    From = proplists:get_value(src_local_ref, Params),
    To = proplists:get_value(dst_local_ref, Params),
    MsgBin = proplists:get_value(user_data, Params),
    Controller ! {sccp_message, To, From, MsgBin};

    % Long unitdata
sccp_receive_dispatch(?SCCP_MSGT_LUDT, {2,0}, Params, Controller) ->
    From = proplists:get_value(src_local_ref, Params),
    To = proplists:get_value(dst_local_ref, Params),
    MsgBin = proplists:get_value(user_data, Params),
    Controller ! {sccp_message, To, From, MsgBin};

    % unknown messages are discarded (GSM 08.06 section 5.3, paragraph regarding Q.712 subclause 1.10)
sccp_receive_dispatch(Type, Class, Params, _Controller) ->
    io:format("Unknown message type ~p (~p)~n -->~p~n", [Type, Class, Params]),
    ok.


get_cur_local_ref() ->
    case get(local_ref_max) of
	undefined ->
	    put(local_ref_max, 0),
	    get_cur_local_ref();
	_ ->
	    Ref = get(local_ref_max),
	    put(local_ref_max, Ref + 1),
	    Ref
    end.

get_cur_remote_ref(Local) ->
    get({remote_ref, Local}).

% Loop process to handle an SCCP connection.
%
% Downlink is the pid of the process that we use to send messages
% outbound.
sccp_socket_loop(incoming, LocalRef, undefined, Downlink, Uplink) ->
    receive
	{sccp_connect_request, LocalRef, RemoteRef, Msg} ->
	    io:format("Sccp ref=~p/~p: accepting~n", [LocalRef, RemoteRef]),
	    mobile_mm_fsm:assign(Uplink, self(), Msg),
	    mobile_mm_fsm:incoming(Uplink, Msg),
	    sccp_socket_loop(incoming, LocalRef, RemoteRef, Downlink, Uplink);
	{_, LocalRef, RemoteRef} ->
	    io:format("Sccp ref=~p/~p: NOPE~n", [LocalRef, RemoteRef]),
	    Downlink ! {sccp_released, LocalRef, RemoteRef, ?SCCP_CAUSE_REL_INCONS_CONN_DAT}
    end;

sccp_socket_loop(incoming, LocalRef, RemoteRef, Downlink, Uplink) ->
    Downlink ! {sccp_connect_confirm, LocalRef, RemoteRef},
    sccp_socket_loop(established, LocalRef, RemoteRef, Downlink, Uplink);

sccp_socket_loop(outgoing, LocalRef, undefined, Downlink, Uplink) ->
    Downlink ! {sccp_connect, LocalRef, undefined};

sccp_socket_loop(outgoing, LocalRef, RemoteRef, Downlink, Uplink) ->
    receive
	{sccp_connect_confirm, LocalRef, _RemoteRef, Msg} ->
	    io:format("Sccp ref=~p: Confirmed~n", [LocalRef]),
	    Userdata = proplists:get_value(user_data, Msg),
	    if
		% if there's userdata, loop it back in so I can process it later
		is_binary(Userdata) ->
		    io:format("Looping in userdata ~p~n", [Msg]),
		    self() ! {sccp_message, LocalRef, RemoteRef, Msg}
	    end,
	    sccp_socket_loop(established, LocalRef, RemoteRef, Downlink, Uplink);
	Msg ->
	    io:format("Sccp ref=~p/~p: Failure to confirm (~p), killing~n", [LocalRef, RemoteRef, Msg]),
	    Downlink ! {sccp_released, LocalRef, RemoteRef, ?SCCP_CAUSE_REL_SCCP_FAILURE}
    end;
sccp_socket_loop(established, LocalRef, RemoteRef, Downlink, Uplink) ->
    receive
	{sccp_message, LocalRef, _, Msg} ->
	    mobile_mm_fsm:incoming(Uplink, Msg),
	    sccp_socket_loop(established, LocalRef, RemoteRef, Downlink, Uplink);
	{sccp_message, _, LocalRef, Msg} ->
	    io:format("Sccp ref=~p/~p: Sending a message~n", [LocalRef, RemoteRef]),
	    Downlink ! {sccp_message_out, LocalRef, RemoteRef, Msg}, % not sure about the tuple member order
	    sccp_socket_loop(established, LocalRef, RemoteRef, Downlink, Uplink);
	{sccp_ping, LocalRef, RemoteRef} ->
	    Downlink ! {sccp_ping, RemoteRef, LocalRef},
	    sccp_socket_loop(established, LocalRef, RemoteRef, Downlink, Uplink);
	{sccp_released, LocalRef, RemoteRef, Cause} ->
	    Downlink ! {sccp_release_compl, LocalRef, RemoteRef},
	    self() ! {kill},
	    sccp_socket_loop(established, LocalRef, RemoteRef, Downlink, Uplink);
	{sccp_release_compl, LocalRef, RemoteRef} ->
	    self() ! {kill},
	    sccp_socket_loop(established, LocalRef, RemoteRef, Downlink, Uplink);
	{close, Cause} ->
% by GSM 08.06 sec 6.2, this can only be initiated by the MSC/network side.
	    io:format("Sccp ref=~p/~p: Killing myself~n", [LocalRef, RemoteRef]),
	    mobile_mm_fsm:terminate(Uplink, Cause),
	    sccp_socket_loop(established, LocalRef, RemoteRef, Downlink, Uplink);
	{kill} ->
	    io:format("Sccp ref=~p/~p: Killing myself~n", [LocalRef, RemoteRef]),
	    Downlink ! {killed, LocalRef};
	Msg ->
	    io:format("Sccp ref=~p/~p: Unknown message:~n --> ~p~n", [LocalRef, RemoteRef, Msg]),
	    sccp_socket_loop(established, LocalRef, RemoteRef, Downlink, Uplink)
    end.


datagram_print_loop() ->
    io:format("Getting new packet ...~n"),
    receive
	{Name, Msg} ->
	    io:format("Got ~w message ~p~n", [Name, Msg]),
	    datagram_print_loop()
    end.

