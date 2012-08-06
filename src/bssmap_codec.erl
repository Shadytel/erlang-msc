-module(bssmap_codec).
-author('Duncan Smith <Duncan@xrtc.net>').
-include_lib("emsc/include/bssmap.hrl").

-export([parse_message/1, encode_message/1]).

-compile(export_all).

parse_message(<<Type:8, Bin/binary>>) ->
    {bssmap, Type, parse_ies(Bin)}.

encode_message({Type, Args}) ->
    {Mand, Opt} = message_from_mt(Type),
    erlang:list_to_binary([<< Type:1/bytes >>, encode_message(Type, Mand, Opt, Args)]).

encode_message(Type, [], [], Args) ->
    << >>;
encode_message(Type, [], [Cur|Opt], Args) ->
    [encode_ie(Cur, Args) | encode_message(Type, [], Opt, Args)];
encode_message(Type, [Cur|Mand], Opt, Args) ->
    [encode_ie(Cur, Args) | encode_message(Type, Mand, Opt, Args)].

encode_ie(_Type, _Value) ->
    <<>>.

parse_ies(Bin) ->
    parse_ies([], Bin, []).

%% [{?BSSMAP_ASSIGN_REQ, assign_req},
%%  {?BSSMAP_ASSIGN_COMPL, assign_compl},
%%  {?BSSMAP_ASSIGN_FAIL, assign_fail},
%%  {?bssmap_hand_reque, handover_request},
%%  {?bssmap_hand_requi, handover_required},
%%  {?bssmap_hand_cmd, handover_command},
%%  {?bssmap_hand_compl, handover_complete},
%%  {?bssmap_hand_succeed, handover_succeed},
%%  {?bssmap_hand_fail, handover_fail},
%%  {?bssmap_hand_perf, handover_performed},
%%  {?bssmap_hand_cand_enq, handover_candidate_enq},
%%  {?bssmap_hand_cand_rsp, handover_candidate_resp},
%%  {?bssmap_hand_req_rej, handover_req_reject},
%%  {?bssmap_hand_detect, handover_detect},
%%  {?bssmap_clr_cmd, clear_command},
%%  {?bssmap_clr_compl, clear_compl},
%%  {?bssmap_clr_req, clear_request},
%%  {?bssmap_sapi_rej, sapi_reject},
%%  {?bssmap_confusion, confusion},
%%  {?bssmap_suspend, suspend},
%%  {?bssmap_resume, resume},
%%  {?bssmap_reset, reset},
%%  {?bssmap_reset_ack, reset_ack},
%%  {?bssmap_overload, overload},
%%  {?bssmap_reset_ckt, reset_ckt},
%%  {?bssmap_reset_ckt_ack, reset_ckt_ack},
%%  {?bssmap_msc_inv_trace, msc_inv_trace},
%%  {?bssmap_bss_inv_trace, bss_inv_trace},
%%  {?BSSMAP_BLOCK, block},
%%  {?BSSMAP_BLOCK_ACK, block_ack},
%%  {?BSSMAP_unblock, unblock},
%%  {?bssmap_unblock_ack, unblock_ack},
%%  {?bssmap_cgrp_block, cgrp_block



message_from_mt(Type) ->
    case Type of
	% Type -> {[mandatory], [optional]}
	?BSSMAP_ASSIGN_REQ -> {[], [?ELEM_L3_HEAD, ?ELEM_PRIORITY, ?ELEM_CKT_ID, ?ELEM_DOWNLINK_DTX, ?ELEM_INTERFER_BAND, ?ELEM_CLASSMARK_IND_2, ?ELEM_GRP_CALL_REF, ?ELEM_TALKER_FLAG, ?ELEM_CONF_EVOL_IND]};
	?BSSMAP_ASSIGN_COMPL -> {[], [?ELEM_RR_CAUSE, ?ELEM_CKT_ID, ?ELEM_CELL_ID, ?ELEM_CHOSEN_CHAN, ?ELEM_CHOSEN_CRYPTO, ?ELEM_CKT_POOL, ?ELEM_SPEECH_VERSION]};
	?BSSMAP_ASSIGN_FAIL -> {[?ELEM_CAUSE], [?ELEM_RR_CAUSE, ?ELEM_CKT_POOL, ?ELEM_CKT_POOL_LIST]};
	?BSSMAP_BLOCK -> {[?ELEM_CKT_ID, ?ELEM_CAUSE], [?ELEM_CONN_REL_REQTED]};
	?BSSMAP_BLOCK_ACK -> {[?ELEM_CKT_ID], []};
	?BSSMAP_UNBLOCK -> {[?ELEM_CKT_ID], []};
	?BSSMAP_UNBLOCK_ACK -> {[?ELEM_CKT_ID], []};
	?BSSMAP_HAND_REQUE -> {[?ELEM_CHAN_TYPE, ?ELEM_CRYPTO_INFO, ?ELEM_CLASSMARK_IND_2, ?ELEM_CELL_ID], [?ELEM_INTERFER_BAND, ?ELEM_CAUSE, ?ELEM_CLASSMARK_IND_3, ?ELEM_CURRENT_CHAN, ?ELEM_SPEECH_VERSION, ?ELEM_GRP_CALL_REF, ?ELEM_TALKER_FLAG, ?ELEM_CONF_EVOL_IND, ?ELEM_CHOSEN_CRYPTO]};
	% cell id list is preferred targets; is actually mandatory but
	% for the expressiveness of my code (response request is
	% optional; its presence or absence encodes its information).
	?BSSMAP_HAND_REQUI -> {[?ELEM_CAUSE], [?ELEM_RSP_REQ, ?ELEM_CELL_ID_LIST, ?ELEM_CKT_POOL_LIST, ?ELEM_CURRENT_CHAN, ?ELEM_SPEECH_VERSION, ?ELEM_QUEUE_IND]};
	?BSSMAP_HAND_REQ_ACK -> {[?ELEM_L3_MESSAGE], [?ELEM_CHOSEN_CHAN, ?ELEM_CHOSEN_CRYPTO, ?ELEM_CKT_POOL, ?ELEM_SPEECH_VERSION, ?ELEM_CKT_ID]};
	?BSSMAP_HAND_CMD -> {[?ELEM_L3_MESSAGE], [?ELEM_CELL_ID]};
	?BSSMAP_HAND_COMPL -> {[], [?ELEM_RR_CAUSE]};
	?BSSMAP_HAND_SUCCEED -> {[], []};
	?BSSMAP_HAND_CAND_ENQ -> {[?ELEM_MS_COUNT, ?ELEM_CELL_ID_LIST, ?ELEM_CELL_ID], []};
	?BSSMAP_HAND_CAND_RESP -> {[?ELEM_MS_COUNT, ?ELEM_CELL_ID], []};
	?BSSMAP_HAND_FAIL -> {[?ELEM_CAUSE], [?ELEM_RR_CAUSE, ?ELEM_CKT_POOL, ?ELEM_CKT_POOL_LIST]};
	?BSSMAP_RSRC_REQ -> {[?ELEM_PERIODICITY, ?ELEM_RSRC_IND_METH, ?ELEM_CELL_ID], [?ELEM_EXT_RSRC_INDn]};
	% cell id is actually mandatory but for the expressiveness of
	% my code (resource available is not sent in all cases; its
	% presence or absence encodes its information).
	?BSSMAP_RSRC_IND -> {[?ELEM_RSRC_IND_METH], [?ELEM_RSRC_AVAIL, ?ELEM_CELL_ID, ?ELEM_TOTAL_AVAIL]};
	% cell id list is mandatory; tmsi is optional.
	?BSSMAP_PAGING -> {[?ELEM_IMSI], [?ELEM_TMSI, ?ELEM_CELL_ID_LIST, ?ELEM_CHAN_NEEDED, ?ELEM_EMLPP_PRIO]};
	?BSSMAP_CLR_REQ -> {[?ELEM_CAUSE], []};
	% cause is mandatory, l3 header is optional and deprecated
	?BSSMAP_CLR_CMD -> {[], [?ELEM_L3_HEAD, ?ELEM_CAUSE]};
	?BSSMAP_CLR_COMPL -> {[], []};
	?BSSMAP_RESET -> {[?ELEM_CAUSE], []};
	?BSSMAP_RESET_ACK -> {[], []};
	?BSSMAP_HAND_PERF -> {[?ELEM_CAUSE, ?ELEM_CELL_ID], [?ELEM_CHOSEN_CHAN, ?ELEM_CHOSEN_CRYPTO, ?ELEM_SPEECH_VERSION]};
	?BSSMAP_OVERLOAD -> {[?ELEM_CAUSE, ?ELEM_CELL_ID], []};
	% trace ref is mandatory
	?BSSMAP_MSC_INV_TRACE -> {[?ELEM_TRACE_TYPE], [?ELEM_TRIGGER_ID, ?ELEM_TRACE_REF, ?ELEM_TRANS_ID, ?ELEM_MOBILE_ID, ?ELEM_OMC_ID]};
	% trace ref is mandatory
	?BSSMAP_BSS_INV_TRACE -> {[?ELEM_TRACE_TYPE], [?ELEM_FORWARD_IND, ?ELEM_TRIGGER_ID, ?ELEM_TRACE_REF, ?ELEM_TRANS_ID, ?ELEM_OMC_ID]};
	?BSSMAP_CLASSMARK_UPD -> {[?ELEM_CLASSMARK_2], [?ELEM_CLASSMARK_3]};
	% crypto info is mandatory; l3 head is optional and deprecated
	?BSSMAP_CIPHER_CMD -> {[], [?ELEM_L3_HEAD, ?ELEM_CRYPTO_INFO, ?ELEM_CIPHER_RSP_MODE]};
	?BSSMAP_CIPHER_COMPL -> {[], [?ELEM_L3_MESSAGE, ?ELEM_CHOSEN_CRYPTO]};
	?BSSMAP_COMPL_L3_INF -> {[?ELEM_CELL_ID, ?ELEM_L3_INFO], [?ELEM_CHOSEN_CHAN]};
	?BSSMAP_QUEUE_IND -> {[], []};
	?BSSMAP_SAPI_REJ -> {[?ELEM_DLCI, ?ELEM_CAUSE], []};
	?BSSMAP_HAND_REQ_REJ -> {[?ELEM_CAUSE], []};
	?BSSMAP_RESET_CKT -> {[?ELEM_CKT_ID, ?ELEM_CAUSE], []};
	?BSSMAP_RESET_CKT_ACK -> {[?ELEM_CKT_ID], []};
	?BSSMAP_HAND_DETECT -> {[], []};
	?BSSMAP_CGRP_BLOCK -> {[?ELEM_CAUSE, ?ELEM_CKT_ID, ?ELEM_CKT_ID_LIST], []};
	?BSSMAP_CGRP_BLOCK_ACK -> {[?ELEM_CKT_ID, ?ELEM_CKT_ID_LIST], []};
	?BSSMAP_CGRP_UNBLOCK -> {[?ELEM_CKT_ID, ?ELEM_CKT_ID_LIST], []};
	?BSSMAP_CGRP_UNBLOCK_ACK -> {[?ELEM_CKT_ID, ?ELEM_CKT_ID_LIST], []};
	?BSSMAP_CONFUSION -> {[?ELEM_CAUSE, ?ELEM_DIAG], []};
	?BSSMAP_CLASSMARK_REQ -> {[], []};
	?BSSMAP_CIPHER_MODE_REJ -> {[?ELEM_CAUSE], []};
	?BSSMAP_LOAD_IND -> {[?ELEM_TIME_IND, ?ELEM_CELL_ID, ?ELEM_CELL_ID_LIST], [?ELEM_SRSC_SITUATION, ?ELEM_CAUSE]};
	% [ ... omit VBS / VGCS messages ... ]
	?BSSMAP_SUSPEND -> {[?ELEM_DLCI], []};
	?BSSMAP_RESUME -> {[?ELEM_DLCI], []};
	?BSSMAP_CHANGE_CKT -> {[?ELEM_CAUSE], []};
	?BSSMAP_CHANGE_CKT_ACK -> {[?ELEM_CKT_ID], []};
	_ -> {[], []}
    end.


parse_ies(_, <<>>, SoFar) ->
    SoFar;

% 3.2.2.17
parse_ies([], <<?ELEM_CELL_ID:8, Rest/binary>>, SoFar) ->
    parse_ies([cell_id], Rest, SoFar);
parse_ies([cell_id | T], << Len:8, Message:Len/bytes, Rest/binary >>, SoFar) ->
    <<_:4, Type:4, _/binary>> = Message,
    case Type of
	2#0000 ->
	    <<_:8, MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4, LAC:16/big, CI:16/big>> = Message,
	    MCC = MCC3 + MCC2*10 + MCC1*100,
	    MNC = MNC2 + MNC1*10,
	    parse_ies(T, Rest, [{cell_id, [MCC, MNC, LAC, CI]} | SoFar]);
	2#0001 ->
	    <<_:8, LAC:16/big, CI:16/big>> = Message,
	    parse_ies(T, Rest, [{cell_id, [LAC, CI]} | SoFar]);
	2#0010 ->
	    <<_:8, CI:16/big>> = Message,
	    parse_ies(T, Rest, [{cell_id, [CI]} | SoFar]);
	2#0011 ->
	    parse_ies(T, Rest, [{cell_id, []} | SoFar])
    end;

% 3.2.2.2
parse_ies([], <<?ELEM_CKT_ID:8, Rest/binary>>, SoFar) ->
    parse_ies([ckt_id], Rest, SoFar);
parse_ies([ckt_id | T], <<Multiplex:11, Slot:5, Rest/bytes>>, SoFar) ->
    parse_ies(T, Rest, [{circuit_id, [{pcm, Multiplex}, {timeslot, Slot}]} | SoFar]);

% 3.2.2.3
parse_ies([], <<?ELEM_CONN_REL_REQTED:8, Rest/binary>>, SoFar) ->
    parse_ies([release_requested], Rest, SoFar);
parse_ies([release_requested|T], Rest, SoFar) ->
    parse_ies(T, Rest, [{release_requested, true} | SoFar]);

% 3.2.2.4
parse_ies([], <<?ELEM_RSRC_AVAIL:8, Rest/binary>>, SoFar) ->
    parse_ies([resource_available], Rest, SoFar);
parse_ies([resource_available|T], <<Message:20/bytes, Rest/bytes>>, SoFar) ->
    % XXX unimplemented
    parse_ies(T, Rest, [{resource_available, {unparsed, Message}} | SoFar]);

% 3.2.2.5
parse_ies([], <<?ELEM_CAUSE:8, Rest/binary>>, SoFar) ->
    parse_ies([cause], Rest, SoFar);
parse_ies([cause|T], <<Len:8, Message:Len/bytes, Rest/binary>>, SoFar) ->
    <<Ext:1, Class:3, Value:4>> = Message,
    parse_ies(T, Rest, [{cause, {Class, Value}} | SoFar]);

% 3.2.2.6
parse_ies([], <<?ELEM_IMSI:8, Rest/binary>>, SoFar) ->
    parse_ies([imsi], Rest, SoFar);
parse_ies([imsi|T], <<Len:8, Number:Len/bytes, Rest/bytes>>, SoFar) ->
    parse_ies(T, Rest, [{imsi, Number} | SoFar]);

% 3.2.2.7
parse_ies([], <<?ELEM_TMSI:8, Rest/binary>>, SoFar) ->
    parse_ies([tmsi], Rest, SoFar);
parse_ies([tmsi|T], <<8:8, TMSI:48/bytes, Rest/bytes>>, SoFar) ->
    parse_ies(T, Rest, [{tmsi, TMSI} | SoFar]);

% 3.2.2.8
parse_ies([], <<?ELEM_MS_COUNT:8, Rest/binary>>, SoFar) ->
    parse_ies([ms_count], Rest, SoFar);
parse_ies([ms_count|T], <<Count:8, Rest/bytes>>, SoFar) ->
    parse_ies(T, Rest, [{ms_count, Count} | SoFar]);

% 3.2.2.9
parse_ies([], <<?ELEM_L3_HEAD:8, Rest/binary>>, SoFar) ->
    parse_ies(l3_head, Rest, SoFar);
parse_ies([l3_head|T], <<Len:8, ProtoId:8, TransId:8, Rest/bytes>>, SoFar) ->
    parse_ies(l3_head, Rest, [{l3_header, [{protocol, ProtoId},
					   {transaction, TransId}]} | SoFar]);

%    {ok, 4, #l3{ protocol=ProtoId, transaction=TransId, body=codec_0408:parse_msg(binary:part(Bin, 1, Len)) }};

% 3.2.2.10
parse_ies([], <<?ELEM_CRYPTO_INFO:8, Rest/binary>>, SoFar) ->
    parse_ies([crypto_info], Rest, SoFar);
parse_ies([crypto_info|T], <<Len:8, A50:1, A51:1, A52:1, A53:1, A54:1, A55:1, A56:1, A57:1, Tail/bytes>>, SoFar) ->
    if
	(A51+A52+A53+A54+A55+A56+A57 > 0) ->
	    <<_:16, Key:64, Rest/bytes>> = Tail;
	true ->
	    Rest = Tail
    end,
    parse_ies(T, Rest, [{crypto_info, [A50, A51, A52, A53, A54, A55, A56, A57]} | SoFar]);

% 3.2.2.11
parse_ies([], <<?ELEM_CHAN_TYPE:8, Rest/binary>>, SoFar) ->
    parse_ies([chan_type], Rest, SoFar);
parse_ies([chan_type|T], <<Len:8, Message:Len/bytes, Rest/binary>>, SoFar) ->
    % XXX unimplemented
    <<_:4, Speech:4, Rate:8, Ext:1, Version:7>> = Message,
    parse_ies(T, Rest, [{chan_type, [{mode, Speech}, {ratetype, Rate}, {versions, Version}]} | SoFar]);

% 3.2.2.12
parse_ies([], <<?ELEM_PERIODICITY:8, Rest/bytes>>, SoFar) ->
    parse_ies([periodicity], Rest, SoFar);
parse_ies([periodicity|T], <<Number:8, Rest/binary>>, SoFar) ->
    % XXX not quite compliant, see spec
    parse_ies(T, Rest, [{periodicity, Number} | SoFar]);

% 3.2.2.13
parse_ies([], <<?ELEM_EXT_RSRC_IND:8, Rest/bytes>>, SoFar) ->
    parse_ies([extended_rsrc], Rest, SoFar);
parse_ies([extended_rsrc|T], <<_:6, Sm:1, Tarr:1, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{extended_rsrc, {Sm, Tarr}} | SoFar]);

% 3.2.2.14
parse_ies([], <<?ELEM_TOTAL_AVAIL:8, Rest/bytes>>, SoFar) ->
    parse_ies([total_avail], Rest, SoFar);
parse_ies([total_avail|T], <<Fr:16/little, Hr:16/little, Rest/bytes>>, SoFar) ->
    parse_ies(T, Rest, [{total_avail, [{halfrate, Hr}, {fullrate, Fr}]} | SoFar]);

% 3.2.2.18
parse_ies([], <<?ELEM_PRIORITY:8, Rest/bytes>>, SoFar) ->
    parse_ies([priority], Rest, SoFar);
parse_ies([priority|T], <<1:8, _:1, Pci:1, Level:4, Queueing:1, Vuln:1, Rest/bytes>>, SoFar) ->
    parse_ies(T, Rest, [{priority, [{can_preempt, Pci}, {priority, Level}, {allow_queueing, Queueing}, {is_vulnerable, Vuln}]} | Rest]);

% 3.2.2.19
parse_ies([], <<?ELEM_CLASSMARK_IND_2:8, Rest/bytes>>, SoFar) ->
    parse_ies([classmark_2], Rest, SoFar);
parse_ies([classmark_2|T], <<Len:8, Classmark:Len/bytes, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{classmark_2, Classmark} | SoFar]);

% 3.2.2.20
parse_ies([], <<?ELEM_CLASSMARK_IND_3:8, Rest/bytes>>, SoFar) ->
    parse_ies([classmark_3], Rest, SoFar);
parse_ies([classmark_3|T], <<Len:8, Classmark:Len/bytes, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{classmark_3, Classmark} | SoFar]);

% 3.2.2.21
parse_ies([], <<?ELEM_INTERFER_BAND:8, Rest/bytes>>, SoFar) ->
    parse_ies([interference_band], Rest, SoFar);
parse_ies([interference_band|T], <<_:3, Band:5, Rest/bytes>>, SoFar) ->
    parse_ies(T, Rest, [{interference_band, Band} | SoFar]);

% 3.2.2.22
parse_ies([], <<?ELEM_RR_CAUSE:8, Rest/bytes>>, SoFar) ->
    parse_ies([rr_cause], Rest, SoFar);
parse_ies([rr_cause|T], <<Cause:8, Rest/bytes>>, SoFar) ->
    parse_ies(T, Rest, [{rr_cause, Cause} | SoFar]);

% 3.2.2.24
parse_ies([], <<?ELEM_L3_MESSAGE:8, Rest/bytes>>, SoFar) ->
    parse_ies([l3_message], Rest, SoFar);
parse_ies([l3_message|T], <<Len:8, Message:Len/bytes, Rest/bytes>>, SoFar) ->
    io:format("L3 Message: ~p~n", [Message]),
    parse_ies(T, Rest, [{l3_message, {unparsed, Message}} | SoFar]);

% 3.2.2.25
parse_ies([], <<?ELEM_DLCI:8, Rest/bytes>>, SoFar) ->
    parse_ies([dlci], Rest, SoFar);
parse_ies([dlci|T], <<Dlci:8, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{dlci, Dlci} | SoFar]);

% 3.2.2.26
parse_ies([], <<?ELEM_DOWNLINK_DTX:8, Rest/bytes>>, SoFar) ->
    parse_ies([downlink_dtx], Rest, SoFar);
parse_ies([downlink_dtx|T], <<_:7, Ena:1, Rest/bytes>>, SoFar) ->
    parse_ies(T, Rest, [{downlink_dtx, Ena} | SoFar]);

% 3.2.2.27
parse_ies([], <<?ELEM_CELL_ID_LIST:8, Rest/bytes>>, SoFar) ->
    parse_ies([cell_id_list], Rest, SoFar);
parse_ies([cell_id_list|T], <<Len:8, Body:Len/bytes, Rest/binary>>, SoFar) ->
    % XXX not done yet, see 3.2.2.27
    parse_ies(T, Rest, [{cell_id_list, {unparsed, Body}} | SoFar]);

% 3.2.2.28
parse_ies([], <<?ELEM_RSP_REQ:8, Rest/binary>>, SoFar) ->
    parse_ies([response_request], Rest, SoFar);
parse_ies([response_request|T], Rest, SoFar) ->
    parse_ies(T, Rest, [{response_request, true} | SoFar]);

% 3.2.2.29
parse_ies([], <<?ELEM_RSRC_IND_METH:8, Rest/binary>>, SoFar) ->
    parse_ies([rsrc_ind_method], Rest, SoFar);
parse_ies([rsrc_ind_method|T], <<_:4, Method:4, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{rsrc_ind_method, Method} | SoFar]);

% 3.2.2.30
parse_ies([], <<?ELEM_CLASSMARK_IND_1:8, Rest/binary>>, SoFar) ->
    parse_ies([classmark_1], Rest, SoFar);
parse_ies([classmark_1|T], <<Classmark:1/bytes, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{classmark_1, Classmark} | SoFar]);

% 3.2.2.31
parse_ies([], <<?ELEM_CIC_LIST:8, Rest/binary>>, SoFar) ->
    parse_ies([ckt_id_list], Rest, SoFar);
parse_ies([ckt_id_list|T], <<Len:8, Body:Len/bytes, Rest/binary>>, SoFar) ->
    <<Range:8, Status/bits>> = Body,
    parse_ies(T, Rest, [{ckt_id_list, {Range, Status}} | SoFar]);

% 3.2.2.32
parse_ies([], <<?ELEM_DIAG:8, Rest/binary>>, SoFar) ->
    parse_ies([diag], Rest, SoFar);
parse_ies([diag|T], <<Len:8, Body:Len/bytes, Rest/binary>>, SoFar) ->
    <<Pointer:8, _:4, PointerBit: 4, Message/binary>> = Body,
    parse_ies(T, Rest, [{diag, [{pointer, Pointer}, {bit, PointerBit}, {message, Message}]} | SoFar]);

% 3.2.2.33
parse_ies([], <<?ELEM_CHOSEN_CHAN:8, Rest/binary>>, SoFar) ->
    parse_ies([chosen_chan], Rest, SoFar);
parse_ies([chosen_chan|T], <<Mode:4, Chan:4, Rest/binary>>, SoFar) ->
    % XXX not fully decoded
    parse_ies(T, Rest, [{chosen_chan, [{mode, Mode}, {count, Chan}]} | SoFar]);

% 3.2.2.34
parse_ies([], <<?ELEM_CIPHER_RSP_MODE:8, Rest/binary>>, SoFar) ->
    parse_ies([cipher_resp_mode], Rest, SoFar);
parse_ies([cipher_resp_mode|T], <<_:7, Mode:1, Rest/binary>>, SoFar) ->
    case Mode of
	1 -> parse_ies(T, Rest, [{cipher_resp_mode, include_imeisv} | SoFar]);
	0 -> parse_ies(T, Rest, [{cipher_resp_mode, exclude_imeisv} | SoFar])
    end;

% 3.2.2.35
parse_ies([], <<?ELEM_L3_BODY:8, Rest/binary>>, SoFar) ->
    parse_ies([l3_body], Rest, SoFar);
parse_ies([l3_body|T], <<Len:8, Message:Len/bytes, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{l3_body, {unparsed, Message}} | SoFar]);

% 3.2.2.36
parse_ies([], <<?ELEM_CHAN_NEEDED:8, Rest/binary>>, SoFar) ->
    parse_ies([chan_needed], Rest, SoFar);
parse_ies([chan_needed|T], <<_:6, Type:2, Rest/binary>>, SoFar) ->
    if
	Type == 2#00 ->
	    Needed = any;
	Type == 2#01 ->
	    Needed = sdcch;
	Type == 2#10 ->
	    Needed = tchf;
	Type == 2#11 ->
	    % either TCH/H or TCH/F will do
	    Needed = tchfh
    end,
    parse_ies(T, Rest, [{chan_needed, Needed} | SoFar]);

% 3.2.2.37
parse_ies([], <<?ELEM_TRACE_TYPE:8, Rest/binary>>, SoFar) ->
    parse_ies([trace_type], Rest, SoFar);
parse_ies([trace_type|T], <<Type:8, Rest/binary>>, SoFar) ->
    % XXX not fully decoded, see 08.08 sec 3.2.2.37
    parse_ies(T, Rest, [{trace_type, Type} | SoFar]);

% 3.2.2.38
parse_ies([], <<?ELEM_TRIGGER_ID:8, Rest/binary>>, SoFar) ->
    parse_ies([trigger_id], Rest, SoFar);
parse_ies([trigger_id|T], <<Len:8, Message:Len/bytes, Rest/binary>>, SoFar) ->
    % XXX what sort of identity is this
    parse_ies(T, Rest, [{trigger_id, {unparsed, Message}} | SoFar]);

% 3.2.2.39
parse_ies([], <<?ELEM_TRACE_REF:8, Rest/binary>>, SoFar) ->
    parse_ies([trace_ref], Rest, SoFar);
parse_ies([trace_ref|T], <<TraceRef:16, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{trace_ref, TraceRef} | SoFar]);

% 3.2.2.40
parse_ies([], <<?ELEM_TRANS_ID:8, Rest/binary>>, SoFar) ->
    parse_ies([trans_id], Rest, SoFar);
parse_ies([trans_id|T], <<2:8, TransNr:16, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{trans_id, TransNr} | SoFar]);

% 3.2.2.41
parse_ies([], <<?ELEM_MOBILE_ID:8, Rest/binary>>, SoFar) ->
    parse_ies([mobile_id], Rest, SoFar);
parse_ies([mobile_id|T], <<Len:8, Message:Len/bytes, Rest/binary>>, SoFar) ->
    Ident = common_0408:parse_mobile_id(Message),
    parse_ies(T, Rest, [{mobile_id, Ident} | SoFar]);

% 3.2.2.42
parse_ies([], <<?ELEM_OMC_ID:8, Rest/binary>>, SoFar) ->
    parse_ies([omc_id], Rest, SoFar);
parse_ies([omc_id|T], <<Len:8, ID:Len/bytes, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{omc_id, ID} | SoFar]);

% 3.2.2.43
parse_ies([], <<?ELEM_FORWARD_IND:8, Rest/binary>>, SoFar) ->
    parse_ies([forward_ind], Rest, SoFar);
parse_ies([forward_ind|T], <<_:4, Fwd:4, Rest/binary>>, SoFar) ->
    Ind = case Fwd of
	2#0001 -> forward;
	2#0010 -> forward_and_trace
    end,
    parse_ies(T, Rest, [{forward_ind, Ind} | SoFar]);

% 3.2.2.44
parse_ies([], <<?ELEM_CHOSEN_CRYPTO:8, Rest/binary>>, SoFar) ->
    parse_ies([chosen_crypto], Rest, SoFar);
parse_ies([chosen_crypto|T], <<AlgId:8, Rest/binary>>, SoFar) ->
    Algorithm = case AlgId of
		    2#0001 -> none;
		    2#0010 -> a51;
		    2#0011 -> a52;
		    2#0100 -> a53;
		    2#0101 -> a54;
		    2#0110 -> a55;
		    2#0111 -> a56;
		    2#1000 -> a57;
		    _ -> unknown
		end,
    parse_ies(T, Rest, [{chosen_crypto, Algorithm} | SoFar]);

% 3.2.2.45
parse_ies([], <<?ELEM_CKT_POOL:8, Rest/binary>>, SoFar) ->
    parse_ies([ckt_pool], Rest, SoFar);
parse_ies([ckt_pool|T], <<Number, Rest/bytes>>, SoFar) ->
    parse_ies(T, Rest, [{ckt_pool, Number} | SoFar]);

% 3.2.2.46
parse_ies([], <<?ELEM_CKT_POOL_LIST:8, Rest/binary>>, SoFar) ->
    parse_ies([ckt_pool_list], Rest, SoFar);
parse_ies([ckt_pool_list|T], <<Len:8, Message:Len/bytes, Rest/binary>>, SoFar) ->
    % XXX not done, I'm lazy
    parse_ies(T, Rest, [{ckt_pool_list, {unparsed, Message}} | SoFar]);

% 3.2.2.47
parse_ies([], <<?ELEM_TIME_IND:8, Rest/binary>>, SoFar) ->
    parse_ies([time_ind], Rest, SoFar);
parse_ies([time_ind|T], <<Time:8, Rest/binary>>, SoFar) ->
    if
	Time == 255 ->
	    parse_ies(T, Rest, [{time_ind, inf} | SoFar]);
	true ->
	    parse_ies(T, Rest, [{time_ind, Time*10} | SoFar])
    end;

% 3.2.2.48
parse_ies([], <<?ELEM_RSRC_SITUATION:8, Rest/binary>>, SoFar) ->
    parse_ies([resource_situation], Rest, SoFar);
parse_ies([resource_situation|T], <<Len:8, Message:Len/bytes, Rest/binary>>, SoFar) ->
    % XXX Complex reporting message, see 08.08 sec 3.2.2.48
    parse_ies(T, Rest, [{resource_situation, {unparsed, Message}} | SoFar]);

% 3.2.2.49
parse_ies([], <<?ELEM_CURRENT_CHAN:8, Rest/binary>>, SoFar) ->
    parse_ies([current_chan], Rest, SoFar);
parse_ies([current_chan|T], <<Mode:4, Chan:4, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{current_chan, {Mode, Chan}} | SoFar]);

% 3.2.2.50
parse_ies([], <<?ELEM_QUEUE_IND:8, Rest/binary>>, SoFar) ->
    parse_ies([queue_ind], Rest, SoFar);
parse_ies([queue_ind|T], <<_:6, Rec:1, _:1, Rest/binary>>, SoFar) ->
    if
	Rec == 0 ->
	    parse_ies(T, Rest, [{queue_ind, disallow} | SoFar]);
	Rec == 1 ->
	    parse_ies(T, Rest, [{queue_ind, allow} | SoFar])
    end;

% 3.2.2.51
parse_ies([], <<?ELEM_SPEECH_VERSION:8, Rest/binary>>, SoFar) ->
    parse_ies([speech_version], Rest, SoFar);
parse_ies([speech_version|T], <<_:1, Version:7, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{speech_version, Version} | SoFar]);

% 3.2.2.52
parse_ies([], <<?ELEM_ASSIGN_REQMT:8, Rest/binary>>, SoFar) ->
    parse_ies([assign_reqmt], Rest, SoFar);
parse_ies([assign_reqmt|T], <<Mode:8, Rest/binary>>, SoFar) ->
    if
	Mode == 2#00000001 ->
	    parse_ies(T, Rest, [{assign_reqmt, immediate} | SoFar]);
	Mode == 2#00000000 ->
	    parse_ies(T, Rest, [{assign_reqmt, delayed} | SoFar])
    end;

% 3.2.2.54
parse_ies([], <<?ELEM_TALKER_FLAG:8, Rest/binary>>, SoFar) ->
    parse_ies([talker_flag], Rest, SoFar);
parse_ies([talker_flag|T], Rest, SoFar) ->
    parse_ies(T, Rest, [{talker_flag, true} | SoFar]);

% 3.2.2.55
parse_ies([], <<?ELEM_GRP_CALL_REF:8, Rest/binary>>, SoFar) ->
    parse_ies([group_call_ref], Rest, SoFar);
parse_ies([group_call_ref|T], <<Len:8, Message:Len/bytes, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{group_call_ref, {unparsed, Message}} | SoFar]);

% 3.2.2.56
parse_ies([], <<?ELEM_EMLPP_PRIO:8, Rest/binary>>, SoFar) ->
    parse_ies([emlpp_prio], Rest, SoFar);
parse_ies([emlpp_prio|T], <<_:5, Pri:3, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{emlpp_prio, Pri} | SoFar]);

% 3.2.2.57
parse_ies([], <<?ELEM_CONF_EVOL_IND:8, Rest/binary>>, SoFar) ->
    parse_ies([conf_evol_ind], Rest, SoFar);
parse_ies([conf_evol_ind|T], <<_:4, SMI:4, Rest/binary>>, SoFar) ->
    parse_ies(T, Rest, [{conf_evol_ind, SMI} | SoFar]);

% catchall
parse_ies([], <<Type:8, Len:8, Message:Len/bytes, Rest/binary>>, SoFar) ->
    parse_ies([], Rest, [{unknown, {Type, Message}} | SoFar]).

