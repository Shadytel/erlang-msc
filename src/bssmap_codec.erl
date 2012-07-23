-module(bssmap_codec).
-author('Duncan Smith <Duncan@xrtc.net>').
-include_lib("emsc/include/bssmap.hrl").

-export([parse_bssmap/1, encode_bssmap/1]).



%parse_bssmap(?BSSMAP_ASSIGN_REQ, DataBin) ->
%    {ok, ChanLen, ChanType} = parse_el_channeltype(DataBin),
%    {ok, L3Len, L3Head} = parse_el_l3head(binary:part(DataBin, ChanLen, 4)),
%    {ok, PrioLen, Priority} = parse_el_priority(binary:part(DataBin, ChanLen+L3Len, 3)),
%    {ok, CICLen, CIC} = parse_el_cic(binary:part(DataBin, ChanLen+L3Len+PrioLen, 3)),
%    {ok, 

parse_bssmap(<<Type:8, Bin/binary>>) ->
    {Type, parse_el_list(Bin)}.

encode_bssmap({Type, Args}) ->
    ok.

encode_el_list(Order, Attrs) ->
    encode_el_list(Order, Attrs, <<>>).

encode_el_list([This|Order], Attrs, SoFar) ->
    encode_el(This, proplists:get_value(This, Attrs)).

encode_el(Type, Value) ->
    <<>>.

parse_el_list(DataBin) ->
    lists:reverse(parse_el_list(DataBin, [])).
parse_el_list(<<>>, List) ->
    List;
parse_el_list(DataBin, List) ->
    {ok, Length, Element} = parse_el(DataBin),
    parse_el_list(binary:part(DataBin, Length, byte_size(DataBin)-Length), [Element|List]).

parse_el(DataBin) ->
    <<Element:8, Rest/binary>> = DataBin,
    parse_el(Element, Rest).

parse_el(?ELEM_CELL_ID, DataBin) ->
    <<Len:8, _:4, Type:4, _Rest/binary>> = DataBin,
    Length = Len+2,
    case Type of
	2#0000 ->
	    <<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4, LAC:16/big, CI:16/big, _/binary>> = _Rest,
	    MCC = MCC3 + MCC2*10 + MCC1*100,
	    MNC = MNC2 + MNC1*10,
	    {ok, Length, #cell_id{mcc=MCC, mnc=MNC, cid=CI, lac=LAC}};
	2#0001 ->
	    <<LAC:16/big, CI:16/big, _/binary>> = _Rest,
	    {ok, Length, #cell_id{cid=CI, lac=LAC}};
	2#0010 ->
	    <<CI:16/big, _/binary>> = _Rest,
	    {ok, Length, #cell_id{cid=CI}};
	2#0011 ->
	    {ok, Length, #cell_id{}}
    end;
parse_el(?ELEM_CKT_ID, Bin) ->
    <<Multiplex:11, Slot:5, _/bytes>> = Bin,
    {ok, 3, #circuit_id{ pcm=Multiplex, timeslot=Slot }};
parse_el(?ELEM_CONN_REL_REQTED, Bin) ->
    {ok, 1, release_requested};
parse_el(?ELEM_RSRC_AVAIL, Bin) ->
    % XXX unimplemented
    {ok, 21, unimpl};
parse_el(?ELEM_CAUSE, Bin) ->
    <<Len:8, Ext:1, Class:3, Value:4, _/bytes>> = Bin,
    {ok, Len+2, #cause{ class=Class, value=Value }};
parse_el(?ELEM_IMSI, Bin) ->
    <<Len:8, _/bytes>> = Bin,
    {ok, Len+2, #imsi{ number=binary:part(Bin, 2, Len) }};
parse_el(?ELEM_TMSI, Bin) ->
    <<8:8, TMSI:48/bytes, _/bytes>> = Bin,
    {ok, 8+2, #tmsi{ number=TMSI }};
parse_el(?ELEM_MS_COUNT, Bin) ->
    <<Count:8, _/bytes>> = Bin,
    {ok, 2, #ms_count{ number=Count }};
parse_el(?ELEM_L3_HEAD, Bin) ->
    <<Len:8, ProtoId:8, TransId:8, _/bytes>> = Bin,
    io:format("L3 Head at ~p of len ~p~n", [Bin, Len]),
    {ok, 4, #l3{ protocol=ProtoId, transaction=TransId, body=binary:part(Bin, 2, Len - 1) }};
parse_el(?ELEM_CRYPTO_INFO, Bin) ->
    <<Len:8, A50:1, A51:1, A52:1, A53:1, A54:1, A55:1, A56:1, A57:1, _/bytes>> = Bin,
    if
	(A51+A52+A53+A54+A55+A56+A57 > 0) ->
	    <<_:16, Key:64, _/bytes>> = Bin
       end,
    {ok, Len+2, #crypto_info{ key=Key, a50=A50, a51=A51, a52=A52, a53=A53, a54=A54, a55=A55, a56=A56, a57=A57 }};
parse_el(?ELEM_CHAN_TYPE, Bin) ->
    % XXX unimplemented
    <<Len:8, _:4, Speech:4, Rate:8, Ext:1, Version:7, _/binary>> = Bin,
    {ok, Len+2, #chan_type{ mode=Speech, ratetype=Rate, versions=[Version]}};
parse_el(?ELEM_PERIODICITY, Bin) ->
    % XXX not quite compliant, see 3.2.2.12 of ETSI GSM TS 08.08.
    <<Number, _/binary>> = Bin,
    {ok, 2, #period{ period=Number }};
parse_el(?ELEM_EXT_RSRC_IND, Bin) ->
    <<_:6, Sm:1, Tarr:1, _/binary>> = Bin,
    {ok, 2, #ext_rsrc{ tarr=Tarr, mode=Sm }};
parse_el(?ELEM_TOTAL_AVAIL, Bin) ->
    <<Fr:16/little, Hr:16/little, _/binary>> = Bin,
    {ok, 5, #rsrcs_available{ halfrate=Hr, fullrate=Fr }};
parse_el(?ELEM_PRIORITY, Bin) ->
    <<_:1, Pci:1, Level:4, Queueing:1, Vuln:1, _/binary>> = Bin,
    {ok, 2, #priority{ can_preempt=Pci, priority=Level, allow_queueing=Queueing, vulnerable=Vuln }};
parse_el(?ELEM_CLASSMARK_IND_2, Bin) ->
    % XXX I'm not yet decoding the classmark, see 3.2.2.19 of ETSI GSM TS 08.08.
    <<Len:8, Classmark:3/bytes, _/binary>> = Bin,
    {ok, Len+2, #classmark2{ classmark=Classmark }};
parse_el(?ELEM_CLASSMARK_IND_3, Bin) ->
    <<Len:8, Classmark:3/bytes, _/binary>> = Bin,
    {ok, Len+2, #classmark3{ classmark=Classmark }};
parse_el(?ELEM_INTERFER_BAND, Bin) ->
    % Not yet decoding
    <<_:3, Bands:5, _/binary>> = Bin,
    {ok, 2, #interference{ band_mask=Bands }};
parse_el(?ELEM_L3_BODY, Bin) ->
    % "Layer 3 Information", 08.08 sec 3.2.2.24
    <<Len:8, _/binary>> = Bin,
    io:format("L3 Info at ~p of len ~p~n", [Bin, Len]),
    {ok, Len+2, #l3{ body=binary:part(Bin, 2, Len - 1) }};
parse_el(?ELEM_DLCI, Bin) ->
    <<Dlci:8, _/binary>> = Bin,
    {ok, 2, #dlci{ dlci=Dlci }};
parse_el(?ELEM_DOWNLINK_DTX, Bin) ->
    % XXX not yet decoded, 3.2.2.26 of 08.08.
    <<Dtx:8, _/binary>> = Bin,
    {ok, 2, #dtx{ dtx=Dtx }};
parse_el(?ELEM_CELL_ID_LIST, Bin) ->
    <<Len:8, _:4, Discrim:4, _/binary>> = Bin,
    % XXX not done yet, see 3.2.2.28
    {ok, Len+2, []};
parse_el(?ELEM_RSP_REQ, Bin) ->
    {ok, 1, response_request};
parse_el(?ELEM_RSRC_IND_METH, Bin) ->
    <<_:4, Method:4, _/binary>> = Bin,
    {ok, 2, #rsrc_ind_method{ method=Method }};
parse_el(?ELEM_CLASSMARK_IND_1, Bin) ->
    <<Mark:1/bytes, _/binary>> = Bin,
    {ok, 2, #classmark1{ classmark=Mark }};
parse_el(?ELEM_CIC_LIST, Bin) ->
    <<Len:8, Range:8, Rest/binary>> = Bin,
    {ok, Len+2, #cic_list{ circuits=binary:part(Rest, 0, Len) }};
parse_el(?ELEM_DIAG, Bin) ->
    <<Len:8, Pointer:8, _:4, PointerBit: 4, Message/binary>> = Bin,
    {ok, Len+2, #diag{ index=Pointer, bit=PointerBit, message=binary:part(Message, 0, Len-2) }};
parse_el(?ELEM_CHOSEN_CHAN, Bin) ->
    <<Mode:4, Chan:4, _/binary>> = Bin,
    % XXX not fully decoded
    {ok, 2, #chan_kind{ mode=Mode, count=Chan }};
parse_el(?ELEM_CIPHER_RSP_MODE, Bin) ->
    <<_:7, Mode:1, _/binary>> = Bin,
    if
	Mode == 1 ->
	    {ok, 2, #cipher_rsp_mode{ mode=include_imeisv }};
	Mode == 0 ->
	    {ok, 2, #cipher_rsp_mode{ mode=exclude_imeisv }}
    end;
parse_el(?ELEM_CHAN_NEEDED, Bin) ->
    <<_:6, Type:2, _/binary>> = Bin,
    if
	Type == 2#00 ->
	    {ok, 2, #chan_needed{ type=any }};
	Type == 2#01 ->
	    {ok, 2, #chan_needed{ type=sdcch }};
	Type == 2#10 ->
	    {ok, 2, #chan_needed{ type=tchf }};
	Type == 2#11 ->
	    % either will do
	    {ok, 2, #chan_needed{ type=tchfh }}
    end;
parse_el(?ELEM_TRACE_TYPE, Bin) ->
    <<Type:8, _/binary>> = Bin,
    % XXX not fully decoded, see 08.08 sec 3.2.2.37
    {ok, 2, #trace_type{ type=Type }};
parse_el(?ELEM_TRIGGER_ID, Bin) ->
    <<Len:8, Rest/binary>> = Bin,
    % XXX what sort of identity is this
    {ok, Len+2, #trigger_id{ id=binary:part(Rest, 0, Len) }};
parse_el(?ELEM_TRACE_REF, Bin) ->
    <<TraceRef:16/binary, _/binary>> = Bin,
    {ok, 2, #trace_ref{ ref=TraceRef }};
parse_el(?ELEM_TRANS_ID, Bin) ->
    <<Len:8, TransNr:16, _/binary>> = Bin,
    {ok, Len+2, #trans_id{ id=TransNr }};
parse_el(?ELEM_MOBILE_ID, Bin) ->
    <<Len:8, Rest/binary>> = Bin,
    {ok, Len+2, #ms_id{ imei=binary:part(Rest, 0, Len) }};
parse_el(?ELEM_OMC_ID, Bin) ->
    <<Len:8, Rest/binary>> = Bin,
    {ok, Len+2, #omc_id{ id=binary:part(Rest, 0, Len) }};
parse_el(?ELEM_FORWARD_IND, Bin) ->
    <<_:4, Fwd:4>> = Bin,
    if
	Fwd == 2#0001 ->
	    {ok, 2, #forward{ ind=forward }};
	Fwd == 2#0010 ->
	    {ok, 2, #forward{ ind=forward_and_trace }}
    end;
parse_el(?ELEM_CHOSEN_CRYPTO, Bin) ->
    <<A50:1, A51:1, A52:1, A53:1, A54:1, A55:1, A56:1, A57:1, _/bytes>> = Bin,
    if
	(A51+A52+A53+A54+A55+A56+A57 > 0) ->
	    <<_:16, Key:64, _/bytes>> = Bin
       end,
    {ok, 2, #crypto_chosen{ key=Key, a50=A50, a51=A51, a52=A52, a53=A53, a54=A54, a55=A55, a56=A56, a57=A57 }};
parse_el(?ELEM_CKT_POOL, Bin) ->
    <<Number, _/bytes>> = Bin,
    {ok, 2, #circuit_pool{ pool=Number }};
parse_el(?ELEM_CKT_POOL_LIST, Bin) ->
    <<Len, Rest/binary>> = Bin,
    % XXX not done, I'm lazy
    {ok, Len+2, #circuit_pool{ pool=binary:part(Rest, 0, Len) }};
parse_el(?ELEM_TIME_IND, Bin) ->
    <<Time, _/binary>> = Bin,
    if
	Time == 255 ->
	    {ok, 2, #time_ind{ seconds=inf }};
	true ->
	    {ok, 2, #time_ind{ seconds=Time*10 }}
    end;
parse_el(?ELEM_RSRC_SITUATION, Bin) ->
    % XXX Complex reporting message, see 08.08 sec 3.2.2.48
    <<Len, _/binary>> = Bin,
    {ok, Len+2, resource_situation_message};
parse_el(?ELEM_QUEUE_IND, Bin) ->
    <<_:6, Rec:1, _:1, _/binary>> = Bin,
    if
	Rec == 0 ->
	    {ok, 2, #queue_ind{ advice=disallow }};
	Rec == 1 ->
	    {ok, 2, #queue_ind{ advice=allow }}
    end;
parse_el(?ELEM_SPEECH_VERSION, Bin) ->
    <<_:1, Version:7, _/binary>> = Bin,
    {ok, 2, #speech_type{ type=Version }};
parse_el(?ELEM_ASSIGN_REQMT, Bin) ->
    <<Mode:8, _/binary>> = Bin,
    if
	Mode == 2#00000001 ->
	    {ok, 2, #assignment_require{ mode=immediate }};
	Mode == 2#00000000 ->
	    {ok, 2, #assignment_require{ mode=delayed }}
    end;
parse_el(?ELEM_TALKER_FLAG, Bin) ->
    {ok, 1, talker_flag};
parse_el(?ELEM_GRP_CALL_REF, Bin) ->
    <<Len:8, Group:40/binary, _/binary>> = Bin,
    {ok, Len+2, #group_ref{ ref=Group }};
parse_el(?ELEM_EMLPP_PRIO, Bin) ->
    <<_:5, Pri:3, _/binary>> = Bin,
    {ok, 2, #priority{ priority=Pri }};
parse_el(?ELEM_CONF_EVOL_IND, Bin) ->
    <<_:4, SMI:4, _/binary>> = Bin,
    {ok, 2, #evol_ind{ smi_count=SMI }};
parse_el(Type, Bin) ->
    {ok, 1, {unknown, Type}}.

