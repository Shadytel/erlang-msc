-module(bssap_codec).
-author('Duncan Smith <Duncan@xrtc.net>').
-include_lib("emsc/include/bssmap.hrl").

parse_message(DataBin) ->
    Discrim = binary:first(DataBin),
    Parsed = parse_msgt(Discrim, DataBin),
    case Discrim of
	16#00 ->
	    {ok, bssmap, Parsed};
	16#01 ->
	    {ok, dtap, Parsed}
    end.

% BSSMAP message
parse_msgt(?SCCP_DISCRIM_BSSMAP, DataBin) ->
    <<_:8, Length:8, Type:8, Remain/binary>> = DataBin,
    ok;
parse_msgt(?SCCP_DISCRIM_DTAP, DataBin) ->
    <<_:8, DLCI:8, Length:8, Remain/binary>> = DataBin,
    ok.

%parse_bssmap(?BSSMAP_ASSIGN_REQ, DataBin) ->
%    {ok, ChanLen, ChanType} = parse_el_channeltype(DataBin),
%    {ok, L3Len, L3Head} = parse_el_l3head(binary:part(DataBin, ChanLen, 4)),
%    {ok, PrioLen, Priority} = parse_el_priority(binary:part(DataBin, ChanLen+L3Len, 3)),
%    {ok, CICLen, CIC} = parse_el_cic(binary:part(DataBin, ChanLen+L3Len+PrioLen, 3)),
%    {ok, 
parse_bssmap(?BSSMAP_COMPL_L3_INF, DataBin) ->
    {ok, CI_len, CellId} = parse_el(DataBin),
    {ok, L3_len, L3_info} = parse_el(binary:part(DataBin, CI_len)),
    % optional parts
    Channel = parse_el(binary:part(DataBin, CI_len+L3_len)),
    ok.

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
    <<Len:8, _:4, Type:4, Rest/bytes>> = DataBin,
    Length = Len+3,
    case Type of
	2#0000 ->
	    % cell global id is used
	    <<MCC_2:4, MCC_1:4, 2#1111:4, MCC_3:4, MNC_2:4, MNC_1:4, LAC:16/big, CI:16/big, _/bytes>> = Rest,
	    MCC = MCC_3 + MCC_2*10 + MCC_1*100,
	    MNC = MNC_2 + MNC_1*10,
	    {ok, Length, #cell_id{mcc=MCC, mnc=MNC, cid=CI, lac=LAC}};
	2#0001 ->
	    <<LAC:16/big, CI:16/big, _/bytes>> = Rest,
	    {ok, Length, #cell_id{cid=CI, lac=LAC}};
	2#0010 ->
	    <<CI:16/big, _/bytes>> = Rest,
	    {ok, Length, #cell_id{cid=CI}};
	2#0011 ->
	    {ok, Length, #cell_id{}}
	end.


