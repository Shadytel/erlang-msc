-module(common_0408).
-author("Duncan Smith <Duncan@xrtc.net>").

-export([binarize/1]).
-export([parse_mobile_id/1, encode_mobile_id/2]).
-export([parse_classmark_1/1, encode_classmark_1/1]).
-export([parse_classmark_2/1, encode_classmark_2/1]).
-export([decode_0338_ascii/2, encode_0338_ascii/1]).
-export([from_hex/1, to_hex/1]).

to_hex(D) ->
    lists:nth(D+1, "0123456789abcdef").

from_hex(C) ->
    if (C >= $0), (C =< $9)  ->  C - $0;
       (C >= $a), (C =< $f)  ->  C - $a + 10;
       (C >= $A), (C =< $F)  ->  C - $A + 10;
       true -> 0
    end.

parse_dest_number(<<Ext:1, Type:3, Plan:4, T/binary>>) ->
    case Ext of
	1 -> <<Ext2:1, Present:2, _:3, Screen:2, Tail/binary>> = T;
	_ -> Ext2 = 0,
	     Present = 0,
	     Screen = 0,
	     Tail = T
    end,
    TextE = [ [ to_hex(Y), to_hex(X) ] || <<X:4, Y:4>> <= Tail],
    
    [{plan, case Plan of
		2#0000 -> unknown;
		2#0001 -> e164;
		2#0011 -> data;
		2#0100 -> telex;
		2#1000 -> national;
		2#1001 -> private;
		_ -> reserved
	    end},
     {type, case Type of
		2#000 -> unknown;
		2#001 -> international;
		2#010 -> national;
		2#011 -> network;
		2#100 -> shortcode;
		_ -> reserved
	    end},
     {presentation, case Present of
			2#00 -> allowed;
			2#01 -> restricted;
			2#10 -> unavail;
			2#11 -> reserved
		    end},
     {screening, case Screen of
		     2#00 -> unscreened;
		     2#01 -> passed;
		     2#10 -> failed;
		     2#11 -> trusted
		 end},
     {number, TextE}].


parse_mobile_id(<<Dig1:4, Odd:1, TypeBin:3, Tail/binary>>) ->
    TextE = [ [ to_hex(Y), to_hex(X) ] || <<X:4, Y:4>> <= Tail],

    case Odd of
	2#0 -> Text = list_to_binary(TextE);
	2#1 -> Text = list_to_binary([to_hex(Dig1) | TextE])
    end,
    Type = case TypeBin of
	       2#001 -> imsi;
	       2#010 -> imei;
	       2#011 -> imeisv;
	       2#100 -> tmsi;
	       _ -> undefined
	   end,
    {Type, binary_to_list(Text)}.

encode_mobile_id(Type, Text) ->
    Odd = byte_size(list_to_binary(Text)) rem 2,
    [T_Head|T_Rem] = case {Type, Odd} of
			 {tmsi, 0} -> "F" ++ Text;
			 {_, 0} -> Text ++ "F";
			 _ -> Text
		     end,
    EncText = << << (from_hex(X)):4, (from_hex(Y)):4 >> || <<Y:8, X:8>> <= list_to_binary(T_Rem) >>,
    T_bin = case Type of
		imsi -> 2#001;
		imei -> 2#010;
		imeisv -> 2#011;
		tmsi -> 2#100;
		_ -> 2#000
	    end,
    << (from_hex(T_Head)):4, Odd:1, T_bin:3, EncText/binary >>.

% encode and decode textual data as per the fucked-up packing scheme
% described in GSM TS 03.38.
decode_0338_ascii(SpareCount, Data) ->
    error.

% return {SMSCoding, CBCoding, Spare, EncodedText}
encode_0338_ascii(Text) ->
    Bitted = erlang:list_to_bitstring([lists:reverse(lists:map( ( fun (E) -> << E:7 >> end ), Text))]),
    Sp = (8 - (erlang:bit_size(Bitted) rem 8)),
    Spare = if (Sp == 8) -> 0;
	       true -> Sp
	    end,
    Encoded = erlang:list_to_binary(lists:reverse(erlang:binary_to_list(<< 0:Spare, Bitted/bits >>))),
    {2#00000000, 2#01010000, Spare, Encoded}.



parse_classmark_1(<<_:1, Rev:2, Early:1, A51:1, Power:3>>) ->
    lists:flatten([{revision, Rev},
		   {power_class, Power},
		   if (A51 == 1) -> {encryption, [a51]};
		      true -> [] end,
		   {early_sending, (Early == 1)}
		  ]).

binarize(true) -> 1;
binarize(false) -> 0;
binarize(1) -> 1;
binarize(0) -> 0;
binarize(_) -> 0.

encode_classmark_1(Marks) ->
    <<0:1,
      (proplists:get_value(revision, Marks)):2,
      (binarize(proplists:get_value(early_sending, Marks))):1,
      (binarize(lists:member(a51, proplists:get_value(encryption, Marks)))):1,
      (proplists:get_value(power_class, Marks)):3>>.

parse_classmark_2(<<CM1:8/bits, _:1, PS:1, Screen:2, SMS:1, 0:2, Freq:1, CM3:1, _:5, A53:1, A52:1>>) ->
    CM_a = lists:flatten([parse_classmark_1(CM1),
			  {pseudosync, (PS ==1)},
			  {ss_screening, (Screen == 1)},
			  {sms, (SMS == 1)},
			  if (A53 == 1) -> {encryption, a53};
			     true -> [] end,
			  if (A52 == 1) -> {encryption, a52};
			     true -> [] end,
			  {freq_ext, (Freq == 1)},
			  {has_cm3, (CM3 == 1)}
			 ]),
    Crypto = proplists:append_values(encryption, CM_a),
    [{encryption, Crypto} | proplists:delete(encryption, CM_a)].

encode_classmark_2(Marks) ->
    <<(encode_classmark_1(Marks)):8/bits,
      0:1,
      (binarize(proplists:get_value(pseudosync, Marks))):1,
      (binarize(proplists:get_value(ss_screening, Marks))):2,
      (binarize(proplists:get_value(sms, Marks))):1,
      0:2,
      (binarize(proplists:get_value(freq_ext, Marks))):1,
      (binarize(proplists:get_value(has_cm3, Marks))):1,
      0:5,
      (binarize(lists:member(a53, proplists:get_value(encryption, Marks)))):1,
      (binarize(lists:member(a52, proplists:get_value(encryption, Marks)))):1
    >>.
