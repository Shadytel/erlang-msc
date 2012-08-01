-module(common_0408).
-author("Duncan Smith <Duncan@xrtc.net>").

-export([parse_mobile_id/1, encode_mobile_id/2,
	 parse_classmark_1/1, encode_classmark_1/1,
	 parse_classmark_2/1, encode_classmark_2/1]).

parse_mobile_id(<<Dig1:4, Odd:1, TypeBin:3, Tail/binary>>) ->

    TextE = [ [ Y+ $0, X+ $0] || <<X:4,Y:4>> <= Tail],

    case Odd of
	2#0 -> Text = list_to_binary(TextE);
	2#1 -> Text = list_to_binary([Dig1+16#30 | TextE])
    end,
    Type = case TypeBin of
	       2#001 -> imsi;
	       2#010 -> imei;
	       2#011 -> imeisv;
	       2#100 -> tmsi;
	       _ -> undefined
	   end,
    {Type, Text}.

encode_mobile_id(Type, Text) ->
    ok.

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
