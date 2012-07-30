-module(common_0408).
-author("Duncan Smith <Duncan@xrtc.net>").

-export([parse_mobile_id/1]).

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

