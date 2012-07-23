-module(bcd).
-compile(export_all).
% pack the digits of an integer as BCD in a given size of binary
% pad with leading zeros
encode(N, Size) ->
    << <<X:4>> || X <- lists:flatten(io_lib:fwrite("~*..0B", [Size*2, N])) >>.
% unpack the given size of BCD binary into an integer
% strip leading zeros
decode(N, Size) ->
    decode(N, Size, <<>>).

decode(_, 0, Text) ->
    Text;
decode(<<N1:4, N2:4, Num/binary>>, Size, Text) ->
    decode(Num, Size-1, <<Text/binary, (N2+16#30), (N1+16#30)>>).
