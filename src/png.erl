-module(png).

-export([header/0, chunk/1, chunk/2]).

-include_lib("png/include/png.hrl").


header() ->
    <<16#89, 16#50, 16#4E, 16#47, 16#0D, 16#0A, 16#1A, 16#0A>>.


chunk('IHDR', #png_config{width = Width,
                        height = Height,
                        bit_depth = BitDepth,
                        color_type = ColorType}) ->
    % We only support basic compression, filter and interlace methods
    CompressionMethod = 0,
    FilterMethod = 0,
    InterlaceMethod = 0,
    Data = <<Width:32,
             Height:32,
             BitDepth:8,
             ColorType:8,
             CompressionMethod:8,
             FilterMethod:8,
             InterlaceMethod:8>>,
    chunk(<<"IHDR">>, Data);

chunk('IDAT', {rows, Rows}) ->
    Raw = list_to_binary([[0, Row] || Row <- Rows]),
    chunk('IDAT', {raw, Raw});

chunk('IDAT', {raw, Data}) ->
    chunk('IDAT', {compressed, compress(Data)});

chunk('IDAT', {compressed, CompressedData}) ->
    chunk(<<"IDAT">>, CompressedData);

chunk('PLTE', {rgb, BitDepth, ColorTuples}) ->
    L = [<<R:BitDepth, G:BitDepth, B:BitDepth>> || {R, G, B} <- ColorTuples],
    chunk(<<"PLTE">>, list_to_binary(L));

chunk(Type, Data) when is_binary(Type), is_binary(Data) ->
    Length = byte_size(Data),
    TypeData = <<Type/binary, Data/binary>>,
    Crc = erlang:crc32(TypeData),
    <<Length:32, TypeData/binary, Crc:32>>.

chunk('IEND') ->
    chunk(<<"IEND">>, <<>>).


compress(Data) ->
    Z = zlib:open(),
    zlib:deflateInit(Z),
    B2 = zlib:deflate(Z, Data, finish),
    zlib:deflateEnd(Z),
    zlib:close(Z),
    list_to_binary(B2).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

header_test() ->
    Result = header(),
    Target = <<16#89, 16#50, 16#4E, 16#47, 16#0D, 16#0A, 16#1A, 16#0A>>,
    ?assertEqual(Target, Result).

'IHDR_width_height_test'() ->
    Result = chunk('IHDR', #png_config{width = 32,
                                     height = 16}),
    Target = <<0,0,0,13,73,72,68,82,0,0,0,32,0,0,0,16,1,0,0,0,0,95,123,64,244>>,
    ?assertEqual(Target, Result).

'IHDR_width_height_bit_depth_test'() ->
    Result = chunk('IHDR', #png_config{width = 32,
                                     height = 16,
                                     bit_depth = 8}),
    Target = <<0,0,0,13,73,72,68,82,0,0,0,32,0,0,0,16,8,0,0,0,0,82,107,34,133>>,
    ?assertEqual(Target, Result).

'IHDR_width_height_bit_depth_color_type_test'() ->
    Result = chunk('IHDR', #png_config{width = 32,
                                     height = 16,
                                     bit_depth = 8,
                                     color_type = ?PNG_RGB}),
    Target = <<0,0,0,13,73,72,68,82,0,0,0,32,0,0,0,16,8,2,0,0,0,248,98,234,14>>,
    ?assertEqual(Target, Result).

compressed_IDAT_test() ->
    Data = <<0, 1, 2, 3>>,
    Result = chunk('IDAT', {compressed, Data}),
    Target = Data,
    ?assertEqual(Target, Data).

raw_IDAT_test() ->
    Data = <<0, 1, 2, 3>>,
    Result = chunk('IDAT', {raw, Data}),
    Target = <<0,0,0,12,73,68,65,84,120,156,99,96,100,98,6,0,0,14,0,7,215,111,228,120>>,
    ?assertEqual(Target, Result).

'PLTE_test'() ->
    Result = chunk('PLTE', {rgb, 8, [{255, 0, 0}, {0, 0, 255}]}),
    Target = <<0,0,0,6,80,76,84,69,255,0,0,0,0,255,108,161,253,142>>,
    ?assertEqual(Target, Result).


-endif.
