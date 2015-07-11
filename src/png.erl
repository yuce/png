-module(png).

-export([create/1, append/2, close/1]).
-export([check_config/1]).
-export([header/0, chunk/1, chunk/2]).

-include_lib("png/include/png.hrl").

-define(SCANLINE_FILTER, 0).


create(#{file := File} = Png) ->
    Callback = fun(Data) ->
                    file:write(File, Data) end,
    Png2 = maps:remove(file, Png#{call => Callback}),
    create(Png2);

create(#{size := {Width, Height} = Size,
         mode := Mode,
         call := Callback} = Png) ->
    Config = #png_config{size = {Width, Height},
                         mode = Mode},
    ok = Callback(header()),
    ok = Callback(chunk('IHDR', Config)),
    ok = append_palette(Png),
    Z = zlib:open(),
    ok = zlib:deflateInit(Z),
    #{size => Size,
      mode => Mode,
      call => Callback,
      z => Z}.

append_palette(#{call := Callback, palette := Palette}) ->
    Chunk = chunk('PLTE', Palette),
    ok = Callback(Chunk);

append_palette(#{}) ->
    ok.


append(Png, {row, Row}) ->
    append(Png, {data, [0, Row]});

append(Png, {rows, Rows}) ->
    F = fun(Row) ->
        [0, Row] end,
    append(Png, {data, lists:map(F, Rows)});

append(#{z := Z} = Png, {data, RawData}) ->
    Compressed = zlib:deflate(Z, RawData),
    append(Png, {compressed, Compressed}),
    Png;

append(Png, {compressed, []}) ->
    Png;

append(#{call := Callback} = Png, {compressed, Compressed}) ->
    Chunks = chunk('IDAT', {compressed, Compressed}),
    ok = Callback(Chunks),
    Png.

close(#{z := Z, call := Callback} = Png) ->
    Compressed = zlib:deflate(Z, <<>>, finish),
    append(Png, {compressed, Compressed}),
    ok = zlib:deflateEnd(Z),
    ok = zlib:close(Z),
    ok = Callback(chunk('IEND')),
    ok.


valid_mode({ColorType, _}) when ColorType /= grayscale,
                                ColorType /= grayscale_alpha,
                                ColorType /= indexed,
                                ColorType /= rgb,
                                ColorType /= rgba ->
    {error, invalid};

valid_mode({_, Bits}) when Bits /= 1,
                           Bits /= 2,
                           Bits /= 4,
                           Bits /= 8,
                           Bits /= 16 ->
    {error, invalid};

valid_mode({_, 8}) ->
    ok;

valid_mode({indexed, 16}) ->
    {error, invalid};

valid_mode({_, 16}) ->
    ok;

valid_mode({_, _}) ->
    {error, unsupported}.


check_config(#png_config{size = {Width, Height}})
                when Width < 1 orelse Height < 1 ->
    {error, invalid};

check_config(#png_config{mode = Mode,
                         compression_method = 0,
                         filter_method = 0,
                         interlace_method = 0}) ->
    valid_mode(Mode);

check_config(_) ->
    {error, not_supported}.


header() ->
    <<16#89, 16#50, 16#4E, 16#47, 16#0D, 16#0A, 16#1A, 16#0A>>.


chunk('IHDR', #png_config{size = {Width, Height},
                          mode = {ColorType, BitDepth}}) ->
    % We only support basic compression, filter and interlace methods
    CompressionMethod = 0,
    FilterMethod = 0,
    InterlaceMethod = 0,
    ColorTypeByte = case ColorType of
                        grayscale -> 0;
                        rgb -> 2;
                        indexed -> 3;
                        grayscale_alpha -> 4;
                        rgba -> 6 end,
    Data = <<Width:32,
             Height:32,
             BitDepth:8,
             ColorTypeByte:8,
             CompressionMethod:8,
             FilterMethod:8,
             InterlaceMethod:8>>,
    chunk(<<"IHDR">>, Data);

chunk('IDAT', {rows, Rows}) ->
    % We don't currently support any scanline filters (other than None)
    Raw = list_to_binary([[?SCANLINE_FILTER, Row] || Row <- Rows]),
    chunk('IDAT', {raw, Raw});

chunk('IDAT', {raw, Data}) ->
    chunk('IDAT', {compressed, compress(Data)});

chunk('IDAT', {compressed, CompressedData}) when is_list(CompressedData) ->
    F = fun(Part) ->
        chunk(<<"IDAT">>, Part) end,
    lists:map(F, CompressedData);

chunk('PLTE', {rgb, BitDepth, ColorTuples}) ->
    L = [<<R:BitDepth, G:BitDepth, B:BitDepth>> || {R, G, B} <- ColorTuples],
    chunk(<<"PLTE">>, list_to_binary(L));

chunk(Type, Data) when is_binary(Type),
                       is_binary(Data) ->
    Length = byte_size(Data),
    TypeData = <<Type/binary, Data/binary>>,
    Crc = erlang:crc32(TypeData),
    <<Length:32, TypeData/binary, Crc:32>>.

chunk('IEND') ->
    chunk(<<"IEND">>, <<>>).


-spec compress(binary()) -> binary().
compress(Data) ->
    Z = zlib:open(),
    ok = zlib:deflateInit(Z),
    Compressed = zlib:deflate(Z, Data, finish),
    ok = zlib:deflateEnd(Z),
    ok = zlib:close(Z),
    Compressed.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

create_append_test() ->
    E = ets:new(state, []),

    Cb = fun(Bin) ->
            NewContents = case ets:lookup(E, contents) of
                                [] ->
                                    [Bin];
                                [{_, Contents}] ->
                                    [Contents, Bin] end,
        true = ets:insert(E, {contents, list_to_binary(NewContents)}),
        ok end,

    Png = create(#{size => {4, 2},
                   mode => {indexed, 8},
                   call => Cb,
                   palette => {rgb, 8, [{255, 0, 0}, {0, 0, 255}]}}),
    Png = append(Png, {rows, [0, 1, 0, 1]}),
    ok = png:close(Png),
    [{_, Result}] = ets:lookup(E, contents),
    Target = <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,4,0,0,0,2,
               8,3,0,0,0,
               72,118,141,81,0,0,0,6,80,76,84,69,255,0,0,0,0,255,108,161,253,
               142,0,0,0,
               14,73,68,65,84,120,156,99,96,96,96,100,0,98,0,0,14,0,3,216,95,
               69,48,0,0,0,0,73,69,78,68,174,66,96,130>>,
    ?assertEqual(Target, Result).

check_config_test() ->
    Config1 = #png_config{},
    ?assertEqual({error, invalid}, check_config(Config1)),

    Config2 = #png_config{size = {0, 1}},
    ?assertEqual({error, invalid}, check_config(Config2)),

    Config3 = #png_config{size = {1, 1}},
    ?assertEqual(ok, check_config(Config3)),

    Config4 = #png_config{size = {1, 1}, mode = ?PNG_INDEXED_8},
    ?assertEqual(ok, check_config(Config4)).

header_test() ->
    Result = header(),
    Target = <<16#89, 16#50, 16#4E, 16#47, 16#0D, 16#0A, 16#1A, 16#0A>>,
    ?assertEqual(Target, Result).

'IHDR_width_height_test'() ->
    Result = chunk('IHDR', #png_config{size = {32, 16}}),
    Target = <<0,0,0,13,73,72,68,82,0,0,0,
               32,0,0,0,16,8,0,0,0,0,82,107,34,133>>,
    ?assertEqual(Target, Result).

'IHDR_width_height_bit_depth_test'() ->
    Result = chunk('IHDR', #png_config{size = {32, 16},
                                       mode = ?PNG_GRAYSCALE_8}),
    Target = <<0,0,0,13,73,72,68,82,0,0,0,
               32,0,0,0,16,8,0,0,0,0,82,107,34,133>>,
    ?assertEqual(Target, Result).

'IHDR_width_height_bit_depth_color_type_test'() ->
    Result = chunk('IHDR', #png_config{size = {32, 16},
                                       mode = ?PNG_RGB_8}),
    Target = <<0,0,0,13,73,72,68,82,0,0,0,
               32,0,0,0,16,8,2,0,0,0,248,98,234,14>>,
    ?assertEqual(Target, Result).

compressed_IDAT_test() ->
    Data = [<<0, 1, 2, 3>>],
    Result = chunk('IDAT', {compressed, Data}),
    Target = [<<0,0,0,4,73,68,65,84,0,1,2,3,64,222,190,8>>],
    ?assertEqual(Target, Result).

raw_IDAT_test() ->
    Data = <<0, 1, 2, 3>>,
    Result = chunk('IDAT', {raw, Data}),
    Target = [<<0,0,0,12,73,68,65,84,120,156,
                99,96,100,98,6,0,0,14,0,7,215,111,228,120>>],
    ?assertEqual(Target, Result).

rows_IDAT_test() ->
    Data = [<<1, 2, 3>>],
    Result = chunk('IDAT', {rows, Data}),
    Target = [<<0,0,0,12,73,68,65,84,120,156,
                99,96,100,98,6,0,0,14,0,7,215,111,228,120>>],
    ?assertEqual(Target, Result).

'PLTE_test'() ->
    Result = chunk('PLTE', {rgb, 8, [{255, 0, 0}, {0, 0, 255}]}),
    Target = <<0,0,0,6,80,76,84,69,255,0,0,0,0,255,108,161,253,142>>,
    ?assertEqual(Target, Result).


-endif.
