-module(png).

-export([create/1, append/2, close/1]).
-export([check_config/1]).
-export([header/0, chunk/1, chunk/2]).

-include_lib("png/include/png.hrl").

-define(SCANLINE_FILTER, 0).


-spec create(map()) -> map().

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


-spec append(map(), {row, iodata()} | {rows, iodata()} | {data, iodata()} |
                    {compressed, iodata()}) -> map().

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


-spec close(map()) -> ok.

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
        chunk(<<"IDAT">>, iolist_to_binary(Part)) end,
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


-spec compress(binary()) -> [binary()].
compress(Data) ->
    Z = zlib:open(),
    ok = zlib:deflateInit(Z),
    Compressed = zlib:deflate(Z, Data, finish),
    ok = zlib:deflateEnd(Z),
    ok = zlib:close(Z),
    Compressed.

