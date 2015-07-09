#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

-include("../_build/default/lib/png/include/png.hrl").
-include("common.hrl").

-define(THICKNESS, 10).


main([]) ->
    BitDepth = 8,
    Pallette = {rgb, BitDepth, [{255, 0, 0}, {0, 255, 0}, {0, 0, 255}]},
    Width = 50,
    Height = 50,
    Rows = make_rows(Width, Height, Pallette),
    Data = {rows, Rows},
    PngConfig = #png_config{width = Width,
                            height = Height,
                            bit_depth = BitDepth,
                            color_type = ?PNG_COLOR_INDEXED},
    IoData = [png:header(),
              png:chunk('IHDR', PngConfig),
              png:chunk('PLTE', Pallette),
              png:chunk('IDAT', Data),
              png:chunk('IEND')],
    io:format("Module: ~p~n", [?MODULE]),
    ok = file:write_file("indexed_8.png", IoData).


make_rows(Width, Height, {rgb, _, Colors}) ->
    ColorCount = length(Colors),
    F = fun(Y) ->
            make_row(Width, Y, ColorCount, 2) end,
    for(F, 1, Height).

make_row(Width, Y, ColorCount, _PixSize) ->
    F = fun(X) ->
            get_color_index(X, Y, ColorCount) end,
    list_to_binary(for(F, 1, Width)).

get_color_index(X, Y, _ColorCount) when X > (Y + ?THICKNESS) ->
    2;

get_color_index(X, Y, _ColorCount) when (X + ?THICKNESS) < Y ->
    1;

get_color_index(_X, _Y, _ColorCount) ->
    0.

