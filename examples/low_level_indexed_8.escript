#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

-include_lib("png/include/png.hrl").


main([]) ->
    BitDepth = 8,
    Pallette = {rgb, BitDepth, [{255, 0, 0}, {0, 255, 0}, {0, 0, 255}]},
    Width = 50,
    Height = 50,
    Rows = make_rows(Width, Height, Pallette),
    Data = {rows, Rows},
    PngConfig = #png_config{size = {Width, Height},
                            mode = {indexed, 8}},
    IoData = [png:header(),
              png:chunk('IHDR', PngConfig),
              png:chunk('PLTE', Pallette),
              png:chunk('IDAT', Data),
              png:chunk('IEND')],
    ok = file:write_file("low_level_indexed_8.png", IoData).


make_rows(Width, Height, {rgb, _, Colors}) ->
    ColorCount = length(Colors),
    F = fun(Y) ->
            make_row(Width, Y, ColorCount, 2) end,
    lists:map(F, lists:seq(1, Height)).


make_row(Width, Y, _ColorCount, _PixSize) ->
    Thickness = Width div 4,
    F = fun(X) ->
            if
                X > (Y + Thickness) -> 2;
                (X + Thickness) < Y -> 1;
                true -> 0 end end,
    lists:map(F, lists:seq(1, Width)).
