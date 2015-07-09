#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

-include("../_build/default/lib/png/include/png.hrl").
-include("common.hrl").


main([]) ->
    Width = 100,
    Height = 100,
    PngConfig = #png_config{size = {Width, Height},
                            mode = {rgb, 8}},
    IoData = [png:header(),
              png:chunk('IHDR', PngConfig),
              png:chunk('IDAT', {rows, make_rows(Width, Height)}),
              png:chunk('IEND')],
    ok = file:write_file("rgb_8.png", IoData).


make_rows(Width, Height) ->
    F = fun(Y, Rows) ->
            [make_row(Y, Width, Height) | Rows] end,
    lists:reverse(lists:foldl(F, [], lists:seq(1, Height))).

make_row(Y, Width, Height) ->
    F = fun(X) ->
            R = trunc(X / Width * 255),
            B = trunc(Y / Height * 255),
            <<R, 128, B>> end,
    for(F, 1, Width).
