#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

-include("../_build/default/lib/png/include/png.hrl").
-include("common.hrl").


main([]) ->
    BitDepth = 16,
    Width = 100,
    Height = 100,
    Rows = make_rows(Width, Height),
    Data = {rows, Rows},
    PngConfig = #png_config{width = Width,
                            height = Height,
                            bit_depth = BitDepth,
                            color_type = ?PNG_COLOR_RGB},
    IoData = [png:header(),
              png:chunk('IHDR', PngConfig),
              png:chunk('IDAT', Data),
              png:chunk('IEND')],
    ok = file:write_file("rgb_16.png", IoData).


make_rows(Width, Height) ->
    F = fun(Y) ->
            make_row(Y, Width, Height) end,
    for(F, 1, Height).


make_row(Y, Width, Height) ->
    F = fun(X) ->
              get_color(X, Y, Width, Height) end,
    list_to_binary(for(F, 1, Width)).

get_color(X, Y, Width, Height) ->
    R = trunc(X / Width * 65535),
    B = trunc(Y / Height * 65535),
    <<R:16, 32768:16, B:16>>.
