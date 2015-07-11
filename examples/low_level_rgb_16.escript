#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

-include_lib("png/include/png.hrl").


main([]) ->
    Width = 100,
    Height = 100,
    Rows = make_rows(Width, Height),
    Data = {rows, Rows},
    PngConfig = #png_config{size = {Width, Height},
                            mode = {rgb, 16}},
    IoData = [png:header(),
              png:chunk('IHDR', PngConfig),
              png:chunk('IDAT', Data),
              png:chunk('IEND')],
    ok = file:write_file("low_level_rgb_16.png", IoData).


make_rows(Width, Height) ->
    F = fun(Y) ->
            make_row(Y, Width, Height) end,
    lists:map(F, lists:seq(1, Height)).


make_row(Y, Width, Height) ->
    F = fun(X) ->
            R = trunc(X / Width * 65535),
            B = trunc(Y / Height * 65535),
            <<R:16, 32768:16, B:16>> end,
    lists:map(F, lists:seq(1, Width)).

