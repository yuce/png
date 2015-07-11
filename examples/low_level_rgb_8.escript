#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

%%% This example shows how to create a PNG image using low level png functions

-include_lib("png/include/png.hrl").


main([]) ->
    Width = 100,
    Height = 100,
    PngConfig = #png_config{size = {Width, Height},
                            mode = {rgb, 8}},
    IoData = [png:header(),
              png:chunk('IHDR', PngConfig),
              png:chunk('IDAT', {rows, make_rows(Width, Height)}),
              png:chunk('IEND')],
    ok = file:write_file("low_level_rgb_8.png", IoData).


make_rows(Width, Height) ->
    F = fun(Y) ->
            make_row(Y, Width, Height) end,
    lists:map(F, lists:seq(1, Height)).


make_row(Y, Width, Height) ->
    F = fun(X) ->
            R = trunc(X / Width * 255),
            B = trunc(Y / Height * 255),
            <<R, 128, B>> end,
    lists:map(F, lists:seq(1, Width)).
