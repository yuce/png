#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

-include_lib("png/include/png.hrl").


main([]) ->
    Width = 50,
    Height = 50,
    Rows = make_rows(Width, Height),
    Data = {rows, Rows},
    PngConfig = #png_config{size = {Width, Height},
                            mode = {rgba, 8}},
    IoData = [png:header(),
              png:chunk('IHDR', PngConfig),
              png:chunk('IDAT', Data),
              png:chunk('IEND')],
    ok = file:write_file("low_level_rgba_8.png", IoData).


make_rows(Width, Height) ->
    F = fun(Y) ->
            make_row(Y, Width, Height) end,
    lists:map(F, lists:seq(1, Height)).


make_row(Y, Width, Height) ->
    F = fun(X) ->
            R = trunc(X / Width * 255),
            B = trunc(Y / Height * 255),
            A = trunc((X / Width + Y / Height) / 2 * 255),
            <<R, 128, B, A>> end,
    lists:map(F, lists:seq(1, Width)).
