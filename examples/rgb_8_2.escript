#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

-include("common.hrl").


main([]) ->
    Width = 100,
    Height = 100,
    {ok, File} = file:open("rgb_8_2.png", [write]),
    % create an 8bit rgb img
    Png = png:create(#{size => {Width, Height},
                       mode => {rgb, 8},
                       file => File}),
    ok = append_rows(Png),
    ok = png:close(Png),
    ok = file:close(File).

append_rows(Png) ->
    append_row(Png, 0).


append_row(#{size := {_, Height}}, Height) ->
    ok;

append_row(#{size := {Width, Height}} = Png, Y) ->
    F = fun(X) ->
            R = trunc(X / Width * 255),
            B = trunc(Y / Height * 255),
            <<R, 128, B>> end,
    png:append(Png, {row, for(F, 1, Width)}),
    append_row(Png, Y + 1).
