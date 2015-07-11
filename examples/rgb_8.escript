#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

main([]) ->
    Width = 100,
    Height = 100,
    {ok, File} = file:open("rgb_8.png", [write]),
    % create an 8bit rgb img
    Png = png:create(#{size => {Width, Height},
                       mode => {rgb, 8},
                       file => File}),
    ok = append_rows(Png),
    ok = png:close(Png),
    ok = file:close(File).

append_rows(#{size := {_, Height}} = Png) ->
    F = fun(Y) ->
            append_row(Png, Y) end,
    lists:foreach(F, lists:seq(1, Height)).

append_row(#{size := {Width, Height}} = Png, Y) ->
    F = fun(X) ->
            R = trunc(X / Width * 255),
            B = trunc(Y / Height * 255),
            <<R, 128, B>> end,
    Row = lists:map(F, lists:seq(1, Width)),
    png:append(Png, {row, Row}).
