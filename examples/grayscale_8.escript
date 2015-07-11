#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

main([]) ->
    Width = 100,
    Height = 100,
    random:seed(),
    {ok, File} = file:open("grayscale_8.png", [write]),

    Png = png:create(#{size => {Width, Height},
                       mode => {grayscale, 8},
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
            G = random:uniform(trunc(255 * (X / Width + Y / Height) / 2)),
            <<G>> end,
    Row = lists:map(F, lists:seq(1, Width)),
    png:append(Png, {row, Row}).
