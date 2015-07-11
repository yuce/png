#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

main([]) ->
    Width = 100,
    Height = 100,
    Palette = {rgb, 8, [{255, 0, 0}, {0, 255, 0}, {0, 0, 255}]},
    Callback = fun(Bin) ->
                    io:format("Wrote: ~p~n", [Bin]) end,
    Png = png:create(#{size => {Width, Height},
                       mode => {indexed, 8},
                       call => Callback,
                       palette => Palette}),
    ok = append_rows(Png),
    ok = png:close(Png),
    ok.


append_rows(Png) ->
    append_row(Png, 0).


append_row(#{size := {_, Height}}, Height) ->
    ok;

append_row(#{size := {Width, _Height}} = Png, Y) ->
    Thickness = Width div 4,
    F = fun
            (X) when X > (Y + Thickness) -> 2;
            (X) when (X + Thickness) < Y -> 1;
            (_) -> 0 end,
    Row = lists:map(F, lists:seq(1, Width)),
    png:append(Png, {row, Row}),
    append_row(Png, Y + 1).
