#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

-include("common.hrl").

-define(THICKNESS, 10).


main([]) ->
    Width = 100,
    Height = 100,
    Palette = {rgb, 8, [{255, 0, 0}, {0, 255, 0}, {0, 0, 255}]},
    Callback = fun(Bin) ->
                io:format("Wrote: ~p~n", [Bin]) end,
    % create an 8bit rgb img
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
    F = fun
            (X) when X > (Y + ?THICKNESS) -> 2;
            (X) when (X + ?THICKNESS) < Y -> 1;
            (_) -> 0 end,
    png:append(Png, {row, for(F, 1, Width)}),
    append_row(Png, Y + 1).
