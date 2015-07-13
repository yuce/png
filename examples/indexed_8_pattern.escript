#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

main([]) ->
    random:seed(erlang:now()),
    Width = 30,
    Height = 30,
    % define an 8bit RGB palette with 4 colors:
    Palette = {rgb, 8, [{255, 0, 0}, {128, 255, 128}, {64, 64, 255}, {0, 0, 0}]},
    {ok, File} = file:open("sample.png", [write]),

    % We create an 8bit indexed PNG
    Png = png:create(#{size => {Width, Height},
                       mode => {indexed, 8},
                       file => File,
                       palette => Palette}),

    % make the png image row by row
    AppendRow = fun(_) ->
                    % An image row is composed of palette indices for indexed PNGs
                    Row = lists:map(fun(_) -> random:uniform(4) - 1 end,
                                    lists:seq(1, Width)),
                    png:append(Png, {row, Row}) end,
    lists:foreach(AppendRow, lists:seq(1, Height)),
    % need to "close" the image
    ok = png:close(Png),
    ok = file:close(File).
