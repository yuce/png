#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

-include("../_build/default/lib/png/include/png.hrl").


main([]) ->
    BitDepth = 8,
    Pallette = {rgb, BitDepth, [{255, 0, 0}, {0, 255, 0}, {0, 0, 255}]},
    % 5x5 rectangle with green upper triangle, blue lower triangle,
    % divided by a red
    Rows = [<<0, 1, 0, 1>>,
            <<2, 0, 2, 0>>],
    Data = {rows, Rows},
    PngConfig = #png_config{width = 4,
                            height = 2,
                            bit_depth = BitDepth,
                            color_type = ?PNG_COLOR_INDEXED},
    IoData = [png:header(),
              png:chunk('IHDR', PngConfig),
              png:chunk('PLTE', Pallette),
              png:chunk('IDAT', Data),
              png:chunk('IEND')],
    ok = file:write_file("sample1.png", IoData).


