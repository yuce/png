#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/png/ebin

-include("../_build/default/lib/png/include/png.hrl").


main([]) ->
    BitDepth = 8,
    % 5x5 rectangle with green upper triangle, blue lower triangle,
    % divided by a red
    Red = <<255, 0, 0>>,
    Green = <<0, 255, 0>>,
    Blue = <<0, 0, 255>>,
    Color = <<255, 128, 64>>,
    RawData = <<0, Red/binary, Green/binary, Blue/binary>>,
    Data = {raw, RawData},
    io:format("Data size: ~p~n", [byte_size(RawData)]),
    PngConfig = #png_config{width = 3,
                            height = 1,
                            bit_depth = BitDepth,
                            color_type = ?PNG_COLOR_RGB},
    IoData = [png:header(),
              png:chunk('IHDR', PngConfig),
              png:chunk('IDAT', Data),
              png:chunk('IEND')],
    ok = file:write_file("sample3.png", IoData).


