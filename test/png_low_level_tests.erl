-module(png_low_level_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("png/include/png.hrl").

low_level_test_() ->
    [fun get_header/0,
     fun 'IHDR_width_height'/0,
     fun 'IHDR_width_height_bit_depth'/0,
     fun 'IHDR_width_height_bit_depth_color_type'/0,
     fun compressed_IDAT/0,
     fun raw_IDAT/0,
     fun rows_IDAT/0,
     fun 'PLTE'/0].


get_header() ->
    Result = png:header(),
    Target = <<16#89, 16#50, 16#4E, 16#47, 16#0D, 16#0A, 16#1A, 16#0A>>,
    [?_assertEqual(Target, Result)].

'IHDR_width_height'() ->
    Result = png:chunk('IHDR', #png_config{size = {32, 16}}),
    Target = <<0,0,0,13,73,72,68,82,0,0,0,
               32,0,0,0,16,8,0,0,0,0,82,107,34,133>>,
    [?_assertEqual(Target, Result)].

'IHDR_width_height_bit_depth'() ->
    Result = png:chunk('IHDR', #png_config{size = {32, 16},
                                       mode = ?PNG_GRAYSCALE_8}),
    Target = <<0,0,0,13,73,72,68,82,0,0,0,
               32,0,0,0,16,8,0,0,0,0,82,107,34,133>>,
    [?_assertEqual(Target, Result)].

'IHDR_width_height_bit_depth_color_type'() ->
    Result = png:chunk('IHDR', #png_config{size = {32, 16},
                                       mode = ?PNG_RGB_8}),
    Target = <<0,0,0,13,73,72,68,82,0,0,0,
               32,0,0,0,16,8,2,0,0,0,248,98,234,14>>,
    [?_assertEqual(Target, Result)].

compressed_IDAT() ->
    Data = [<<0, 1, 2, 3>>],
    Result = png:chunk('IDAT', {compressed, Data}),
    Target = [<<0,0,0,4,73,68,65,84,0,1,2,3,64,222,190,8>>],
    [?_assertEqual(Target, Result)].

raw_IDAT() ->
    Data = <<0, 1, 2, 3>>,
    Result = png:chunk('IDAT', {raw, Data}),
    Target = [<<0,0,0,12,73,68,65,84,120,156,
                99,96,100,98,6,0,0,14,0,7,215,111,228,120>>],
    [?_assertEqual(Target, Result)].

rows_IDAT() ->
    Data = [<<1, 2, 3>>],
    Result = png:chunk('IDAT', {rows, Data}),
    Target = [<<0,0,0,12,73,68,65,84,120,156,
                99,96,100,98,6,0,0,14,0,7,215,111,228,120>>],
    [?_assertEqual(Target, Result)].

'PLTE'() ->
    Result = png:chunk('PLTE', {rgb, 8, [{255, 0, 0}, {0, 0, 255}]}),
    Target = <<0,0,0,6,80,76,84,69,255,0,0,0,0,255,108,161,253,142>>,
    [?_assertEqual(Target, Result)].

