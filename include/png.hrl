
-define(PNG_COLOR_GRAYSCALE, 0).
-define(PNG_COLOR_RGB, 2).
-define(PNG_COLOR_INDEXED, 3).
-define(PNG_COLOR_GRAYSCALE_ALPHA, 4).
-define(PNG_COLOR_RGBA, 6).

-record(png_config, {width = 0,
               height = 0,
               bit_depth = 8,
               color_type = 0,
               compression_method = 0,
               filter_method = 0,
               interlace_method = 0}).
