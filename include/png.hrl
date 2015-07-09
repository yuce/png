
-define(PNG_INDEXED_8, {indexed, 8}).
-define(PNG_GRAYSCALE_8, {grayscale, 8}).
-define(PNG_GRAYSCALE_16, {grayscale, 16}).
-define(PNG_GRAYSCALE_ALPHA_8, {grayscale_alpha, 8}).
-define(PNG_GRAYSCALE_ALPHA_16, {grayscale_alpha, 16}).
-define(PNG_RGB_8, {rgb, 8}).
-define(PNG_RGB_16, {rgb, 16}).
-define(PNG_RGBA_8, {rgba, 8}).
-define(PNG_RGBA_16, {rgba, 16}).

-record(png_config, {size = {0, 0},
                     mode = ?PNG_GRAYSCALE_8,
                     compression_method = 0,
                     filter_method = 0,
                     interlace_method = 0}).
