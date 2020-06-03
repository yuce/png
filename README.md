png
=====

**png** is a pure Erlang library for creating PNG images. It can currently create *8* and *16* bit *RGB*, *RGB with alpha*, *indexed*, *grayscale* and *grayscale with alpha* images.

Install (Erlang)
----------------

**png** requires at least Erlang 17.0, since it uses maps.

Include the library in your *rebar* configuration:

```erlang
{deps, [
    {png, ".*", {git, "https://github.com/yuce/png.git", "master"}}]}.
```

Alternatively, for *rebar 3*:

```erlang
{deps, [png]}.
```


Taster
------

```erlang
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
% need to finalize the image
ok = png:close(Png),
ok = file:close(File).
```

Mini-Tutorial
-------------

Creating a PNG image using **png** is a 3-step process:

1. Pass configuration and initialize the image,
2. Append the pixels, row by row or all at once,
3. Finalize the image.

The configuration currently is a `map` which consists of the dimensions (size) of the image, color mode and bits per pixel (or index), an optional palette (for indexed PNGs) and finally a callback which is called every time **png** needs to write some data.

```erlang
Width = 100,
Height = 150,
ColorMode = indexed,
Bits = 8,
Palette = {ColorMode, Bits, [{255, 0, 0}, {0, 0, 128}]},
Callback = fun(IoData) -> io:format("Received: ~p~n", [IoData]) end,
Config = #{size => {Width, Height},
           mode => {ColorMode, Bits},
           palette => Palette,
           call => Callback},
```

There is a shortcut for writing to a file instead of a callback, using the `file` key:

```erlang
{ok, File} = file:open("my.png", [write]),
Config = #{size => {Width, Height},
           mode => {ColorMode, Bits},
           palette => Palette,
           file => File},
```

Once we have the configuration, let's start creating the image:

```erlang
Png = png:create(Config),
```

`png:create` writes the PNG header and the palette to the callback or the file and returns a `map` with the configuration + some state data.

Next step is appending pixels to the image.

The representation of a pixel depends on the *color mode* and *bits*. Here's a summary of currently supported image modes:

Color Mode Name  | Color Mode        | Bits | Representation
------------------|-------------------|------|---------------
Grayscale         | `grayscale`       |   8  | `<<L:8>>`
Grayscale + Alpha | `grayscale_alpha` |   8  | `<<L:8, A:8>>`
Indexed           | `indexed`         |   8  | `<<I:8>>`
RGB               | `rgb`             |   8  | `<<R:8, G:8, B:8>>`
RGB + Alpha       | `rgba`            |   8  | `<<R:8, G:8, B:8, A:8>>`
Grayscale         | `grayscale`       |  16  | `<<L:16>>`
Grayscale + Alpha | `grayscale_alpha` |  16  | `<<L:16, A:16>>`
Indexed           | `indexed`         |  16  | `<<I:16>>`
RGB               | `rgb`             |  16  | `<<R:16, G:16, B:16>>`
RGB + Alpha       | `rgba`            |  16  | `<<R:16, G:16, B:16, A:16>>`

Where, `L` is the luminance, `A` is alpha (*opacity*), `I` is the palette index, `R`, `G` and `B` are red, green and blue respectively.

A palette is required for `indexed` images. The palette is represented as a tuple of color mode, bits and list of red, green, blue tuples:

```erlang
ColorMode = indexed,
Bits = 8,
Red = {255, 0, 0},
Green = {0, 255, 0},
Blue = {0, 0, 255},
Black = {0, 0, 0},
Palette = {ColorMode, Bits, [Red, Green, Blue, Black]},
```

**png** supports adding pixels row by row (PNG specification uses the term *scanline*), a list of rows or as raw data. Being able to add partial image data is important when you want to keep the required memory low, e.g., creating an image on the fly in response to a web request and sending it in chunks.

Here are supported data layouts:

Name     | Atom | Data Type       | Example
---------|------|-----------------|--------
A Row    | row  | binary          | `{row, <<1, 2, 3>>}`
Rows     | rows | binary list     | `{rows, [<<1, 2, 3>>, <<2, 3, 1>>]}`
Raw data | data | binary          | `{data, <<0, 1, 2, 3>>}`

Note that, each row (*scanline*) must start with a *filter method ID*. Currently only filter `0` (no filter) is supported (see [PNG specification](http://www.libpng.org/pub/png/spec/1.2/PNG-DataRep.html#DR.Filtering) for more information). For `row` and `rows`, **png** prepends the filter method ID to each row, but for `data`, you must prepend `0` to every row yourself.

Appending image data is done with `png:append` (*surprise!*). An example:

```erlang
Data = {row, <<1, 2, 3>>},
Png = png:append(Png, Data),
```

You can call `png:append` as much as as necessary. After appending all data, you must finalize the image with `png:close`:

```erlang
ok = png:close(Png)
```

Examples
--------

There are some examples on https://github.com/yuce/png/tree/master/examples which show the usage and adding PNG chunks manually.
