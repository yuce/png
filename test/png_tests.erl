-module(png_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("png/include/png.hrl").

iface_test_() ->
    [fun create_append/0].

check_config_test_() ->
    [fun check_config/0].


create_append() ->
    E = ets:new(state, []),

    Cb = fun(Bin) ->
            NewContents = case ets:lookup(E, contents) of
                                [] ->
                                    [Bin];
                                [{_, Contents}] ->
                                    [Contents, Bin] end,
        true = ets:insert(E, {contents, list_to_binary(NewContents)}),
        ok end,

    Png = png:create(#{size => {4, 2},
                   mode => {indexed, 8},
                   call => Cb,
                   palette => {rgb, 8, [{255, 0, 0}, {0, 0, 255}]}}),
    Png = png:append(Png, {rows, [0, 1, 0, 1]}),
    ok = png:close(Png),
    [{_, Result}] = ets:lookup(E, contents),
    Target = <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,4,0,0,0,2,
               8,3,0,0,0,
               72,118,141,81,0,0,0,6,80,76,84,69,255,0,0,0,0,255,108,161,253,
               142,0,0,0,
               14,73,68,65,84,120,156,99,96,96,96,100,0,98,0,0,14,0,3,216,95,
               69,48,0,0,0,0,73,69,78,68,174,66,96,130>>,
    [?_assertEqual(Target, Result)].

check_config() ->
    Config1 = #png_config{},
    ?assertEqual({error, invalid}, png:check_config(Config1)),

    Config2 = #png_config{size = {0, 1}},
    ?assertEqual({error, invalid}, png:check_config(Config2)),

    Config3 = #png_config{size = {1, 1}},
    ?assertEqual(ok, png:check_config(Config3)),

    Config4 = #png_config{size = {1, 1}, mode = ?PNG_INDEXED_8},
    ?assertEqual(ok, png:check_config(Config4)).
