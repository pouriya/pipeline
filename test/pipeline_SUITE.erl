-module(pipeline_SUITE).
-include("pipeline.hrl").

-export([init_per_suite/1
        ,end_per_suite/1
        ,all/0
        ,init_per_testcase/2
        ,end_per_testcase/2]).

-export(['1'/1
        ,'2'/1
        ,'3'/1
        ,'4'/1
        ,'5'/1
        ,'6'/1
        ,'7'/1
        ,'8'/1]).

-define(test, ?pipeline('atom', is_atom())).


all() ->
    [erlang:list_to_atom(erlang:integer_to_list(Int))
    || Int <- lists:seq(1, erlang:length(?MODULE:module_info(exports))-7)].


init_per_suite(Config) ->
    Config.


end_per_suite(Config) ->
    Config.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.


'1'(_Cfg) ->
    _ = ?test,

%%  foo = element(1, {foo}),
    foo = ?pipeline({foo}, element(1)),
    foo = ?pipeline({foo}, {element(1), 2}),
    foo = ?pipeline(1, {element({foo}), 1}),


%%  is_boolean(is_integer(element(2, {foo, 1}))),
    true = ?pipeline({foo, 1}, element(2), is_integer(), is_boolean()),

%%  string:to_upper(element(2, lists:split(7, "Hello, world!")) -- "!"),
    "WORLD" = ?pipeline("Hello, world!"
                       ,lists:split(7)
                       ,element(2)
                       ,{'--', "!"}
                       ,string:to_upper()),

%%  1 / 10,
    0.1 = ?pipeline(1, {'/', 10}),
    0.1 = ?pipeline(1, {'/', right, 10}),

%%  10 / 1,
    10.0 = ?pipeline(1, {'/', left, 10}),

%%  lists:keyreplace(age,  1, [{age, 2}], {age, 2}),
    [{age, 1}] = ?pipeline([{age, 2}], {lists:keyreplace(age, 1, {age, 1}), 3}),

    ok.


'2'(_Cfg) ->
    case ?test of
        _ ->
            ?test
    end.


'3'(_Cfg) ->
    if
        true ->
            ?test
    end.


'4'(_Cfg) ->
    try ?test of
        _ ->
            ?test
    catch
        _:_ ->
            ?test
    after
        _ = ?test
    end,
    try exit(oops) of
        _ ->
            ok
    catch
        _:_ ->
            ?test
    after
        _ = ?test
    end.


'5'(_Cfg) ->
    F = fun() -> ?test end,
    F().


'6'(_Cfg) ->
    io:format(?pipeline(test, atom_to_list())).


'7'(_cfg) ->
    begin
        ?test
    end.


'8'(_Cfg) ->
    ((((?test)))).