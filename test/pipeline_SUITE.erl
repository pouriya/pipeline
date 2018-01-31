%%% ------------------------------------------------------------------------------------------------
%%% "pipeline" is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2018-2019, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
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
        ,'8'/1
        ,'9'/1]).

-define(test, 'atom' -- is_atom()).


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
    foo = {foo} -- element(1),
    foo = {foo} -- element(1, ?arg),
    foo = 1 -- element(?arg, {foo}),


%%  is_boolean(is_integer(element(2, {foo, 1}))),
    true = {foo, 1} -- element(2) -- is_integer() -- is_boolean(),

%%  string:to_upper(element(2, lists:split(7, "Hello, world!")) -- "!"),
    "WORLD" = "Hello, world!"--lists:split(7)--element(2)-- (?arg -- "!") --string:to_upper(),

%%  1 / 10,
    0.1 = 1 -- (?arg / 10),

%%  10 / 1,
    10.0 = 1 -- (10 / ?arg),

%%  lists:keyreplace(age,  1, [{age, 2}], {age, 2}),
    [{age, 1}] = [{age, 2}] -- lists:keyreplace(age, 1, ?arg, {age, 1}),

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
    io:format(test -- atom_to_list()).


'7'(_cfg) ->
    begin
        ?test
    end.


'8'(_Cfg) ->
    ((((?test)))).


'9'(_Cfg) ->
    self() ! msg,
    receive
        msg ->
            _ = ?test
    end,
    Ref = erlang:make_ref(),
    receive
        Ref ->
            _ = ?test
    after 0 ->
        _ = ?test
    end,
    self() ! Ref,
    receive
        Ref ->
            _ = ?test
    after 0 ->
        _ = ?test
    end,
    receive
    after 0 ->
        _ = ?test
    end.