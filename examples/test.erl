-module(test).

%% Don't forget to include pipeline header file for using ?pipeline macro and compile code correctly
-include_lib("pipeline/include/pipeline.hrl").


-export([print_hello_world/0
        ,replace/4
        ,terminate/2
        ,replace2/4
        ,timestamp/0]).


print_hello_world() ->
    "Hello, world!\n" -- string:to_upper() -- io:format().


replace(Name, Age, Location, Opts) ->
    Replace =
        fun(Key, Val, Opts2) ->
            lists:keyreplace(Key, 1, Opts2, {Key, Val})
        end,
    Opts -- Replace(name, Name) -- Replace(age, Age) -- Replace(location, Location).


terminate(SupRef, ChildId) ->
    Terminate =
        fun
            ({_, Pid, _, _}) when is_pid(Pid) ->
                sys:terminate(Pid, normal);
            (_) ->
                ok
        end,
    SupRef -- supervisor:which_children() -- lists:keyfind(ChildId, 1) -- Terminate().


replace2(Name, Age, Location, Opts) ->
    Opts --
    lists:keyreplace(name, 1, ?arg, {name, Name}) --
    lists:keyreplace(age, 1, ?arg, {age, Age}) --
    lists:keyreplace(location, 1, ?arg, {location, Location}).


timestamp() ->
    {MegaSec, Sec, MicroSec} = os:timestamp(),
    MegaSec -- (?arg * 1000000) -- (?arg + Sec) -- (?arg * 1000000) -- (?arg + MicroSec) -- (?arg div 1000).