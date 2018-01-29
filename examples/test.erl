-module(test).

%% Don't forget to include pipeline header file for using ?pipeline macro and compile code correctly
-include("include/pipeline.hrl").


-export([print_hello_world/0
        ,replace/4
        ,terminate/2
        ,replace2/4
        ,timestamp/0
        ,test_case/0]).


print_hello_world() ->
    ?pipeline("Hello, world!\n", string:to_upper(), io:format()).


replace(Name, Age, Location, Opts) ->
    Replace =
        fun(Key, Val, Opts2) ->
            lists:keyreplace(Key, 1, Opts2, {Key, Val})
        end,
    ?pipeline(Opts, Replace(name, Name), Replace(age, Age), Replace(location, Location)).


terminate(SupRef, ChildId) ->
    Terminate =
        fun
            ({_, Pid, _, _}) when is_pid(Pid) ->
                sys:terminate(Pid, normal);
            (_) ->
                ok
        end,
    ?pipeline(SupRef, supervisor:which_children(), lists:keyfind(ChildId, 1), Terminate()).


replace2(Name, Age, Location, Opts) ->
    ?pipeline(Opts
             ,{lists:keyreplace(name, 1, {name, Name}), 3}
             ,{lists:keyreplace(age, 1, {age, Age}), 3}
             ,{lists:keyreplace(location, 1, {location, Location}), 3}).


timestamp() ->
    {MegaSec, Sec, MicroSec} = os:timestamp(),
    ?pipeline(MegaSec, {'*', 1000000}, {'+', Sec}, {'*', 1000000}, {'+', MicroSec}, {'div', 1000}).


test_case() ->
    0.1 = ?pipeline(1, {'/', 10}),
    0.1 = ?pipeline(1, {'/', right, 10}),
    10.0 = ?pipeline(1, {'/', left, 10}).