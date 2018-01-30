![pipeline travis test status](https://travis-ci.org/Pouriya-Jahanbakhsh/pipeline.png?branch=master) [![Hex version](https://img.shields.io/hexpm/v/pl.svg "Hex version")](https://hex.pm/packages/pl)

# `pipeline`
By using this library you can pass result of an expression `A` as one parameter of another expression `B` and pass result of `B` as one parameter of `C` and so on. It's usefull in function call chaining. Isntead of writing:
```erlang
foo(bar(baz(new_function(other_function()))))
```
Just write:
```erlang
?pipeline(other_function(), new_function(), baz(), bar(), foo())
```
By default result of every expression passes as last argument of next expression. Except first argument that can be anything, other arguments of `?pipeline` macro should be one of the following:
* A function call (`Mod:Func(Args)` or `Func(Args)`).  
    ```erlang
    ?pipeline("Hello, world!\n", string:to_upper(), io:format())
    ```
* A fun call.
    ```erlang
    %% Replace some items in a proplist
    Replace = 
        fun(Key, Val, Opts) ->
            lists:keyreplace(Key, 1, Opts, {Key, Val})
        end,
    ?pipeline([{name, foo}, {age, 23}, {location, earth}], Replace(name, baz), Replace(age, 18), Replace(location, moon))
    
    %% Terminate a child of supervisor if it was alive
    Terminate =
        fun
            ({_, Pid, _, _}) when is_pid(Pid) ->
                sys:terminate(Pid, normal);
            (_) ->
                ok
        end,
    ?pipeline(SupRef, supervisor:which_children(), lists:keyfind(ChildId, 1), Terminate())
    ```
* A two membered tuple containing a fun or function call as first element and an integer as second element. Integer is index of argument. Default is 0 which means last argument.
    ```erlang
    %% Replace some items in a proplist
    ?pipeline([{name, foo}, {age, 23}, {location, earth}]
             ,{lists:keyreplace(name, 1, {name, baz}), 3} %% This function needs result of above expression as its third argument
             ,{lists:keyreplace(age, 1, {age, 18}), 3}
             ,{lists:keyreplace(location, 1, {location, moon}), 3})
    
    ```
* A two membered tuple containing an atom (one of the erlang operators, `'++'`, `'!'`, etc) as first element and anything as second element.
    ```erlang
    %% Timestamp in milli-seconds
    {MegaSec, Sec, MicroSec} = os:timestamp(),
    ?pipeline(MegaSec, {'*', 1000000}, {'+', Sec}, {'*', 1000000}, {'+', MicroSec}, {'div', 1000}).
    ```
* A three membered tuple contating an atom (one of erlang operators, `'++'`, `'!'`, etc) as first element and one of the atoms `left` or `right` as second element and anything as third element.
    ```erlang
    0.1 = ?pipeline(1, {'/', 10}), % By default passes 10 in right of operator (1 / 10)
    0.1 = ?pipeline(1, {'/', right, 10}),
    10.0 = ?pipeline(1, {'/', left, 10}), % (10 / 1)
    ```
# Example
Runnning above codes:
```erlang
-module(test).

%% Don't forget to include pipeline header file for using ?pipeline macro and compile code correctly
-include_lib("pipeline/include/pipeline.hrl").


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
```
```erlang
Erlang/OTP 19 [erts-8.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.0  (abort with ^G)

1> c(test).
{ok,test}

2> test:print_hello_world().
HELLO, WORLD!
ok

3> test:replace(baz, 18, moon, [{name, foo}, {age, 23}, {location, earth}]).
[{name,baz},{age,18},{location,moon}]

4> supervisor:which_children(httpc_sup).    
[{httpc_handler_sup,<0.132.0>,supervisor,[httpc_handler_sup]},
 {httpc_profile_sup,<0.72.0>,supervisor,[httpc_profile_sup]}]

5> test:terminate(httpc_sup, httpc_handler_sup).
ok

6> supervisor:which_children(httpc_sup).        
[{httpc_handler_sup,<0.154.0>,supervisor,[httpc_handler_sup]}, %% Pid changed, then worked
 {httpc_profile_sup,<0.72.0>,supervisor,[httpc_profile_sup]}]

7> test:terminate(httpc_sup, foo). %% inexistent child
ok

8> test:replace2(baz, 18, moon, [{name, foo}, {age, 23}, {location, earth}]).
[{name,baz},{age,18},{location,moon}]

9> erlang:timestamp().
{1517,264503,800212}

10> test:timestamp().  
1517264504646

11> test:test_case().
10.0
```
You can use this macro in blocks (`case`, `if`, `begin`, `try` and `receive`), argument of other function or fun call, body of fun. **Don't** use as element of tuple (also record), list or map.

### License
**`BSD 3-Clause`**


### Author
**`pouriya.jahanbakhsh@gmail.com`**

### Hex version
[**`18.1.30`**](https://hex.pm/packages/pl)
