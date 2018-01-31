![pipeline travis test status](https://travis-ci.org/Pouriya-Jahanbakhsh/pipeline.png?branch=master) [![Hex version](https://img.shields.io/hexpm/v/pl.svg "Hex version")](https://hex.pm/packages/pl)

# `pipeline`
By using this library you can pass result of an expression `A` as one parameter of another expression `B` and pass result of `B` as one parameter of `C` and so on. It's usefull in function call chaining. Isntead of writing:
```erlang
foo(bar(baz(new_function(other_function())))).
```
Use erlang operator `--` for pipelining and Just write:
```erlang
other_function() -- new_function() -- baz() -- bar() -- foo().
```
By default result of every expression passes as last argument of next expression. Except first argument that can be anything, other arguments should be one of the following:
* A function call (`Mod:Func(Args)` or `Func(Args)`).  
    ```erlang
    "Hello, world!\n" -- string:to_upper() -- io:format()
    
    %% If you want to pass result of one expression as different argument of next expression, use macro ?arg
    [1,2,3] -- length() -- element(?arg, {foo, bar, baz}) %% Gives baz
    
    %% Example of Replacing some items in a proplist
    [{name, foo}, {age, 23}, {location, earth}] --
    lists:keyreplace(name, 1, ?arg, {name, baz}) --       %% This function needs result of above expression as its third argument
    lists:keyreplace(age, 1, ?arg, {age, 18}) --          %% This function needs result of above expression as its third argument
    lists:keyreplace(location, 1, ?arg, {location, moon}) %% This function needs result of above expression as its third argument
    ```
* A fun call.
    ```erlang
    %% Example of Replacing some items in a proplist
    Opts = [{name, foo}, {age, 23}, {location, earth}],
    Replace = 
        fun(Key, Val, Opts2) ->
            lists:keyreplace(Key, 1, Opts2, {Key, Val})
        end,
    Opts -- Replace(name, baz) -- Replace(age, 18) -- Replace(location, moon)
    
    %% Terminate a child of supervisor if it was alive
    Terminate =
        fun
            ({_, Pid, _, _}) when is_pid(Pid) ->
                sys:terminate(Pid, normal);
            (_) ->
                ok
        end,
    SupRef -- supervisor:which_children() -- lists:keyfind(ChildId, 1) -- Terminate()
    ```
* Parentheses containing an Operation with valid erlang operator and at least one macro `?arg` in left or right of operator.
    ```erlang
    %% Example of wrapping timestamp in milli-seconds
    {MegaSec, Sec, MicroSec} = os:timestamp(), 
    (MegaSec * 1000000) -- (?arg + Sec) -- (?arg * 1000000) -- (?arg + MicroSec) -- (?arg div 1000).
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
```

You can use this macro in blocks (`case`, `if`, `begin`, `try` and `receive`), argument of other function or fun call, body of fun. **Don't** use as element of tuple (also record), list or map. Then If you want to use operator `--` with its default usage, just put that in one membered tuple or list. for example:
```erlang
{"foo"} = {"foobar" -- "bar"},  
```
**always test the code that includes pipeline's header file**.

## Debug
If you want to see processing and replaced expression, export `ERL_PIPELINE_DEBUG` and then compile the code:
```sh
~/projects/pipeline $ export ERL_PIPELINE_DEBUG=1
~/projects/pipeline $ rebar3 shell
```
```erlang
===> Verifying dependencies...
===> Compiling pipeline
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]
Eshell V8.3  (abort with ^G)

1> c(test).
```
```txt
Processing expression "Hello, world!\n" -- string:to_upper() -- io:format() in line 15
New call io:format(string:to_upper("Hello, world!\n")) generated

Processing expression Opts
--
Replace(name, Name) -- Replace(age, Age) -- Replace(location, Location) in line 23
New call Replace(location,
        Location,
        Replace(age, Age, Replace(name, Name, Opts))) generated

Processing expression SupRef
--
supervisor:which_children() -- lists:keyfind(ChildId, 1) -- Terminate() in line 34
New call Terminate(lists:keyfind(ChildId, 1, supervisor:which_children(SupRef))) generated

Processing expression Opts
--
lists:keyreplace(name, 1, pipeline:argument(), {name,Name})
--
lists:keyreplace(age, 1, pipeline:argument(), {age,Age})
--
lists:keyreplace(location, 1, pipeline:argument(), {location,Location}) in line 38
New call lists:keyreplace(location,
                 1,
                 lists:keyreplace(age,
                                  1,
                                  lists:keyreplace(name,
                                                   1,
                                                   Opts,
                                                   {name,Name}),
                                  {age,Age}),
                 {location,Location}) generated

Processing expression MegaSec
--
pipeline:argument() * 1000000
--
pipeline:argument() + Sec
--
pipeline:argument() * 1000000
--
pipeline:argument() + MicroSec -- pipeline:argument() div 1000 in line 46
New call erlang:'div'(erlang:'+'(erlang:'*'(erlang:'+'(erlang:'*'(MegaSec,
                                                         1000000),
                                              Sec),
                                   1000000),
                        MicroSec),
             1000) generated
```
```erlang
{ok,test}
2>
```

### License
**`BSD 3-Clause`**


### Author
**`pouriya.jahanbakhsh@gmail.com`**

### Hex version
[**`18.1.30`**](https://hex.pm/packages/pl)
