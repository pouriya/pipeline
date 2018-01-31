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
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  18.1.30
%% @doc
%%           Erlang pipeline syntax compiler.
%% @end
%% -------------------------------------------------------------------------------------------------
-module(pipeline).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% compiler callback:
-export([parse_transform/2]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(OPERATOR, '--').

%% -------------------------------------------------------------------------------------------------
%% compiler callback:

parse_transform(AST, _Opts) ->
    replace(AST, []).

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

replace([Item|AST], AST2) ->
    Item2 =
        case erl_syntax:type(Item) of
            function ->
                Item3 = erl_syntax:function(erl_syntax:function_name(Item)
                                           ,replace_2(erl_syntax:function_clauses(Item), [])),
                revert(Item3, erl_syntax:get_pos(Item));
            _ ->
                Item
        end,
    replace(AST, [Item2|AST2]);
replace([], AST2) ->
    lists:reverse(AST2).


replace_2([Clause|Clauses], Clauses2) ->
    {ClausePatterns, ClauseGuards, ClauseBody} = {erl_syntax:clause_patterns(Clause)
                                                 ,erl_syntax:clause_guard(Clause)
                                                 ,erl_syntax:clause_body(Clause)},
    Clause2 = erl_syntax:clause(ClausePatterns, ClauseGuards, replace_3(ClauseBody, [])),
    replace_2(Clauses, [revert(Clause2, erl_syntax:get_pos(Clause))|Clauses2]);
replace_2([], Clauses2) ->
    lists:reverse(Clauses2).


replace_3([Expr|Body], Body2) ->
    Expr2 =
        case erl_syntax:type(Expr) of
            infix_expr ->
                Operator = erl_syntax:infix_expr_operator(Expr),
                case erl_syntax:type(Operator) of
                    operator ->
                        case erl_syntax:operator_name(Operator) of
                            ?OPERATOR ->
                                debug("Processing expression ~ts in line ~tp\n"
                                     ,[lists:flatten(erl_pp:expr(Expr)), erl_syntax:get_pos(Expr)]),
                                replace_4(calls(erl_syntax:infix_expr_right(Expr)
                                               ,[erl_syntax:infix_expr_left(Expr)])
                                         ,[]);
                            _ ->
                                {[Left], [Right]} = {replace_3([erl_syntax:infix_expr_left(Expr)]
                                                              ,[])
                                                    ,replace_3([erl_syntax:infix_expr_right(Expr)]
                                                              ,[])},
                                erl_syntax:infix_expr(Left, Operator, Right)
                        end
                end;
            application ->
                CallOperator = erl_syntax:application_operator(Expr),
                CallArgs = replace_3(erl_syntax:application_arguments(Expr), []),
                erl_syntax:application(CallOperator, CallArgs);
            match_expr ->
                erl_syntax:match_expr(erl_syntax:match_expr_pattern(Expr)
                                     ,erlang:hd(replace_3([erl_syntax:match_expr_body(Expr)], [])));
            case_expr ->
                CaseArg = erlang:hd(replace_3([erl_syntax:case_expr_argument(Expr)], [])),
                CaseClauses = replace_2(erl_syntax:case_expr_clauses(Expr), []),
                erl_syntax:case_expr(CaseArg, CaseClauses);
            if_expr ->
                IfClauses = replace_2(erl_syntax:if_expr_clauses(Expr), []),
                erl_syntax:if_expr(IfClauses);
            try_expr ->
                TryBody = replace_3(erl_syntax:try_expr_body(Expr), []),
                TryClauses = replace_2(erl_syntax:try_expr_clauses(Expr), []),
                TryHandlers = replace_2(erl_syntax:try_expr_handlers(Expr), []),
                TryAfter = replace_3(erl_syntax:try_expr_after(Expr), []),
                erl_syntax:try_expr(TryBody, TryClauses, TryHandlers, TryAfter);
            fun_expr ->
                erl_syntax:fun_expr(replace_2(erl_syntax:fun_expr_clauses(Expr), []));
            block_expr ->
                erl_syntax:block_expr(replace_3(erl_syntax:block_expr_body(Expr), []));
            receive_expr ->
                RecvClauses = replace_2(erl_syntax:receive_expr_clauses(Expr), []),
                case erl_syntax:receive_expr_timeout(Expr) of
                    none ->
                        erl_syntax:receive_expr(RecvClauses);
                    Timeout ->
                        RecvActions = replace_3(erl_syntax:receive_expr_action(Expr), []),
                        erl_syntax:receive_expr(RecvClauses, Timeout, RecvActions)
                end;
            _ ->
                Expr
        end,
    replace_3(Body, [revert(Expr2, erl_syntax:get_pos(Expr))|Body2]);
replace_3([], Body2) ->
    lists:reverse(Body2).


replace_4([Item|Items], []) -> % First element of pipeline:pipeline/1-256 can be anything
    replace_4(Items, [Item]);
replace_4([Item | Items], Calls) ->
    case erl_syntax:type(Item) of
        application ->
            replace_4(Items, [Item|Calls]);
        infix_expr ->
            Operator = erl_syntax:infix_expr_operator(Item),
            case erl_syntax:type(Operator) of
                operator ->
                    {Left, Right} = {erl_syntax:infix_expr_left(Item)
                                    ,erl_syntax:infix_expr_right(Item)},
                    Call = operator_call(erl_syntax:operator_name(Operator)
                                        ,[Left, Right]
                                        ,erl_syntax:get_pos(Item)),
                    replace_4(Items, [Call|Calls]);
                Other ->
                    Rsn = erlang:list_to_atom("An argument should be a fun or function call or oper"
                                              "ation in parentheses with at least one ?arg macro in"
                                              " left or right of operator"),
                    erlang:error({syntax_error, [{line, erl_syntax:get_pos(Item)}
                                                ,{reason, Rsn}
                                                ,{detected, Other}]})
            end;
        Other ->
            Rsn = erlang:list_to_atom("An argument should be a fun or function call or operation in"
                                      " parentheses with at least one ?arg macro inleft or right of"
                                      " operator"),
            erlang:error({syntax_error, [{line, erl_syntax:get_pos(Item)}
                                        ,{reason, Rsn}
                                        ,{detected, Other}]})
    end;
replace_4([], Calls) ->
    Calls2 = lists:reverse(Calls),
    Fold =
        fun(Call, Call3) ->
            AppOperator = erl_syntax:application_operator(Call),
            AppArgs = erl_syntax:application_arguments(Call),
            Call4 = erl_syntax:application(AppOperator, add_arg(Call3, AppArgs, [], false)),
            revert(Call4, erl_syntax:get_pos(Call))
        end,
    [Call|Calls3] = Calls2,
    Ret = lists:foldl(Fold, Call, Calls3),
    debug("New call ~ts generated\n\n", [lists:flatten(erl_pp:expr(Ret))]),
    Ret.


revert(Expr, Pos) ->
    case erl_syntax:is_tree(Expr) of
        true ->
            erl_syntax:revert(erl_syntax:set_pos(Expr, Pos));
        _ ->
            Expr
    end.


debug(Txt, Args) ->
    case os:getenv("ERL_PIPELINE_DEBUG") of
        false ->
            ok;
        _ ->
            io:format(Txt, Args)
    end.

add_arg(Call, [Arg|Args], Args2, Added) ->
    case is_argument_call(Arg) of
        true ->
            add_arg(Call, Args, [Call|Args2], true);
        _ ->
            add_arg(Call, Args, [Arg|Args2], Added)
    end;
add_arg(Call, [], Args2, false) ->
    lists:reverse([Call|Args2]);
add_arg(_, [], Args2, _) ->
    lists:reverse(Args2).


operator_call(Operator, Args, Pos) when Operator =:= '++' orelse
                                        Operator =:= '--' orelse
                                        Operator =:= '+' orelse
                                        Operator =:= '-' orelse
                                        Operator =:= '*' orelse
                                        Operator =:= '/' orelse
                                        Operator =:= 'div' orelse
                                        Operator =:= 'rem' orelse
                                        Operator =:= '<' orelse
                                        Operator =:= '>' orelse
                                        Operator =:= '=<' orelse
                                        Operator =:= '>=' orelse
                                        Operator =:= '==' orelse
                                        Operator =:= '=:=' orelse
                                        Operator =:= '/=' orelse
                                        Operator =:= '=/=' orelse
                                        Operator =:= 'and' orelse
                                        Operator =:= 'or' orelse
                                        Operator =:= 'not' orelse
                                        Operator =:= 'xor' orelse
                                        Operator =:= '!' orelse
                                        Operator =:= 'band' orelse
                                        Operator =:= 'bor' orelse
                                        Operator =:= 'bnot' orelse
                                        Operator =:= 'bxor' orelse
                                        Operator =:= 'bsl' orelse
                                        Operator =:= 'bsr' ->
    [Left, Right] = Args,
    Check =
        case {is_argument_call(Left), is_argument_call(Right)} of
            {true, _} ->
                ok;
            {_, true} ->
                ok;
            _ ->
                error
        end,
    case Check of
        ok ->
            revert(erl_syntax:application(erl_syntax:atom('erlang')
                                         ,erl_syntax:atom(Operator)
                                         ,Args)
                  ,Pos);
        error ->
            erlang:error({syntax_error
                         ,[{line, Pos}
                          ,{reason, 'At least one arguemnt of operator should be ?arg macro'}]})
    end;
operator_call(Other, _, Pos) ->
    erlang:error({syntax_error, [{line, Pos}
                                ,{reason, 'Could not found valid erlang operator'}
                                ,{detected, Other}]}).


calls(Expr, Calls) ->
    case erl_syntax:type(Expr) of
        infix_expr ->
            Left = erl_syntax:infix_expr_left(Expr),
            Right = erl_syntax:infix_expr_right(Expr),
            Operator = erl_syntax:infix_expr_operator(Expr),
            case erl_syntax:type(Operator) of
                operator ->
                    case erl_syntax:operator_name(Operator) of
                        ?OPERATOR ->
                            Calls ++ calls(Right, [Left]);
                        _ ->
                            Calls ++ [Expr]
                    end;
                _ ->
                    Calls ++ [Expr]
            end;
        _ ->
            Calls ++ [Expr]

    end.


is_argument_call(Expr) ->
    case erl_syntax:type(Expr) of
        application ->
            case erl_syntax_lib:analyze_application(Expr) of
                {?MODULE, {argument, 0}} ->
                    true;
                _ ->
                    fasle
            end;
        _ ->
            fasle
    end.