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
%% @version  0.0.1
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
            application ->
                case erl_syntax_lib:analyze_application(Expr) of
                    {?MODULE, {?MODULE, _}} -> % found pipeline:pipeline(...
                        debug("processing \"~ts\"\n", [lists:flatten(erl_pp:expr(Expr))]),
                        CallArgs = erl_syntax:application_arguments(Expr),
                        CallArgs2 = replace_3(CallArgs, []),
                        case erlang:length(CallArgs2) of
                            0 ->
                                Rsn = 'Pipeline should has at least one argument',
                                erlang:error({syntax_error, [{line, erl_syntax:get_pos(Expr)}
                                                            ,{reason, Rsn}]});
                            _ ->
                                replace_4(CallArgs2, [])
                        end;
                    _ ->
                        CallOperator = erl_syntax:application_operator(Expr),
                        CallArgs = replace_3(erl_syntax:application_arguments(Expr), []),
                        erl_syntax:application(CallOperator, CallArgs)
                end;
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
            parentheses ->
                erl_syntax:parentheses(replace_3(erl_syntax:parentheses_body(Expr), []));
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
            replace_4(Items, [{Item, 0}|Calls]);
        tuple ->
            TupleElems = erl_syntax:tuple_elements(Item),
            case erlang:length(TupleElems) of
                2 ->
                    [Item2, Item3] = TupleElems,
                    case {erl_syntax:type(Item2), erl_syntax:type(Item3)} of
                        {application, integer} -> % It's {M:F(...)|F(...), 0...256}
                            Int = erl_syntax:integer_value(Item3),
                            ArgCount = erlang:length(erl_syntax:application_arguments(Item2)),
                            case Int == 0 orelse Int =< ArgCount + 1 of
                                true ->
                                    replace_4(Items, [{Item2, Int}|Calls]);
                                _ ->
                                    Rsn = erlang:list_to_atom("Argument index should be 0 or equal "
                                                              "to call arguments plus one"),
                                    erlang:error({syntax_error, [{line, erl_syntax:get_pos(Item3)}
                                                                ,{reason, Rsn}]})
                            end;
                        {atom, _} -> % It's {Operator:: '++', '!', ..., _}
                            Call = operator_call(erl_syntax:atom_value(Item2)
                                                ,Item3
                                                ,erl_syntax:get_pos(Item2)),
                            replace_4(Items, [{Call, 1}|Calls]);
                        _ ->
                            Rsn = erlang:list_to_atom("Two membered Tuple argument should contain a"
                                                      " fun or function call and an integer or an o"
                                                      "perator atom ('++', '!', etc) and an express"
                                                      "ion"),
                            erlang:error({syntax_error, [{line, erl_syntax:get_pos(Item)}
                                                        ,{reason, Rsn}]})
                    end;
                3 -> %% It's {Operator:: '++', '!', ..., right|left, _}
                    [Item2, Item3, Item4] = TupleElems,
                    case {erl_syntax:type(Item2), erl_syntax:type(Item3)} of
                        {atom, atom} ->
                            Side = erl_syntax:atom_value(Item3),
                            if
                                Side == left orelse Side == right ->
                                    Call = operator_call(erl_syntax:atom_value(Item2)
                                                        ,Item4
                                                        ,erl_syntax:get_pos(Item2)),
                                    Side2 =
                                        case Side of
                                            right ->
                                                1;
                                            _ ->
                                                2
                                        end,
                                    replace_4(Items, [{Call, Side2}|Calls]);
                                true ->
                                    Rsn = erlang:list_to_atom("Three membered Tuple argument should"
                                                              " contain an operator atom ('++', '!'"
                                                              ", etc) and 'left' or 'right' and an "
                                                              "expression"),
                                    erlang:error({syntax_error, [{line, erl_syntax:get_pos(Item)}
                                                                ,{reason, Rsn}]})
                            end;
                        _ ->
                            Rsn = erlang:list_to_atom("Three membered Tuple argument should contain"
                                                      " an operator atom ('++', '!', etc) and 'left"
                                                      "' or 'right' and an expression"),
                            erlang:error({syntax_error, [{line, erl_syntax:get_pos(Item)}
                                                        ,{reason, Rsn}]})
                    end;
                _ ->
                    Rsn = 'Tuple arguments can contains two or three elements',
                    erlang:error({syntax_error, [{line, erl_syntax:get_pos(Item)}, {reason, Rsn}]})
            end;
        Other ->
            Rsn = erlang:list_to_atom("An argument should be two or three membered tuple or a fun c"
                                      "all or a function call (Except first argument)"),
            erlang:error({syntax_error, [{line, erl_syntax:get_pos(Item)}
                                        ,{reason, Rsn}
                                        ,{detected, Other}]})
    end;
replace_4([], Calls) ->
    Calls2 = lists:reverse(Calls),
    Fold =
        fun({Call, Index}, Call3) ->
            AppOperator = erl_syntax:application_operator(Call),
            AppArgs = erl_syntax:application_arguments(Call),
            Call4 = erl_syntax:application(AppOperator, add_arg(Call3, Index, AppArgs)),
            revert(Call4, erl_syntax:get_pos(Call))
        end,
    [Call|Calls3] = Calls2,
    Ret = lists:foldl(Fold, Call, Calls3),
    debug("New call \"~ts\" generated\n\n", [lists:flatten(erl_pp:expr(Ret))]),
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


add_arg(Call, 1, Args) ->
    [Call|Args];
add_arg(Call, Index, Args) when Index =:= 0 orelse Index =:= erlang:length(Args) + 1 ->
    Args ++ [Call];
add_arg(Call, Index, Args) ->
    {Left, Right} = lists:split(Index-1, Args),
    Left ++ [Call] ++ Right.


operator_call(Operator, Expr, Pos) when Operator =:= '++' orelse
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

    revert(erl_syntax:application(erl_syntax:atom('erlang'), erl_syntax:atom(Operator), [Expr])
          ,Pos);
operator_call(Other, _, Pos) ->
    erlang:error({syntax_error, [{line, Pos}
                                ,{reason, 'Could not found valid erlang operator'}
                                ,{detected, Other}]}).