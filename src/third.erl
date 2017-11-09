%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2017
%%% @doc
%%%     Solution for test task #3:
%%%     Calculator. 
%%%     Examples: 
%%%         "3 2 1 - *" => 3
%%%         "7 4 5 + * 3 - 10 /" => 6
%%% @end
%%%-------------------------------------------------------------------
-module(third).

-include_lib("eunit/include/eunit.hrl").
-export([calculator/1]).

-define(IS_OP(Op), Op == $+; Op == $-; Op == $*; Op == $/).
-define(SPACE, 32).

%%% API

%%--------------------------------------------------------------------------------
-spec calculator( Number :: string() ) -> integer() | float().
%% @doc Evaluates string expression and calculate result.
%%      At least one space should stay between numbers/operators.
%% @end
calculator(Str) -> 
    calculator(Str, []).

%%% INTERNAL FUNCTIONS

calculator([], []) ->
    0;
calculator([], [Divided|_]) ->
    maybe_to_int(Divided);

calculator(Str, Divided) ->
    case extract_token(Str) of 
        {Op, Tail} when ?IS_OP(Op) ->
            [S2, S1|DivTail] = Divided,
            V = calc_pair(S1, S2, Op),
            calculator(Tail, [V|DivTail]);
        {[], []} ->
            calculator([], Divided);
        {T, Tail} ->
            Number = to_number(T),
            calculator(Tail, [Number|Divided])
    end.

extract_token(Str) ->
   Extracted = lists:dropwhile(fun(El) -> El =:= ?SPACE end, Str),
   extract_token(Extracted, []).


extract_token([$-, S|Tail], StrNum) when S /= ?SPACE -> extract_token(Tail, [S, $-|StrNum]);
extract_token([Op|Tail], [])        when ?IS_OP(Op)  -> {Op, Tail};
extract_token([Op|_Tail], _StrNum)  when ?IS_OP(Op)  -> error(invalid_expr);
extract_token([?SPACE|Tail], StrNum)                -> {lists:reverse(StrNum), Tail};
extract_token([], StrNum)                           -> {lists:reverse(StrNum), []};
extract_token([S|Tail], StrNum)                     -> extract_token(Tail, [S|StrNum]).

to_number(StrNum) -> 
    case catch list_to_float(StrNum) of 
        {'EXIT', _} -> list_to_integer(StrNum);
        Value -> Value
    end.

calc_pair(D1, D2, $+) -> D1 + D2;
calc_pair(D1, D2, $*) -> D1 * D2;
calc_pair(D1, D2, $-) -> D1 - D2;
calc_pair(D1, D2, $/) -> D1 / D2.

%% Convert floats like 6.0 to 6. Task requirement,
maybe_to_int(Number) when is_float(Number) ->
    Integer = erlang:list_to_integer(erlang:float_to_list(Number, [{decimals,0}])),
    case Integer == Number of 
        true -> Integer;
        false -> Number
    end;
maybe_to_int(Number) ->
    Number.

%%% TESTS

calculator_test() ->
    ?assertMatch(3, calculator("3 2 1 - *")),
    ?assertMatch(6, calculator("7 4 5 + * 3 - 10 /")),
    ?assertMatch(-1, calculator("7 4 -5 + * 3 - 10 /")),
    ?assertMatch(0, calculator("-1 1 +")),
    ?assertMatch(0, calculator("1 -1 +")),
    ?assertMatch(3, calculator("3")),
    ?assertMatch(0, calculator("")),
    ?assertMatch(0, calculator("0")),
    ?assertMatch(0, calculator(" ")),
    ?assertMatch(3, calculator(" 3  2  1  -  *  ")),
    ?assertMatch(1, calculator(" 3  2  1 ")),
    ?assertMatch(2.5, calculator("1.25 2 *")).








