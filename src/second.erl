%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2017
%%% @doc
%%%     Solution for test task #2:
%%%     Take a number and return numeral as string with correct
%%%     ordinal indicator suffix. Calling with 0 should return "0" as string
%%%     Two funcions are implemented:
%%%         number_to_ordinal_v1 - BIF integer_to_list/1 is used.
%%%         number_to_ordinal_v2 - includes implementation integer to list
%%% @end
%%%-------------------------------------------------------------------
-module(second).

-include_lib("eunit/include/eunit.hrl").

-export([number_to_ordinal_v1/1, number_to_ordinal_v2/1]).

%%% API

%%--------------------------------------------------------------------------------
-spec number_to_ordinal_v1( Number :: non_neg_integer() ) -> string().
%% @doc Take a number and return numeral as string with ordinal indicator suffix. 
%%      Calling with 0 should return "0" as string. Based on integer_to_list/1
%% @end
number_to_ordinal_v1(0) ->
    "0";
number_to_ordinal_v1(Number) ->
    Suffix = digit_to_suffix(Number rem 20),
    integer_to_list(Number) ++ Suffix.

%%--------------------------------------------------------------------------------
-spec number_to_ordinal_v2( Number :: non_neg_integer() ) -> string().
%% @doc Take a number and return numeral as string with ordinal indicator suffix. 
%%      Calling with 0 should return "0" as string. 0 <= Number <= 10000.
%% @end
number_to_ordinal_v2(0) -> 
    "0";

number_to_ordinal_v2(Number) ->
    Suffix = digit_to_suffix(Number rem 20),
    number_to_ordinal_v2(Number, Suffix).

%%% INTERNAL FUNCTIONS

number_to_ordinal_v2(0, Str) ->
    Str;

number_to_ordinal_v2(Number, Str) when Number < 10 ->
    [digit_to_char(Number)|Str];

number_to_ordinal_v2(Number, Str) ->
    Res1 = [digit_to_char(Number rem 10)|Str],
    number_to_ordinal_v2(Number div 10, Res1).

digit_to_suffix(1) -> "st";
digit_to_suffix(2) -> "nd";
digit_to_suffix(3) -> "rd";
digit_to_suffix(N) when N < 20 -> "th".

digit_to_char(D) -> $0 + D.

%%% TESTS

v1_test() ->
    ftest(fun number_to_ordinal_v1/1).

v2_test() ->
    ftest(fun number_to_ordinal_v2/1).

compare_test() ->
    [begin
        S = number_to_ordinal_v1(N),
        ?assertMatch(S, number_to_ordinal_v1(N))
     end || N <- lists:seq(0, 10000)].

ftest(Fun) ->
    ?assertMatch("0", Fun(0)),
    ?assertMatch("1st", Fun(1)),
    ?assertMatch("2nd", Fun(2)),
    ?assertMatch("3rd", Fun(3)),
    ?assertMatch("4th", Fun(4)),
    ?assertMatch("11th", Fun(11)),
    ?assertMatch("12th", Fun(12)),
    ?assertMatch("13th", Fun(13)),
    ?assertMatch("20th", Fun(20)),
    ?assertMatch("21st", Fun(21)),
    ?assertMatch("22nd", Fun(22)),
    ?assertMatch("23rd", Fun(23)),
    ?assertMatch("24th", Fun(24)),
    ?assertMatch("101st", Fun(101)),
    ?assertMatch("102nd", Fun(102)),
    ?assertMatch("103rd", Fun(103)),
    ?assertMatch("111th", Fun(111)),
    ?assertMatch("112th", Fun(112)),
    ?assertMatch("113th", Fun(113)),
    ?assertMatch("1112th", Fun(1112)).