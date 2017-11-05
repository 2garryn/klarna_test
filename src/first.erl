%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2017
%%% @doc
%%%     Solution for test task #1: 
%%%     Function maskify, which masks all but the first and last four characters with '#'.
%%%     Empty strings should be handled. 
%%%     Don't replace non-digits characters.
%%%     Don't replace sanything in strings less than 6 chars.
%%%     
%%% @end
%%%-------------------------------------------------------------------
-module(first).

-include_lib("eunit/include/eunit.hrl").
-export([maskify/1]).

%%% API

%%--------------------------------------------------------------------------------
-spec maskify( Str :: string()) -> string().
%% @doc Mask all but the first and last four characters with '#'. 
%%      String less than 6 chars are left as is. Non-digits will not be replaced. 
%% @end
maskify(Str) when is_list(Str) ->
    Length = length(Str),
    case Length < 6 of  
        true -> 
            Str;
        false -> 
            [C|Str1] = Str,
            do_maskify(Str1, Length - 1, [C])
    end.


%% INTERNAL FUNCTIONS

do_maskify([C|Str], Proc, Result) when Proc < 5 -> 
    do_maskify(Str, Proc - 1, [C|Result]);

do_maskify([C|Str], Proc, Result) when C >= $0, C =< $9 -> 
    do_maskify(Str, Proc - 1, [$#|Result]);

do_maskify([C|Str], Proc, Result) -> 
    do_maskify(Str, Proc - 1, [C|Result]);

do_maskify([], _Proc, Result) ->
    lists:reverse(Result).

%% TESTS

first_test() ->
    ?assertMatch("5###########0694", maskify("5512103073210694")),
    ?assertMatch("6######5616", maskify("64607935616")),
    ?assertMatch("54321", maskify("54321")),
    ?assertMatch("6#4321", maskify("654321")),
    ?assertMatch("Nananananananananananananananana Batman!", maskify("Nananananananananananananananana Batman!")),
    ?assertMatch("", maskify("")),
    ?assertMatch("4###-####-####-5616", maskify("4556-3646-0793-5616")),
    ?assertMatch("1###-SKIP-####-4567", maskify("1234-SKIP-3456-4567")),
    ?assertMatch("A###-####-####-ABCD", maskify("A234-2345-3456-ABCD")).





