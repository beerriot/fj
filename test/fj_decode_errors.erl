-module(fj_decode_errors).

-include_lib("eunit/include/eunit.hrl").

-define(FJP(J, E), ?assertEqual(E, fj:parse(J))).
-define(_FJP(J, E), ?_assertEqual(E, fj:parse(J))).

empty_test() ->
    ?FJP(<<>>, {error, {unexpected_end, 0}}).

unclosed_array_test() ->
    ?FJP(<<"[">>, {error, {unexpected_end, 1}}).

unclosed_object_test() ->
    ?FJP(<<"{">>, {error, {unexpected_end, 1}}).

unclosed_string_test() ->
    ?FJP(<<"\"">>, {error, {unexpected_end, 1}}).

invalid_integer_test_() ->
    [
     ?_FJP(<<"a">>, {error, {unexpected_char, 0}}),
     ?_FJP(<<"1a">>, {error, {unexpected_char, 1}}),
     ?_FJP(<<"1e">>, {error, {unexpected_end, 2}}),
     ?_FJP(<<"-">>, {error, {bad_integer, 0}}),
     ?_FJP(<<"1ee1">>, {error, {unexpected_char, 2}})
    ].

invalid_float_test_() ->
    [
     ?_FJP(<<".1">>, {error, {unexpected_char, 0}}),
     ?_FJP(<<"0.">>, {error, {bad_float, 0}}),
     ?_FJP(<<"0.a">>, {error, {bad_float, 0}}),
     ?_FJP(<<"0.1e">>, {error, {unexpected_end, 4}}),
     ?_FJP(<<"0.1e+">>, {error, {bad_float, 1}}),
     ?_FJP(<<"0.1ee">>, {error, {unexpected_char, 4}})
    ].

unbalanced_test_() ->
    [
     ?_FJP(<<"[{\"foo\":1]">>, {error, {unexpected_char, 9}}),
     ?_FJP(<<"[{\"foo\":1]}">>, {error, {unexpected_char, 9}}),
     ?_FJP(<<"{\"foo\":[1}">>, {error, {unexpected_char, 9}}),
     ?_FJP(<<"{\"foo\":[1}]">>, {error, {unexpected_char, 9}})
    ].

extra_comma_test_() ->
    [
     ?_FJP(<<"[1,2,3,]">>, {error, {unexpected_char, 7}}),
     %% one trailing comma is allowed for objects - ignoring for now
     ?_FJP(<<"{\"a\":1,}">>, {ok, {struct, [{<<"a">>, 1}]}}),
     ?_FJP(<<"{\"a\":1,,}">>, {error, {unexpected_char, 7}})
    ].

missing_separator_test_() ->
    [
     ?_FJP(<<"[1 2]">>, {error, {unexpected_char, 3}}),
     ?_FJP(<<"{\"a\" 1}">>, {error, {unexpected_char, 5}}),
     ?_FJP(<<"{\"a\":1 \"b\":2}">>, {error, {unexpected_char, 7}})
    ].
