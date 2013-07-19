-module(fj_decode_errors).

-include_lib("eunit/include/eunit.hrl").

-define(FJP(J, E), ?assertEqual(E, fj:parse(J))).
-define(_FJP(J, E), ?_assertEqual(E, fj:parse(J))).

empty_test() ->
    ?FJP(<<>>, {error, {premature_end, 0}}).

unclosed_array_test() ->
    ?FJP(<<"[">>, {error, {premature_end, 1}}).

unclosed_object_test() ->
    ?FJP(<<"{">>, {error, {premature_end, 1}}).

unclosed_string_test() ->
    ?FJP(<<"\"">>, {error, {premature_end, 1}}).

invalid_integer_test_() ->
    [
     ?_FJP(<<"a">>, {error, {unexpected, 0}}),
     ?_FJP(<<"1a">>, {error, {unexpected, 1}}),
     ?_FJP(<<"1e">>, {error, {premature_end, 2}}),
     ?_FJP(<<"-">>, {error, {bad_integer, 0}}),
     ?_FJP(<<"1ee1">>, {error, {unexpected, 2}})
    ].
