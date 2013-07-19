%% @doc fast json parsing
-module(fj).

-export([decode/1, parse/1]).

-define(START_OBJECT, start_object).
-define(START_ARRAY, start_array).

%% @doc mochijson2 compatibility
decode(Bin) ->
    {ok, Value} = parse(Bin),
    Value.

parse(Bin) when is_binary(Bin) ->
    case catch value(Bin, []) of
        {error, Reason, Rest} ->
            {error, {Reason, byte_size(Bin)-byte_size(Rest)}};
        {'EXIT', {function_clause, [{fj, _, [Remaining, Stack|_], _}|_]}}
          when is_binary(Remaining), is_list(Stack) ->
            case Remaining of
                <<>> ->
                    {error, {premature_end, byte_size(Bin)}};
                _ ->
                    {error, {unexpected,
                             byte_size(Bin)-byte_size(Remaining)}}
            end;
        {ok, [Value]} when Value /= ?START_OBJECT,
                           Value /= ?START_ARRAY ->
            {ok, Value};
        {ok, _} ->
            {error, {premature_end, byte_size(Bin)}}
    end.

value(<<${, Bin/binary>>, Stack) ->
    value(Bin, [?START_OBJECT|Stack]);
value(<<$}, Bin/binary>>, Stack) ->
    value(Bin, end_object(Stack));
value(<<$:, Bin/binary>>, Stack) ->
    value(Bin, Stack);
value(<<$[, Bin/binary>>, Stack) ->
    value(Bin, [?START_ARRAY|Stack]);
value(<<$], Bin/binary>>, Stack) ->
    value(Bin, end_array(Stack));
value(<<$,, Bin/binary>>, Stack) ->
    value(Bin, Stack);
value(<<$", Bin/binary>>, Stack) ->
    str(Bin, Stack);
value(<<"true", Bin/binary>>, Stack) ->
    value(Bin, [true|Stack]);
value(<<"false", Bin/binary>>, Stack) ->
    value(Bin, [false|Stack]);
value(<<"null", Bin/binary>>, Stack) ->
    value(Bin, [null|Stack]);
value(<<$\s, Bin/binary>>, Stack) ->
    value(Bin, Stack);
value(<<$\t, Bin/binary>>, Stack) ->
    value(Bin, Stack);
value(<<$\n, Bin/binary>>, Stack) ->
    value(Bin, Stack);
value(<<$\r, Bin/binary>>, Stack) ->
    value(Bin, Stack);
value(<<>>, Result) ->
    {ok, Result};
value(Bin, Stack) ->
    num(Bin, Stack).

end_array(Stack) ->
    end_array(Stack, []).
end_array([?START_ARRAY|Stack], Array) ->
    [Array|Stack];
end_array([Item|Stack], Array) ->
    end_array(Stack, [Item|Array]).

end_object(Stack) ->
    end_object(Stack, []).
end_object([?START_OBJECT|Stack], Obj) ->
    [{struct, Obj}|Stack];
end_object([Value,Key|Stack], Obj) when is_binary(Key) ->
    end_object(Stack, [{Key, Value}|Obj]).

str(Bin, Stack) ->
    str(Bin, Stack, []).
str(<<$", Bin/binary>>, Stack, Rev) ->
    value(Bin, [list_to_binary(lists:reverse(Rev))|Stack]);
str(<<$\\, $", Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [$"|Rev]);
str(<<$\\, $\\, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [$\\|Rev]);
str(<<$\\, $/, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [$/|Rev]);
str(<<$\\, $b, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [$\b|Rev]);
str(<<$\\, $f, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [$\f|Rev]);
str(<<$\\, $n, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [$\n|Rev]);
str(<<$\\, $r, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [$\r|Rev]);
str(<<$\\, $t, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [$\t|Rev]);
str(<<$\\, $u, A, B, C, D, Bin/binary>>, Stack, Rev) ->
    case {A bor 16#20, B bor 16#20} of
        {$d, Bl} when (Bl >= $8 andalso Bl =< $9);
                      (Bl >= $a andalso Bl =< $c) ->
            %% coalesce UTF-16 surrogate pair
            <<$\\, $u, E, F, G, H, Bin2/binary>> = Bin,
            str(Bin2, Stack, [utf8(A,B,C,D,E,F,G,H)|Rev]);
        _ ->
            str(Bin, Stack, [utf8(A,B,C,D)|Rev])
    end;
str(<<C, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [C|Rev]).

utf8($0, $0, C, D) ->
    list_to_integer([C,D], 16);
utf8($0, B, C, D) when B < $8 ->
    Left = (B-$0) band 16#07,
    Middle = hexval(C),
    Right = hexval(D),
    [16#C0 bor (Left bsl 2) bor (Middle bsr 2),
     16#80 bor ((Middle bsl 6) band 16#FF) bor Right];
utf8(A, B, C, D) ->
    Left = hexval(A),
    LeftMid = hexval(B),
    Middle = hexval(C),
    Right = hexval(D),
    [16#E0 bor Left,
     16#80 bor (LeftMid bsl 2) bor (Middle bsr 2),
     16#80 bor ((Middle bsl 6) band 16#FF) bor Right].

utf8(_A, B, C, D, _E, F, G, H) -> % A = E = "d"
    HL = hexval(B),
    HM = hexval(C),
    HR = hexval(D),
    LL = hexval(F),
    LM = hexval(G),
    LR = hexval(H),

    HL8 = (((HL band 16#03) bsl 2) bor (HM bsr 2))+1,

    [16#F0 bor (HL8 bsr 2),
     16#80 bor ((HL8 band 16#03) bsl 4) bor
         ((HM band 16#03) bsl 2) bor (HR bsr 2),
     16#80 bor ((HR band 16#03) bsl 4) bor
         ((LL band 16#03) bsl 2) bor (LM bsr 2),
     16#80 bor ((LM band 16#03) bsl 4) bor LR].

hexval(C) ->
    case C bor 16#20 of
        Cd when Cd < $a ->
            Cd-$0;
        Cl ->
            10+Cl-$a
    end.

-define(DIGIT(C), (C >= $0 andalso C =< $9)).

num(<<$-, Bin/binary>>, Stack) ->
    int(Bin, Stack, [$-]);
num(<<C, Bin/binary>>, Stack) when ?DIGIT(C) ->
    int(Bin, Stack, [C]).

int(<<$., Bin/binary>>, Stack, Rev) ->
    flo(Bin, Stack, [$.|Rev]);
int(<<$e, Bin/binary>>, Stack, Rev) ->
    esign(Bin, Stack, [$e|Rev], int);
int(<<$E, Bin/binary>>, Stack, Rev) ->
    esign(Bin, Stack, [$e|Rev], int);
int(<<C, Bin/binary>>, Stack, Rev) when ?DIGIT(C) ->
    int(Bin, Stack, [C|Rev]);
int(Bin, Stack, Rev) ->
    int_final(Bin, Stack, Rev).

flo(<<$e, Bin/binary>>, Stack, Rev) ->
    esign(Bin, Stack, [$e|Rev], flo);
flo(<<$E, Bin/binary>>, Stack, Rev) ->
    esign(Bin, Stack, [$e|Rev], flo);
flo(<<C, Bin/binary>>, Stack, Rev) when ?DIGIT(C) ->
    flo(Bin, Stack, [C|Rev]);
flo(Bin, Stack, Rev) ->
    flo_final(Bin, Stack, Rev).

esign(<<$-, Bin/binary>>, Stack, Rev, Dec) ->
    eint(Bin, Stack, [$-|Rev], Dec);
esign(<<$+, Bin/binary>>, Stack, Rev, Dec) ->
    eint(Bin, Stack, Rev, Dec);
esign(<<C, Bin/binary>>, Stack, Rev, Dec) when ?DIGIT(C) ->
    eint(Bin, Stack, [C|Rev], Dec).

eint(<<C, Bin/binary>>, Stack, Rev, Dec) when ?DIGIT(C) ->
    eint(Bin, Stack, [C|Rev], Dec);
eint(Bin, Stack, Rev, int) ->
    int_final(Bin, Stack, Rev);
eint(Bin, Stack, Rev, flo) ->
    flo_final(Bin, Stack, Rev).

int_final(Bin, Stack, Rev) ->
    case catch list_to_integer(lists:reverse(Rev)) of
        {'EXIT', _} ->
            throw({error, bad_integer,
                   list_to_binary([lists:reverse(Rev),Bin])});
        Int ->
            value(Bin, [Int|Stack])
    end.

flo_final(Bin, Stack, Rev) ->
    case catch list_to_float(lists:reverse(Rev)) of
        {'EXIT', _} ->
            throw({error, bad_float,
                   list_to_binary([lists:reverse(Rev)|Bin])});
        Float ->
            value(Bin, [Float|Stack])
    end.
