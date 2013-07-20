%% @doc fast json parsing
%%
%% Very important: all binary matching done is optimized such that no
%% sub-binaries are created (match contexts are re-used). This helps
%% keep garbage production to a minimum. Use the erlc option
%% +bin_opt_info to check for places where this optimization is not
%% applied.
-module(fj).

-export([decode/1, parse/1]).

-define(START_OBJECT, start_object).
-define(START_ARRAY, start_array).

%% @doc mochijson2 compatibility
decode(Bin) ->
    {ok, Value} = parse(Bin),
    Value.

parse(Bin) when is_binary(Bin) ->
    case catch value(Bin, [], [], done) of
        {error, Reason, Rest} ->
            {error, {Reason, byte_size(Bin)-byte_size(Rest)}};
        {'EXIT', {function_clause,
                  [{fj, _Name, [Remaining, Stack|_], _}|_]}}
          when is_binary(Remaining), is_list(Stack) ->
            case Remaining of
                <<>> ->
                    {error, {unexpected_end, byte_size(Bin)}};
                _ ->
                    {error, {unexpected_char,
                             byte_size(Bin)-byte_size(Remaining)}}
            end;
        {ok, [Value]} when Value /= ?START_OBJECT,
                           Value /= ?START_ARRAY ->
            {ok, Value};
        {ok, _} ->
            {error, {unexpected_end, byte_size(Bin)}}
    end.

value(<<${, Bin/binary>>, Stack, Current, Next) ->
    next(Bin, [{Next, Current}|Stack], [], obj_key);
value(<<$[, Bin/binary>>, Stack, Current, Next) ->
    value(Bin, [{Next, Current}|Stack], [], array_comma);
value(<<$", Bin/binary>>, Stack, Current, Next) ->
    str(Bin, Stack, Current, Next, []);
value(<<"true", Bin/binary>>, Stack, Current, Next) ->
    next(Bin, Stack, [true|Current], Next);
value(<<"false", Bin/binary>>, Stack, Current, Next) ->
    next(Bin, Stack, [false|Current], Next);
value(<<"null", Bin/binary>>, Stack, Current, Next) ->
    next(Bin, Stack, [null|Current], Next);
value(<<$\s, Bin/binary>>, Stack, Current, Next) ->
    value(Bin, Stack, Current, Next);
value(<<$\t, Bin/binary>>, Stack, Current, Next) ->
    value(Bin, Stack, Current, Next);
value(<<$\n, Bin/binary>>, Stack, Current, Next) ->
    value(Bin, Stack, Current, Next);
value(<<$\r, Bin/binary>>, Stack, Current, Next) ->
    value(Bin, Stack, Current, Next);
value(Bin, Stack, Current, Next) ->
    num(Bin, Stack, Current, Next).

next(<<$", Bin/binary>>, Stack, Current, obj_key) ->
    str(Bin, Stack, Current, obj_colon, []);
next(<<$:, Bin/binary>>, Stack, Current, obj_colon) ->
    value(Bin, Stack, Current, obj_comma);
next(<<$,, Bin/binary>>, Stack, Current, obj_comma) ->
    next(Bin, Stack, Current, obj_key);
next(<<$}, Bin/binary>>, [{Next, HD}|Stack], Current, KEYCOMMA)
  when KEYCOMMA == obj_key; KEYCOMMA == obj_comma ->
    next(Bin, Stack, [end_object(Current)|HD], Next);
next(<<$,, Bin/binary>>, Stack, Current, array_comma) ->
    value(Bin, Stack, Current, array_comma);
next(<<$], Bin/binary>>, [{Next, HD}|Stack], Current, array_comma) ->
    next(Bin, Stack, [end_array(Current)|HD], Next);
next(<<$\s, Bin/binary>>, Stack, Current, Next) ->
    next(Bin, Stack, Current, Next);
next(<<$\t, Bin/binary>>, Stack, Current, Next) ->
    next(Bin, Stack, Current, Next);
next(<<$\n, Bin/binary>>, Stack, Current, Next) ->
    next(Bin, Stack, Current, Next);
next(<<$\r, Bin/binary>>, Stack, Current, Next) ->
    next(Bin, Stack, Current, Next);
next(<<>>, [], Result, done) ->
    {ok, lists:reverse(Result)}.

end_array(Current) ->
    lists:reverse(Current).

end_object(Current) ->
    end_object(Current, []).
end_object([], Obj) ->
    {struct, Obj};
end_object([Value,Key|Current], Obj) when is_binary(Key) ->
    end_object(Current, [{Key, Value}|Obj]).

str(<<$", Bin/binary>>, Stack, Current, Next, Rev) ->
    next(Bin, Stack, [list_to_binary(lists:reverse(Rev))|Current], Next);
str(<<$\\, $", Bin/binary>>, St,Cu,Ne, Rev) ->
    str(Bin, St,Cu,Ne, [$"|Rev]);
str(<<$\\, $\\, Bin/binary>>, St,Cu,Ne, Rev) ->
    str(Bin, St,Cu,Ne, [$\\|Rev]);
str(<<$\\, $/, Bin/binary>>, St,Cu,Ne, Rev) ->
    str(Bin, St,Cu,Ne, [$/|Rev]);
str(<<$\\, $b, Bin/binary>>, St,Cu,Ne, Rev) ->
    str(Bin, St,Cu,Ne, [$\b|Rev]);
str(<<$\\, $f, Bin/binary>>, St,Cu,Ne, Rev) ->
    str(Bin, St,Cu,Ne, [$\f|Rev]);
str(<<$\\, $n, Bin/binary>>, St,Cu,Ne, Rev) ->
    str(Bin, St,Cu,Ne, [$\n|Rev]);
str(<<$\\, $r, Bin/binary>>, St,Cu,Ne, Rev) ->
    str(Bin, St,Cu,Ne, [$\r|Rev]);
str(<<$\\, $t, Bin/binary>>, St,Cu,Ne, Rev) ->
    str(Bin, St,Cu,Ne, [$\t|Rev]);
str(<<$\\, $u, A, B, C, D, Bin/binary>>, St,Cu,Ne, Rev) ->
    case {A bor 16#20, B bor 16#20} of
        {$d, Bl} when (Bl >= $8 andalso Bl =< $9);
                      (Bl >= $a andalso Bl =< $c) ->
            %% coalesce UTF-16 surrogate pair
            <<$\\, $u, E, F, G, H, Bin2/binary>> = Bin,
            str(Bin2, St,Cu,Ne, [utf8(A,B,C,D,E,F,G,H)|Rev]);
        _ ->
            str(Bin, St,Cu,Ne, [utf8(A,B,C,D)|Rev])
    end;
str(<<C, Bin/binary>>, St,Cu,Ne, Rev) ->
    str(Bin, St,Cu,Ne, [C|Rev]).

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

num(<<$-, Bin/binary>>, Stack, Current, Next) ->
    int(Bin, Stack, Current, Next, [$-]);
num(<<C, Bin/binary>>, Stack, Current, Next) when ?DIGIT(C) ->
    int(Bin, Stack, Current, Next, [C]).

int(<<$., Bin/binary>>, St,Cu,Ne, Rev) ->
    flo(Bin, St,Cu,Ne, [$.|Rev]);
int(<<$e, Bin/binary>>, St,Cu,Ne, Rev) ->
    esign(Bin, St,Cu,Ne, [$e|Rev], int);
int(<<$E, Bin/binary>>, St,Cu,Ne, Rev) ->
    esign(Bin, St,Cu,Ne, [$e|Rev], int);
int(<<C, Bin/binary>>, St,Cu,Ne, Rev) when ?DIGIT(C) ->
    int(Bin, St,Cu,Ne, [C|Rev]);
int(Bin, St,Cu,Ne, Rev) ->
    int_final(Bin, St,Cu,Ne, Rev).

flo(<<$e, Bin/binary>>, St,Cu,Ne, Rev) ->
    esign(Bin, St,Cu,Ne, [$e|Rev], flo);
flo(<<$E, Bin/binary>>, St,Cu,Ne, Rev) ->
    esign(Bin, St,Cu,Ne, [$e|Rev], flo);
flo(<<C, Bin/binary>>, St,Cu,Ne, Rev) when ?DIGIT(C) ->
    flo(Bin, St,Cu,Ne, [C|Rev]);
flo(Bin, St,Cu,Ne, Rev) ->
    flo_final(Bin, St,Cu,Ne, Rev).

esign(<<$-, Bin/binary>>, St,Cu,Ne, Rev, Dec) ->
    eint(Bin, St,Cu,Ne, [$-|Rev], Dec);
esign(<<$+, Bin/binary>>, St,Cu,Ne, Rev, Dec) ->
    eint(Bin, St,Cu,Ne, Rev, Dec);
esign(<<C, Bin/binary>>, St,Cu,Ne, Rev, Dec) when ?DIGIT(C) ->
    eint(Bin, St,Cu,Ne, [C|Rev], Dec).

eint(<<C, Bin/binary>>, St,Cu,Ne, Rev, Dec) when ?DIGIT(C) ->
    eint(Bin, St,Cu,Ne, [C|Rev], Dec);
eint(Bin, St,Cu,Ne, Rev, int) ->
    int_final(Bin, St,Cu,Ne, Rev);
eint(Bin, St,Cu,Ne, Rev, flo) ->
    flo_final(Bin, St,Cu,Ne, Rev).

int_final(Bin, Stack, Current, Next, Rev) ->
    case catch list_to_integer(lists:reverse(Rev)) of
        {'EXIT', _} ->
            throw({error, bad_integer,
                   list_to_binary([lists:reverse(Rev),Bin])});
        Int ->
            next(Bin, Stack, [Int|Current], Next)
    end.

flo_final(Bin, Stack, Current, Next, Rev) ->
    case catch list_to_float(lists:reverse(Rev)) of
        {'EXIT', _} ->
            throw({error, bad_float,
                   list_to_binary([lists:reverse(Rev)|Bin])});
        Float ->
            next(Bin, Stack, [Float|Current], Next)
    end.
