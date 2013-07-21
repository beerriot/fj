%% @doc fast json parsing
%%
%% It is expected that the input binary will contain exactly one
%% complete JSON value. Zero values, a partial value, or more than one
%% value (including an non-whitespace after the end of the value) will
%% result in an error.
%%
%% Very important: all binary matching done is optimized such that no
%% sub-binaries are created (match contexts are re-used). This helps
%% keep garbage production to a minimum. Use the erlc option
%% +bin_opt_info to check for places where this optimization is not
%% applied.
%%
%% The binary optimization is what requires the parsing process to be
%% written as a tail-recursive iteration, keeping track of its own
%% "stack" in a list instead of using the call stack. Returning the
%% unused portion of the binary from some recursive descent would
%% prevent the optimization.
%%
%% Avoiding garbage production is also the reason for separating
%% processing state into four parameters, instead of encapsulating it
%% in one record.
-module(fj).

-export([decode/1, parse/1]).

-type next() :: obj_key | obj_colon | obj_comma
              | array_comma
              | done.
-type value() :: true | false | null
               | binary()
               | number()
               | [value()]
               | {struct, [{binary(), value()}]}.

%% @doc mochijson2 compatibility
decode(Bin) ->
    {ok, Value} = parse(Bin),
    Value.

%% @doc Parse a binary, return a mochijson2-compatible structure.
%% Errors are of the form `{error, {Reason, ByteOffset}}'.
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
        {ok, [Value]} ->
            {ok, Value};
        {ok, _} ->
            %% no values, or many values
            {error, {unexpected_end, byte_size(Bin)}}
    end.

%% @doc Start looking for a value.
%%
%% Both value/4 and next/4 use the same argument pattern:
%% <ol>
%%
%% <li>`RemainingBinary' is what is left to parse of the input
%% blob</li>
%%
%% <li>`Stack' is the postponed state of processing while we parse an
%% inner array or object. Each entry is a 2-tuple of `{N, C}' where
%% `N' was the value of the `Next' parameter when nested parsing
%% began, and `C' is the value of the `Current' parameter at the same
%% time.</li>
%%
%% <li>`Current' is the state of the innermost element processing. For
%% arrays, this is the reverse list of elements parsed so far. For
%% objects, it is the reversed list of key/value pairs, such that the
%% last element in the list is the first key found, and the next to
%% last element is the first key's value, with the rest of the
%% elements continuing this alternating pattern.</li>
%%
%% <li>`Next' is the next state we'll be in after a value is
%% parsed. This is how strict parsing (requiring colons, commas,
%% balanced brackets, etc.)  is enforced.</li>
%%
%% </ol>
-spec value(binary(), [{next(), [value()]}], [value()], next())
        -> {ok, [value()]}.
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

%% @doc The other half of the processing. This part looks for specific
%% next items like colons and commas. See value/4 for details on
%% parameter types.
-spec next(binary(), [{next(), [value()]}], [value()], next())
        -> {ok, [value()]}.
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
end_object([Value,Key|Current], Obj) ->
    end_object(Current, [{Key, Value}|Obj]).

%% digit-safe lowercase
-define(LOWERC(X), (X bor 16#20)).

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
    case {?LOWERC(A), ?LOWERC(B)} of
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

%% @doc Translate the \u representation of a Unicode point into a
%% UTF-8 character (one-, two-, or three-bytes).
utf8($0, $0, C, D) ->
    list_to_integer([C,D], 16);
utf8($0, B, C, D) when B < $8 ->
    Left = (B-$0) band 16#07,
    Middle = hexval(C),
    Right = hexval(D),
    <<3:2, Left:4, (Middle bsr 2):2,     %% byte 1
      2:2, Middle:2, Right:4>>;          %% byte 2
utf8(A, B, C, D) ->
    Left = hexval(A),
    LeftMid = hexval(B),
    Middle = hexval(C),
    Right = hexval(D),
    <<14:4, Left:4,                      %% byte 1
      2:2, LeftMid:4, (Middle bsr 2):2,  %% byte 2
      2:2, Middle:2, Right:4>>.          %% byte 3

%% @doc Translate the `\u' representation of a UTF-16 surrogate pair
%% into a four-byte UTF-8 character. The first four arguments (ABCD)
%% are the representation of the first UTF-16 element, and the second
%% four (EFGH) are the representation of the second UTF-16 element.
utf8(_A, B, C, D, _E, F, G, H) -> % A = E = "d"
    HL = hexval(B),
    HM = hexval(C),
    HR = hexval(D),
    LL = hexval(F),
    LM = hexval(G),
    LR = hexval(H),

    HL8 = (((HL band 16#03) bsl 2) bor (HM bsr 2))+1,

    <<15:4, 0:2, (HL8 bsr 2):2,          %% byte 1
      2:2, HL8:2, HM:2, (HR bsr 2):2,    %% byte 2
      2:2, HR:2, LL:2, (LM bsr 2):2,     %% byte 3
      2:2, LM:2, LR:4>>.                 %% byte 4

%% @doc Compute the integer value of the hexidecimal character
%% representation. (0-9 == 0-9, a-f == 10-15, A-F == 10-15). Passing
%% in characters other than 0-9, a-f, A-F produces meaningless garbage.
hexval(C) ->
    case ?LOWERC(C) of
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
