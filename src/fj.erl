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
        {error, Rest} ->
            {error, byte_size(Bin)-byte_size(Rest)};
        [Value] ->
            {ok, Value}
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
value(<<32, Bin/binary>>, Stack) ->    %space
    value(Bin, Stack);
value(<<9, Bin/binary>>, Stack) ->     %tab
    value(Bin, Stack);
value(<<10, Bin/binary>>, Stack) ->    %line feed
    value(Bin, Stack);
value(<<13, Bin/binary>>, Stack) ->    %carriage return
    value(Bin, Stack);
value(<<>>, Result) ->
    Result;
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
end_object([Value,Key|Stack], Obj) ->
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
    str(Bin, Stack, [8|Rev]);
str(<<$\\, $f, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [10|Rev]);
str(<<$\\, $n, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [10,13|Rev]);
str(<<$\\, $r, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [13|Rev]);
str(<<$\\, $t, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [9|Rev]);
str(<<C, Bin/binary>>, Stack, Rev) ->
    str(Bin, Stack, [C|Rev]).
%% TODO: unicode & escape sequences

num(Bin, Stack) ->
    num(Bin, Stack, false, []).

num(<<$-, Bin/binary>>, Stack, Dec, Rev) ->
    num(Bin, Stack, Dec, [$-|Rev]);
num(<<$., Bin/binary>>, Stack, _, Rev) ->
    num(Bin, Stack, true, [$.|Rev]);
num(<<$e, Bin/binary>>, Stack, Dec, Rev) ->
    num(Bin, Stack, Dec, [$e|Rev]);
num(<<$E, Bin/binary>>, Stack, Dec, Rev) ->
    num(Bin, Stack, Dec, [$e|Rev]);
num(<<C, Bin/binary>>, Stack, Dec, Rev) when C >= $0, C =< $9 ->
    num(Bin, Stack, Dec, [C|Rev]);
num(Bin, Stack, true, Rev) ->
    case catch list_to_float(lists:reverse(Rev)) of
        {'EXIT', _} ->
            throw({error, Bin});
        Float ->
            value(Bin, [Float|Stack])
    end;
num(Bin, Stack, false, Rev) ->
    case catch list_to_integer(lists:reverse(Rev)) of
        {'EXIT', _} ->
            throw({error, Bin});
        Int ->
            value(Bin, [Int|Stack])
    end.
