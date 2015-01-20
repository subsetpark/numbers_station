-module(numbers_helper).
-export([format/1, get_sequence_names/0, get_term/2, n_terms/2]).

format(List) -> format(List, []).
format([], Results) -> Results;
format([H|T], Results) -> format(T, [json(H)|Results]).

json({_, Id, Name}) ->
    {Id, Name};
json(N) ->
    {result, N}.

-spec get_sequence_names() -> list(binary()).
get_sequence_names() ->
    [<<"natural">>, <<"fibonacci">>, <<"pyramid">>, 
     <<"taxicab">>, <<"abundant">>, <<"padovan">>, 
     <<"sphenic">>, <<"happy">>, <<"golomb">>, 
     <<"recaman">>, <<"susanna">>].

-spec get_term(atom(), integer()) -> {atom(), (integer() | binary())}.
get_term(_, N) when N < 1 -> {error, <<"Bad term.">>};
get_term(natural, N) -> term(numbers_sequences:natural(N));
get_term(fibonacci, N) -> term(numbers_sequences:fibonacci(N));
get_term(pyramid, N) -> term(numbers_sequences:pyramid(N));
get_term(taxicab, N) -> term(numbers_sequences:taxicab(N));
get_term(abundant, N) -> term(numbers_sequences:abundant(N));
get_term(padovan, N) -> term(numbers_sequences:padovan(N));
get_term(sphenic, N) -> term(numbers_sequences:sphenic(N));
get_term(happy, N) -> term(numbers_sequences:happy(N));
get_term(golomb, N) -> term(numbers_sequences:golomb(N));
get_term(recaman, N) -> term(numbers_sequences:recaman(N));
get_term(susanna, N) -> term(numbers_sequences:susanna(N));
get_term(_, _) -> {not_found, <<"Series not found.">>}.

term(R) when is_integer(R) -> 
    {ok, R};
term(R) -> {error, R}.

-spec n_terms(atom(), non_neg_integer()) -> {ok, list(integer())}.
n_terms(Sequence, N) ->
    Parent_Pid = self(),
    Tabulator_Pid = spawn(fun() -> tabulator(Parent_Pid, Sequence, N) end),
    spawn(fun() -> get_term(Tabulator_Pid, Sequence, 1) end),
    n_terms(Tabulator_Pid, Sequence, N, 1).
n_terms(_, _, N, K) when N == K ->
    receive
        {all_terms, L} ->
            {ok, L}
    end;
n_terms(Tabulator_Pid, Sequence, N, K) ->
    spawn(fun() -> get_term(Tabulator_Pid, Sequence, K+1) end),
    n_terms(Tabulator_Pid, Sequence, N, K+1).

get_term(Pid, Sequence, K) ->
    {ok, A} = get_term(Sequence, K),
    Pid ! {Sequence, K, A}.

tabulator(Parent_Pid, Sequence, N) -> tabulator(Parent_Pid, Sequence, N, []).
tabulator(Parent_Pid, _, N, L) when length(L) =:= N ->
    R = [A || {_, A} <- lists:sort(L)],
    Parent_Pid ! {all_terms, R};
tabulator(Parent_Pid, Sequence, N, L) ->
    receive
        {Sequence, K, A} ->
            tabulator(Parent_Pid, Sequence, N, [{K, A}|L])
    end.