-module(numbers_sequences).
-export([get_sequence_names/0, get_term/2, n_terms/2, get_term/3, tabulator/3]).
-compile([export_all]).

-spec get_sequence_names() -> list(binary()).
get_sequence_names() ->
    [<<"natural">>, <<"fibonacci">>, <<"pyramid">>, <<"taxicab">>, <<"abundant">>, <<"padovan">>, <<"sphenic">>, <<"happy">>, <<"golomb">>, <<"recaman">>].

-spec get_term(atom(), integer()) -> {atom(), integer()}.
get_term(_, N) when N < 1 -> {error, <<"Bad term.">>};
get_term(natural, N) -> term(fun natural/1, N);
get_term(fibonacci, N) -> term(fun fibonacci/1, N);
get_term(pyramid, N) -> term(fun pyramid/1, N);
get_term(taxicab, N) -> term(fun taxicab/1, N);
get_term(abundant, N) -> term(fun abundant/1, N);
get_term(padovan, N) -> term(fun padovan/1, N);
get_term(sphenic, N) -> term(fun sphenic/1, N);
get_term(happy, N) -> term(fun happy/1, N);
get_term(golomb, N) -> term(fun golomb/1, N);
get_term(recaman, N) -> term(fun recaman/1, N);
get_term(_, _) -> {not_found, <<"Series not found.">>}.

term(Fun, N) ->
    A = Fun(N),
    if
        is_integer(A) -> {ok, A};
        not is_integer(A) -> {error, A}
    end.

-spec n_terms(atom(), non_neg_integer()) -> {ok, list(integer())}.
n_terms(Sequence, N) ->
    Tabulator_Pid = spawn(numbers_sequences, tabulator, [self(), Sequence, N]),
    spawn(numbers_sequences, get_term, [Tabulator_Pid, Sequence, 1]),
    n_terms(Tabulator_Pid, Sequence, N, 1).
n_terms(_, _, N, K) when N == K ->
    receive
        {all_terms, L} ->
            {ok, L}
    end;
n_terms(Tabulator_Pid, Sequence, N, K) ->
    spawn(numbers_sequences, get_term, [Tabulator_Pid, Sequence, K+1]),
    n_terms(Tabulator_Pid, Sequence, N, K+1).

get_term(Pid, Sequence, K) ->
    {ok, A} = get_term(Sequence, K),
    Pid ! {Sequence, K, A}.

tabulator(Parent_Pid, Sequence, N) -> tabulator(Parent_Pid, Sequence, N, []).
tabulator(Parent_Pid, _, N, L) when length(L) =:= N ->
    R = [ A || {_, A} <- lists:sort(L)],
    Parent_Pid ! {all_terms, R};
tabulator(Parent_Pid, Sequence, N, L) ->
    receive
        {Sequence, K, A} ->
            tabulator(Parent_Pid, Sequence, N, [{K, A}|L])
    end.

% --------------
% Numbers Series

-define(Taxicab, [2, 1729, 87539319, 6963472309248, 48988659276962496, 24153319581254312065344]).
-define(Phi, (1 + math:sqrt(5)) / 2).

% The sequence of natural numbers - the identity sequence
natural(N) -> N.

% The Fibonacci sequence - f(n) = f(n-1) + f(n-2)
fibonacci(N) when N < 3 -> 1;
fibonacci(N) -> fibonacci(N, 1, 1).
fibonacci(3, P1, P2) -> P1 + P2;
fibonacci(N, P1, P2) -> fibonacci(N-1, P2, P1 + P2).

% The Padovan sequence - f(n) = f(n-2) + f(n-3)
padovan(N) when N < 4 -> 1;
padovan(N) -> padovan(N, 1, 1, 1).
padovan(4, _A1, A2, A3) -> A2 + A3;
padovan(N, A1, A2, A3) -> padovan(N-1, A2, A3, A1+A2).

pyramid(N) -> (N * (N+1) * (N + 2)) div 6.

taxicab(N) when N =< length(?Taxicab) ->
    lists:nth(N, ?Taxicab);
taxicab(_) -> <<"Term not known.">>.

% A number is abundant if it is less than the sum of its divisors.
abundant(N) ->
    nth_term(N, fun is_abundant/1).
is_abundant(K) ->
    lists:sum([D || D <- lists:seq(1, K div 2), K rem D == 0]) > K.

% A number is sphenic if it is the product of three distinct primes.
sphenic(N) ->
    nth_term(N, fun is_sphenic/1).
is_sphenic(K) ->
    L = decomp(K),
    (length(L) == 3) and (product(L) == K).

% A number is happy if the repeated summing of the squares of its digits will lead to 1.
% Otherwise it is unhappy and the process will lead to a repeated pattern starting with 4.
happy(N) ->
    nth_term(N, fun is_happy/1).
is_happy(1) -> true;
is_happy(4) -> false;
is_happy(K) -> is_happy(sum_squares(K)).
sum_squares(N) -> sum_squares(N, 0).
sum_squares(0, Acc) -> round(Acc);
sum_squares(N, Acc) -> sum_squares(N div 10, Acc + math:pow(N rem 10, 2)).

golomb(1) -> 1;
golomb(N) -> round(math:pow(?Phi, (2 - ?Phi)) * math:pow(N, (?Phi - 1))).

recaman(N) -> recaman(1, N, [0]).
recaman(Candidate, Target, L=[H|T]) ->
    MinusN = H - Candidate,
    A = case {lists:member(MinusN, T), MinusN > 0} of
        {false, true} ->
                MinusN;
        {_, _} ->
                H + Candidate
    end,
    case Candidate of
        Target ->
            A;
        _ ->
            recaman(Candidate + 1, Target, [A|L])
    end.
% -------------------
% Series Constructors

nth_term(N, Test) ->
    nth_term(N, 0, 1, Test).
nth_term(N, Count, Candidate, Test) ->
    case Test(Candidate) of
        true when Count == (N - 1) -> 
            Candidate;
        true ->
            nth_term(N, (Count + 1), (Candidate + 1), Test);
        _ ->
            nth_term(N, Count, (Candidate + 1), Test)
    end.

decomp(N) -> decomp(N, [], 2).
decomp(N, R, 2) ->
    decomp(N, R, 3);
decomp(N, R, I) when I * I > N ->
    case lists:member(N, R) of
        false -> [N|R];
        true -> R
    end;
decomp(N, R, I) when (N rem I) =:= 0 ->
    case lists:member(I, R) of
        false -> decomp(N div I, [I|R], I);
        true -> decomp(N div I, R, I)
    end;
decomp(N, R, I) -> decomp(N, R, I+2).

product(L) -> lists:foldl(fun(X,Prod) -> X * Prod end, 1, L).