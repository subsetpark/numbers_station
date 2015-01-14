-module(numbers_sequences).
-export([get_sequence_names/0, get_term/2, n_terms/2]).

-spec get_sequence_names() -> list(binary()).
get_sequence_names() ->
    [<<"natural">>, <<"fibonacci">>, <<"pyramid">>, <<"taxicab">>, <<"abundant">>, <<"padovan">>].

-spec get_term(atom(), integer()) -> {atom(), integer()}.
get_term(_, N) when N < 1 -> {error, <<"Bad term.">>};
get_term(natural, N) -> term(fun natural/1, N);
get_term(fibonacci, N) -> term(fun fibonacci/1, N);
get_term(pyramid, N) -> term(fun pyramid/1, N);
get_term(taxicab, N) -> term(fun taxicab/1, N);
get_term(abundant, N) -> term(fun abundant/1, N);
get_term(padovan, N) -> term(fun padovan/1, N);
get_term(_, _) -> {not_found, <<"Series not found.">>}.

term(Fun, N) ->
    A = Fun(N),
    if
        is_integer(A) -> {ok, A};
        not is_integer(A) -> {error, A}
    end.

-spec n_terms(atom(), non_neg_integer()) -> {ok, list(integer())}.
n_terms(Sequence, N) -> n_terms(Sequence, N, []).
n_terms(_, 0, L) -> {ok, L};
n_terms(Sequence, N, L) ->
    {ok, A} = get_term(Sequence, N),
    n_terms(Sequence, N-1, [A|L]).

% --------------
% Numbers Series

-define(Taxicab, [2, 1729, 87539319, 6963472309248, 48988659276962496, 24153319581254312065344]).

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
