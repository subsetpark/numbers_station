-module(numbers_sequences).
-export([get_sequence_names/0, get_term/2, n_terms/2]).

get_sequence_names() ->
    [<<"natural">>, <<"fibonacci">>, <<"pyramid">>, <<"taxicab">>, <<"abundant">>, <<"padovan">>].

get_term(_, N) when N < 1 -> {error, <<"Bad term.">>};
get_term(natural, N) -> {ok, natural(N)};
get_term(fibonacci, N) -> {ok, fibonacci(N)};
get_term(pyramid, N) -> {ok, pyramid(N)};
get_term(taxicab, N) -> {ok, taxicab(N)};
get_term(abundant, N) -> {ok, abundant(N)};
get_term(padovan, N) -> {ok, padovan(N)};
get_term(_, _) -> {not_found, <<"Series not found.">>}.

n_terms(Sequence, N) -> lists:map(fun({ok, A}) -> A end, [get_term(Sequence, Y) || Y <- lists:seq(1, N) ]).
% Numbers Series
% The sequence of natural numbers - the identity sequence
natural(N) -> N.

fibonacci(1) -> 1;
fibonacci(2) -> 1;
fibonacci(N) when N > 2 -> fib(N, 1, 1).

fib(3, P1, P2) -> P1 + P2;
fib(N, P1, P2) -> fib(N-1, P2, P1 + P2).

pyramid(N) -> (N * (N+1) * (N + 2)) div 6.

taxicab(N) ->
    Taxicab = [2, 1729, 87539319, 6963472309248, 48988659276962496, 24153319581254312065344],
    lists:nth(N, Taxicab).

% A number is abundant if it is less than the sum of its divisors.
abundant(N) ->
    nth_term(N, fun is_abundant/1).
is_abundant(K) ->
    lists:sum([D || D <- lists:seq(1, K div 2), K rem D == 0]) > K.

padovan(N) when N < 4 -> 1;
padovan(N) -> padovan(N, 4, [1, 1, 1]).
padovan(N, K, [A1,A2,A3|T]) ->
    A = A2 + A3,
    case K of
        N -> A;
        _ -> padovan(N, K + 1, [A,A1,A2,A3|T])
    end.

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
