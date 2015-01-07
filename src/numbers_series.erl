-module(numbers_series).
-export([get_series_names/0, fibonacci/1, natural/1, pyramid/1, taxicab/1, abundant/1]).

get_series_names() ->
    [<<"natural">>, <<"fibonacci">>, <<"pyramid">>, <<"taxicab">>, <<"abundant">>].

natural(N) ->
    N.

fibonacci(1) -> 1;
fibonacci(2) -> 1;
fibonacci(N) when N > 2 -> fib(N, 1, 1).

fib(3, P1, P2) -> P1 + P2;
fib(N, P1, P2) ->
    fib(N-1, P2, P1 + P2).

pyramid(N) ->
    (N * (N+1) * (N + 2)) div 6.

taxicab(N) ->
    Taxicab = [2, 1729, 87539319, 6963472309248, 48988659276962496, 24153319581254312065344],
    lists:nth(N, Taxicab).

abundant(N) ->
    nth_term(N, fun is_abundant/1).
is_abundant(K) ->
    % A number is abundant if it is less than the sum of its divisors.
    lists:sum([D || D <- lists:seq(1, K-1), K rem D == 0]) > K.

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