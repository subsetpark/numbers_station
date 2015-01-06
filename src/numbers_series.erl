-module(numbers_series).
-export([fibonacci/1, natural/1]).

-spec natural(integer()) -> integer().
natural(N) ->
    N.

-spec fibonacci(integer()) -> integer().
fibonacci(1) -> 1;
fibonacci(2) -> 1;
fibonacci(N) when N > 2 -> fib(N, 1, 1).

fib(3, P1, P2) -> P1 + P2;
fib(N, P1, P2) ->
    fib(N-1, P2, P1 + P2).