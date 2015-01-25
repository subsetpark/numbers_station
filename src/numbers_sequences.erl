-module(numbers_sequences).
-export([natural/1, fibonacci/1, padovan/1, pyramid/1, taxicab/1, abundant/1, sphenic/1, happy/1, golomb/1, susanna/1, recaman/1]).
-compile([export_all]).
% --------------
% Numbers Series

-define(Taxicab, [2, 1729, 87539319, 6963472309248, 48988659276962496, 24153319581254312065344]).
-define(Phi, (1 + math:sqrt(5)) / 2).

% The sequence of natural numbers - the identity sequence
-spec natural(integer()) -> integer(). 
natural(N) -> N.

% The Fibonacci sequence - f(n) = f(n-1) + f(n-2)
-spec fibonacci(integer()) -> integer().
fibonacci(N) when N < 3 -> 1;
fibonacci(N) -> fibonacci(N, 1, 1).
fibonacci(3, P1, P2) -> P1 + P2;
fibonacci(N, P1, P2) -> fibonacci(N-1, P2, P1 + P2).

% The Padovan sequence - f(n) = f(n-2) + f(n-3)
-spec padovan(integer()) -> integer().
padovan(N) when N < 4 -> 1;
padovan(N) -> padovan(N, 1, 1, 1).
padovan(4, _A1, A2, A3) -> A2 + A3;
padovan(N, A1, A2, A3) -> padovan(N-1, A2, A3, A1+A2).


-spec pyramid(integer()) -> integer().
pyramid(N) -> (N * (N + 1) * (N + 2)) div 6.


-spec taxicab(integer()) -> integer() | binary().
taxicab(N) when N =< length(?Taxicab) ->
    lists:nth(N, ?Taxicab);
taxicab(_) -> <<"Term not known.">>.

% A number is abundant if it is less than the sum of its divisors.
-spec abundant(integer()) -> integer().
abundant(N) ->
    nth_term(N, fun is_abundant/1).
is_abundant(K) ->
    lists:sum([D || D <- lists:seq(1, K div 2), K rem D == 0]) > K.

% A number is sphenic if it is the product of three distinct primes.
-spec sphenic(integer()) -> integer().
sphenic(N) ->
    nth_term(N, fun is_sphenic/1).
is_sphenic(K) ->
    L = decomp(K),
    (length(L) == 3) and (product(L) == K).

% A number is happy if the repeated summing of the squares of its digits will lead to 1.
% Otherwise it is unhappy and the process will lead to a repeated pattern starting with 4.
-spec happy(integer()) -> integer().
happy(N) ->
    nth_term(N, fun is_happy/1).
is_happy(1) -> true;
is_happy(4) -> false;
is_happy(K) -> is_happy(sum_squares(K)).
sum_squares(N) -> sum_squares(N, 0).
sum_squares(0, Acc) -> round(Acc);
sum_squares(N, Acc) -> sum_squares(N div 10, Acc + math:pow(N rem 10, 2)).

% This one still confuses the hell out of me.
-spec golomb(integer()) -> integer().
golomb(1) -> 1;
golomb(N) -> round(math:pow(?Phi, (2 - ?Phi)) * math:pow(N, (?Phi - 1))).

-spec recaman(integer()) -> integer().
recaman(N) -> recaman(N, [0]).
recaman(N, L=[H|T]) ->
    MinusN = H - (K=length(L)),
    A = case {lists:member(MinusN, T), MinusN > 0} of
        {false, true} ->
                MinusN;
        {_, _} ->
                H + K
    end,
    case K of
        N -> A;
        _ -> recaman(N, [A|L])
    end.

% susanna(N) = the smallest K such that 2^K contains N in its digit expansion.
-spec susanna(integer()) -> integer().
susanna(N) -> susanna(N, 0).
susanna(N, K) -> 
    case string:str(integer_to_list(round(math:pow(2, K))), integer_to_list(N)) of
        0 -> susanna(N, K+1);
        _ -> K
    end.

-spec baum_sweet(integer()) -> integer().
baum_sweet(N) ->
    case contains_odd_zero_blocks(N) of
        false -> 1;
        true -> 0
    end.
contains_odd_zero_blocks(N) -> contains_odd_zero_blocks(0, integer_to_list(N, 2)).
contains_odd_zero_blocks(Count, []) -> 
    case Count rem 2 of
        1 -> true;
        0 -> false
    end;
contains_odd_zero_blocks(Count, [H|T]) when H == $1 -> 
    case Count rem 2 of
        1 -> true;
        0 -> contains_odd_zero_blocks(0, T)
    end;
contains_odd_zero_blocks(Count, [H|T]) when H == $0 -> 
    contains_odd_zero_blocks(Count + 1, T).

% A Harshad number is one that is divisible by the sum of its digits.
-spec harshad(integer()) -> integer().
harshad(N) -> nth_term(N, fun is_harshad/1).
is_harshad(N) -> 
    Digits = lists:map(fun erlang:list_to_integer/1, lists:map(fun(X) -> [X] end, integer_to_list(N))),
    N rem lists:sum(Digits) =:= 0.

% -------------------
% Series Constructors

% Produce the nth term that satisfies a predicate
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
% Decompose into unique factors
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