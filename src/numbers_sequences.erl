-module(numbers_sequences).
-export([natural/1, fibonacci/1, padovan/1, pyramid/1, taxicab/1, abundant/1, 
         happy/1, golomb/1, susanna/1, recaman/1, thue_morse/1, kolakoski/1, 
         baum_sweet/1, harshad/1, ulam/1, nude/1, omega/1, sphenic/1]).
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

-spec omega(integer()) -> integer().
omega(N) ->
    length(prime_factor(N)).

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
    N rem lists:sum(get_digits(N)) =:= 0.

thue_morse(N) ->
    case count_zeroes(N) rem 2 of
        1 -> 1;
        0 -> 0
    end.
count_zeroes(N) ->
    F = fun(Chr, Count) when Chr == $0 -> Count + 1;
           (_, Count) -> Count
        end,
    lists:foldl(F, 0, integer_to_list(N, 2)).

% The Kolakoski sequences provides its own run-length encoding.
-spec kolakoski(integer()) -> integer().
kolakoski(N) -> kolakoski(N, 1, []).
kolakoski(N, _, L) when length(L) >= N -> lists:nth(N, L);
kolakoski(N, I, L) ->
    Repeat = case length(L) >= I of
        true ->
            lists:nth(I, L);
        false ->
            I
    end,
    Value = if
        I rem 2 == 0 -> 2;
        true -> 1
    end,
    kolakoski(N, I+1, L ++ lists:duplicate(Repeat, Value)).

-spec ulam(integer()) -> integer().
%   least number > a(n-1) which is a unique sum of two distinct earlier terms. 
ulam(N) ->
    ulam(N, [1]).
ulam(N, (L=[H|_])) -> case length(L) of
    N -> H;
    X when X < 2 -> ulam(N, [X + 1|L]);
    _ -> 
        L2 = [X + Y || X <- L, Y <- L, X /= Y, not lists:member(X+Y, L)],
        A = lists:min([A || A <- L2, 
              length(lists:filter(fun(Y) -> Y == A end, L2)) == 2]),
        ulam(N, [A|L])
    end.
    
-spec nude(integer()) -> integer().
% A number is nude if it is divisible by each of its digits.
nude(N) ->
    nth_term(N, fun is_nude/1).
is_nude(K) ->
    lists:all(fun(D) -> (D /= 0) andalso (K rem D == 0) end, get_digits(K)).

-spec sphenic(integer()) -> integer().
% A sphenic number is the product of exactly three primes.
sphenic(N) ->
    nth_term(N, fun is_sphenic/1).
is_sphenic(K) ->
    Factors = prime_factor(K),
    (length(Factors) == 3) and (product(Factors) == K).
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

prime_factor(N) ->
    Parent = self(),
    Pid = spawn_link(fun() -> prime_factor1(N, [], Parent) end),
    spawn_link(fun() -> sieve(lists:seq(2, N), Pid) end),
    receive
        {factors, Factors} ->
            Factors
    end.
prime_factor1(N, Factors, Parent) ->
    receive
        {factor, Factor} ->
            case N rem Factor of
                0 ->
                    prime_factor1(N, [Factor|Factors], Parent);
                _ ->
                    prime_factor1(N, Factors, Parent)
            end;
        done ->
            Parent ! {factors, Factors}
    end.
sieve([K|Ks], Parent) ->
    Parent ! {factor, K},
    % Build the sieve and peel off a new prime one by one
    sieve([X || X <- Ks, X rem K /= 0], Parent);
sieve([], Parent) ->
    Parent ! done.

product(L) -> 
    lists:foldl(fun(X,Prod) -> X * Prod end, 1, L).

get_digits(N) -> 
    lists:map(fun erlang:list_to_integer/1, lists:map(fun(X) -> [X] end, integer_to_list(N))).