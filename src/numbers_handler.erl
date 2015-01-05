-module(numbers_handler).
-compile({parse_transform, leptus_pt}).

-export([init/3]).
-export([cross_domains/3]).
-export([terminate/4]).

-export([get/3]).

-include("numbers_record.hrl").
-include_lib("stdlib/include/qlc.hrl").

cross_domains(_Route, _Req, State) ->
    {['_'], State}.

init(_Route, _Req, State) ->
    {ok, State}.

get("/series", _Req, State) ->
    Query = fun() ->
            qlc:e(
                qlc:q([X || X <- mnesia:table(numbers)])
                )
    end,
    {atomic, Records} = mnesia:transaction(Query),
    Json = numbers_helper:format(Records),
    {200, {json, Json}, State};

get("/series/:id/:n", Req, State) ->
    Id = binary_to_atom(leptus_req:param(Req, id), unicode),
    N = binary_to_integer(leptus_req:param(Req, n)),

    Series = get_series(Id),

    Result = Series(N),

    io:format('~p', [Result]),
    Json = numbers_helper:format([Result]),

    {200, {json, Json}, State}.

natural(N) ->
    N.

fibonacci(1) -> 1;
fibonacci(2) -> 1;
fibonacci(N) when N > 2 -> fib(N, 1, 1).

fib(3, P1, P2) -> P1 + P2;
fib(N, P1, P2) ->
    fib(N-1, P2, P1 + P2).

get_series(natural) -> fun natural/1;
get_series(fibonacci) -> fun fibonacci/1.

terminate(_Reason, _Route, _Req, _State) ->
    ok.
