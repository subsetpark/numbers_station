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


terminate(_Reason, _Route, _Req, _State) ->
    ok.

get("/series", _Req, State) ->
    Series = numbers_series:get_series_names(),
    Json = numbers_helper:format([Series]),
    {200, {json, Json}, State};

get("/series/:id/:n", Req, State) ->
    Id = leptus_req:param(Req, id),
    N = binary_to_integer(leptus_req:param(Req, n)),

    {Status, Response} = get_term(Id, N),

    Json = numbers_helper:format([Response]),
    
    Code = case Status of
        ok -> 200;
        not_found -> 404;
        error -> 400
    end,

    {Code, {json, Json}, State}.

-spec get_term(binary(), integer()) -> {atom(), integer()}.
get_term(Series, N) when (N > 0) -> 
    case Series of 
        <<"natural">> ->    {ok, numbers_series:natural(N)};
        <<"fibonacci">> ->  {ok, numbers_series:fibonacci(N)};
        <<"pyramid">> ->    {ok, numbers_series:pyramid(N)};
        <<"taxicab">> ->    {ok, numbers_series:taxicab(N)};
        <<"abundant">> ->   {ok, numbers_series:abundant(N)};
        _ ->                {not_found, <<"Series not found.">>}
    end;
get_term(_, _) ->       {error, <<"Bad term.">>}.
