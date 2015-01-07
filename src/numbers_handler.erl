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
    Id = binary_to_atom(leptus_req:param(Req, id), unicode),
    N = binary_to_integer(leptus_req:param(Req, n)),

    {Status, Response} = numbers_series:get_term(Id, N),

    Json = numbers_helper:format([Response]),
    
    Code = case Status of
        ok -> 200;
        not_found -> 404;
        error -> 400
    end,

    {Code, {json, Json}, State}.