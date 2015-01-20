-module(numbers_handler).
-compile({parse_transform, leptus_pt}).

-export([init/3]).
-export([cross_domains/3]).
-export([terminate/4]).

-export([get/3]).

cross_domains(_Route, _Req, State) ->
    {['_'], State}.

init(_Route, _Req, State) ->
    {ok, State}.


terminate(_Reason, _Route, _Req, _State) ->
    ok.

get("/sequences", _Req, State) ->
    Series = numbers_helper:get_sequence_names(),
    Json = numbers_helper:format([Series]),
    {200, {json, Json}, State};

get("/sequences/:id/:n", Req, State) ->
    Id = binary_to_atom(leptus_req:param(Req, id), unicode),
    N = binary_to_integer(leptus_req:param(Req, n)),

    {Status, Response} = numbers_helper:get_term(Id, N),

    Json = numbers_helper:format([Response]),
    
    Code = analyze_status(Status),

    {Code, {json, Json}, State};

get("/sequences/:id/first/:n", Req, State) ->
    Id = binary_to_atom(leptus_req:param(Req, id), unicode),
    N = binary_to_integer(leptus_req:param(Req, n)),

    {Status, Response} = numbers_helper:n_terms(Id, N),

    Json = numbers_helper:format([Response]),
    
    Code = analyze_status(Status),

    {Code, {json, Json}, State}.

analyze_status(Status) ->
    case Status of
        ok -> 200;
        not_found -> 404;
        error -> 400
    end.