-module(numbers_helper).
-export([format/1]).

format(List) -> format(List, []).
format([], Results) -> Results;
format([H|T], Results) -> format(T, [json(H)|Results]).

json({_, Id, Name}) ->
    {Id, Name};
json(N) ->
    {result, N}.