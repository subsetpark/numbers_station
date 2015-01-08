-module(numbers_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Start Leptus listener and route all requests to REST handler
    leptus:start_listener(http, [{'_', [{numbers_handler, under}]}]).

stop(_State) ->
    ok.


