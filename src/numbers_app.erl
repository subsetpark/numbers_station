-module(numbers_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("numbers_record.hrl").

start(_StartType, _StartArgs) ->
    %% Start mnesia database in current node
    mnesia:create_schema([node()]),
    mnesia:start(),

    %% Create mnesia table based on record
    mnesia:create_table(series, [
            {attributes, record_info(fields, series)},
            {disc_copies, [node()]} %% disc copies means persistent
        ]),

    %% Define static directory for application
    Opts = [{static_dir, {'_', {priv_dir, ?MODULE, "templates"}}}],

    %% Start Leptus listener and route all requests to REST handler
    leptus:start_listener(http, [{'_', [{numbers_handler, under}]}], Opts).

stop(_State) ->
    ok.


