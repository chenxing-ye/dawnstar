%%%-------------------------------------------------------------------
%% @doc dawnstar public API
%% @end
%%%-------------------------------------------------------------------

-module(dawnstar_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dawnstar_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
