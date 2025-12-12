%%%-------------------------------------------------------------------
%% @doc dawnstar_framework public API
%% @end
%%%-------------------------------------------------------------------

-module(framework_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Initialize configuration module
    fw_conf:start(),
    framework_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
