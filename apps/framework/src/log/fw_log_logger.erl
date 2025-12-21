%%% -------------------------------------------------------------------
%%% 作者 : 晨星格拉拉
%%% 描述 : OTP logger 适配器
%%% -------------------------------------------------------------------
-module(fw_log_logger).

-export([log/4]).

-spec log(fw_log:level(), string(), list(), map()) -> ok.
log(Level, Format, Args, Metadata) ->
    logger:log(map_level(Level), Format, Args, Metadata).

%% ------------------------------------------------------------------
%% 业务级 level -> OTP logger level 映射
%% ------------------------------------------------------------------

-spec map_level(fw_log:level()) -> logger:level().
map_level(debug)   -> debug;
map_level(info)    -> info;
map_level(notice)  -> notice;
map_level(warning) -> warning;
map_level(error)   -> error.