%%% -------------------------------------------------------------------
%%% 作者 : 晨星格拉拉
%%% 描述 : 日志中间层API
%%% -------------------------------------------------------------------
-module(fw_log).

-export([
    log/4,
    log/5,
    get_trace_id/0,
    set_trace_id/1,
    add_meta/1
]).

-export_type([level/0, metadata/0]).

-type level() :: debug | info | notice | warning | error.
-type metadata() :: map().

%% ------------------------------------------------------------------
%% 日志接口
%% ------------------------------------------------------------------

-spec log(level(), string(), list(), metadata()) -> ok.
log(Level, Format, Args, Metadata) ->
    Meta1 = add_meta(Metadata),
    do_log(Level, Format, Args, Meta1).

-spec log(level(), metadata(), string(), list(), metadata()) -> ok.
log(Level, Meta, Format, Args, Loc) ->
    CombinedMeta = maps:merge(Meta, Loc),
    Meta1 = add_meta(CombinedMeta),
    do_log(Level, Format, Args, Meta1).

%% ------------------------------------------------------------------
%% 内部实现
%% ------------------------------------------------------------------

-spec do_log(level(), string(), list(), metadata()) -> ok.
do_log(Level, Format, Args, Metadata) ->
    (impl()):log(Level, Format, Args, Metadata).

-spec impl() -> module().
impl() ->
    application:get_env(framework, log_impl, fw_log_logger).

%% ------------------------------------------------------------------
%% trace_id 管理（进程上下文）
%% ------------------------------------------------------------------

-spec get_trace_id() -> binary() | undefined.
get_trace_id() ->
    erlang:get(fw_trace_id).

-spec set_trace_id(binary() | undefined) -> ok.
set_trace_id(undefined) ->
    erlang:erase(fw_trace_id),
    ok;
set_trace_id(TraceId) when is_binary(TraceId) ->
    erlang:put(fw_trace_id, TraceId),
    ok;
set_trace_id(_) ->
    ok.

%% ------------------------------------------------------------------
%% 元数据处理
%% ------------------------------------------------------------------

-spec add_meta(metadata()) -> metadata().
add_meta(Meta) when is_map(Meta) ->
    case get_trace_id() of
        undefined -> Meta;
        TraceId  -> Meta#{trace_id => TraceId}
    end;
add_meta(_) ->
    add_meta(#{}).
