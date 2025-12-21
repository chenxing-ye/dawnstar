%%% -------------------------------------------------------------------
%%% 作者 : 晨星格拉拉
%%% 描述 : 日志模块测试
%%% 创建 : 2025-12-17
%%% -------------------------------------------------------------------
-module(fw_log_test).

-include_lib("eunit/include/eunit.hrl").
-include("fw_log.hrl").

%% 测试日志功能
log_test() ->
    %% 设置trace_id
    TraceId = <<"test_trace_123">>,
    fw_log:set_trace_id(TraceId),
    ?assertEqual(TraceId, fw_log:get_trace_id()),

    %% 测试不同级别的日志
    ?DEBUG("This is a debug message", []),
    ?INFO("This is an info message", []),
    ?WARN("This is a warning message", []),
    ?ERROR("This is an error message", []),

    %% 测试带参数的日志
    ?DEBUG("Debug message with params: ~p, ~p", [123, <<"test">>]),
    ?INFO("Info message with params: ~p, ~p", [456, <<"test2">>]),
    ?WARN("Warning message with params: ~p, ~p", [789, <<"test3">>]),
    ?ERROR("Error message with params: ~p, ~p", [0, <<"test4">>]),

    %% 测试带元数据的日志
    Meta = #{user_id => 1001, operation => <<"login">>},
    ?DEBUG(Meta, "Debug message with meta", []),
    ?INFO(Meta, "Info message with meta", []),
    ?WARN(Meta, "Warning message with meta", []),
    ?ERROR(Meta, "Error message with meta", []),

    %% 测试带元数据和参数的日志
    ?DEBUG(Meta, "Debug message with meta and params: ~p", ["test"]),
    ?INFO(Meta, "Info message with meta and params: ~p", ["test2"]),
    ?WARN(Meta, "Warning message with meta and params: ~p", ["test3"]),
    ?ERROR(Meta, "Error message with meta and params: ~p", ["test4"]),

    %% 清理trace_id
    fw_log:set_trace_id(undefined),
    ?assertEqual(undefined, fw_log:get_trace_id()),

    ok.
