%%% -------------------------------------------------------------------
%%% 测试文件：配置管理模块测试
%%% -------------------------------------------------------------------
-module(fw_conf_tests).
-include_lib("eunit/include/eunit.hrl").

-define(CONFIG_MODULE, fw_conf_value).
-define(APP_NAME, framework).

%% 测试套件设置
setup() ->
    % 确保应用已经启动
    application:start(sasl),
    application:start(?APP_NAME),
    % 设置一些测试配置
    application:set_env(?APP_NAME, test_key1, test_value1),
    application:set_env(?APP_NAME, test_key2, #{nested => test_value2}),
    % 初始化配置模块
    fw_conf:start().

teardown(_) ->
    % 清理测试环境（兼容旧的测试方式）
    application:stop(?APP_NAME),
    application:stop(sasl).

%% 每个测试用例的独立 setup 函数
setup_test() ->
    % 确保应用已经启动
    application:start(sasl),
    case application:start(?APP_NAME) of
        ok -> ok;
        {error, {already_started, ?APP_NAME}} -> ok
    end,
    % 设置一些测试配置
    application:set_env(?APP_NAME, test_key1, test_value1),
    application:set_env(?APP_NAME, test_key2, #{nested => test_value2}),
    % 初始化配置模块（如果尚未初始化）
    try fw_conf:start() of
        ok -> ok
    catch
        _:_ -> ok
    end,
    % 强制重新加载配置，确保我们的测试配置被正确加载
    fw_conf:reload().

%% 每个测试用例的独立 teardown 函数
teardown_test() ->
    % 清理测试环境
    application:stop(?APP_NAME),
    application:stop(sasl).

%% 测试套件 - 为每个测试用例创建独立的测试环境
fw_conf_test_() ->
    [
     {"get_test", ?_test(get_test_impl())},
     {"get_with_default_test", ?_test(get_with_default_test_impl())},
     {"set_test", ?_test(set_test_impl())},
     {"reload_test", ?_test(reload_test_impl())},
     {"list_all_test", ?_test(list_all_test_impl())},
     {"config_info_test", ?_test(config_info_test_impl())}
    ].

%% 每个测试用例的独立实现

%% 测试 get/1 函数
get_test_impl() ->
    setup_test(),
    try
        % 测试存在的配置项
        ?assertEqual(test_value1, fw_conf:get(test_key1)),
        ?assertEqual(#{nested => test_value2}, fw_conf:get(test_key2)),
        % 测试不存在的配置项，应该抛出错误
        ?assertError({missing_config, non_existent_key}, fw_conf:get(non_existent_key))
    after
        teardown_test()
    end.

%% 测试 get/2 函数
get_with_default_test_impl() ->
    setup_test(),
    try
        % 测试存在的配置项，应该返回配置值而不是默认值
        ?assertEqual(test_value1, fw_conf:get(test_key1, default_value)),
        % 测试不存在的配置项，应该返回默认值
        ?assertEqual(default_value, fw_conf:get(non_existent_key, default_value))
    after
        teardown_test()
    end.

%% 测试 set/2 函数
set_test_impl() ->
    setup_test(),
    try
        % 设置新的配置值
        fw_conf:set(new_key, new_value),
        ?assertEqual(new_value, fw_conf:get(new_key)),
        % 更新已存在的配置值
        fw_conf:set(test_key1, updated_value),
        ?assertEqual(updated_value, fw_conf:get(test_key1))
    after
        teardown_test()
    end.

%% 测试 reload/0 函数
reload_test_impl() ->
    setup_test(),
    try
        % 修改应用环境配置
        application:set_env(?APP_NAME, test_key1, reloaded_value),
        application:set_env(?APP_NAME, reload_key, reload_value),
        % 重新加载配置
        fw_conf:reload(),
        % 验证配置是否已更新
        ?assertEqual(reloaded_value, fw_conf:get(test_key1)),
        ?assertEqual(reload_value, fw_conf:get(reload_key))
    after
        teardown_test()
    end.

%% 测试 list_all/0 函数
list_all_test_impl() ->
    setup_test(),
    try
        % 获取所有配置
        AllConfigs = fw_conf:list_all(),
        % 验证测试配置是否在列表中
        ?assert(lists:keymember(test_key1, 1, AllConfigs)),
        ?assert(lists:keymember(test_key2, 1, AllConfigs))
    after
        teardown_test()
    end.

%% 测试 config_info/1 函数
config_info_test_impl() ->
    setup_test(),
    try
        % 这个函数的行为依赖于 fw_conf_gen 的实现，这里只测试基本功能
        Info = fw_conf:config_info(all_env),
        ?assert(is_list(Info) orelse is_map(Info))
    after
        teardown_test()
    end.