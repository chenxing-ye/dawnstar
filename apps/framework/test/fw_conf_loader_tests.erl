%%% -------------------------------------------------------------------
%%% 测试文件：配置加载模块测试
%%% -------------------------------------------------------------------
-module(fw_conf_loader_tests).
-include_lib("eunit/include/eunit.hrl").

%% 测试套件 - 为每个测试用例创建独立的测试环境
fw_conf_loader_test_() ->
    [
        {"load_file_test", ?_test(load_file_test_impl())},
        {"load_files_test", ?_test(load_files_test_impl())},
        {"merge_configs_test", ?_test(merge_configs_test_impl())},
        {"get_config_path_test", ?_test(get_config_path_test_impl())}
    ].

%% 测试 load_file/1 函数
load_file_test_impl() ->
    % 创建临时测试配置文件
    TestConfig = "test_config.erl",
    ConfigContent = "[{framework, [{key1, value1}, {key2, value2}]}].",
    file:write_file(TestConfig, ConfigContent),

    try
        % 测试加载有效配置文件
        {ok, Config} = fw_conf_loader:load_file(TestConfig),
        ?assertEqual([{framework, [{key1, value1}, {key2, value2}]}], Config),
        % 测试加载不存在的配置文件
        ?assertMatch({error, _}, fw_conf_loader:load_file("non_existent_config.erl"))
    after
        % 清理临时测试配置文件
        file:delete(TestConfig)
    end.

%% 测试 load_files/1 函数
load_files_test_impl() ->
    % 创建两个临时测试配置文件
    TestConfig1 = "test_config1.erl",
    TestConfig2 = "test_config2.erl",

    ConfigContent1 = "[{framework, [{key1, value1}, {key2, value2}]}].",
    ConfigContent2 = "[{framework, [{key2, updated_value}, {key3, value3}]}].",

    file:write_file(TestConfig1, ConfigContent1),
    file:write_file(TestConfig2, ConfigContent2),

    try
        % 测试加载多个配置文件
        {ok, MergedConfig} = fw_conf_loader:load_files([TestConfig1, TestConfig2]),
        % 不依赖顺序的断言方式
        [{framework, FrameworkConfig}] = MergedConfig,
        ?assertEqual(3, length(FrameworkConfig)),
        ?assert(lists:member({key1, value1}, FrameworkConfig)),
        ?assert(lists:member({key2, updated_value}, FrameworkConfig)),
        ?assert(lists:member({key3, value3}, FrameworkConfig)),
        % 测试加载空列表
        {ok, EmptyConfig} = fw_conf_loader:load_files([]),
        ?assertEqual([], EmptyConfig),
        % 测试加载包含不存在文件的列表
        ?assertMatch(
            {error, _}, fw_conf_loader:load_files([TestConfig1, "non_existent_config.erl"])
        )
    after
        % 清理临时测试配置文件
        file:delete(TestConfig1),
        file:delete(TestConfig2)
    end.

%% 测试 merge_configs/2 函数
merge_configs_test_impl() ->
    % 测试合并两个列表配置
    Config1 = [{app1, [{key1, value1}, {key2, value2}]}],
    Config2 = [{app1, [{key2, updated_value}, {key3, value3}]}],
    MergedConfig = fw_conf_loader:merge_configs(Config1, Config2),
    % 不依赖顺序的断言方式
    [{app1, AppConfig}] = MergedConfig,
    ?assertEqual(3, length(AppConfig)),
    ?assert(lists:member({key1, value1}, AppConfig)),
    ?assert(lists:member({key2, updated_value}, AppConfig)),
    ?assert(lists:member({key3, value3}, AppConfig)),

    % 测试合并包含映射的配置
    MapConfig1 = [{app1, #{key1 => value1, key2 => value2}}],
    MapConfig2 = [{app1, #{key2 => updated_value, key3 => value3}}],
    MergedMapConfig = fw_conf_loader:merge_configs(MapConfig1, MapConfig2),
    ?assertEqual(
        [{app1, #{key1 => value1, key2 => updated_value, key3 => value3}}], MergedMapConfig
    ),

    % 测试合并空配置
    EmptyConfig1 = [],
    EmptyConfig2 = [{app1, [{key1, value1}]}],
    ?assertEqual(EmptyConfig2, fw_conf_loader:merge_configs(EmptyConfig1, EmptyConfig2)),
    ?assertEqual(EmptyConfig2, fw_conf_loader:merge_configs(EmptyConfig2, EmptyConfig1)).

%% 测试 get_config_path/0 函数
get_config_path_test_impl() ->
    % 测试获取默认配置路径
    ConfigPath = fw_conf_loader:get_config_path(),
    ?assert(is_list(ConfigPath)),
    % 验证路径以 "config" 结尾
    ?assertEqual("config", filename:basename(ConfigPath)).
