%%% -------------------------------------------------------------------
%%% 测试文件：动态配置模块生成器测试
%%% -------------------------------------------------------------------
-module(fw_conf_gen_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE_NAME, test_conf_gen_module).
-define(TEST_APP_NAME, test_app).

%% 测试套件 - 为每个测试用例创建独立的测试环境
fw_conf_gen_test_() ->
    [
        {"init_test", ?_test(init_test_impl())},
        {"gen_test", ?_test(gen_test_impl())},
        {"set_test", ?_test(set_test_impl())},
        {"module_reload_test", ?_test(module_reload_test_impl())}
    ].

%% 测试 init/2 函数
init_test_impl() ->
    % 确保测试模块不存在
    code:purge(?TEST_MODULE_NAME),
    code:delete(?TEST_MODULE_NAME),

    try
        % 测试初始化新模块
        ok = fw_conf_gen:init(?TEST_MODULE_NAME, ?TEST_APP_NAME),

        % 验证模块已生成并导出了config_info函数
        ?assert(erlang:function_exported(?TEST_MODULE_NAME, config_info, 1)),
        ?assertEqual(true, ?TEST_MODULE_NAME:config_info(inited)),
        ?assertEqual(?TEST_APP_NAME, ?TEST_MODULE_NAME:config_info(app_name)),
        ?assert(is_list(?TEST_MODULE_NAME:config_info(all_env))),

        % 测试初始化已存在的模块（应该返回ok）
        ok = fw_conf_gen:init(?TEST_MODULE_NAME, ?TEST_APP_NAME)
    after
        % 清理测试模块
        code:purge(?TEST_MODULE_NAME),
        code:delete(?TEST_MODULE_NAME)
    end.

%% 测试 gen/3 函数
gen_test_impl() ->
    % 确保测试模块不存在
    code:purge(?TEST_MODULE_NAME),
    code:delete(?TEST_MODULE_NAME),

    try
        % 设置应用环境
        application:set_env(?TEST_APP_NAME, test_key1, test_value1),
        application:set_env(?TEST_APP_NAME, test_key2, #{nested => test_value2}),

        % 生成配置模块
        {module, ?TEST_MODULE_NAME} = fw_conf_gen:gen(
            ?TEST_MODULE_NAME,
            ?TEST_APP_NAME,
            [{test_key1, test_value1}, {test_key2, #{nested => test_value2}}]
        ),

        % 验证模块已生成并包含配置函数
        ?assert(erlang:function_exported(?TEST_MODULE_NAME, test_key1, 0)),
        ?assert(erlang:function_exported(?TEST_MODULE_NAME, test_key2, 0)),
        ?assertEqual(test_value1, ?TEST_MODULE_NAME:test_key1()),
        ?assertEqual(#{nested => test_value2}, ?TEST_MODULE_NAME:test_key2())
    after
        % 清理测试环境
        application:unset_env(?TEST_APP_NAME, test_key1),
        application:unset_env(?TEST_APP_NAME, test_key2),
        code:purge(?TEST_MODULE_NAME),
        code:delete(?TEST_MODULE_NAME)
    end.

%% 测试 set/4 函数
set_test_impl() ->
    % 确保测试模块不存在
    code:purge(?TEST_MODULE_NAME),
    code:delete(?TEST_MODULE_NAME),

    try
        % 初始化测试模块
        ok = fw_conf_gen:init(?TEST_MODULE_NAME, ?TEST_APP_NAME),

        % 设置新的配置值
        ok = fw_conf_gen:set(?TEST_MODULE_NAME, ?TEST_APP_NAME, new_key, new_value),

        % 验证配置已更新
        ?assert(erlang:function_exported(?TEST_MODULE_NAME, new_key, 0)),
        ?assertEqual(new_value, ?TEST_MODULE_NAME:new_key()),

        % 更新已存在的配置值
        ok = fw_conf_gen:set(?TEST_MODULE_NAME, ?TEST_APP_NAME, new_key, updated_value),
        ?assertEqual(updated_value, ?TEST_MODULE_NAME:new_key()),

        % 测试设置嵌套配置
        ok = fw_conf_gen:set(?TEST_MODULE_NAME, ?TEST_APP_NAME, nested_key, #{a => 1, b => 2}),
        ?assertEqual(#{a => 1, b => 2}, ?TEST_MODULE_NAME:nested_key()),

        % 测试应用名称不匹配的情况
        ?assertError(
            {app_name_not_match, ?TEST_APP_NAME},
            fw_conf_gen:set(?TEST_MODULE_NAME, wrong_app_name, test_key, test_value)
        ),

        % 测试模块未初始化的情况
        code:purge(?TEST_MODULE_NAME),
        code:delete(?TEST_MODULE_NAME),
        ?assertError(
            {app_config_not_inited, ?TEST_APP_NAME},
            fw_conf_gen:set(?TEST_MODULE_NAME, ?TEST_APP_NAME, test_key, test_value)
        )
    after
        % 清理测试环境
        application:unset_env(?TEST_APP_NAME, new_key),
        application:unset_env(?TEST_APP_NAME, nested_key),
        code:purge(?TEST_MODULE_NAME),
        code:delete(?TEST_MODULE_NAME)
    end.

%% 测试模块重新加载
module_reload_test_impl() ->
    % 确保测试模块不存在
    code:purge(?TEST_MODULE_NAME),
    code:delete(?TEST_MODULE_NAME),

    try
        % 初始化测试模块
        ok = fw_conf_gen:init(?TEST_MODULE_NAME, ?TEST_APP_NAME),

        % 验证初始状态
        ?assertNot(erlang:function_exported(?TEST_MODULE_NAME, reload_key, 0)),

        % 重新生成模块，添加新配置
        {module, ?TEST_MODULE_NAME} = fw_conf_gen:gen(
            ?TEST_MODULE_NAME,
            ?TEST_APP_NAME,
            [{reload_key, reload_value}]
        ),

        % 验证模块已更新
        ?assert(erlang:function_exported(?TEST_MODULE_NAME, reload_key, 0)),
        ?assertEqual(reload_value, ?TEST_MODULE_NAME:reload_key())
    after
        % 清理测试环境
        application:unset_env(?TEST_APP_NAME, reload_key),
        code:purge(?TEST_MODULE_NAME),
        code:delete(?TEST_MODULE_NAME)
    end.
