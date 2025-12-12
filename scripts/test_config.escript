#!/usr/bin/env escript
%%! -sname test_config -setcookie dawnstar_cookie

main(_Args) ->
    % 获取当前脚本目录
    ScriptDir = filename:dirname(filename:absname(?FILE)),

    % 获取项目根目录
    ProjectRoot = filename:dirname(ScriptDir),

    % 构建ebin目录路径（rebar3编译产物位于_build/default/lib/下）
    BuildDir = filename:join([ProjectRoot, "_build", "default", "lib"]),
    FrameworkEbinDir = filename:join([BuildDir, "framework", "ebin"]),
    DawnstarEbinDir = filename:join([BuildDir, "dawnstar", "ebin"]),
    DynamicCompileEbinDir = filename:join([BuildDir, "dynamic_compile", "ebin"]),

    io:format("项目根目录: ~s~n", [ProjectRoot]),
    io:format("构建目录: ~s~n", [BuildDir]),
    io:format("Framework Ebin目录: ~s~n", [FrameworkEbinDir]),
    io:format("Dawnstar Ebin目录: ~s~n", [DawnstarEbinDir]),
    io:format("DynamicCompile Ebin目录: ~s~n", [DynamicCompileEbinDir]),

    % 添加代码路径
    case code:add_path(FrameworkEbinDir) of
        true -> io:format("成功添加framework ebin目录到代码路径~n");
        {error, Reason} -> io:format("添加framework ebin目录失败: ~p~n", [Reason])
    end,

    case code:add_path(DawnstarEbinDir) of
        true -> io:format("成功添加dawnstar ebin目录到代码路径~n");
        {error, Reason1} -> io:format("添加dawnstar ebin目录失败: ~p~n", [Reason1])
    end,

    case code:add_path(DynamicCompileEbinDir) of
        true -> io:format("成功添加dynamic_compile ebin目录到代码路径~n");
        {error, Reason2} -> io:format("添加dynamic_compile ebin目录失败: ~p~n", [Reason2])
    end,

    % 确保framework应用已启动
    io:format("=== 启动framework应用 ===~n"),
    case application:start(framework) of
        ok -> io:format("framework应用启动成功~n");
        {error, Reason3} -> io:format("framework应用启动失败: ~p~n", [Reason3])
    end,

    % 初始化配置模块
    io:format("=== 初始化配置模块 ===~n"),
    fw_conf:start(),

    % 测试配置加载器
    io:format("=== 测试配置加载器 ===~n"),
    ConfigPath = "config/test/app.config",
    io:format("加载配置文件: ~s~n", [ConfigPath]),

    case fw_conf_loader:load_file(ConfigPath) of
        {ok, Config} ->
            io:format("配置文件加载成功: ~p~n", [Config]),

            % 测试配置合并功能
            io:format("=== 测试配置合并功能 ===~n"),
            Config1 = [{framework, [{a, 1}, {b, 2}]}],
            Config2 = [{framework, [{b, 3}, {c, 4}]}],
            MergedConfig = fw_conf_loader:merge_configs(Config1, Config2),
            io:format("合并前配置1: ~p~n", [Config1]),
            io:format("合并前配置2: ~p~n", [Config2]),
            io:format("合并后配置: ~p~n", [MergedConfig]),

            % 测试获取配置路径功能
            io:format("=== 测试获取配置路径功能 ===~n"),
            DefaultPath = fw_conf_loader:get_config_path(),
            io:format("默认配置路径: ~s~n", [DefaultPath]),

            % 设置一些配置项
            io:format("=== 设置配置项 ===~n"),
            fw_conf:set(test_key1, test_value1),
            fw_conf:set(test_key2, [{subkey1, value1}, {subkey2, value2}]),
            fw_conf:set(test_key3, #{a => 1, b => 2}),

            % 获取配置项
            io:format("=== 获取配置项 ===~n"),
            Value1 = fw_conf:get(test_key1),
            io:format("test_key1 = ~p~n", [Value1]),

            Value2 = fw_conf:get(test_key2),
            io:format("test_key2 = ~p~n", [Value2]),

            Value3 = fw_conf:get(test_key3),
            io:format("test_key3 = ~p~n", [Value3]),

            % 获取不存在的配置项，使用默认值
            Value4 = fw_conf:get(non_existent_key, default_value),
            io:format("non_existent_key = ~p (使用默认值)~n", [Value4]),

            % 列出所有配置项
            io:format("=== 列出所有配置项 ===~n"),
            AllConfigs = fw_conf:list_all(),
            io:format("所有配置: ~p~n", [AllConfigs]),

            % 重新加载配置
            io:format("=== 重新加载配置 ===~n"),
            fw_conf:reload(),

            % 验证重新加载后配置仍然可用
            Value1AfterReload = fw_conf:get(test_key1),
            io:format("重新加载后 test_key1 = ~p~n", [Value1AfterReload]),

            % 测试fw_conf_gen模块功能
            io:format("=== 直接测试fw_conf_gen模块 ===~n"),
            % 测试模块生成功能
            TestModuleName = test_config_gen_module,
            TestEnvs = [{test_key_a, 123}, {test_key_b, "test_value"}, {test_key_c, [1, 2, 3]}],
            io:format("生成测试模块: ~w~n", [TestModuleName]),
            io:format("测试环境变量: ~p~n", [TestEnvs]),

            % 初始化测试模块
            fw_conf_gen:init(TestModuleName, framework),

            % 打印生成的模块信息
            io:format("生成的模块 ~p 信息:~n", [TestModuleName]),
            try TestModuleName:module_info(exports) of
                Exports ->
                    io:format("  导出函数: ~p~n", [Exports])
            catch
                _:Reason3a -> io:format("  无法获取模块信息: ~p~n", [Reason3a])
            end,

            % 调用生成的模块函数
            try TestModuleName:config_info(inited) of
                true -> io:format("测试模块初始化成功~n")
            catch
                _:Reason4 -> io:format("测试模块初始化失败: ~p~n", [Reason4])
            end,

            % 测试动态设置配置
            fw_conf_gen:set(TestModuleName, framework, test_key_d, new_value),
            try TestModuleName:test_key_d() of
                new_value -> io:format("测试模块动态设置配置成功~n")
            catch
                _:Reason5 -> io:format("测试模块动态设置配置失败: ~p~n", [Reason5])
            end,

            % 列出所有配置
            try TestModuleName:config_info(all_env) of
                AllEnv when is_list(AllEnv) ->
                    io:format("测试模块列出所有配置成功，共 ~w 项~n", [length(AllEnv)]);
                _ ->
                    io:format("测试模块列出所有配置失败~n")
            catch
                _:Reason6 -> io:format("测试模块列出所有配置失败: ~p~n", [Reason6])
            end,

            % 测试从文件重新加载配置功能
            io:format("=== 测试从文件重新加载配置功能 ===~n"),
            TestConfigFile = "config/test/app.config",
            io:format("从文件 ~s 重新加载配置~n", [TestConfigFile]),

            % 先设置一个自定义配置项
            fw_conf:set(test_key_from_file, custom_value),
            io:format("重新加载前自定义配置项: ~p~n", [fw_conf:get(test_key_from_file, not_set)]),

            % 重新加载配置文件
            fw_conf:reload(TestConfigFile),

            % 打印重新加载后的配置
            io:format("重新加载后配置:~n"),
            io:format("  test_key1 = ~p~n", [fw_conf:get(test_key1)]),
            io:format("  test_key2 = ~p~n", [fw_conf:get(test_key2)]),
            io:format("  test_key3 = ~p~n", [fw_conf:get(test_key3)]),

            % 验证是否从文件加载了配置
            try fw_conf:get(test_key1) of
                "from_config_file" -> io:format("成功从文件加载配置项 test_key1~n");
                Other -> io:format("配置项 test_key1 加载失败，实际值: ~p~n", [Other])
            catch
                _:Reason7 -> io:format("从文件加载配置失败: ~p~n", [Reason7])
            end,

            % 验证自定义配置项是否被覆盖
            io:format("重新加载后自定义配置项: ~p~n", [fw_conf:get(test_key_from_file, not_set)]),

            io:format("=== 配置管理模块测试完成 ===~n");
        {error, Reason8} ->
            io:format("配置文件加载失败: ~p~n", [Reason8]),
            io:format("=== 配置管理模块测试完成 (部分测试失败) ===~n")
    end.
