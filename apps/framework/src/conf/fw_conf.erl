%%% -------------------------------------------------------------------
%%% 作者  : 晨星格拉拉
%%% 描述 : 配置管理API
%%% 创建 : 2023-01-01
%%% -------------------------------------------------------------------
-module(fw_conf).

-dialyzer({nowarn_function, [list_all/0, config_info/1]}).

-export([
    start/0,
    get/1,
    get/2,
    set/2,
    reload/0,
    reload/1,
    list_all/0,
    config_info/1
]).

-define(CONFIG_MODULE, fw_conf_value).
-define(APP_NAME, framework).

%% @doc 启动配置服务
-spec start() -> ok.
start() ->
    fw_conf_gen:init(?CONFIG_MODULE, ?APP_NAME).

%% @doc 获取配置值，如果不存在则抛出错误
-spec get(Key :: atom()) -> term().
get(Key) ->
    try (?CONFIG_MODULE):Key()
    catch
        error:undef ->
            erlang:error({missing_config, Key});
        Error:Reason ->
            erlang:error({config_error, Key, Error, Reason})
    end.

%% @doc 获取配置值，如果不存在则返回默认值
-spec get(Key :: atom(), Default :: term()) -> term().
get(Key, Default) ->
    try (?CONFIG_MODULE):Key()
    catch
        _:_ -> Default
    end.

%% @doc 设置配置值
-spec set(Key :: atom(), Value :: term()) -> ok.
set(Key, Value) ->
    fw_conf_gen:set(?CONFIG_MODULE, ?APP_NAME, Key, Value).

%% @doc 从应用环境重新加载配置
-spec reload() -> ok.
reload() ->
    Envs = application:get_all_env(?APP_NAME),
    fw_conf_gen:gen(?CONFIG_MODULE, ?APP_NAME, Envs),
    ok.

%% @doc 从文件重新加载配置
-spec reload(File :: string()) -> ok.
reload(File) ->
    case fw_conf_loader:load_file(File) of
        {ok, Config} ->
            % 提取framework应用的配置
            case lists:keyfind(?APP_NAME, 1, Config) of
                {?APP_NAME, AppConfig} ->
                    % 更新应用环境
                    lists:foreach(
                        fun({Key, Value}) ->
                            application:set_env(?APP_NAME, Key, Value)
                        end,
                        AppConfig
                    ),
                    % 重新生成动态配置模块
                    fw_conf_gen:gen(?CONFIG_MODULE, ?APP_NAME, AppConfig),
                    ok;
                false ->
                    % 配置文件中没有framework应用的配置
                    io:format("配置文件 ~p 中没有 ~p 应用的配置\n", [File, ?APP_NAME]),
                    ok
            end;
        {error, Reason} ->
            io:format("从文件 ~p 加载配置失败: ~p\n", [File, Reason]),
            ok
    end.

%% @doc 列出所有配置值
-spec list_all() -> [{Key :: atom(), Value :: term()}].
list_all() ->
    try (?CONFIG_MODULE):config_info(all_env)
    catch
        _:_ -> []
    end.

%% @doc 获取配置信息
-spec config_info(Key :: atom()) -> term().
config_info(Key) ->
    try (?CONFIG_MODULE):config_info(Key)
    catch
        _:_ -> undefined
    end.
