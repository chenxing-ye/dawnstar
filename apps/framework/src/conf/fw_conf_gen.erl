%%% -------------------------------------------------------------------
%%% 作者  : 晨星格拉拉
%%% 描述 : 动态配置模块生成器
%%% 创建 : 2023-01-01
%%% -------------------------------------------------------------------
-module(fw_conf_gen).

-export([
    init/2,
    gen/3,
    make/3,
    set/4
]).

%% @doc 初始化配置模块
-spec init(Tag :: atom(), App :: atom()) -> ok.
init(Tag, App) ->
    try Tag:config_info(inited) of
        true ->
            ok
    catch
        error:undef ->
            % 从应用环境加载配置
            Envs = application:get_all_env(App),
            gen(Tag, App, Envs),
            ok
    end.

%% @doc 生成并加载动态配置模块
-spec gen(Tag :: atom(), App :: atom(), Envs :: list()) -> {module, Tag :: atom()}.
gen(Tag, App, Envs) ->
    Binary = make(Tag, App, Envs),
    code:load_binary(Tag, atom_to_list(Tag), Binary).

%% @doc 生成动态配置模块二进制代码
-spec make(Tag :: atom(), App :: atom(), Envs :: list()) -> binary().
make(Tag, App, Envs) ->
    % 模块头部
    TextH1 = io_lib:format("-module(~w).\n", [Tag]),
    TextH2 = "-compile([export_all]).\n",
    
    % 生成配置函数
    TextCauses = [make_cause(E) || E <- Envs],
    
    % 生成配置信息函数
    TextInfo1 = make_info_kv(inited, true),
    TextInfo2 = make_info_kv(all_env, Envs),
    TextInfo3 = make_info_kv(app_name, App),
    TextInfoEnd = make_info_end(),
    
    % 组合所有代码文本
    Text = lists:flatten([TextH1, TextH2, TextCauses,
        TextInfo1, TextInfo2, TextInfo3, TextInfoEnd]),
    
    % 动态编译
    {_Module, Binary} = dynamic_compile:from_string(Text),
    Binary.

%% @doc 生成配置函数
-spec make_cause({Key :: atom(), Val :: term()}) -> string().
make_cause({Key, Val}) ->
    io_lib:format("~w()->~w.\n", [Key, Val]).

%% @doc 生成配置信息键值对函数
-spec make_info_kv(Key :: atom(), Val :: term()) -> string().
make_info_kv(Key, Val) ->
    io_lib:format("config_info(~w)->~w;~n", [Key, Val]).

%% @doc 生成配置信息函数结束部分
-spec make_info_end() -> string().
make_info_end() ->
    "config_info(X)->erlang:error({no_config_info,X}).\n".

%% @doc 设置配置值并重新生成模块
-spec set(Tag :: atom(), App :: atom(), Key :: atom(), Val :: term()) -> ok.
set(Tag, App, Key, Val) ->
    try Tag:config_info(app_name) of
        App ->
            % 更新应用环境
            application:set_env(App, Key, Val),
            % 获取所有环境变量
            Envs = application:get_all_env(App),
            % 重新生成模块
            gen(Tag, App, Envs),
            ok;
        App1 ->
            erlang:error({app_name_not_match, App1})
    catch
        error:undef ->
            erlang:error({app_config_not_inited, App})
    end.
