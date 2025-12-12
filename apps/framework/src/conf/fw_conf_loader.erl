%%% -------------------------------------------------------------------
%%% 作者  : 晨星格拉拉
%%% 描述 : 从文件加载配置
%%% 创建 : 2023-01-01
%%% -------------------------------------------------------------------
-module(fw_conf_loader).

-export([
    load_file/1,
    load_files/1,
    merge_configs/2,
    get_config_path/0
]).

%% @doc 加载单个配置文件
-spec load_file(File :: string()) -> {ok, Config :: term()} | {error, Reason :: term()}.
load_file(File) ->
    case file:consult(File) of
        {ok, [Config|_]} ->
            {ok, Config};
        {error, Reason} ->
            io:format("加载配置文件 ~p 失败: ~p\n", [File, Reason]),
            {error, Reason}
    end.

%% @doc 加载多个配置文件并合并它们
-spec load_files(Files :: [string()]) -> {ok, MergedConfig :: term()} | {error, Reason :: term()}.
load_files([]) ->
    {ok, []};
load_files([File | Rest]) ->
    case load_file(File) of
        {ok, Config1} ->
            case load_files(Rest) of
                {ok, Config2} ->
                    {ok, merge_configs(Config1, Config2)};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc 合并两个配置
-spec merge_configs(Config1 :: term(), Config2 :: term()) -> MergedConfig :: term().
merge_configs(Config1, Config2) when is_list(Config1), is_list(Config2) ->
    merge_lists(Config1, Config2).

%% @doc 合并两个列表，后面的会覆盖前面的
-spec merge_lists(List1 :: list(), List2 :: list()) -> list().
merge_lists(List1, List2) ->
    lists:foldl(fun
        ({Key, Val}, Acc) when is_tuple(Key) ->
            % 处理特殊的元组键（如 {env, Key}）
            merge_tuple_key(Key, Val, Acc);
        ({Key, Val}, Acc) when is_atom(Key) ->
            % 合并映射或其他值
            case lists:keytake(Key, 1, Acc) of
                {value, {Key, OldVal}, Rest} when is_map(OldVal), is_map(Val) ->
                    [{Key, maps:merge(OldVal, Val)} | Rest];
                {value, {Key, OldVal}, Rest} when is_list(OldVal), is_list(Val) ->
                    % 递归合并列表类型的配置值（如应用配置）
                    [{Key, merge_lists(OldVal, Val)} | Rest];
                {value, {Key, _OldVal}, Rest} ->
                    [{Key, Val} | Rest];
                false ->
                    [{Key, Val} | Acc]
            end;
        (Item, Acc) ->
            % 保留非元组项
            [Item | Acc]
    end, List1, List2).

%% @doc 合并元组键
-spec merge_tuple_key(Key :: tuple(), Val :: term(), Acc :: list()) -> list().
merge_tuple_key(Key, Val, Acc) ->
    case lists:keytake(Key, 1, Acc) of
        {value, {Key, OldVal}, Rest} when is_map(OldVal), is_map(Val) ->
            [{Key, maps:merge(OldVal, Val)} | Rest];
        {value, {Key, _OldVal}, Rest} ->
            [{Key, Val} | Rest];
        false ->
            [{Key, Val} | Acc]
    end.

%% @doc 获取默认配置路径
-spec get_config_path() -> string().
get_config_path() ->
    case file:get_cwd() of
        {ok, Cwd} ->
            filename:join([Cwd, "config"]);
        {error, _} ->
            "config"
    end.
