%%% -------------------------------------------------------------------
%%% 作者 : 晨星格拉拉
%%% 描述 : 日志宏定义
%%% 创建 : 2025-12-17
%%% -------------------------------------------------------------------
-ifndef(FW_LOG_HRL).
-define(FW_LOG_HRL, 1).

-define(Loc, #{
    mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
    line => ?LINE,
    file => ?FILE
}).

%% 日志宏 - 调用中间层API，显式传递位置信息
-define(DEBUG(Fmt, Args), fw_log:log(debug, Fmt, Args, ?Loc)).
-define(INFO(Fmt, Args), fw_log:log(info, Fmt, Args, ?Loc)).
-define(NOTICE(Fmt, Args), fw_log:log(notice, Fmt, Args, ?Loc)).
-define(WARN(Fmt, Args), fw_log:log(warning, Fmt, Args, ?Loc)).
-define(ERROR(Fmt, Args), fw_log:log(error, Fmt, Args, ?Loc)).

%% 带元数据的日志宏
-define(DEBUG(Meta, Fmt, Args), fw_log:log(debug, Meta, Fmt, Args, ?Loc)).
-define(INFO(Meta, Fmt, Args), fw_log:log(info, Meta, Fmt, Args, ?Loc)).
-define(NOTICE(Meta, Fmt, Args), fw_log:log(notice, Meta, Fmt, Args, ?Loc)).
-define(WARN(Meta, Fmt, Args), fw_log:log(warning, Meta, Fmt, Args, ?Loc)).
-define(ERROR(Meta, Fmt, Args), fw_log:log(error, Meta, Fmt, Args, ?Loc)).

-endif.
