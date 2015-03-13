%%%-------------------------------------------------------------------
%%% @doc Erlang Server Interface (ESI) module doing content generation
%%%-------------------------------------------------------------------
-module(grade_web_esi).

%% API
-export([list_apps/3]).

%% @doc Request /grade/grade_web_esi:list_apps
%% Returns JSON list of all running applications on a node
list_apps(Sid, Env, In) -> try_do(fun do_list_apps/3, Sid, Env, In).
do_list_apps(Sid, _Env, _In) ->
  Json = grade_util:list_nodes_and_apps(),
  Resp = jsx:encode(Json),
  mod_esi:deliver(Sid, binary_to_list(Resp)).

%%==============================================================================
%% @private
%% redirect(Sid, Env) ->
%%   H = proplists:get_value(http_host, Env),
%%   mod_esi:deliver(Sid, "status: 302 Found\r\n"
%%                        "Location: http://" ++ H ++ "/index.html\r\n"
%%                        "\r\n"),
%%   Body = io_lib:format("<html>"
%%                        "<script>window.location='/index.html';</script>"
%%                        "</html>", []),
%%   mod_esi:deliver(Sid, Body).

%% @private
try_do(F, Sid, Env, In) ->
  try
    F(Sid, Env, In)
  catch T:E ->
    Stack = erlang:get_stacktrace(),
    io:format("~p~n", [{T, E, Stack}]),
    Json = [{error, "exception in Grade web handler"},
            {what, grade_util:fmt("%s:%p", [T, E])},
            {stack, grade_util:fmt("%p", [Stack])}
           ],
    Resp = jsx:encode(Json),
    mod_esi:deliver(Sid, binary_to_list(Resp))
  end.

%% @private
%% @doc Splits a=b&c=d into [{"a", "b"}, {"c", "d"}], leaves URL encoding intact
%% parse_query_string(Str) ->
%%   Pairs = string:tokens(Str, "&"),
%%   KeyValues0 = lists:map(fun(Pair) -> string:tokens(Pair, "=") end, Pairs),
%%   KeyValues1 = lists:map(fun erlang:list_to_tuple/1, KeyValues0),
%%   [{Key, grade_util:unquote(Value)} || {Key, Value} <- KeyValues1].
