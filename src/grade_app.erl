-module(grade_app).
-behaviour(application).

-export([start/2, start/0]).
-export([stop/1]).

start() ->
  case application:start(grade) of
    ok ->
      start_web(),
      ok;
    {error, {already_started, _}} ->
      ok
  end.

start(_StartType, _StartArgs) ->
%%   grade_store:start(),
  grade_sup:start_link().


stop(_State) ->
  ok.

%% @doc Call this after grade application started
start_web() ->
  inets:start(),
  {ok, WebPort}  = application:get_env(grade, port),
  {ok, ConfHost} = application:get_env(grade, host),
  {ok, WebHost}  = inet_parse:address(ConfHost),
  DocRoot = priv_dir(grade),
  SrvRoot = filename:absname(DocRoot ++ "/../"),

  Options = [{modules, [ mod_esi
                       , mod_get]}

            , {port, WebPort}
            , {server_name, "127.0.0.1"}
            , {server_root, SrvRoot}
            , {document_root, DocRoot}
            , {bind_address, WebHost}

            , {erl_script_alias, {"/grade", [grade_web_esi]}}

            , {mime_types, [ {"html", "text/html"}
                           , {"css", "text/css"}
                           , {"js", "application/x-javascript"}
                           ]}
            , {mime_type, "application/octet-stream"}
            ],
  {ok, Pid} = inets:start(httpd, Options),
  case WebPort of
    0 ->
      Info = httpd:info(Pid),
      {port, ListenPort} = proplists:lookup(port, Info),
      application:set_env(grade, port, ListenPort);
    _ -> ok
  end,

  %% Reread selected port (if was set to 0 in app config)
  {ok, SelectedPort} = application:get_env(grade, port),
  io:format("~n[grade] http server started. Visit http://~s:~p/index.html~n"
           , [ConfHost, SelectedPort]).

priv_dir(App) ->
  case code:priv_dir(App) of
    {error, bad_name} ->
      {ok, Cwd} = file:get_cwd(),
      Cwd ++ "/" ++ "priv/";
    Priv ->
      Priv ++ "/"
  end.
