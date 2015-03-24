-module(grade_populate).

%% API
-export([add_app/2,
        add_mod/2,
        start/0
        ]).

-define(XREF, grade).

%% @doc NOTE: Digraph creates ets tables, which will be owned by current process and
%% be destroyed if you crash (exception in shell for example) or end the process.
start() ->
  xref:start(?XREF),
  %%xref:add_release(?XREF, Dir),
  xref:add_application(?XREF, "ebin"),
  ets:new(?MODULE, [named_table, ordered_set, public]),
  grade_db:new(digraph).


%% @doc Query xref about all modules in app, return modified database with new nodes
add_app(Db, App) ->
  io:format("[grade] xref for app ~s~n", [App]),
  case code:lib_dir(App) of
    {error, Reason} -> io:format("~p", [{error, Reason}]);
    Dir             ->
      xref:add_application(?XREF, Dir)
  end,
  io:format("[grade] xref for app ~s~n", [App]),
  AppNode = grade_db:create_node(Db, [{name, App}, {type, 'app'}]),

  {ok, Modules} = xref:q(?XREF, "(Mod) '" ++ atom_to_list(App) ++ "'"),
  lists:foreach(
    fun(MFold) ->
      ModNode = add_mod(Db, MFold),
      grade_db:create_edge(Db, AppNode, ModNode, app_contains)
    end, Modules).


%% @doc Query xref about module, returns graph node for module
add_mod(Db0, Module) ->
  io:format("[grade] xref for mod ~s~n", [Module]),
  ModuleNode = grade_db:create_node(Db0, [{name, Module}, {type, 'mod'}]),

  {ok, Funs} = xref:q(?XREF, "'" ++ atom_to_list(Module) ++ "' : Mod * F"),
  lists:foreach(fun(MFA) -> add_fun(Db0, ModuleNode, MFA) end, Funs),
  ModuleNode.


%% @doc Query xref about function, returns graph node for function
add_fun(Db, ModuleNode, {M, F, A}) ->
  FunName = fun_name(M, F, A),
  ModName = grade_util:as_binary(M),
  FunNode = grade_db:create_node(Db, [{name, FunName},
                                      {type, 'fun'},
                                      {mfa, <<ModName/binary, ":", FunName/binary>>}
                                     ]),
  _Edge = grade_db:create_edge(Db, ModuleNode, FunNode, implements),
  ets:insert(?MODULE, {{M, F, A}, FunNode}),
  FunNode.

%% @private
fun_name(_M, F, A) ->
  F1 = grade_util:as_binary(F),
  A1 = grade_util:as_binary(A),
  <<F1/binary, "/", A1/binary>>.
