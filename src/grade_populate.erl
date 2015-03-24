-module(grade_populate).

%% API
-export([add_app/2,
        add_mod/2,
        start/0
        ]).

-export([merge_edge/5]).

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
    Dir             -> xref:add_application(?XREF, Dir)
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
add_fun(Db, ModuleNode, MFA) ->
  FunNode = grade_db:create_node(Db, node_format(MFA)),
  _Edge = grade_db:create_edge(Db, ModuleNode, FunNode, implements),
  %ets:insert(?MODULE, {{M, F, A}, FunNode}),
  FunNode.

%% @doc Add a traced call to the graph
merge_edge(Db, Caller, Relationship, Callee, TraceData) ->
  grade_db:merge_edge( Db
                     , {node_format(Caller)}
                     , Relationship
                     , {node_format(Callee)}
                     , {[{grade_util:as_binary(P), grade_util:as_binary(V)}
                        || {P, V} <- TraceData]}).

%% @private
node_format({M, F, A}) ->
  Plist = [ {name, F}
   , {type, 'fun'}
   , {mfa, <<(grade_util:as_binary(M))/binary
     , ":", (grade_util:as_binary(F))/binary
     , "/", (grade_util:as_binary(A))/binary>>}
  ],
  [{grade_util:as_binary(P), grade_util:as_binary(V)} || {P, V} <- Plist].
