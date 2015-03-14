-module(grade_populate).

%% API
-export([add_app/2,
  add_mod/2,
  start/0
  , add_fun/2]).

-define(XREF, grade).

start() ->
  xref:start(?XREF),
  %%xref:add_release(?XREF, Dir),
  xref:add_directory(?XREF, "."),
  ets:new(?MODULE, [named_table, ordered_set, public]),
  grade_db:new(digraph).


%% @doc Query xref about all modules in app, return modified database with new nodes
add_app(Db0, App) ->
  io:format("[grade] xref for app ~s~n", [App]),
  Db1 = grade_db:create_node(Db0, [{name, App}]),

  {ok, Modules} = xref:q(?XREF, "(Mod) '" ++ atom_to_list(App) ++ "'"),

  %% Remember to return modified Db
  lists:foldl(fun(M, DbFold) -> add_mod(DbFold, M) end, Db1, Modules).


%% @doc Query xref about module, return modified database with new nodes/edges
add_mod(Db0, Module) ->
  io:format("[grade] xref for mod ~s~n", [Module]),
  Db1 = grade_db:create_node(Db0, [{name, Module}]),

  {ok, Funs} = xref:q(?XREF, "'" ++ atom_to_list(Module) ++ "' : Mod * F"),

  %% Remember to return modified Db
  lists:foldl(fun(F, DbFold) -> add_fun(DbFold, F) end, Db1, Funs).


%% @doc Query xref about function, return modified database with new nodes/edges
add_fun(Db0, Fun) ->
  FunName = fun_name(F, A),
  ModName = atom_to_binary(M, utf8),
  Node = neo4j:create_node(Neo, {[ {<<"name">>, FunName}
    , {<<"mfa">>, <<ModName/binary, ":", FunName/binary>>}]}),
  neo4j:create_relationship(Module, Node, <<"implements">>),
  ets:insert(?MODULE, {{M, F, A}, Node}),
  Db.

%% @private
fun_name(F, A) ->
  AB = grade_util:as_binary(A),
  <<(atom_to_binary(F, utf8))/binary, "/", AB/binary>>.
