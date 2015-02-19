%%% @doc
-module(grade_populate).

%% API
-export([start/1]).

-define(XREF, grade).
-define(BASE_URI, <<"http://localhost:7474/db/data/">>).

start(Dir) ->
  xref:start(?XREF),
  xref:add_release(?XREF, Dir),
  ets:new(?MODULE, [named_table, ordered_set, public]),

  {ok, Apps} = xref:q(?XREF, "A"),

  Neo = neo4j:connect([{base_uri, ?BASE_URI}]),
  neo4j:create_node_index(Neo, <<"node_name_index">>, {[ {<<"type">>, <<"exact">>}
                                                       , {<<"provider">>, <<"lucene">>}
                                                       ]}),
  neo4j:create_node_index(Neo, <<"edge_name_index">>, {[ {<<"type">>, <<"exact">>}
                                                       , {<<"provider">>, <<"lucene">>}
                                                       ]}),

  lists:foreach(fun(A) -> populate_app(Neo, A) end, Apps),
  io:format("APPS ~p~n", [Apps]),
  lists:foreach(fun(A) -> populate_calls(Neo, A) end, Apps),

  neo4j:cypher(Neo, <<"MATCH (app)-[:contains]->(mod)-[:implements]->(fun) "
                      "set app: `application`, mod :`module`, fun :`function`"
                    >>),

  xref:stop(?XREF),
  ets:delete(?MODULE).


populate_app(Neo, App) ->
  io:format("Creating app ~p~n", [App]),
  Node = neo4j:create_node(Neo, {[ {<<"name">>, atom_to_binary(App, utf8)} ]}),

  {ok, Modules} = xref:q(?XREF, "(Mod) '" ++ atom_to_list(App) ++ "'"),

  lists:foreach(fun(M) -> populate_module(Neo, Node, M) end, Modules).


populate_module(Neo, App, Module) ->
  io:format("... module ~p~n", [Module]),
  Node = neo4j:create_node(Neo, {[ {<<"name">>, atom_to_binary(Module, utf8)} ]}),
  neo4j:create_relationship(App, Node, <<"contains">>),

  {ok, Funs} = xref:q(?XREF, "'" ++ atom_to_list(Module) ++ "' : Mod * F"),
  lists:foreach(fun(F) -> populate_functions(Neo, Node, F) end, Funs).


populate_functions(Neo, Module, {M, F, A}) ->
  FunName = fun_name(F, A),
  ModName = atom_to_binary(M, utf8),
  Node = neo4j:create_node(Neo, {[ {<<"name">>, FunName}
                                 , {<<"mfa">>, <<ModName/binary, ":", FunName/binary>>}]}),
  neo4j:create_relationship(Module, Node, <<"implements">>),
  ets:insert(?MODULE, {{M, F, A}, Node}).


populate_calls(Neo, App) ->
  NameStr = "'" ++ atom_to_list(App) ++ "'",
  {ok, Calls} = xref:q(?XREF, "E ||| " ++ NameStr),

  lists:foreach(fun(C) -> populate_call(Neo, C) end, Calls).

populate_call(_Neo, {{FromM, FromF, FromA}, {ToM, ToF, ToA}}) ->
  From = ets:lookup(?MODULE, {FromM, FromF, FromA}),
  To = ets:lookup(?MODULE, {ToM, ToF, ToA}),

  case From of
    [] ->
      io:format("Cannot find from: ~p~n", [{FromM, FromF, FromA}]);
    [{_, FromNode}] ->
      case To of
        [] ->
          io:format("Cannot find to: ~p~n", [{ToM, ToF, ToA}]);
        [{_, ToNode}] ->
          neo4j:create_relationship(FromNode, ToNode, <<"calls">>)
      end
  end.


fun_name(F, A) ->
  AB = list_to_binary(integer_to_list(A)),
  <<(atom_to_binary(F, utf8))/binary, "/", AB/binary>>.
