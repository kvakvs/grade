%%% @doc
-module(grade_populate).

%% API
-export([start/1]).

-define(XREF, grade).
-define(BASE_URI, <<"http://localhost:7474/db/data/">>).

start(Dir) ->
  xref:start(?XREF),
  xref:add_release(?XREF, Dir),

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

  xref:stop(?XREF).


populate_app(Neo, App) ->
  io:format("Creating app ~p~n", [App]),
  Node = neo4j:create_node(Neo, {[ {<<"name">>, atom_to_binary(App, utf8)} ]}),
%%   neo4j:add_node_labels(Node, <<"application">>),

  {ok, Modules} = xref:q(?XREF, "(Mod) '" ++ atom_to_list(App) ++ "'"),

  lists:foreach(fun(M) -> populate_module(Neo, Node, M) end, Modules).


populate_module(_, _, alarms_utils) ->
  ok;
populate_module(Neo, App, Module) ->
  io:format("... module ~p~n", [Module]),
  Node = neo4j:create_node(Neo, {[ {<<"name">>, atom_to_binary(Module, utf8)} ]}),
%%   io:format("...   !created~n"),
%%   neo4j:add_node_labels(Node, <<"module">>),
%%   io:format("...   !labels~n"),
  neo4j:create_relationship(App, Node, <<"contains">>),
%%   io:format("...   !contains~n"),

  {ok, Funs} = xref:q(?XREF, "'" ++ atom_to_list(Module) ++ "' : Mod * F"),
  lists:foreach(fun(F) -> populate_functions(Neo, Node, F) end, Funs).


populate_functions(_Neo, alarms_utils, _) ->
  ok;
populate_functions(Neo, Module, {_, F, A}) ->
  FunName = fun_name(F, A),
%%   io:format("...... ~p/~p~n", [F, A]),
  Node = neo4j:create_node(Neo, {[ {<<"name">>, FunName} ]}),
%%   io:format("......   !created~n"),
%%   neo4j:add_node_labels(Node, <<"function">>),
%%   io:format("......   !labels~n"),
  neo4j:create_relationship(Module, Node, <<"implements">>).
%%   io:format("......   !implements~n").



populate_calls(Neo, App) ->
  NameStr = "'" ++ atom_to_list(App) ++ "'",
%%   io:format("CALLS BEFORE"),
  {ok, Calls} = xref:q(?XREF, "E ||| " ++ NameStr),
%%   io:format("CALLS ~p~n", [Calls]),

  lists:foreach(fun(C) -> populate_call(Neo, C) end, Calls).

populate_call(Neo, {{FromM, FromF, FromA}, {ToM, ToF, ToA}}) ->

  GetNode = <<"MATCH (n) -[:implements]->(f) "
              "WHERE n.name={module} AND f.name={fun} "
              "RETURN f">>,

  PropsFrom = get_call_props(FromM, FromF, FromA),
  NodeFrom = data(neo4j:cypher(Neo, GetNode, PropsFrom)),
%%   io:format("NODEFROM ~p~n", [NodeFrom]),

  PropsTo = get_call_props(ToM, ToF, ToA),
  NodeTo = data(neo4j:cypher(Neo, GetNode, PropsTo)),
%%   io:format("NODETO ~p~n", [NodeTo]),

  neo4j:create_relationship(NodeFrom, NodeTo, <<"calls">>).


get_call_props(M, F, A) ->
  {[ {<<"module">>,  atom_to_binary(M, utf8)}
   , {<<"fun">>, fun_name(F, A)}
   ]}.

fun_name(F, A) ->
  AB = list_to_binary(integer_to_list(A)),
  <<(atom_to_binary(F, utf8))/binary, "/", AB/binary>>.

data({D}) ->
  [[N]] = proplists:get_value(<<"data">>, D),
  N.