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

  lists:foreach(fun(A) -> populate_app(Neo, A) end, Apps),

  xref:stop(?XREF).


populate_app(Neo, App) ->
  Node = neo4j:create_node(Neo, {[ {<<"name">>, atom_to_binary(App, utf8)}
                                 , {<<"type">>, <<"application">>}]}),
  neo4j:add_node_labels(Node, atom_to_binary(App, utf8)),

  {ok, Modules} = xref:q(?XREF, "(Mod) '" ++ atom_to_list(App) ++ "'"),

  lists:foreach(fun(M) -> populate_module(Neo, Node, M) end, Modules).


populate_module(Neo, App, Module) ->
  Node = neo4j:create_node(Neo, {[{<<"name">>, atom_to_binary(Module, utf8)}]}),
  neo4j:add_node_labels(Node, atom_to_binary(Module, utf8)),

  neo4j:create_relationship(App, Node, <<"contains">>).