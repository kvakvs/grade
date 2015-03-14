%%%-------------------------------------------------------------------
%%% @doc Provides graph database capabilities and hides the database from caller.
%%% @end
%%%-------------------------------------------------------------------
-module(grade_db).

%% API
-export([new/1, create_node/2]).

-define(BASE_URI, <<"http://localhost:7474/db/data/">>).

%% @doc Creates connection to database or in-memory database
%%      Supports neo4j (external database) and digraph (in-memory)
new(neo4j) ->
  Neo = neo4j:connect([{base_uri, ?BASE_URI}]),
  neo4j:create_node_index(Neo, <<"node_name_index">>, {[ {<<"type">>, <<"exact">>}
                                                       , {<<"provider">>, <<"lucene">>}
                                                       ]}),
  neo4j:create_node_index(Neo, <<"edge_name_index">>, {[ {<<"type">>, <<"exact">>}
                                                       , {<<"provider">>, <<"lucene">>}
                                                       ]}),
  {neo4j, Neo};
new(digraph) ->
  {digraph, digraph:new()}.

%% @doc Creates a node (a proplist) with given {name, N}
create_node(Db={neo4j, Neo}, Node0) ->
  Node = [{grade_util:as_binary(A), grade_util:as_binary(B)} || {A, B} <- Node0],
  neo4j:create_node(Neo, {Node}),
  Db;
create_node({digraph, Dg0}, Node) ->
  Dg = digraph:add_vertex(Dg0, Node, proplists:get_value(name, Node, undefined)),
  {digraph, Dg}.
