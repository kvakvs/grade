%%%-------------------------------------------------------------------
%%% @doc Provides graph database capabilities and hides the database from caller.
%%% @end
%%%-------------------------------------------------------------------
-module(grade_db).

%% API
-export([new/1, create_node/2, create_edge/4, merge_edge/5]).

-define(BASE_URI, <<"http://localhost:7474/db/data/">>).
-define(digraph, grade_db_digraph). % atom tag for digraph storage
-define(neo4j, grade_db_neo4j).     % atom tag for neo4j storage

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
  {?neo4j, Neo};
new(digraph) ->
  {?digraph, digraph:new()}.

%% @doc Creates a node (a proplist) with given {name, N}
%% Returns: new node
create_node({?neo4j, Neo}, Node0) ->
  Node = [{grade_util:as_binary(A), grade_util:as_binary(B)} || {A, B} <- Node0],
  neo4j:create_node(Neo, {Node});
create_node({?digraph, Dg}, Node) ->
  digraph:add_vertex(Dg, Node, proplists:get_value(name, Node, undefined)).

%% @doc Connects nodes with edge. Returns: edge
create_edge({?neo4j, _}, N1, N2, Name) ->
  neo4j:create_relationship(N1, N2, grade_util:as_binary(Name));
create_edge({?digraph, Dg}, N1, N2, Name) ->
  digraph:add_edge(Dg, N1, N2, Name).

merge_edge({?neo4j, Conn}, N1, Rel, N2, Data) ->
  Query = [{<<"MERGE (f1:function {name: {n1}.name, mfa: {n1}.mfa})\n",
              "MERGE (f2:function {name: {n2}.name, mfa: {n2}.mfa})\n",
              "CREATE f1-[:", (grade_util:as_binary(Rel))/binary, " {trace}]->f2">>
           , {[{<<"n1">>, N1}, {<<"n2">>, N2}, {<<"trace">>, Data}]}
           , [<<"REST">>]}],
  io:format("Q: ~p~n", [Query]),
  T = neo4j:transaction_begin(Conn, Query),
  io:format("T: ~p~n", [T]),
  neo4j:transaction_commit(T).
