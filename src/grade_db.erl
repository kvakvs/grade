%%%-------------------------------------------------------------------
%%% @doc Provides graph database capabilities and hides the database from caller.
%%% @end
%%%-------------------------------------------------------------------
-module(grade_db).

%% API
-export([new/1, create_node/2, create_edge/4, merge_edge/5, merge_node/2]).

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

merge_node({?neo4j, Conn}, Node) ->
  T = neo4j:transaction_begin(Conn, merge_node_trans(Node)),
  neo4j:transaction_commit(T).

merge_node_trans(Node) ->
  [ {merge_node_query(Node, <<"node">>, <<"f1">>)
  , {[{<<"node">>, {as_binary(Node)}}]}
  , [<<"REST">>]}].

merge_node_query(Node, VarName, NodeVarName) ->
  {type, Type} = lists:keyfind(type, 1, Node),
  <<"MERGE (", NodeVarName/binary, ":", (grade_util:as_binary(Type))/binary,
    " {name: {", VarName/binary, "}.name, type: {", VarName/binary, "}.type, desc: {", VarName/binary, "}.desc})">>.

merge_edge({?neo4j, Conn}, N1, Rel, N2, Data) ->
  Query = [{<<(merge_node_query(N1, <<"n1">>, <<"f1">>))/binary, "\n",
            (merge_node_query(N2, <<"n2">>, <<"f2">>))/binary, "\n",
            "CREATE f1-[:", (grade_util:as_binary(Rel))/binary, " {trace}]->f2">>
           , {[ {<<"n1">>,    {as_binary(N1)}}
              , {<<"n2">>,    {as_binary(N2)}}
              , {<<"trace">>, {as_binary(Data)}}
              ]}
           , [<<"REST">>]}],
  T = neo4j:transaction_begin(Conn, Query),
  neo4j:transaction_commit(T).

as_binary(Props) ->
  [{grade_util:as_binary(P), grade_util:as_binary(V)} || {P, V} <- Props].
