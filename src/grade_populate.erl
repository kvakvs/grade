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

  lists:foreach(fun(A) -> populate(Neo, A) end, Apps),

  lists:foreach(fun(A) ->
  xref:stop(?XREF).


populate(Neo, App) ->
  neo4j:create_node(Neo, {[ {<<"name">>, atom_to_binary(App, utf8)}
                          , {<<"type">>, <<"application">>}]}),
  
