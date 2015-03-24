-module(grade_populate).

%% API
-export([add_app/2,
        add_mod/2,
        start/0
        ]).

-export([mfa_props/1]).

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
  AppProps = [{name, App}, {type, 'app'}, {desc, App}],
  %grade_db:merge_node(Db, AppProps),

  {ok, Modules} = xref:q(?XREF, "(Mod) '" ++ atom_to_list(App) ++ "'"),
  lists:foreach(
    fun(MFold) ->
      ModNode = add_mod(Db, MFold),
      grade_db:merge_edge(Db, AppProps, app_contains, ModNode, [])
    end, Modules).


%% @doc Query xref about module, returns graph node for module
add_mod(Db0, Module) ->
  io:format("[grade] xref for mod ~s~n", [Module]),
  Props = [{name, Module}, {type, 'mod'}, {desc, Module}],
  grade_db:merge_node(Db0, Props),

  {ok, Funs} = xref:q(?XREF, "'" ++ atom_to_list(Module) ++ "' : Mod * F"),
  lists:foreach(fun(MFA) -> add_fun(Db0, Props, MFA) end, Funs),
  Props.

%% @doc Query xref about function, returns graph node for function
add_fun(Db, ModuleNode, MFA) ->
  grade_db:merge_edge(Db, ModuleNode, implements, mfa_props(MFA), []).
  %ets:insert(?MODULE, {{M, F, A}, FunNode}),

%% @private
mfa_props({M, F, A}) ->
  [ {name, F}
  , {type, 'fun'}
  , {desc, <<(grade_util:as_binary(M))/binary
            , ":", (grade_util:as_binary(F))/binary
            , "/", (grade_util:as_binary(A))/binary>>}
  ].
