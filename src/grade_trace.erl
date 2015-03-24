-module(grade_trace).

-record(state, { current_mfa, db, trace_id }).

-export([ consume/2
        , start_trace/1
        , stop_trace/0
        , apply/2
        , apply/4
        , do_apply/3
        ]).

-type target() :: {file, string()}.

%% @doc Start tracing
-spec start_trace(target()) -> ok.
start_trace({file, Filename}) ->
  PortFun = dbg:trace_port(file, Filename),
  erlang:trace_pattern({'_', '_', '_'}, true, [local]),
  erlang:trace(all, true, [ call, return_to, timestamp, {tracer, PortFun()} ]),
  ok.

-spec stop_trace() -> ok.
stop_trace() ->
  erlang:trace(all, false, [all]),
  dbg:flush_trace_port(),
  ok.

-spec apply(target(), function()) -> any().
apply(Target, Fun) ->
  Parent = self(),
  spawn_link(?MODULE, do_apply, [Target, Parent, Fun]),
  receive Result ->
      Result
  end.

-spec apply(target(), atom(), atom(), list()) -> any().
apply(Target, M, F, Args) ->
  ?MODULE:apply(Target, fun() ->
                    erlang:apply(M, F, Args)
                end).

-spec consume(target(), any()) -> pid().
consume({file, Filename}, Db) ->
  TraceId = grade_util:fmt("~p ~p ~p", [node(), now(), Filename]),
  State   = #state{ current_mfa = {?MODULE, do_apply, 3}
                  , db = Db
                  , trace_id = TraceId
                  },
  dbg:trace_client(file, Filename, {fun handler_fun/2, State}).





do_apply(Target, Parent, Fun) ->
  start_trace(Target),
  try
    Result = Fun(),
    Parent ! Result
  catch E:R ->
      stop_trace(),
      io:format("ARRRRRGH! ~p:~p", [E, R]),
      erlang:exit({E, R})
  after
    stop_trace()
  end.

handler_fun({trace_ts, Pid, call, {M, F, Args}, Timestamp}, State) ->
  Arity = erlang:length(Args),
  ArgsStr = grade_util:fmt("~p", [Args]),
  merge_edge(Pid, Timestamp, State, calls, M, F, Arity, [{args, ArgsStr}]),
%  io:format("call ~p~n", [{M, F, Arity}]),
  State#state{current_mfa = {M, F, Arity}};
handler_fun({trace_ts, Pid, return_to, {M, F, Arity}, Timestamp}, State) ->
  merge_edge(Pid, Timestamp, State, returns, M, F, Arity, []),
%  io:format("call ~p~n", [{M, F, Arity}]),
  State#state{current_mfa = {M, F, Arity}};
handler_fun(end_of_trace, State) ->
  State.

merge_edge(Pid, {Ms, S, Us}, State, Relationship, M, F, Arity, Props) ->
  Timestamp = Ms * 1000000 * 1000000 + S * 1000000 + Us,
  Data  = [ {pid, Pid}
          , {timestamp, Timestamp}
          , {trace_id, State#state.trace_id}
            | Props
          ],
  grade_db:merge_edge(State#state.db,
                      grade_populate:mfa_props(State#state.current_mfa),
                      Relationship,
                      grade_populate:mfa_props({M, F, Arity}),
                      Data).
