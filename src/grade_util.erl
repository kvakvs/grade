%%%-------------------------------------------------------------------
%%% @doc Utilities + borrowed stuff where marked
%%% @end
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% Created : 21. Sep 2014
%%%-------------------------------------------------------------------
-module(grade_util).

%% API
-export([unquote/1, as_string/1, as_binary/1, fmt/2, list_nodes_and_apps/0]).

%% @private
%% @doc Unquote a URL encoded string.
%% File: mochiweb_util.erl
%% author: Bob Ippolito <bob@mochimedia.com>
%% copyright: 2007 Mochi Media, Inc.
-spec unquote(string() | binary()) -> string().
unquote(Binary) when is_binary(Binary) ->
  unquote(binary_to_list(Binary));
unquote(String) ->
  qs_revdecode(lists:reverse(String)).

%% @private
%% File: mochiweb_util.erl
%% author: Bob Ippolito <bob@mochimedia.com>
%% copyright: 2007 Mochi Media, Inc.
qs_revdecode(S) -> qs_revdecode(S, []).

-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
  (C >= $a andalso C =< $f) orelse
  (C >= $A andalso C =< $F))).

%% @private
%% File: mochiweb_util.erl
%% author: Bob Ippolito <bob@mochimedia.com>
%% copyright: 2007 Mochi Media, Inc.
qs_revdecode([], Acc) ->
  Acc;
qs_revdecode([$+ | Rest], Acc) ->
  qs_revdecode(Rest, [$\s | Acc]);
qs_revdecode([Lo, Hi, $% | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
  qs_revdecode(Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc]);
qs_revdecode([C | Rest], Acc) ->
  qs_revdecode(Rest, [C | Acc]).

%% @private
%% File: mochiweb_util.erl
%% author: Bob Ippolito <bob@mochimedia.com>
%% copyright: 2007 Mochi Media, Inc.
unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.


as_string(X) when is_list(X) -> X;
as_string(X) when is_binary(X) -> binary_to_list(X);
as_string(X) when is_integer(X) -> integer_to_list(X);
as_string(X) when is_atom(X) -> atom_to_list(X).

as_binary(X) when is_binary(X) -> X;
as_binary(X) when is_list(X) -> list_to_binary(X);
as_binary(X) when is_integer(X) -> as_binary(integer_to_list(X));
as_binary(X) when is_atom(X) -> atom_to_binary(X, utf8).

fmt(Format, Args) ->
  lists:flatten(io_lib:format(Format, Args)).

%% @doc Scans all currently visible nodes and requests apps list from each node
list_nodes_and_apps() ->
  LocalApps0 = application:which_applications(),
  LocalApps = lists:map(fun(AppDescr) -> element(1, AppDescr) end, LocalApps0),
  [{nodes, [{node(), LocalApps}]
           ++ [{N, list_apps_on_node(N)} || N <- nodes()]
   }].

list_apps_on_node(N) ->
  case rpc:call(N, application, which_applications, []) of
    {badrpc, _E} -> [];
    L ->
      %% Returned L contains tuples [{stdlib,"ERTS  CXC 138 10","1.18.3"},...]
      lists:map(fun(AppDescr) -> element(1, AppDescr) end, L)
  end.
