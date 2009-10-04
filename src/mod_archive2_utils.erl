%%%----------------------------------------------------------------------
%%% File    : mod_archive2_utils.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Helper common functions
%%% Created : 4 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_archive2_utils).
-author('ejabberd@ndl.kiev.ua').

-include("mod_archive2_storage.hrl").

-export([elem_index/2, get_table_info/2]).

%%
%% Missing functionality from lists module: get element index in list.
%%
elem_index(Value, List) -> elem_index(Value, List, 1).

elem_index(_Value, [], _) -> undefined;
elem_index(Value, [Value | _], N) -> N;
elem_index(Value, [_ | Tail], N) -> elem_index(Value, Tail, N + 1).

%%
%% Returns table information given table, record or MS.
%%
get_table_info(Table, DbInfo) when is_atom(Table) ->
    lists:keyfind(Table, #table.name, DbInfo#backend.schema);

get_table_info(R, DbInfo) when is_tuple(R) ->
    lists:keyfind(element(1, R), #table.name, DbInfo#backend.schema);

get_table_info([{MatchHead, _, _}], DbInfo) ->
    lists:keyfind(element(1, MatchHead), #table.name, DbInfo#backend.schema).
