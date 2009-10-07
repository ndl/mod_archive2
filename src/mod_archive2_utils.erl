%%%----------------------------------------------------------------------
%%% File    : mod_archive2_utils.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 helper functionality
%%% Created : 04 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%%
%%% mod_archive2, Copyright (C) 2009 Alexander Tsvyashchenko
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
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
%% Returns table information given record or MS.
%%
get_table_info(R, DbInfo) when is_tuple(R) ->
    lists:keyfind(element(1, R), #table.name, DbInfo#backend.schema);

get_table_info([{MatchHead, _, _}], DbInfo) ->
    lists:keyfind(element(1, MatchHead), #table.name, DbInfo#backend.schema).
