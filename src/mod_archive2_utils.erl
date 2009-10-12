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

-export([elem_index/2, min/1, max/1, get_table_info/2, get_full_ms_head/1,
         get_ms_body/2, resolve_fields_names/2, to_record/3, encode_brackets/1,
         decode_brackets/1, list_to_bool/1]).

%%
%% Missing functionality from lists module: get element index in list.
%%
elem_index(Value, List) -> elem_index(Value, List, 1).

elem_index(_Value, [], _) -> undefined;
elem_index(Value, [Value | _], N) -> N;
elem_index(Value, [_ | Tail], N) -> elem_index(Value, Tail, N + 1).

list_to_bool("false") -> false;
list_to_bool("true") -> true;
list_to_bool("0") -> false;
list_to_bool("1") -> true;
list_to_bool(undefined) -> undefined.

%%
%% Returns table information given record or MS.
%%
get_table_info(TableName, #backend{} = DbInfo) when is_atom(TableName) ->
    lists:keyfind(TableName, #table.name, DbInfo#backend.schema);

get_table_info(R, #backend{} = DbInfo) when is_tuple(R) ->
    lists:keyfind(element(1, R), #table.name, DbInfo#backend.schema);

get_table_info([{MatchHead, _, _}], #backend{} = DbInfo) ->
    lists:keyfind(element(1, MatchHead), #table.name, DbInfo#backend.schema);

get_table_info(TableName, Schema) when is_atom(TableName) andalso
    is_list(Schema) ->
    lists:keyfind(TableName, #table.name, Schema).

%% Generate head for matching specification that matches all fields.
get_full_ms_head(TableInfo) ->
    list_to_tuple(
        [TableInfo#table.name |
         [list_to_atom("$" ++ integer_to_list(Var)) ||
         Var <- lists:seq(1, length(TableInfo#table.fields))]]).

get_ms_body(Fields, TableInfo) ->
    [{list_to_tuple(resolve_fields_names(Fields, TableInfo))}].

resolve_fields_names({Op, Value}, TableInfo) ->
    {Op, resolve_fields_names(Value, TableInfo)};

resolve_fields_names({Op, Value1, Value2}, TableInfo) ->
    {Op, resolve_fields_names(Value1, TableInfo),
     resolve_fields_names(Value2, TableInfo)};

resolve_fields_names(Conditions, TableInfo) when is_list(Conditions) ->
    [resolve_fields_names(C, TableInfo) || C <- Conditions];

resolve_fields_names(Value, TableInfo) when is_atom(Value) ->
    case mod_archive2_utils:elem_index(Value, TableInfo#table.fields) of
        undefined -> Value;
        N when is_integer(N) -> list_to_atom("$" ++ integer_to_list(N))
    end;

resolve_fields_names(Value, _TableInfo) ->
    Value.

to_record(R, Fields, TableInfo) ->
    Values = tuple_to_list(R),
    list_to_tuple(
        [TableInfo#table.name |
         lists:map(
             fun(Index) ->
                 ElemIndex = mod_archive2_utils:elem_index(
                    lists:nth(Index, TableInfo#table.fields),
                    Fields),
                 if ElemIndex =/= undefined -> lists:nth(ElemIndex, Values);
                    true -> undefined
                 end
             end,
             lists:seq(1, length(TableInfo#table.fields)))]).

encode_brackets(R) when is_tuple(R) ->
    {list_to_tuple([encode_brackets(Value) || Value <- tuple_to_list(R)])};

encode_brackets(R) -> R.

decode_brackets({R}) when is_tuple(R) ->
    list_to_tuple([decode_brackets(Value) || Value <- tuple_to_list(R)]);

decode_brackets(R) -> R.

min([]) -> undefined;
min([Value | T]) -> min2(T, Value).

min2([], Value) -> Value;
min2([undefined | T], Min) -> min2(T, Min);
min2([Value | T], undefined) -> min2(T, Value);
min2([Value | T], Min) when Value < Min -> min2(T, Value);
min2([Value | T], Min) when Value >= Min -> min2(T, Min).

max([]) -> undefined;
max([Value | T]) -> max2(T, Value).

max2([], Value) -> Value;
max2([undefined | T], Max) -> max2(T, Max);
max2([Value | T], undefined) -> max2(T, Value);
max2([Value | T], Max) when Value > Max -> max2(T, Value);
max2([Value | T], Max) when Value =< Max -> max2(T, Max).