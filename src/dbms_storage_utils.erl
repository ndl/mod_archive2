%%%----------------------------------------------------------------------
%%% File    : dbms_storage_utils.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : ejabberd storage helper functionality
%%% Created : 04 Oct 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-module(dbms_storage_utils).
-author('xmpp@endl.ch').

-include("dbms_storage.hrl").

-export([elem_index/2, get_table_info/2, get_full_ms_head/1, get_ms_body/2,
         resolve_fields_names/2, encode_brackets/1, decode_brackets/1]).

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

get_table_info(R, #storage_backend{} = DbInfo) when is_tuple(R) ->
    lists:keyfind(element(1, R), #table.name, DbInfo#storage_backend.schema);

get_table_info([{MatchHead, _, _}], #storage_backend{} = DbInfo) ->
    lists:keyfind(element(1, MatchHead), #table.name, DbInfo#storage_backend.schema);

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
    [{list_to_tuple([TableInfo#table.name |
        [case lists:member(Field, Fields) of
            false ->
                undefined;
            true ->
                list_to_atom("$" ++ integer_to_list(
                    elem_index(Field, TableInfo#table.fields)))
        end || Field <- TableInfo#table.fields]])}].

resolve_fields_names({Op, Value}, TableInfo) ->
    {Op, resolve_fields_names(Value, TableInfo)};

resolve_fields_names({Op, Value1, Value2}, TableInfo) ->
    {Op, resolve_fields_names(Value1, TableInfo),
     resolve_fields_names(Value2, TableInfo)};

resolve_fields_names(Conditions, TableInfo) when is_list(Conditions) ->
    [resolve_fields_names(C, TableInfo) || C <- Conditions];

resolve_fields_names(Value, TableInfo) when is_atom(Value) ->
    case elem_index(Value, TableInfo#table.fields) of
        undefined -> Value;
        N when is_integer(N) -> list_to_atom("$" ++ integer_to_list(N))
    end;

resolve_fields_names(Value, _TableInfo) ->
    Value.

encode_brackets(R) when is_tuple(R) ->
    {list_to_tuple([encode_brackets(Value) || Value <- tuple_to_list(R)])};

encode_brackets(R) -> R.

decode_brackets({R}) when is_tuple(R) ->
    list_to_tuple([decode_brackets(Value) || Value <- tuple_to_list(R)]);

decode_brackets(R) -> R.
