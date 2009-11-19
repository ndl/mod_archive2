%%%----------------------------------------------------------------------
%%% File    : ejabberd_storage_odbc.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : ejabberd ODBC storage support
%%% Created : 03 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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

-module(ejabberd_storage_odbc).
-author('ejabberd@ndl.kiev.ua').

-include_lib("exmpp/include/exmpp.hrl").
-include("ejabberd_storage.hrl").

-export([handle_query/2, encode/2, ms_to_sql/2]).

%% Should be OK for most of modern DBs, I hope ...
-define(MAX_QUERY_LENGTH, 32768).

%%--------------------------------------------------------------------
%% Queries interface
%%--------------------------------------------------------------------

handle_query({delete, R}, DbInfo) when is_tuple(R) ->
    TableInfo = ejabberd_storage_utils:get_table_info(R, DbInfo),
    Result =
        sql_query(
            join_non_empty(
                ["delete from",
                 atom_to_list(TableInfo#table.name),
                 "where",
                 encode_keys(R, TableInfo)], " ")),
    convert_change_query_result(Result, deleted, DbInfo);

handle_query({delete, MS}, DbInfo) when is_list(MS) ->
    TableInfo = ejabberd_storage_utils:get_table_info(MS, DbInfo),
    {WhereMS, _BodyMS} =
        ms_to_sql(MS, TableInfo),
    Result =
        sql_query(
            join_non_empty(
                ["delete from",
                 atom_to_list(TableInfo#table.name),
                 get_if_not_empty("where", WhereMS)], " ")),
    convert_change_query_result(Result, deleted, DbInfo);

handle_query({read, R}, DbInfo) ->
    TableInfo = ejabberd_storage_utils:get_table_info(R, DbInfo),
    {selected, _, Rows} =
        sql_query(
            join_non_empty(
                ["select * from",
                 atom_to_list(TableInfo#table.name),
                 "where",
                 encode_keys(R, TableInfo)], " ")),
    Fields = TableInfo#table.fields,
    {selected, convert_rows(Rows,
        {Fields, [{value, TableInfo#table.name} |
            [{field, Field} || Field <- Fields]]},
        TableInfo, undefined)};

handle_query({select, MS, Opts}, DbInfo) ->
    TableInfo = ejabberd_storage_utils:get_table_info(MS, DbInfo),
    {WhereMS, {RequestedFields, _} = BodyMS} = ms_to_sql(MS, TableInfo),
    {OrderBy, OrderType} =
        proplists:get_value(order_by, Opts, {undefined, undefined}),
    Offset = proplists:get_value(offset, Opts, undefined),
    Limit = proplists:get_value(limit, Opts, undefined),
    Aggregate = proplists:get_value(aggregate, Opts, undefined),
    OrderedClause =
        case OrderBy of
            undefined -> "";
            _ ->
                FieldName = lists:nth(OrderBy - 1, TableInfo#table.fields),
                "order by " ++
                atom_to_list(FieldName) ++
                " " ++
                atom_to_list(OrderType)
        end,
    OffsetClause =
        case Offset of
            undefined -> "";
            _ -> "offset " ++ encode(Offset, TableInfo)
        end,
    LimitClause =
        case Limit of
            undefined -> "";
            _ -> "limit " ++ encode(Limit, TableInfo)
        end,
    Where = get_if_not_empty("where", WhereMS),
    Body =
        case Aggregate of
            count ->
                "count(*)";
            {MinMax, Index} when MinMax =:= min orelse MinMax =:= max ->
                atom_to_list(MinMax) ++
                "(" ++
                atom_to_list(lists:nth(Index - 1, TableInfo#table.fields)) ++
                ")";
            _ when RequestedFields =:= TableInfo#table.fields ->
                "*";
            _ ->
                string:join([atom_to_list(Field) ||
                    Field <- RequestedFields], ", ")
        end,
    {selected, _, Rows} =
        sql_query(
            join_non_empty(["select", Body,
                            "from", atom_to_list(TableInfo#table.name),
                            Where, OrderedClause, OffsetClause, LimitClause],
                           " ")),
    {selected, convert_rows(Rows, BodyMS, TableInfo, Aggregate)};

handle_query({update, R}, DbInfo) ->
    TableInfo = ejabberd_storage_utils:get_table_info(R, DbInfo),
    Set = get_update_set_stmt(R, TableInfo),
    Result =
        sql_query(
            join_non_empty(
                ["update",
                 atom_to_list(TableInfo#table.name),
                 "set",
                 Set,
                 "where",
                 encode_keys(R, TableInfo)], " ")),
    convert_change_query_result(Result, updated, DbInfo);

handle_query({update, R, MS}, DbInfo) ->
    TableInfo = ejabberd_storage_utils:get_table_info(R, DbInfo),
    Set = get_update_set_stmt(R, TableInfo),
    {WhereMS, _BodyMS} = ms_to_sql(MS, TableInfo),
    Result =
        sql_query(
            join_non_empty(
                ["update",
                 atom_to_list(TableInfo#table.name),
                 "set",
                 Set,
                 get_if_not_empty("where", WhereMS)], " ")),
    convert_change_query_result(Result, updated, DbInfo);

handle_query({insert, Records}, DbInfo) ->
    % TODO: optimize for those RDBMS that have multiple insert!
    {Count, TableInfo} =
        lists:foldl(
            fun(R, {N, _}) ->
                TableInfo = ejabberd_storage_utils:get_table_info(R, DbInfo),
                Result =
                    sql_query(
                        join_non_empty(
                        ["insert into",
                         atom_to_list(TableInfo#table.name),
                         get_insert_fields(TableInfo),
                         "values",
                         get_insert_values(R, TableInfo)], " ")),
                case Result of
                    {error, _} ->
                        throw(Result);
                    _ ->
                    {N + 1, TableInfo}
                end
            end,
            {0, undefined},
            Records),
    LastKey =
        case lists:member(autoid, TableInfo#table.types) of
            true ->
                case TableInfo#table.rdbms of
                    mysql ->
                        {selected, _, [{ID}]} =
                            sql_query("select LAST_INSERT_ID()"),
                        decode(ID, integer, TableInfo);
                    sqlite ->
                        {selected, _, [{ID}]} =
                            sql_query("select last_insert_rowid()"),
                        decode(ID, integer, TableInfo);
                    pgsql ->
                        {selected, _, [{ID}]} =
                            sql_query("select currval('" ++
                                      atom_to_list(TableInfo#table.name) ++
                                      "_id_seq')"),
                        decode(ID, integer, TableInfo)
                end;
            _ ->
                undefined
        end,
    {inserted, Count, LastKey};

handle_query({sql_query, Query}, _DbInfo) ->
    sql_query(Query);

handle_query({transaction, F}, DbInfo) ->
    case ejabberd_odbc:sql_transaction(DbInfo#storage_backend.host, F) of
        % Work-around ejabberd_odbc buggy error handling, assume
        % that errors are always thrown, not returned.
        {atomic, {error, Error}} ->
            {aborted, {throw, {error, Error}}};
        Result ->
            Result
    end.

%%--------------------------------------------------------------------
%%
%% Matching expressions support in SQL.
%%
%% WARNING! WARNING! WARNING!
%% We support EXTREMELY LIMITED subset of match expressions!
%%
%% Some of the limitations:
%%
%% - Only matching with explicit records specification, and only for those
%%   records that we have schema for is supported.
%%
%% - No repetitions of matching variables in matching head is allowed: use
%%   explicit equality condition.
%%
%% - Only single-clause match expressions are supported: allowing multiple
%%   clauses may lead to specification of different output results in different
%%   clauses, which is not supported by SQL.
%%
%% - The only recognized forms of match expression body are:
%%    * Tuple of matching variables, f.e. '{utc, with_server, with_user}'.
%%    * Record with matching variables, f.e. '#archive_collection{utc = UTC}'
%%      where UTC is bound in fun head.
%%    * Single matching variable that matches the whole record, f.e.
%%      'C' where C is bound in fun head to the whole record.
%%    * 'ok' atom for queries returning no results.
%%   No other forms are allowed: specifically, no variables that are not part
%%%  of tuple, constants, lists or whatever are allowed, as values tuples is the
%%   only way SQL will return us its results.
%%
%% - The set of supported functions and operators is also quite limited: if you
%%   extend it, make sure it is done in portable way across different RDBMS
%%   implementations! (possibly using RDBMS-specific handling code)
%%
%%--------------------------------------------------------------------
ms_to_sql([{MatchHead, MatchConditions, MatchBody}], TableInfo) ->
    % Fill dictionary of matching-variable-to-field-name mapping and head
    % matches, if any.
    % Fail if repetitive use of the same matching variable is detected.
    {MatchVarsDict, HeadMatches, _} =
        lists:foldl(
            fun(MatchItem, {MatchVars, HM, Index}) ->
                FieldName = lists:nth(Index, TableInfo#table.fields),
                case is_match_variable(MatchItem) of
                    true ->
                        error = dict:find(MatchItem, MatchVars),
                        {dict:store(MatchItem, FieldName, MatchVars),
                         HM,
                         Index + 1};
                    _ ->
                        case MatchItem of
                            '_' ->
                                {MatchVars, HM, Index + 1};
                             _ ->
                                {MatchVars,
                                 ["(" ++
                                  encode(FieldName, TableInfo) ++
                                  " = " ++
                                  encode(MatchItem, TableInfo)
                                  ++")"| HM],
                                  Index + 1}
                        end
                end
            end,
            {dict:new(), [], 1},
            lists:nthtail(1, tuple_to_list(MatchHead))),
    % Transform match conditions to SQL syntax.
    Conditions =
        lists:reverse(HeadMatches) ++
        [parse_match_condition(MC, {MatchVarsDict, TableInfo}) ||
         MC <- MatchConditions],
    % Generate the list of fields requested and result body.
    Body =
        case MatchBody of
            ['$_'] ->
                {TableInfo#table.fields,
                 [{value, TableInfo#table.name} |
                    [{field, Field} || Field <- TableInfo#table.fields]]};
            ['ok'] ->
                {[], [{value, ok}]};
            _ ->
                [SingleMBR] = MatchBody,
                MBR = ejabberd_storage_utils:decode_brackets(SingleMBR),
                BodyResults =
                    [parse_match_body(Expr, MatchVarsDict) ||
                        Expr <- tuple_to_list(MBR)],
                RequestFields =
                    [Field || {Type, Field} <- BodyResults, Type =:= field],
                {RequestFields, BodyResults}
        end,
    {string:join(Conditions, " and "), Body}.

% Atoms support: should decide between enums and matching variables.
parse_match_condition(Value, {MatchVarsDict, TableInfo}) when is_atom(Value) ->
    case dict:find(Value, MatchVarsDict) of
        {ok, Field} ->
            atom_to_list(Field);
        _ ->
            encode(Value, TableInfo)
    end;

% Constants support.
parse_match_condition(Value, {_MatchVarsDict, TableInfo})
    when is_list(Value) orelse is_integer(Value) orelse is_float(Value) ->
    encode(Value, TableInfo);

% Variable tuples/records support.
parse_match_condition({element, N, {const, R}}, Context)
    when is_integer(N) andalso is_tuple(R) ->
    parse_match_condition(ejabberd_storage_utils:encode_brackets(element(N, R)),
                          Context);

% Variables support.
parse_match_condition({const, Value}, Context) ->
    parse_match_condition(ejabberd_storage_utils:encode_brackets(Value), Context);

% Record support: currently used for DateTime only.
parse_match_condition({_} = R, {_, TableInfo}) ->
    encode(ejabberd_storage_utils:decode_brackets(R), TableInfo);

% Unary operations support.
parse_match_condition({GuardFun, Op1}, Context) ->
    OpName =
    case GuardFun of
        'not' -> "not";
	    'abs' -> "abs"
    end,
    generate_unary_op(OpName, Op1, Context);

% Binary operations support.
parse_match_condition({GuardFun, Op1, Op2}, Context) ->
    OpName = 
    case GuardFun of
        'and' -> "&";
	    'andalso' -> "and";
	    'or' -> "|";
	    'orelse' -> "or";
        '==' -> "=";
        '=:=' -> "=";
        '<' -> "<";
        '=<' -> "<=";
        '>' -> ">";
        '>=' -> ">=";
        '=/=' -> "<>";
        '/=' -> "<>";
        '+' -> "+";
        '-' -> "-";
        '*' -> "*";
        '/' -> "/"
    end,
    generate_binary_op(OpName, Op1, Op2, Context).

generate_unary_op(OpName, Op1, Context) ->
    OpName ++ "(" ++ parse_match_condition(Op1, Context) ++ ")".

generate_binary_op(OpName, Op1, Op2, Context) ->
    Val1 = parse_match_condition(Op1, Context),
    Val2 = parse_match_condition(Op2, Context),
    FixedOpName =
        case Val2 of
            "null" ->
                case OpName of
                    "=" ->
                        "is";
                    "<>" ->
                        "is not"
                end;
            _ ->
                OpName
        end,
    "(" ++ Val1 ++ " " ++ FixedOpName ++ " " ++ Val2 ++ ")".

parse_match_body(Expr, MatchVarsDict) ->
    case dict:find(Expr, MatchVarsDict) of
        {ok, Field} ->
            {field, Field};
        _ ->
            {value, Expr}
    end.

is_match_variable(Variable) when is_atom(Variable) ->
    VarName = atom_to_list(Variable),
    case catch list_to_integer(string:substr(VarName, 2)) of
        N when is_integer(N) -> [Ch | _] = VarName, Ch =:= $$;
        _ -> false
    end;
is_match_variable(_Variable) ->
    false.

%%--------------------------------------------------------------------
%% Utility functions for database interaction.
%%--------------------------------------------------------------------

%%
%% Wrapper for ejabberd_odbc:sql_query_t for easier debugging.
%%
sql_query(Query) ->
    %%?MYDEBUG("running query: ~p", [lists:flatten(Query)]),
    case catch ejabberd_odbc:sql_query_t(Query) of
        {'EXIT', Error} ->
            %?ERROR_MSG("unhandled exception during query: ~p", [Error]),
            exit(Error);
        {error, Error} ->
            %?ERROR_MSG("error during query: ~p", [Error]),
            throw({error, Error});
        aborted ->
            %?ERROR_MSG("query aborted", []),
            throw(aborted);
        R -> %?MYDEBUG("query result: ~p", [R]),
	    R
    end.

convert_change_query_result(Result, Op, DbInfo) ->
    case DbInfo#storage_backend.rdbms of
        sqlite ->
            case Result of
                {selected, [], []} ->
                    {Op, undefined};
                _ -> Result
            end;
        _ ->
            case Result of
                {updated, Count} when is_integer(Count) ->
                    {Op, Count};
                _ ->
                    Result
            end
    end.

%% Returns joined Prefix and Text if Text is not empty and "" otherwise.
get_if_not_empty(Prefix, Text) ->
    if Text =/= [] -> Prefix ++ " " ++ Text;
       true -> ""
    end.

%% Joins all values that are not empty.
join_non_empty(Values, Sep) ->
    string:join([S || S <- Values, length(S) > 0], Sep).

%%
%% Compose SET statement for UPDATE from all non-undefined fields except
%% key ones: start index is choosen to avoid overwriting keys fields.
%%
get_update_set_stmt(R, TableInfo) ->
    string:join(
        [atom_to_list(lists:nth(N - 1, TableInfo#table.fields)) ++
         " = " ++ encode(Value, TableInfo) ||
         N <- lists:seq(TableInfo#table.keys + 2, tuple_size(R)),
         (Value = element(N, R)) =/= undefined], ", ").

%%
%% Compose FIELDS and VALUES clause for INSERT command based on record fields.
%%

get_insert_fields(TableInfo) ->
    Fields =
        [atom_to_list(Field) || {Type, Field} <- lists:zip(
            TableInfo#table.types,
            TableInfo#table.fields), Type =/= autoid],
    "(" ++ string:join(Fields, ", ") ++ ")".

get_insert_values(R, TableInfo) ->
    Values =
        [encode(Value, TableInfo) ||
            {Type, Value} <- lists:zip(
                TableInfo#table.types,
                lists:nthtail(1, tuple_to_list(R))), Type =/= autoid],
    "(" ++ string:join(Values, ", ") ++ ")".

%%
%% Composes WHERE part of SQL command from record keys.
%%
encode_keys(R, TableInfo) ->
    string:join([atom_to_list(lists:nth(N, TableInfo#table.fields)) ++
                 encode_equality(encode(element(N + 1, R), TableInfo)) ||
                 N <- lists:seq(1, TableInfo#table.keys)],
        " and ").

encode_equality("null") -> " is null";

encode_equality(Value) -> " = " ++ Value.

%%
%% Converts data returned by SQL engine back to their corresponding types.
%%
convert_rows(Rows, BodyMS, TableInfo, Aggregate) ->
    lists:map(
        fun(Row) ->
            Values =
                case Aggregate of
                    undefined ->
                        convert_row(tuple_to_list(Row), BodyMS, TableInfo);
                    count ->
                        {Count} = Row, [decode(Count, integer, TableInfo)];
                    {_, Index} ->
                        {Value} = Row,
                        [decode(Value,
                                lists:nth(Index - 1, TableInfo#table.types),
                                TableInfo)]
                end,
            list_to_tuple(Values)
         end,
         Rows).

convert_row(Values, {RequestedFields, BodyResults}, TableInfo) ->
    [convert_value(Values, RequestedFields, Result, TableInfo) ||
        Result <- BodyResults].

convert_value(Values, RequestedFields, Result, TableInfo) ->
    case Result of
        {field, Field} ->
            TypeIndex =
                ejabberd_storage_utils:elem_index(Field,
                    TableInfo#table.fields),
            ValueIndex =
                ejabberd_storage_utils:elem_index(Field, RequestedFields),
            decode(lists:nth(ValueIndex, Values),
                lists:nth(TypeIndex, TableInfo#table.types), TableInfo);
        {value, Value} ->
            Value
    end.

%%--------------------------------------------------------------------
%%
%% Encoding/decoding for SQL.
%%
%%--------------------------------------------------------------------

%% RDBMS-independent escaping.
encode(null, _) ->
    "null";

encode(undefined, _) ->
    "null";

encode(true, _) ->
    "1";

encode(false, _) ->
    "0";

% TODO: locales?
encode(N, _) when is_integer(N) ->
    integer_to_list(N);

% TODO: locales?
encode(N, _) when is_float(N) ->
    float_to_list(N);

encode({{Year, Month, Day}, {Hour, Minute, Second}}, _) ->
    lists:flatten(
        io_lib:format("'~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w'",
                      [Year, Month, Day, Hour, Minute, Second]));

encode(#xmlel{} = XML, TableInfo) ->
    encode(exmpp_xml:document_to_list(XML), TableInfo);

%% RDBMS-specific escaping.

%% Noone seems to follow standards these days :-(
%% We have to perform DB-specific escaping,as f.e. SQLite does not understand
%% '\' as escaping character (which is exactly in accordance with the standard,
%% by the way), while most other DBs do.
encode(Str, TableInfo) when is_list(Str) andalso
                            TableInfo#table.rdbms =:= sqlite ->
    "'" ++ [escape_char_ansi_sql(C) || C <- Str] ++ "'";

encode(Str, _) when is_list(Str) ->
	lists:flatten("'" ++ ejabberd_odbc:escape(Str) ++ "'");

encode(Value, TableInfo) when is_atom(Value) ->
    case ejabberd_storage_utils:elem_index(Value, TableInfo#table.enums) of
        N when is_integer(N) ->
            integer_to_list(
                ejabberd_storage_utils:elem_index(Value, TableInfo#table.enums) - 1);
        _ -> atom_to_list(Value)
    end.

decode(null, _Type, _TableInfo) ->
    undefined;

decode(undefined, _Type, _TableInfo) ->
    undefined;

decode("null", _Type, _TableInfo) ->
    undefined;

decode(Value, string, _TableInfo) ->
    Value;

decode(Value, integer, _TableInfo) when is_list(Value) ->
    list_to_integer(Value);

decode(Value, integer, _TableInfo) when is_integer(Value) ->
    Value;

decode(Value, autoid, TableInfo) ->
    decode(Value, integer, TableInfo);

decode(Value, bool, TableInfo) ->
    case decode(Value, integer, TableInfo) of
        0 -> false;
        1 -> true
    end;

% Some SQL drivers (currently erlang pgsql only?) do us a favor and parse time
% structure on their own.
decode({{_, _, _}, {_, _, _}} = Value, time, _TableInfo) ->
    Value;

decode(Value, time, _TableInfo) ->
    parse_sql_datetime(Value);

decode(Value, xml, _TableInfo) ->
    case exmpp_xml:parse_document_fragment(Value, [{root_depth, 0}]) of
        [#xmlel{} = R] -> R;
        _ -> undefined
    end;

decode(Value, enum, TableInfo) ->
    lists:nth(decode(Value, integer, TableInfo) + 1, TableInfo#table.enums).

%% Escaping for ANSI SQL compliant RDBMS - so far SQLite only?
escape_char_ansi_sql($')  -> "''";
escape_char_ansi_sql(C)  -> C.

%%
%% Date/time handling, partially copied from jlib and modified to support
%% SQL syntax.
%%
parse_sql_datetime(TimeStr) ->
    [Date, Time] = string:tokens(TimeStr, " "),
    {parse_sql_date(Date), parse_sql_time(Time)}.

%% yyyy-mm-dd
parse_sql_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    {list_to_integer(Y), list_to_integer(M), list_to_integer(D)}.

%% hh:mm:ss[.sss]
parse_sql_time(Time) ->
    [HMS | _] =  string:tokens(Time, "."),
    [H, M, S] = string:tokens(HMS, ":"),
    {list_to_integer(H), list_to_integer(M), list_to_integer(S)}.
