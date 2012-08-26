%%%----------------------------------------------------------------------
%%% File    : dbms_storage_odbc.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : ejabberd ODBC storage support
%%% Created : 03 Oct 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-module(dbms_storage_odbc).
-author('xmpp@endl.ch').

-include_lib("exmpp/include/exmpp.hrl").
-include("dbms_storage.hrl").

-export([handle_query/2, encode/3, ms_to_sql/2]).

%% Should be OK for most of modern DBs, I hope ...
-define(MAX_QUERY_LENGTH, 32768).

%%--------------------------------------------------------------------
%% Queries interface
%%--------------------------------------------------------------------

handle_query({delete, R}, DbInfo) when is_tuple(R) ->
    TableInfo = dbms_storage_utils:get_table_info(R, DbInfo),
    Result =
        sql_query(
            join_non_empty(
                ["delete from",
                 atom_to_list(TableInfo#table.name),
                 "where",
                 encode_keys(R, TableInfo)], " ")),
    convert_change_query_result(Result, deleted, DbInfo);

handle_query({delete, MS}, DbInfo) when is_list(MS) ->
    TableInfo = dbms_storage_utils:get_table_info(MS, DbInfo),
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
    TableInfo = dbms_storage_utils:get_table_info(R, DbInfo),
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
    TableInfo = dbms_storage_utils:get_table_info(MS, DbInfo),
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
            _ -> "offset " ++ encode(Offset, integer, TableInfo)
        end,
    LimitClause =
        case Limit of
            undefined -> "";
            _ -> "limit " ++ encode(Limit, integer, TableInfo)
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
    TableInfo = dbms_storage_utils:get_table_info(R, DbInfo),
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
    TableInfo = dbms_storage_utils:get_table_info(R, DbInfo),
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
    RDBMS = DbInfo#storage_backend.rdbms,
    {LastTableInfo, LastStmt} =
        lists:foldl(
            fun(R, {PrevTableInfo, Stmt}) ->
                TableInfo = dbms_storage_utils:get_table_info(R, DbInfo),
                Values = get_insert_values(R, TableInfo),
                NewStmt =
                    if Stmt =/= [] andalso
                        (RDBMS =:= sqlite orelse
                         length(Stmt) + length(Values) >
                             ?MAX_QUERY_LENGTH  - 2 orelse
                         TableInfo#table.name =/= PrevTableInfo#table.name) ->
                        sql_query(Stmt), [];
                       true ->
                        Stmt
                    end,
                NewStmt2 =
                    case NewStmt of
                        [] ->
                            join_non_empty(
                                ["insert into",
                                 atom_to_list(TableInfo#table.name),
                                 get_insert_fields(TableInfo),
                                 "values",
                                 Values], " ");
                        _ ->
                            Stmt ++ ", " ++ Values
                    end,
                {TableInfo, NewStmt2}
            end,
            {undefined, []},
            Records),
    case LastStmt of
        [] ->
            ok;
        _ ->
            sql_query(LastStmt)
    end,
    LastKey =
        case lists:member(autoid, LastTableInfo#table.types) of
            true ->
                case RDBMS of
                    mysql ->
                        {selected, _, [{ID}]} =
                            sql_query("select LAST_INSERT_ID()"),
                        decode(ID, integer, LastTableInfo);
                    sqlite ->
                        {selected, _, [{ID}]} =
                            sql_query("select last_insert_rowid()"),
                        decode(ID, integer, LastTableInfo);
                    pgsql ->
                        {selected, _, [{ID}]} =
                            sql_query("select currval('" ++
                                      atom_to_list(LastTableInfo#table.name) ++
                                      "_id_seq')"),
                        decode(ID, integer, LastTableInfo)
                end;
            _ ->
                undefined
        end,
    {inserted, length(Records), LastKey};

handle_query({sql_query, Query}, _DbInfo) ->
    sql_query(lists:flatten(Query));

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
%%    * Tuple of matching variables, f.e. '{UTC, WithServer, WithUser}' were
%%      variables are bound in fun head.
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
                                  atom_to_list(FieldName) ++
                                  " = " ++
                                  encode(MatchItem,
                                         lists:nth(Index, TableInfo#table.types),
                                         TableInfo)
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
        [encode_match_condition(
            annotate_match_condition(MC, {MatchVarsDict, TableInfo}),
            TableInfo) ||
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
                MBR = dbms_storage_utils:decode_brackets(SingleMBR),
                BodyResults =
                    [parse_match_body(Expr, MatchVarsDict) ||
                        Expr <- tuple_to_list(MBR)],
                RequestFields =
                    [Field || {Type, Field} <- BodyResults, Type =:= field],
                {RequestFields, BodyResults}
        end,
    {string:join(Conditions, " and "), Body}.

annotate_match_condition(Value, {MatchVarsDict, TableInfo}) when is_atom(Value) ->
    Field =
        case dict:find(Value, MatchVarsDict) of
            {ok, F} ->
                F;
            _ ->
                case lists:member(Value, TableInfo#table.fields) of
                    false ->
                        undefined;
                    true ->
                        Value
                end
        end,
    case Field of
        undefined ->
            {{value, Value}, undefined};
        _ ->
            {{field, Field},
             lists:nth(dbms_storage_utils:elem_index(Field,
                TableInfo#table.fields), TableInfo#table.types)}
    end;

% Constants support.
annotate_match_condition(Value, _Context) when is_list(Value) ->
    {{value, Value}, string};
annotate_match_condition(Value, _Context) when is_integer(Value) ->
    {{value, Value}, integer};
annotate_match_condition(Value, _Context) when is_float(Value) ->
    {{value, Value}, float};

% Variable tuples/records support.
annotate_match_condition({element, N, {const, R}}, Context)
    when is_integer(N) andalso is_tuple(R) ->
    annotate_match_condition(
        dbms_storage_utils:encode_brackets(element(N, R)), Context);

% Variables support.
annotate_match_condition({const, Value}, Context) ->
    annotate_match_condition(
        dbms_storage_utils:encode_brackets(Value), Context);

% Record support: currently used for DateTime only.
annotate_match_condition({_} = R, _Context) ->
    {{value, dbms_storage_utils:decode_brackets(R)}, time};

% Unary operations support.
annotate_match_condition({GuardFun, Op1}, Context) ->
    VT = annotate_match_condition(Op1, Context),
    {_Value, Type} = VT,
    case GuardFun of
        'not' ->
            if Type =:= bool orelse Type =:= undefined ->
                {{unary, GuardFun, VT}, bool};
               true ->
                erlang:error(badarg)
            end;
        _ ->
        {{unary, GuardFun, VT}, Type}
    end;

% Binary operations support.
annotate_match_condition({GuardFun, Op1, Op2}, Context) ->
    VT1 = annotate_match_condition(Op1, Context),
    {_Value1, Type1} = VT1,
    VT2 = annotate_match_condition(Op2, Context),
    {_Value2, Type2} = VT2,
    {NewVT1, NewVT2, CommonType} =
        case {Type1, Type2} of
            {undefined, undefined} ->
                {VT1, VT2, undefined};
            {undefined, _} ->
                {propagate_type(VT1, Type2), VT2, Type2};
            {_, undefined} ->
                {VT1, propagate_type(VT2, Type1), Type1};
            {CT, CT} ->
                {VT1, VT2, CT};
            {integer, float} ->
                {VT1, VT2, float};
            {float, integer} ->
                {VT1, VT2, float};
            {integer, autoid} ->
                {VT1, VT2, autoid};
            {autoid, integer} ->
                {VT1, VT2, autoid};
            _ ->
                erlang:error(badarg)
        end,
    NewType =
        case lists:member(GuardFun, ['andalso', 'orelse', '==', '=:=', '<',
                                     '=<', '>', '>=', '=/=', '/=']) of
            true -> bool;
            false -> CommonType
        end,
    {{binary, GuardFun, NewVT1, NewVT2}, NewType}.

propagate_type({{OpType, _Value} = Expr, _OldType}, NewType)
    when OpType =:= value orelse OpType =:= field ->
    {Expr, NewType};

propagate_type({{unary, Op, Value}, _OldType}, NewType) ->
    {{unary, Op, propagate_type(Value, NewType)}, NewType};

propagate_type({{binary, Op, Value1, Value2}, _OldType}, NewType) ->
    {{binary, Op, propagate_type(Value1, NewType),
      propagate_type(Value2, NewType)}, NewType}.

encode_match_condition({{field, Field}, _Type}, _TableInfo) ->
    atom_to_list(Field);

encode_match_condition({{value, Value}, Type}, TableInfo) ->
    encode(Value, Type, TableInfo);

% Unary operations support.
encode_match_condition({{unary, GuardFun, Op1}, _Type}, TableInfo) ->
    OpName =
    case GuardFun of
        'not' -> "not";
	    'abs' -> "abs"
    end,
    generate_unary_op(OpName, Op1, TableInfo);

% Binary operations support.
encode_match_condition({{binary, GuardFun, Op1, Op2}, _Type}, TableInfo) ->
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
    generate_binary_op(OpName, Op1, Op2, TableInfo).

generate_unary_op(OpName, Op1, TableInfo) ->
    OpName ++ "(" ++ encode_match_condition(Op1, TableInfo) ++ ")".

generate_binary_op(OpName, Op1, Op2, TableInfo) ->
    Val1 = encode_match_condition(Op1, TableInfo),
    Val2 = encode_match_condition(Op2, TableInfo),
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
    %?MYDEBUG("running query: ~p", [lists:flatten(Query)]),
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
         " = " ++ encode(Value, lists:nth(N - 1, TableInfo#table.types),
                         TableInfo) ||
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
        [encode(Value, Type, TableInfo) ||
            {Type, Value} <- lists:zip(
                TableInfo#table.types,
                lists:nthtail(1, tuple_to_list(R))), Type =/= autoid],
    "(" ++ string:join(Values, ", ") ++ ")".

%%
%% Composes WHERE part of SQL command from record keys.
%%
encode_keys(R, TableInfo) ->
    string:join([atom_to_list(lists:nth(N, TableInfo#table.fields)) ++
                 encode_equality(encode(element(N + 1, R),
                                        lists:nth(N, TableInfo#table.types),
                                        TableInfo)) ||
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
                dbms_storage_utils:elem_index(Field,
                    TableInfo#table.fields),
            ValueIndex =
                dbms_storage_utils:elem_index(Field, RequestedFields),
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
encode(null, _, _) ->
    "null";

encode(undefined, _, _) ->
    "null";

encode(true, bool, _) ->
    "1";

encode(false, bool, _) ->
    "0";

% TODO: locales?
encode(N, autoid, _) ->
    integer_to_list(N);

% TODO: locales?
encode(N, integer, _) ->
    integer_to_list(N);

% TODO: locales?
encode(N, float, _) ->
    float_to_list(N);

encode({{Year, Month, Day}, {Hour, Minute, Second}}, time, _) ->
    lists:flatten(
        io_lib:format("'~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w'",
                      [Year, Month, Day, Hour, Minute, Second]));

encode(Value, {enum, Enums}, _TableInfo) ->
    integer_to_list(dbms_storage_utils:elem_index(Value, Enums) - 1);

encode(#xmlel{} = XML, xml, TableInfo) ->
    encode(exmpp_xml:document_to_list(XML), string, TableInfo);

%% RDBMS-specific escaping.

%% Noone seems to follow standards these days :-(
%% We have to perform DB-specific escaping,as f.e. SQLite does not understand
%% '\' as escaping character (which is exactly in accordance with the standard,
%% by the way), while most other DBs do.
%% What's even more "fun" is that PostgreSQL changed their mind right in flight
%% and started to behave accordingly to the standard since 9.1 version, so we
%% now treat pgsql as standard-conformant as well, although this will break for
%% versions below 9.1 (use 'set standard_conforming_strings = on' to fix that
%% for earlier versions).
encode(Str, string, TableInfo) when
    TableInfo#table.rdbms =:= sqlite orelse
    TableInfo#table.rdbms =:= pgsql ->
    "'" ++ [escape_char_ansi_sql(C) || C <- Str] ++ "'";

encode(Str, string, _) ->
	lists:flatten("'" ++ ejabberd_odbc:escape(Str) ++ "'").

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

decode(Value, {enum, Enums}, TableInfo) ->
    lists:nth(decode(Value, integer, TableInfo) + 1, Enums).

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
