%%%----------------------------------------------------------------------
%%% File    : mod_archive2_storage.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Unified RDBMS and Mnesia Storage Support
%%% Created : 27 Sep 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_archive2_odbc).
-author('ejabberd@ndl.kiev.ua').

-include("mod_archive2_storage.hrl").

-export([handle_query/2, ms_to_sql/2]).

%% Should be OK for most of modern DBs, I hope ...
-define(MAX_QUERY_LENGTH, 32768).

%%--------------------------------------------------------------------
%% Queries interface
%%--------------------------------------------------------------------

handle_query({delete, R}, DbInfo) ->
    TableInfo = get_table_info(R, DbInfo),
    {updated, Count} =
        sql_query(
            string:join(
                ["delete from",
                 atom_to_list(TableInfo#table.name),
                 "where",
                 encode_keys(R, TableInfo)], " ")),
    {deleted, Count};

handle_query({delete, Table, MS}, DbInfo) ->
    TableInfo = get_table_info(Table, DbInfo),
    {WhereMS, _BodyMS} =
        ms_to_sql(MS, TableInfo),
    {updated, Count} =
        sql_query(
            string:join(
                ["delete from",
                 atom_to_list(Table),
                 "where",
                 WhereMS], " ")),
    {deleted, Count};

handle_query({read, R}, DbInfo) ->
    TableInfo = get_table_info(R, DbInfo),
    {selected, _, Rows} =
        sql_query(
            string:join(
                ["select * from",
                 atom_to_list(TableInfo#table.name),
                 "where",
                 encode_keys(R, TableInfo)], " ")),
    {selected, convert_rows(Rows, [], TableInfo)};

handle_query({select, Table, MS, Opts}, DbInfo) ->
    TableInfo = get_table_info(Table, DbInfo),
    {WhereMS, BodyMS} = ms_to_sql(MS, TableInfo),
    {OrderBy, OrderType} =
        proplists:get_value(order_by, Opts, {undefined, undefined}),
    Offset = proplists:get_value(offset, Opts, undefined),
    Limit = proplists:get_value(limit, Opts, undefined),
    Aggregate = proplists:get_value(aggregate, Opts, undefined),
    WhereOrdered =
        case OrderBy of
            undefined -> "";
            _ ->
                FieldName = lists:nth(OrderBy, TableInfo#table.fields),
                string:join(
                    ["order by",
                     atom_to_list(FieldName),
                     ",",
                     atom_to_list(OrderType)], " ")
        end,
    WhereOffset =
        case Offset of
            undefined -> "";
            _ -> "offset " ++ encode(Offset, TableInfo)
        end,
    WhereLimit =
        case Limit of
            undefined -> "";
            _ -> "limit " ++ encode(Limit, TableInfo)
        end,
    Where = string:join([WhereMS, WhereOrdered, WhereOffset, WhereLimit], " "),
    Body =
        case Aggregate of
            count -> "count(*)";
            _ when BodyMS =:= [] -> "*";
            _ -> BodyMS
        end,
    {selected, _, Rows} =
        sql_query(
            string:join(
                ["select",
                 Body,
                 "from",
                 atom_to_list(Table),
                 "where",
                 Where], " ")
        ),
    {selected, convert_rows(Rows, BodyMS, TableInfo)};

handle_query({update, R}, DbInfo) ->
    TableInfo = get_table_info(R, DbInfo),
    Set = get_update_set_stmt(R, TableInfo),
    {updated, Count} =
        sql_query(
            string:join(
                ["update",
                 atom_to_list(TableInfo#table.name),
                 "where",
                 encode_keys(R, TableInfo),
                 "set",
                 Set], " ")),
    {updated, Count};

handle_query({update, R, MS}, DbInfo) ->
    TableInfo = get_table_info(R, DbInfo),
    Set = get_update_set_stmt(R, TableInfo),
    {WhereMS, _BodyMS} = ms_to_sql(MS, TableInfo),
    {updated, Count} =
        sql_query(
            string:join(
                ["update",
                 TableInfo#table.name,
                 "where",
                 WhereMS,
                 "set",
                 Set], " ")),
    {updated, Count};

handle_query({insert, Records}, DbInfo) ->
    % TODO: optimize for those RDBMS that have multiple insert!
    {Count, TableInfo} =
        lists:foldl(
            fun(R, {N, _}) ->
                TableInfo = get_table_info(R, DbInfo),
                {updated, _} =
                    sql_query(
                        string:join(
                        ["insert into ",
                         atom_to_list(TableInfo#table.name),
                         "values",
                         get_record_values(R, TableInfo)], " ")),
                {N + 1, TableInfo}
            end,
            {0, undefined},
            Records),
    LastKey =
        case TableInfo#table.rdbms of
            "mysql" ->
                {selected, _, [{ID}]} =
                    sql_query(["select LAST_INSERT_ID()"]),
                decode(ID, integer, TableInfo);
            "sqlite" ->
                {selected, _, [{ID}]} =
                    sql_query(["select last_insert_rowid()"]),
    		    decode(ID, integer, TableInfo);
	        "pgsql" ->
                {selected, _, [{ID}]} =
                    sql_query(["select currval('",
                               atom_to_list(TableInfo#table.name),
                               "_id_seq')"]),
                decode(ID, integer, TableInfo)
        end,
    {inserted, Count, LastKey};

handle_query({transaction, F}, DbInfo) ->
    ejabberd_odbc:sql_transaction(DbInfo#backend.host, F).

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
%%   records that are explicitly registered in init/1 call is supported.
%%
%% - No repetitions of matching variables in matching head is allowed: use
%%   explicit equality condition.
%%
%% - Only single-clause match expressions are supported: allowing multiple
%%   clauses may lead to specification of different output results in different
%%   clauses, which is not supported by SQL.
%%
%% - The only recognized form of match expression body is the tuple of matching
%%   variables or single matching variable for the whole record: no variables
%%   that are not part of tuple, constants, lists or whatever are allowed, as
%%   tuple of variables is the way SQL will return us its results.
%%
%% - The set of supported functions and operators is also quite limited: if you
%%   extend it, make sure it is done in portable way across different RDBMS
%%   implementations! (possibly using RDBMS-specific handling code)
%%
%%--------------------------------------------------------------------
ms_to_sql([{MatchHead, MatchConditions, MatchBody}], TableInfo) ->
    % Fill dictionary of matching-variable-to-field-name mapping.
    % Throw if repetitive use of the same matching variable is detected.
    {MatchVarsDict, _} = 
    lists:foldl(
        fun(MatchItem, {MatchVars, Index}) ->
	    case is_match_variable(MatchItem) of
	        true ->
	            case dict:find(MatchItem, MatchVars) of
		        {ok, _} ->
		            throw({error, badmatchexpr});
		        _ ->
  	                {dict:store(MatchItem,
                        lists:nth(Index, TableInfo#table.fields),
                        MatchVars),
                     Index + 1}
		    end;
	        _ -> {MatchVars, Index + 1}
	    end
	end,
	{dict:new(), 0},
	tuple_to_list(MatchHead)),
    % Transform match conditions to SQL syntax.
    Conditions = lists:map(
        fun(MatchCondition) ->
                parse_match_condition(
                    MatchCondition,
                    {MatchVarsDict, TableInfo})
        end, MatchConditions),
    % Generate request body: empty if full record is requested, otherwise it's the list of fields.
    Body =
    if MatchBody =:= ['$_'] -> [];
        true ->
	    [{MatchBodyRecord}] = MatchBody,
            lists:map(
                fun(Expr) ->
                        parse_match_body(Expr, MatchVarsDict)
                end, tuple_to_list(MatchBodyRecord))
    end,
    {string:join(Conditions, " and "), string:join(Body, ", ")}.

parse_match_condition(Value, {MatchVarsDict, TableInfo}) when is_atom(Value) ->
    case dict:find(Value, MatchVarsDict) of
        {ok, Field} ->
            atom_to_list(Field);
        _ ->
            encode(Value, TableInfo)
    end;

parse_match_condition(Value, {_MatchVarsDict, TableInfo})
    when is_list(Value) orelse is_integer(Value) orelse is_float(Value) ->
    encode(Value, TableInfo);

parse_match_condition({GuardFun, Op1}, Context) ->
    OpName =
    case GuardFun of
        'not' -> "not";
	'abs' -> "abs";
	_ -> throw({error, badmatchexpr})
    end,
    generate_unary_op(OpName, Op1, Context);

parse_match_condition({GuardFun, Op1, Op2}, Context) ->
    OpName = 
    case GuardFun of
        'and' -> "and";
	'andalso' -> "and";
	'or' -> "or";
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
	'/' -> "/";
	_ -> throw({error, badmatchexpr})
    end,
    generate_binary_op(OpName, Op1, Op2, Context).

generate_unary_op(OpName, Op1, Context) ->
    OpName ++ "(" ++ parse_match_condition(Op1, Context) ++ ")".

generate_binary_op(OpName, Op1, Op2, Context) ->
    "(" ++
        parse_match_condition(Op1, Context) ++
        " " ++ OpName ++ " " ++
        parse_match_condition(Op2, Context) ++ ")".

parse_match_body(Expr, MatchVarsDict) ->
    case dict:find(Expr, MatchVarsDict) of
        {ok, Field} -> atom_to_list(Field);
	    _ -> throw({error, badmatchexpr})
    end.

is_match_variable(Variable) ->
    case catch list_to_integer(string:substr(atom_to_list(Variable), 2)) of
        N when is_integer(N) -> true;
        _ -> false
    end.

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

%%
%% Compose SET statement for UPDATE from all non-undefined fields except
%% key ones.
%%
get_update_set_stmt(R, TableInfo) ->
    string:join(get_update_set_stmt(R, TableInfo, tuple_size(R), []), ", ").

% Do not overwrite key field.
get_update_set_stmt(_R, TableInfo, N, Stmts) when N =:= TableInfo#table.keys + 1->
    Stmts;

get_update_set_stmt(R, TableInfo, N, Stmts) ->
    NewStmts =
        case element(N, R) of
            undefined ->
                Stmts;
            Value ->
                [atom_to_list(lists:nth(N, TableInfo#table.fields)) ++
                 " = " ++
                 encode(Value, TableInfo) | Stmts]
        end,
    get_update_set_stmt(R, TableInfo, N - 1, NewStmts).

%%
%% Compose VALUES clause for INSERT command based on record fields.
%%
get_record_values(R, TableInfo) ->
    Values =
        lists:map(
            fun(Value) ->
                encode(Value, TableInfo)
            end,
            lists:nthtail(1, tuple_to_list(R))),
    "(" ++ string:join(Values, ", ") ++ ")".

%%
%% Composes WHERE part of SQL command from record keys.
%%
encode_keys(R, TableInfo) ->
    string:join([atom_to_list(lists:nth(N, TableInfo)) ++
                 " = " ++
                 encode(element(2, R), TableInfo) ||
                 N <- lists:seq(1, TableInfo#table.keys)],
        " and ").

%%
%% Converts data returned by SQL engine back to their corresponding types.
%%
convert_rows(Rows, BodyMS, TableInfo) ->
    lists:map(
         fun(Row) ->
             Values =
                convert_row(tuple_to_list(Row),
                            BodyMS,
                            [],
                            TableInfo),
                list_to_tuple(
                    if BodyMS =:= TableInfo#table.fields ->
                        TableInfo#table.name ++ Values;
                       true ->
                        Values
                    end)
         end,
         Rows).

convert_row([], [], Row, _TableInfo) ->
    lists:reverse(Row);

convert_row([Value | VT], [Field | FT], Row, TableInfo) ->
    Index = elem_index(Field, TableInfo#table.fields),
    NewRow = [decode(Value, lists:nth(Index, TableInfo#table.types), TableInfo) |
              Row],
    convert_row(VT, FT, NewRow, TableInfo).

%%
%% Returns table information given table or record.
%%
get_table_info(Table, DbInfo) when is_atom(Table) ->
    {ok, TableInfo} = dict:find(Table, DbInfo#backend.schema),
    TableInfo;

get_table_info(R, DbInfo) ->
    {ok, TableInfo} = dict:find(element(1, R), DbInfo#backend.schema),
    TableInfo.


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
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [Year, Month, Day, Hour, Minute, Second]);

%% RDBMS-specific escaping.

%% Noone seems to follow standards these days :-(
%% We have to perform DB-specific escaping,as f.e. SQLite does not understand
%% '\' as escaping character (which is exactly in accordance with the standard,
%% by the way), while most other DBs do.
encode(Str, TableInfo) when is_list(Str) andalso TableInfo#table.rdbms =:= sqlite ->
    "'" ++ [escape_char_ansi_sql(C) || C <- Str] ++ "'";

encode(Str, _) when is_list(Str) ->
	"'" ++ ejabberd_odbc:escape(Str) ++ "'";

encode(Value, TableInfo) when is_atom(Value) ->
    integer_to_list(elem_index(Value, TableInfo#table.enums)).

decode(null, _Type, _TableInfo) ->
    undefined;

decode("null", _Type, _TableInfo) ->
    undefined;

decode(Value, string, _TableInfo) ->
    Value;

decode(Value, integer, _TableInfo) when is_list(Value) ->
    list_to_integer(Value);

decode(Value, integer, _TableInfo) when is_integer(Value) ->
    Value;

decode(Value, bool, TableInfo) ->
    case decode(Value, integer, TableInfo) of
        0 -> false;
        1 -> true
    end;

decode(Value, time, _TableInfo) ->
    parse_sql_datetime(Value);

decode(Value, enum, TableInfo) ->
    lists:nth(decode(Value, integer, TableInfo), TableInfo#table.enums).

%%
%% Missing functionality from lists module: get element index in list.
%%
elem_index(Value, List) -> elem_index(Value, List, 1).

elem_index(_Value, [], _) -> undefined;
elem_index(Value, [Value | _], N) -> N;
elem_index(Value, [_ | Tail], N) -> elem_index(Value, Tail, N + 1).

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