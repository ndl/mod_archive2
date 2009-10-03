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

-include("mod_archive2.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([handle_query/2]).

-record(state, {host, records}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, _Opts]) ->
    {ok, #state{host = Host}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{host = _Host}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Queries interface
%%--------------------------------------------------------------------

handle_query({delete, R}, {RDBMS, RecordsInfo}) ->
    {updated, Count} =
        sql_query(
            string:join(
                ["delete from",
                 atom_to_list(element(1, R)),
                 "where",
                 atom_to_list(get_field(1, R, RecordsInfo)),
                 "=",
                 escape(RDBMS, element(2, R))], " ")),
    {deleted, Count};

handle_query({delete, Tab, MS}, Info) ->
    {WhereMS, _BodyMS} = ms_to_sql(Info, MS),
    {updated, Count} =
        sql_query(
            string:join(
                ["delete from",
                 atom_to_list(Tab),
                 "where",
                 WhereMS], " ")),
    {deleted, Count};

handle_query({read, R}, {RDBMS, RecordsInfo}) ->
    Table = get_field(1, R, RecordsInfo),
    {selected, _, Rows} =
        sql_query(
            string:join(
                ["select * from",
                 atom_to_list(element(1, R)),
                 "where",
                 atom_to_list(Table),
                 "=",
                 escape(RDBMS, element(2, R))], " ")),
    {selected, convert_rows(Rows, Table, [])};

handle_query({select, Tab, MS, Opts}, {RDBMS, RecordsInfo} = Info) ->
    {WhereMS, BodyMS} = ms_to_sql(Info, MS),
    {OrderBy, OrderType} =
        proplists:get_value(order_by, Opts, {undefined, undefined}),
    Offset = proplists:get_value(offset, Opts, undefined),
    Limit = proplists:get_value(limit, Opts, undefined),
    Aggregate = proplists:get_value(aggregate, Opts, undefined),
    WhereOrdered =
        case OrderBy of
            undefined -> "";
            _ ->
                FieldName = get_field(OrderBy, Tab, RecordsInfo),
                string:join(
                    ["order by",
                     atom_to_list(FieldName),
                     ",",
                     atom_to_list(OrderType)], " ")
        end,
    WhereOffset =
        case Offset of
            undefined -> "";
            _ -> "offset " ++ escape(RDBMS, Offset)
        end,
    WhereLimit =
        case Limit of
            undefined -> "";
            _ -> "limit " ++ escape(RDBMS, Limit)
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
                 atom_to_list(Tab),
                 "where",
                 Where], " ")
        ),
    {selected, convert_rows(Rows, Tab, BodyMS)};

handle_query({update, R}, {RDBMS, RecordsInfo} = Info) ->
    Set = get_update_set_stmt(R, Info),
    {updated, Count} =
        sql_query(
            string:join(
                ["update",
                 atom_to_list(element(1, R)),
                 "where",
                 atom_to_list(get_field(1, R, RecordsInfo)),
                 "=",
                 escape(RDBMS, element(2, R)),
                 "set",
                 Set], " ")),
    {updated, Count};

handle_query({update, R, MS}, Info) ->
    Set = get_update_set_stmt(R, Info),
    {WhereMS, _BodyMS} = ms_to_sql(Info, MS),
    {updated, Count} =
        sql_query(
            string:join(
                ["update",
                 atom_to_list(element(1, R)),
                 "where",
                 WhereMS,
                 "set",
                 Set], " ")),
    {updated, Count};

handle_query({insert, Records}, {RDBMS, _RecordsInfo}) ->
    % TODO: optimize for those RDBMS that have multiple insert!
    {Count, Table} =
        lists:foldl(
            fun(R, {N, _}) ->
                Table = element(1, R),
                {updated, _} =
                    sql_query(
                        string:join(
                        ["insert into ",
                         atom_to_list(Table),
                         "values",
                         get_record_values(R, RDBMS)], " ")),
                {N + 1, Table}
            end,
            {0, undefined},
            Records),
    LastKey =
        case RDBMS of
            "mysql" ->
                {selected, _, [{ID}]} =
                    sql_query(["select LAST_INSERT_ID()"]),
                to_integer(ID);
            "sqlite" ->
                {selected, _, [{ID}]} =
                    sql_query(["select last_insert_rowid()"]),
    		    to_integer(ID);
	        "pgsql" ->
                {selected, _, [{ID}]} =
                    sql_query(["select currval('", atom_to_list(Table), "_id_seq')"]),
                to_integer(ID)
        end,
    {inserted, Count, LastKey}.

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
%% @spec (state(), MS()) -> {Where, Body}
%% @throws ?ERR_INTERNAL_SERVER_ERROR()
ms_to_sql({RDBMS, RecordsInfo}, [{MatchHead, MatchConditions, MatchBody}]) ->
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
                                get_field(Index, MatchHead, RecordsInfo),
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
                parse_match_condition(RDBMS, MatchCondition, MatchVarsDict)
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

parse_match_condition(_RDBMS, Val, MatchVarsDict) when is_atom(Val) ->
    case dict:find(Val, MatchVarsDict) of
        {ok, Field} -> atom_to_list(Field);
        _ -> atom_to_list(Val)
    end;

parse_match_condition(RDBMS, Val, _MatchVarsDict)
    when is_list(Val) orelse is_integer(Val) orelse is_float(Val) ->
    escape(RDBMS, Val);

parse_match_condition(RDBMS, {GuardFun, Op1}, MatchVarsDict) ->
    OpName =
    case GuardFun of
        'not' -> "not";
	'abs' -> "abs";
	_ -> throw({error, badmatchexpr})
    end,
    generate_unary_op(RDBMS, OpName, Op1, MatchVarsDict);

parse_match_condition(RDBMS, {GuardFun, Op1, Op2}, MatchVarsDict) ->
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
    generate_binary_op(RDBMS, OpName, Op1, Op2, MatchVarsDict).

generate_unary_op(RDBMS, OpName, Op1, MatchVarsDict) ->
    OpName ++ "(" ++ parse_match_condition(RDBMS, Op1, MatchVarsDict) ++ ")".

generate_binary_op(RDBMS, OpName, Op1, Op2, MatchVarsDict) ->
    "(" ++
        parse_match_condition(RDBMS, Op1, MatchVarsDict) ++
        " " ++ OpName ++ " " ++
        parse_match_condition(RDBMS, Op2, MatchVarsDict) ++ ")".

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
%% Utility functions to make database interaction easier.
%%--------------------------------------------------------------------
sql_query(Query) ->
    %%?MYDEBUG("running query: ~p", [lists:flatten(Query)]),
    case catch ejabberd_odbc:sql_query_t(Query) of
        {'EXIT', Err} ->
            ?ERROR_MSG("unhandled exception during query: ~p", [Err]),
            exit(Err);
        {error, Err} ->
            ?ERROR_MSG("error during query: ~p", [Err]),
            throw({error, Err});
        aborted ->
            ?ERROR_MSG("query aborted", []),
            throw(aborted);
        R -> %?MYDEBUG("query result: ~p", [R]),
	    R
    end.

get_field(Index, Table, RecordsInfo) when is_atom(Table) ->
    {ok, Info} = dict:find(Table, RecordsInfo),
    lists:nth(Index, Info);

get_field(Index, R, RecordsInfo) when is_atom(R) ->
    get_field(Index, element(1, R), RecordsInfo).

get_update_set_stmt(R, Info) ->
    string:join(get_update_set_stmt(R, Info, tuple_size(R), []), ", ").

get_update_set_stmt(R, {RDBMS, _RecordsInfo} = Info, N, Stmts) ->
    NewStmts =
        case element(N, R) of
            undefined ->
                Stmts;
         Value ->
                [string:join(
                    [atom_to_list(get_field(N, R, Info)),
                     "=",
                     escape(RDBMS, Value)], " ") | Stmts]
        end,
    get_update_set_stmt(R, Info, N - 1, NewStmts);

% Do not overwrite key field.
get_update_set_stmt(_R, _Info, 2, Stmts) ->
    Stmts.

get_record_values(R, RDBMS) ->
    Values =
        lists:map(
            fun(Value) ->
                escape(RDBMS, Value)
            end,
            lists:nthtail(1, tuple_to_list(R))),
    "(" ++ string:join(Values, ", ") ++ ")".

convert_rows(Rows, Table, BodyMS) ->
    case BodyMS of
        [] ->
            lists:map(
                fun(Row) ->
                    list_to_tuple(Table ++ tuple_to_list(Row))
                end,
            Rows);
        _ ->
            Rows
    end.

%% Noone seems to follow standards these days :-(
%% We have to perform DB-specific escaping,as f.e. SQLite does not understand
%% '\' as escaping character (which is exactly in accordance with the standard,
%% by the way), while most other DBs do.

%% RDBMS-independent escaping.
escape(_, null) ->
    "null";
escape(_, undefined) ->
    "null";
escape(_, infinity) ->
    integer_to_list(?INFINITY);
% TODO: locales?
escape(_, N) when is_integer(N) ->
    integer_to_list(N);
% TODO: locales?
escape(_, N) when is_float(N) ->
    float_to_list(N);

%% RDBMS-specific escaping.
escape(sqlite, Str) when is_list(Str) ->
    "'" ++ [escape_char_ansi_sql(C) || C <- Str] ++ "'";
escape(_, Str) when is_list(Str) ->
	"'" ++ ejabberd_odbc:escape(Str) ++ "'";
escape(_, _) ->
    throw({error, badarg}).

%% Escaping for ANSI SQL compliant RDBMS - so far SQLite only?
escape_char_ansi_sql($')  -> "''";
escape_char_ansi_sql(C)  -> C.

to_integer(N) when is_integer(N) ->
    N;
to_integer(null) ->
    undefined;
to_integer(Str) ->
    list_to_integer(Str).
