%%%----------------------------------------------------------------------
%%% File    : mod_archive2_storage.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Message Archiving (XEP-136) Storage Support
%%% Created : 27 Sep 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_archive2_storage).
-author('ejabberd@ndl.kiev.ua').

-include("mod_archive2.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%-behaviour(gen_server).

%% gen_server callbacks
-export([init/1]).
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%         terminate/2, code_change/3]).

-export([ms_to_sql/2]).

-record(state, {rdbms, records}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([RDBMS, RecordsInfo]) ->
    {ok, #state{rdbms = RDBMS, records = dict:from_list(RecordsInfo)}}.

%%
%% WARNING! WARNING! WARNING!
%% We support EXTREMELY LIMITED subset of match expressions!
%%
%% Some of the limitations:
%%
%% - Only matching with explicit records specification, and only for those records that are
%%   explicitly registered in init/1 call is supported.
%%
%% - No repetitions of matching variables in matching head is allowed: use explicit equality
%%   condition.
%%
%% - Only single-clause match expressions are supported: allowing multiple clauses may lead to
%%   specification of different output results in different clauses, which is not supported by SQL.
%%
%% - The only recognized form of match expression body is the tuple of matching variables or single
%%   matching variable for the whole record: no variables that are not part of tuple, constants,
%%   lists or whatever are allowed, as tuple of variables is the way SQL will return us its results.
%%
%% - The set of supported functions and operators is also quite limited: if you extend it, make
%%   sure it is done in portable way across different RDBMS implementations! (possibly using
%%   RDBMS-specific handling code)
%%
%% @spec (state(), MS()) -> {Where, Body}
%% @throws ?ERR_INTERNAL_SERVER_ERROR()
ms_to_sql(State, [{MatchHead, MatchConditions, MatchBody}]) ->
    RecordName = element(1, MatchHead),
    % Get record info, which is necessary for proper position-to-field-name mapping.
    RecordInfo =
    case dict:find(RecordName, State#state.records) of
        {ok, Info} -> Info;
        _ -> throw({error, ?ERR_INTERNAL_SERVER_ERROR})
    end,
    % Fill dictionary of matching-variable-to-field-name mapping.
    % Throw if repetitive use of the same matching variable is detected.
    {MatchVarsDict, _} = 
    lists:foldl(
        fun(MatchItem, {MatchVars, Index}) ->
	    case is_match_variable(MatchItem) of
	        true ->
	            case dict:find(MatchItem, MatchVars) of
		        {ok, _} ->
		            throw({error, ?ERR_INTERNAL_SERVER_ERROR});
		        _ ->
  	                    {dict:store(MatchItem, lists:nth(Index, RecordInfo), MatchVars), Index + 1}
		    end;
	        _ -> {MatchVars, Index + 1}
	    end
	end,
	{dict:new(), 0},
	tuple_to_list(MatchHead)),
    % Transform match conditions to SQL syntax.
    Conditions = lists:map(fun(MatchCondition) -> parse_match_condition(MatchCondition, MatchVarsDict) end, MatchConditions),
    % Generate request body: empty if full record is requested, otherwise it's the list of fields.
    Body =
    if MatchBody =:= ['$_'] -> [];
        true ->
	    [{MatchBodyRecord}] = MatchBody,
            lists:map(fun(Expr) -> parse_match_body(Expr, MatchVarsDict) end, tuple_to_list(MatchBodyRecord))
    end,
    {string:join(Conditions, " and "), string:join(Body, ", ")}.

parse_match_condition(Val, MatchVarsDict) when is_atom(Val) ->
    case dict:find(Val, MatchVarsDict) of
        {ok, Field} -> atom_to_list(Field);
        _ -> atom_to_list(Val)
    end;

% TODO: proper escaping !!!
parse_match_condition(Val, _MatchVarsDict) when is_list(Val) ->
    "\"" ++ Val ++ "\"";

parse_match_condition(Val, _MatchVarsDict) when is_integer(Val) ->
    integer_to_list(Val);

parse_match_condition(Val, _MatchVarsDict) when is_float(Val) ->
    float_to_list(Val);
    
parse_match_condition({GuardFun, Op1}, MatchVarsDict) ->
    OpName =
    case GuardFun of
        'not' -> "not";
	'abs' -> "abs";
	_ -> throw({error, ?ERR_INTERNAL_SERVER_ERROR})
    end,
    generate_unary_op(OpName, Op1, MatchVarsDict);

parse_match_condition({GuardFun, Op1, Op2}, MatchVarsDict) ->
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
	_ -> throw({error, ?ERR_INTERNAL_SERVER_ERROR})
    end,
    generate_binary_op(OpName, Op1, Op2, MatchVarsDict).

generate_unary_op(OpName, Op1, MatchVarsDict) ->
    OpName ++ "(" ++ parse_match_condition(Op1, MatchVarsDict) ++ ")".

generate_binary_op(OpName, Op1, Op2, MatchVarsDict) ->
    "(" ++ parse_match_condition(Op1, MatchVarsDict) ++ " " ++ OpName ++ " " ++ parse_match_condition(Op2, MatchVarsDict) ++ ")".

parse_match_body(Expr, MatchVarsDict) ->
    case dict:find(Expr, MatchVarsDict) of
        {ok, Field} -> atom_to_list(Field);
	_ -> throw({error, ?ERR_INTERNAL_SERVER_ERROR})
    end.

is_match_variable(Variable) ->
    case catch list_to_integer(string:substr(atom_to_list(Variable), 2)) of
        N when is_integer(N) -> true;
        _ -> false
    end.
