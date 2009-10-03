%%%----------------------------------------------------------------------
%%% File    : mod_archive2_mnesia.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Unified RDBMS and Mnesia Storage Support
%%% Created : 2 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_archive2_mnesia).
-author('ejabberd@ndl.kiev.ua').

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([handle_query/1]).

-define(SELECT_NOBJECTS, 64).

-record(state, {host}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, _Opts]) ->
    {ok, #state{host = Host}}.

handle_call({transaction, F}, _From, State) ->
    {reply, mnesia:transaction(F), State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Queries interface
%%--------------------------------------------------------------------

handle_query({delete, R}) ->
    mnesia:delete({element(1, R), element(2, R)}),
    {deleted, 1};

handle_query({delete, Tab, MS}) ->
    {deleted, delete2(mnesia:select(Tab, MS, ?SELECT_NOBJECTS, write), 0)};

handle_query({read, R}) ->
    {selected, mnesia:read(element(1, R), element(2, R))};

handle_query({select, Tab, MS, Opts}) ->
    {selected, select(Tab, MS, Opts)};

handle_query({update, R}) ->
    OldR = mnesia:read(element(1, R), element(2, R)),
    mnesia:write(update3(OldR, R, tuple_size(R))),
    {updated, 1};

handle_query({update, R, MS}) ->
    {updated,
     update2(mnesia:select(element(1, R), MS, ?SELECT_NOBJECTS, write), R, 0)};

handle_query({insert, Records}) ->
    {Count, LastKey} =
        lists:foldl(
            fun(R, {N, _}) ->
                Key = {node(), now()},
                NewR = setelement(2, R, Key),
                mnesia:write(NewR),
                {N + 1, Key}
            end,
            {0, undefined},
            Records),
    {inserted, Count, LastKey}.

%%--------------------------------------------------------------------
%% 'delete' Mnesia support
%%--------------------------------------------------------------------

delete2({Records, Cont}, Total) ->
    NewTotal =
        lists:foldl(
            fun(R, Count) ->
                mnesia:delete_object(R),
                Count + 1
            end,
            Total,
            Records),
    delete2(mnesia:select(Cont), NewTotal);

delete2('$end_of_table', Total) -> Total.

%%--------------------------------------------------------------------
%% 'select' Mnesia support
%%--------------------------------------------------------------------

select(Tab, MS, Opts) ->
    {OrderBy, OrderType} =
        mod_archive2_utils:get_opt(order_by, Opts, {undefined, undefined}),
    Offset = mod_archive2_utils:get_opt(offset, Opts, undefined),
    Limit = mod_archive2_utils:get_opt(limit, Opts, undefined),
    Aggregate = mod_archive2_utils:get_opt(aggregate, Opts, undefined),
    FieldPos =
        case OrderBy of
            undefined -> undefined;
            _ -> get_field_pos(MS, OrderBy)
        end,
    select2(
        mnesia:select(Tab, MS, ?SELECT_NOBJECTS, read),
        {{Offset, Limit}, {FieldPos, OrderType}, Aggregate}).

%%
%% Implementation is heavily based on this one:
%% http://erlanganswers.com/web/mcedemo/mnesia/OrderedBy.html
%%
select2(SelectOutput, {Range, _, Aggregate} = Opts) ->
    case Aggregate of
        undefined when Range =/= {undefined, undefined} ->
            select3(SelectOutput, {tree, gb_sets:new()}, Opts);
        undefined ->
            select3(SelectOutput, {list, []}, Opts);
        count ->
            select3(SelectOutput, {count, 0}, Opts)
    end.

select3('$end_of_table', Acc, Opts) ->
    finalize_results(Acc, Opts);

select3({Results, Cont}, Acc, Opts) ->
    NewAcc = append_results(Results, Acc, Opts),
    select3(mnesia:select(Cont), NewAcc, Opts).

append_results(Results, {AccType, AccStruct}, {Range, Order, _Aggregate}) ->
    case AccType of
        tree ->
            {tree, append_results_to_tree(Results, AccStruct, Range, Order)};
        list ->
            {list, append_results_to_list(Results, AccStruct)};
        count ->
            {count, update_count(Results, AccStruct)}
    end.

append_results_to_tree(Results, Acc, Range, {FieldPos, OrderType}) ->
    NewTree =
        lists:foldl(fun(Record, AccTree) ->
                        Element = {element(FieldPos, Record), Record},
                        gb_sets:add(Element, AccTree)
                    end,
                    Acc,
                    Results),
    case get_tree_limit(Range) of
        undefined -> Acc;
        TreeLimit -> prune_tree(NewTree, TreeLimit, OrderType)
    end.

append_results_to_list(Results, Acc) -> lists:append(Results, Acc).

update_count(Results, Acc) -> Acc + length(Results).

finalize_results({AccType, AccStruct}, {Range, Order, _Aggregate}) ->
    case AccType of
        tree -> finalize_tree(AccStruct, Range, Order);
        list -> finalize_list(AccStruct, Order);
        count -> finalize_count(AccStruct, Range)
    end.

finalize_tree(Tree, {Offset, Limit}, {_FieldPos, OrderType}) ->
    Records = gb_sets:to_list(Tree),
    SortedRecords =
        case OrderType of
            asc -> Records;
            desc -> lists:reverse(Records)
        end,
    Candidates = [Record || { _, Record } <- SortedRecords],
    {_, Rest} = safe_split(Offset, Candidates),
    {Result, _} = safe_split(Limit, Rest),
    Result.

finalize_list(List, {FieldPos, OrderType}) ->
    lists:sort(
        fun(E1, E2) ->
            Key1 = element(FieldPos, E1),
            Key2 = element(FieldPos, E2),
            case OrderType of
                asc -> Key1 =< Key2;
                desc -> Key1 >= Key2
            end
        end,
        List).

finalize_count(Count, {_, Limit}) when Count < Limit -> Count;
finalize_count(_, {_, Limit}) -> Limit.

prune_tree(Tree, Max, OrderType) ->
    case gb_sets:size(Tree) > Max of
        true ->
            {_, NewTree} =
                case OrderType of
                    asc -> gb_sets:take_largest(Tree);
                    desc -> gb_sets:take_smallest(Tree)
                end,
            prune_tree(NewTree, Max, OrderType);
        false ->
            Tree
    end.

safe_split(N, L) when is_integer(N), length(L) >= N -> lists:split(N, L);
safe_split(N, L) when is_integer(N) -> {L, []};
safe_split(_, L) -> {L, L}.

get_tree_limit({undefined, undefined}) -> undefined;
get_tree_limit({undefined, Limit}) -> Limit;
get_tree_limit({Offset, undefined}) -> Offset;
get_tree_limit({Offset, Limit}) -> Offset + Limit.

get_field_pos([{MatchHead, _MatchConditions, MatchBody}], Index) ->
%    Index = get_index_in_list(Field, RecordInfo),
    MatchVar = lists:nth(Index, tuple_to_list(MatchHead)),
    case MatchBody of
        ['$_'] ->
            Index;
	    [{MatchBodyRec}] ->
            get_index_in_list(MatchVar, tuple_to_list(MatchBodyRec))
    end.

get_index_in_list(Field, Fields) -> get_index_in_list(Field, Fields, 1).

get_index_in_list(N, [ Field | _ ], Field) -> N;
get_index_in_list(N, [ _ | T ], Field) -> get_index_in_list(N + 1, T, Field).

%%--------------------------------------------------------------------
%% 'update' Mnesia support
%%--------------------------------------------------------------------

update2({Results, Cont}, R, Total) ->
    NewTotal =
        lists:foldl(
            fun(OldR, Count) ->
                mnesia:write(update3(OldR, R, tuple_size(R))),
                Count + 1
            end,
            Total,
            Results),
    update2(mnesia:select(Cont), R, NewTotal);

update2('end_of_table', _R, Total) -> Total.

% Do not overwrite key field.
update3(OldR, _R, 2) ->
    OldR;
update3(OldR, R, N) ->
    case element(N, R) of
        undefined ->
            update3(OldR, R, N - 1);
        null ->
            update3(setelement(N, OldR, undefined), R, N - 1);
        Value ->
            update3(setelement(N, OldR, Value), R, N - 1)
    end.