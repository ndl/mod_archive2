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

-export([handle_query/2]).

-include("mod_archive2_storage.hrl").

-define(SELECT_NOBJECTS, 64).

%%--------------------------------------------------------------------
%% Queries interface
%%--------------------------------------------------------------------

handle_query({transaction, F}, _DbInfo) ->
    mnesia:transaction(F);

handle_query({delete, R}, _DbInfo) when is_tuple(R) ->
    mnesia:delete({element(1, R), element(2, R)}),
    {deleted, 1};

handle_query({delete, MS}, _DbInfo) when is_list(MS) ->
    {deleted,
     delete2(mnesia:select(get_table(MS), MS, ?SELECT_NOBJECTS, write), 0)};

handle_query({read, R}, _DbInfo) ->
    {selected, mnesia:read(element(1, R), element(2, R))};

handle_query({select, MS, Opts}, _DbInfo) ->
    {selected, select(MS, Opts)};

handle_query({update, R}, _DbInfo) ->
    [OldR] = mnesia:read(element(1, R), element(2, R)),
    mnesia:write(update3(OldR, R, tuple_size(R))),
    {updated, 1};

handle_query({update, R, MS}, _DbInfo) ->
    {updated,
     update2(mnesia:select(element(1, R), MS, ?SELECT_NOBJECTS, write), R, 0)};

handle_query({insert, Records}, DbInfo) ->
    {Count, LastKey} =
        lists:foldl(
            fun(R, {N, _}) ->
                TableInfo = mod_archive2_utils:get_table_info(R, DbInfo),
                Key =
                    if element(2, R) =:= undefined andalso
                        TableInfo#table.keys =:= 1 ->
                        {node(), now()};
                       true ->
                        element(2, R)
                    end,
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
%%
%% Implementation is based on this one:
%% http://erlanganswers.com/web/mcedemo/mnesia/OrderedBy.html
%%
%%--------------------------------------------------------------------

select(MS, Opts) ->
    {OrderBy, OrderType} =
        proplists:get_value(order_by, Opts, {undefined, undefined}),
    Offset = proplists:get_value(offset, Opts, undefined),
    Limit = proplists:get_value(limit, Opts, undefined),
    Aggregate = proplists:get_value(aggregate, Opts, undefined),
    FieldPos =
        case OrderBy of
            undefined -> undefined;
            _ -> correct_field_index(MS, OrderBy)
        end,
    SelectOpts = {{Offset, Limit}, {FieldPos, OrderType}, Aggregate},
    select2(
        mnesia:select(get_table(MS), MS, ?SELECT_NOBJECTS, read),
        get_acc(SelectOpts), SelectOpts).

%% Get accumulator that corresponds to query type. If both aggregation and
%% range are present - perform range first, aggregation is done afterwards.
get_acc({Range, _Order, Aggregate}) ->
    case Aggregate of
        _ when Range =/= {undefined, undefined} ->
            {tree, gb_sets:new()};
        undefined ->
            {list, []};
        count ->
            {count, 0};
        {MinMax, _Index} when MinMax =:= min orelse MinMax =:= max ->
            {Aggregate, undefined}
    end.

select2('$end_of_table', Acc, Opts) ->
    finalize_results(Acc, Opts);

select2({Results, Cont}, Acc, Opts) ->
    NewAcc = append_results(Results, Acc, Opts),
    select2(mnesia:select(Cont), NewAcc, Opts).

append_results(Results, {AccType, AccStruct}, {Range, Order, _Aggregate}) ->
    case AccType of
        tree ->
            {tree, append_results_to_tree(Results, AccStruct, Range, Order)};
        list ->
            {list, append_results_to_list(Results, AccStruct)};
        count ->
            {count, update_count(Results, AccStruct)};
        {min, Index} ->
            {AccType, update_minmax(Results, fun min/1, Index, AccStruct)};
        {max, Index} ->
            {AccType, update_minmax(Results, fun max/1, Index, AccStruct)}
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
        undefined -> NewTree;
        TreeLimit -> prune_tree(NewTree, TreeLimit, OrderType)
    end.

append_results_to_list(Results, Acc) -> lists:append(Results, Acc).

update_count(Results, Acc) -> Acc + length(Results).

update_minmax(Results, MinMaxFun, Index, Acc) ->
    Values = [element(Index, Value) || Value <- Results],
    case Acc of
        undefined -> MinMaxFun(Values);
        _ -> MinMaxFun([Acc | Values])
    end.

finalize_results({AccType, AccStruct}, {Range, Order, Aggregate}) ->
    Results =
        case AccType of
            tree -> finalize_tree(AccStruct, Range, Order);
            list -> finalize_list(AccStruct, Order);
            count -> finalize_count(AccStruct, Range);
            {min, _} -> finalize_minmax(AccStruct);
            {max, _} -> finalize_minmax(AccStruct)
        end,
    % Handle delayed aggregation for ranged + aggregated query
    if AccType =:= tree andalso Aggregate =/= undefined ->
        NewOpts = {{undefined, undefined}, Order, Aggregate},
        Acc = get_acc(NewOpts),
        NewAcc = append_results(Results, Acc, NewOpts),
        finalize_results(NewAcc, NewOpts);
       true ->
           Results
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

finalize_list(List, {FieldPos, OrderType} = Order) ->
    case Order of
        {undefined, undefined} -> List;
        _ ->
            lists:sort(
                fun(E1, E2) ->
                    Key1 = element(FieldPos, E1),
                    Key2 = element(FieldPos, E2),
                    case OrderType of
                        asc -> Key1 =< Key2;
                        desc -> Key1 >= Key2
                    end
                end,
                List)
    end.

finalize_count(Count, {_, Limit}) when Count < Limit -> [{Count}];
finalize_count(_, {_, Limit}) -> [{Limit}].

finalize_minmax(MinMax) -> [{MinMax}].

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

get_tree_limit({_, undefined}) -> undefined;
get_tree_limit({undefined, Limit}) -> Limit;
get_tree_limit({Offset, Limit}) -> Offset + Limit.

correct_field_index([{MatchHead, _MatchConditions, MatchBody}], Index) ->
    MatchVar = lists:nth(Index, tuple_to_list(MatchHead)),
    case MatchBody of
        ['$_'] ->
            Index;
	    [{MatchBodyRecord}] ->
            mod_archive2_utils:elem_index(MatchVar,
                                          tuple_to_list(MatchBodyRecord))
    end.

min([]) -> undefined;
min([Value | T]) -> min2(T, Value).

min2([], Value) -> Value;
min2([Value | T], Min) when Value < Min -> min2(T, Value);
min2([Value | T], Min) when Value >= Min -> min2(T, Min).

max([]) -> undefined;
max([Value | T]) -> max2(T, Value).

max2([], Value) -> Value;
max2([Value | T], Max) when Value > Max -> max2(T, Value);
max2([Value | T], Max) when Value =< Max -> max2(T, Max).

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

update2('$end_of_table', _R, Total) -> Total.

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

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------
get_table([{MatchHead, _, _}]) -> element(1, MatchHead).