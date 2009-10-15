%%%----------------------------------------------------------------------
%%% File    : mod_archive2_management.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 collections management
%%% Created : 07 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%%
%%% mod_archive2, Copyright (C) 2009 Alexander Tsvyashchenko
%%%
%%% Based on earlier works by:
%%%  - Olivier Goffart <ogoffar@kde.org> (mnesia version)
%%%  - Alexey Shchepin <alexey@process-one.net> (PostgreSQL version)
%%%  - Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua> (ODBC version)
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

-module(mod_archive2_management).
-author('ejabberd@ndl.kiev.ua').

%% Our hooks
-export([list/2, retrieve/2]).

-include("mod_archive2.hrl").
-include("mod_archive2_storage.hrl").

-record(range,
        {start_time,
         end_time,
         start_id,
         end_id,
         with,
         exactmatch}).

%%--------------------------------------------------------------------
%% Lists collections according to specified parameters
%%--------------------------------------------------------------------

list(From, #iq{payload = SubEl} = IQ) ->
    TableInfo = ejabberd_storage_utils:get_table_info(archive_collection,
        ?MOD_ARCHIVE2_SCHEMA),
    F = fun() ->
            Range = parse_cmd_range(SubEl),
            Fields = [id, with_user, with_server, with_resource, utc, version],
            Results =
                case get_items_ranged(IQ, TableInfo,
                    Range, get_with_conditions(From, Range),
                    #archive_collection.utc, Fields) of
                    {[], undefined} ->
                        [];
                    {Items, OutRSM} ->
                        [mod_archive2_xml:collection_to_xml(chat, C) ||
                            C <- Items] ++ OutRSM
                end,
            exmpp_iq:result(IQ,
                exmpp_xml:element(?NS_ARCHIVING, list, [], Results))
        end,
    ejabberd_storage:transaction(exmpp_jid:prep_domain_as_list(From), F).

%%--------------------------------------------------------------------
%% Retrieves collection and its messages
%%--------------------------------------------------------------------

retrieve(From, #iq{payload = SubEl} = IQ) ->
    F = fun() ->
            InC = mod_archive2_xml:collection_from_xml(From, SubEl),
            case mod_archive2_storage:get_collection(InC, by_link) of
                undefined ->
                    {error, 'item-not-found'};
                C ->
                    TableInfo =
                        ejabberd_storage_utils:get_table_info(archive_message,
                            ?MOD_ARCHIVE2_SCHEMA),
                    Fields = TableInfo#table.fields,
                    Range = #range{exactmatch = false},
                    Results =
                        case get_items_ranged(IQ, TableInfo,
                            Range, [{'=:=', coll_id,
                                     ejabberd_storage_utils:encode_brackets(
                                         C#archive_collection.id)}],
                            #archive_message.utc, Fields) of
                            {[], undefined} ->
                                [];
                            {Items, OutRSM} ->
                                [exmpp_xml:append_children(
                                    mod_archive2_xml:collection_to_xml(chat, C),
                                    [mod_archive2_xml:message_to_xml(M,
                                        C#archive_collection.utc) ||
                                     M <- Items] ++ OutRSM)]
                        end,
                    exmpp_iq:result(IQ,
                        exmpp_xml:element(?NS_ARCHIVING, retrieve, [], Results))
            end
        end,
    ejabberd_storage:transaction(exmpp_jid:prep_domain_as_list(From), F).

%%--------------------------------------------------------------------
%% Helper functions for range requests
%%--------------------------------------------------------------------
get_items_ranged(IQ, TableInfo, Range, Conditions, UTCField, Fields) ->
    InRSM =
        case jlib:rsm_decode(IQ) of
            none -> #rsm_in{};
            R -> R
        end,
    MSHead = ejabberd_storage_utils:get_full_ms_head(TableInfo),
    {selected, [{Count}]} =
        ejabberd_storage:select(
            [{MSHead,
              get_range_matching_conditions(Range, Conditions, TableInfo),
              [ok]}],
            [{aggregate, count}]),
    if Count > 0 ->
        CombiRange = combine_ranges(Range, InRSM),
        Opts = get_range_opts(InRSM, UTCField),
        {selected, Rows} =
            ejabberd_storage:select(
                [{MSHead,
                  get_range_matching_conditions(CombiRange, Conditions,
                      TableInfo),
                ejabberd_storage_utils:get_ms_body(Fields, TableInfo)}], Opts),
        Items =
            if InRSM#rsm_in.direction =:= before ->
                lists:reverse(Rows);
               true ->
                Rows
            end,
        OutRSM =
            case Items of
                [] ->
                    jlib:rsm_encode(#rsm_out{count = Count});
                _ ->
                    [FirstRecord | _] = Items,
                    First = element(2, FirstRecord),
                    ActualStart = element(UTCField, FirstRecord),
                    LastRecord = lists:last(Items),
                    Last = element(2, LastRecord),
                    ActualEnd = element(UTCField, LastRecord),
                    StartRange = Range#range{start_id = undefined,
                                             end_time = ActualStart,
                                             end_id = First},
                    {selected, [{Index}]} =
                        ejabberd_storage:select(
                            [{MSHead,
                              get_range_matching_conditions(StartRange,
                                  Conditions, TableInfo),
                             [ok]}],
                            [{aggregate, count}]),
                    jlib:rsm_encode(#rsm_out{count = Count, index = Index,
                        first = encode_rsm_position({ActualStart, First}),
                        last = encode_rsm_position({ActualEnd, Last})})
            end,
        {Items, OutRSM};
       true ->
        {[], undefined}
    end.

get_range_matching_conditions(R, Conditions, TableInfo) ->
    RangeConditions =
        filter_undef([
            if R#range.start_time =/= undefined ->
                Start =
                    ejabberd_storage_utils:encode_brackets(R#range.start_time),
                if R#range.start_id =/= undefined ->
                    ID = ejabberd_storage_utils:encode_brackets(
                        R#range.start_id),
                    {'orelse',
                     {'>', utc, Start},
                     {'andalso',
                      {'=:=', utc, Start},
                      {'>', id, ID}}};
                   true ->
                    {'>=', utc, Start}
                end;
              true ->
               undefined
            end,
            if R#range.end_time =/= undefined ->
                End = ejabberd_storage_utils:encode_brackets(R#range.end_time),
                if R#range.end_id =/= undefined ->
                    ID = ejabberd_storage_utils:encode_brackets(
                        R#range.end_id),
                    {'orelse',
                     {'<', utc, End},
                     {'andalso',
                      {'=:=', utc, End},
                      {'<', id, ID}}};
                   true ->
                    {'<', utc, End}
                end;
               true ->
                undefined
            end]),
    ejabberd_storage_utils:resolve_fields_names(Conditions ++ RangeConditions,
        TableInfo).

get_with_conditions(From, R) ->
    [{'=:=', us, exmpp_jid:prep_bare_to_list(From)}] ++
    if R#range.with =/= undefined ->
        User = exmpp_jid:prep_node_as_list(R#range.with),
        Server = exmpp_jid:prep_domain_as_list(R#range.with),
        Resource = exmpp_jid:prep_resource_as_list(R#range.with),
        filter_undef([
            if User =/= undefined orelse R#range.exactmatch ->
                {'=:=', with_user, User};
               true ->
                undefined
            end,
            if Server =/= undefined orelse R#range.exactmatch ->
                {'=:=', with_server, Server};
               true ->
                undefined
            end,
            if Resource =/= undefined orelse R#range.exactmatch ->
                {'=:=', with_resource, Resource};
               true ->
                undefined
            end]);
       true ->
           []
    end.

parse_cmd_range(#xmlel{} = Range) ->
    #range{
        with =
            case exmpp_xml:get_attribute_as_list(Range, with, undefined) of
                   undefined -> undefined;
                   R -> exmpp_jid:parse(R)
            end,
        start_time = mod_archive2_xml:datetime_from_xml(
            exmpp_xml:get_attribute_as_list(Range, start, undefined)),
        end_time = mod_archive2_xml:datetime_from_xml(
            exmpp_xml:get_attribute_as_list(Range, 'end', undefined)),
        start_id = undefined,
        end_id = undefined,
        exactmatch = list_to_bool(
            exmpp_xml:get_attribute_as_list(Range, exactmatch, "false"))}.

combine_ranges(Range, InRSM) ->
    case InRSM#rsm_in.direction of
        undefined -> Range;
        before ->
            {DateTime, ID} = decode_rsm_position(InRSM#rsm_in.id),
            Range#range{
                end_time =
                    lists:min(filter_undef([Range#range.end_time, DateTime])),
                end_id = ID};
        aft ->
            {DateTime, ID} = decode_rsm_position(InRSM#rsm_in.id),
            Range#range{
                start_time =
                    lists:max(filter_undef([Range#range.start_time, DateTime])),
                start_id = ID}
    end.

get_range_opts(InRSM, Field) ->
    filter_undef([
        if InRSM#rsm_in.index =/= undefined ->
            {offset, InRSM#rsm_in.index};
           true ->
            undefied
        end,
        if InRSM#rsm_in.max =/= undefined ->
            {limit, InRSM#rsm_in.max};
           true ->
            undefied
        end,
        case InRSM#rsm_in.direction of
            before ->
                {order_by, {Field, desc}};
            aft ->
                {order_by, {Field, asc}};
            undefined ->
                {order_by, {Field, asc}}
        end]).

decode_rsm_position(Pos) ->
    [DateTimeStr, IDStr] = string:tokens(Pos, "@"),
    {calendar:gregorian_seconds_to_datetime(list_to_integer(DateTimeStr)),
     decode_id(IDStr)}.

encode_rsm_position({DateTime, ID}) ->
    integer_to_list(calendar:datetime_to_gregorian_seconds(DateTime)) ++
    "@" ++
    encode_id(ID).

decode_id(IDStr) ->
    case string:tokens(IDStr, "/") of
        [Node, Host, MegaSecs, Secs, MicroSecs] ->
            {list_to_atom(Node ++ "@" ++ Host),
             {list_to_integer(MegaSecs), list_to_integer(Secs),
              list_to_integer(MicroSecs)}};
        _ -> list_to_integer(IDStr)
    end.

encode_id(ID) when is_integer(ID) ->
    integer_to_list(ID);

encode_id({NodeID, {MegaSecs, Secs, MicroSecs}}) ->
    [Node, Host] = string:tokens(atom_to_list(NodeID), "@"),
    Node ++ "/" ++ Host ++ "/" ++ integer_to_list(MegaSecs) ++ "/" ++
    integer_to_list(Secs) ++ "/" ++ integer_to_list(MicroSecs).
