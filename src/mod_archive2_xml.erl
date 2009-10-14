%%%----------------------------------------------------------------------
%%% File    : mod_archive2_xml.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Support XEP-136 for messages archiving
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

-module(mod_archive2_xml).
-author('ejabberd@ndl.kiev.ua').

%% Our hooks
-export([collection_from_xml/2, collection_to_xml/2,
         message_from_xml/2, message_to_xml/2,
         datetime_from_xml/1]).

-include("mod_archive2.hrl").

%%--------------------------------------------------------------------
%% Collections conversion to/from XML.
%%--------------------------------------------------------------------

collection_to_xml(_, undefined) ->
    undefined;

collection_to_xml(Name, #archive_collection{} = C) ->
    [PrevLink, NextLink] =
        mod_archive2_storage:get_collection_links(
            [C#archive_collection.prev_id, C#archive_collection.next_id]),
    PrevLinkXML = collection_to_xml(previous, PrevLink),
    NextLinkXML = collection_to_xml(next, NextLink),
    ExtraXML =
        case C#archive_collection.extra of
            #xmlel{} = R -> R;
            _ ->
                undefined
        end,
    exmpp_xml:element(undefined, Name,
        filter_undef([
            exmpp_xml:attribute(with, jid_to_string(C)),
            exmpp_xml:attribute(start,
                datetime_to_utc_string(C#archive_collection.utc)),
            if C#archive_collection.subject =/= undefined ->
                exmpp_xml:attribute(subject, C#archive_collection.subject);
               true ->
                undefined
            end,
            if C#archive_collection.thread =/= undefined ->
                exmpp_xml:attribute(thread, C#archive_collection.thread);
               true ->
                undefined
            end,
            if C#archive_collection.crypt =:= true ->
                exmpp_xml:attribute(crypt, true);
               true ->
                undefined
            end,
            if C#archive_collection.version =/= undefined ->
                exmpp_xml:attribute(version, C#archive_collection.version);
               true ->
                undefined
            end]),
        filter_undef([PrevLinkXML, NextLinkXML, ExtraXML])).

collection_from_xml(_From, undefined) ->
    undefined;

collection_from_xml(From, XC) ->
    LinkPrevXML = exmpp_xml:get_element(XC, previous),
    LinkNextXML = exmpp_xml:get_element(XC, next),
    Extra = exmpp_xml:get_element(XC, x),
    FromUser = exmpp_jid:prep_node_as_list(From),
    FromServer = exmpp_jid:prep_domain_as_list(From),
    With = exmpp_jid:parse(exmpp_xml:get_attribute_as_list(XC, with, undefined)),
    #archive_collection{
        prev_id = mod_archive2_storage:get_collection_id(
            collection_from_xml(From, LinkPrevXML)),
        next_id = mod_archive2_storage:get_collection_id(
            collection_from_xml(From, LinkNextXML)),
        us = exmpp_jid:prep_to_list(exmpp_jid:make(FromUser, FromServer)),
        with_user = exmpp_jid:prep_node_as_list(With),
        with_server = exmpp_jid:prep_domain_as_list(With),
        with_resource = exmpp_jid:prep_resource_as_list(With),
        utc = datetime_from_xml(exmpp_xml:get_attribute_as_list(XC, start,
                                                                undefined)),
        change_utc = calendar:now_to_datetime(now()),
        version =
            case exmpp_xml:get_attribute_as_list(XC, version, undefined) of
                undefined -> undefined;
                R -> list_to_integer(R)
            end,
        deleted = 0,
        subject = exmpp_xml:get_attribute_as_list(XC, subject, undefined),
        thread = exmpp_xml:get_attribute_as_list(XC, thread, undefined),
        crypt = list_to_bool(
            exmpp_xml:get_attribute_as_list(XC, crypt, undefined)),
        extra = Extra}.

%%--------------------------------------------------------------------
%% Messages conversion to/from XML.
%%--------------------------------------------------------------------

message_to_xml(#archive_message{} = M, Start) ->
    Secs =
        calendar:datetime_to_gregorian_seconds(M#archive_message.utc) -
        calendar:datetime_to_gregorian_seconds(Start),
    exmpp_xml:element(undefined, M#archive_message.direction,
        filter_undef([
            if M#archive_message.direction == note orelse Secs < 0 ->
                exmpp_xml:attribute(utc,
                     datetime_to_utc_string(M#archive_message.utc));
               true ->
                exmpp_xml:attribute(secs, Secs)
            end,
            case M#archive_message.name of
                undefined -> undefined;
                Name -> exmpp_xml:attribute(name, Name)
            end,
            case M#archive_message.jid of
                undefined -> undefined;
                JID -> exmpp_xml:attribute(jid, JID)
            end]),
        [if M#archive_message.direction == note ->
             exmpp_xml:cdata(M#archive_message.body);
            true ->
             exmpp_xml:element(undefined, body, [],
                               [exmpp_xml:cdata(M#archive_message.body)])
		 end]).

message_from_xml(#xmlel{name = Name} = XM, Start) ->
    #archive_message{
        direction = Name,
        utc =
            case exmpp_xml:get_attribute_as_list(XM, secs, undefined) of
                undefined ->
                    datetime_from_xml(
                        exmpp_xml:get_attribute_as_list(XM, utc, undefined));
                Secs ->
                    calendar:gregorian_seconds_to_datetime(
                        calendar:datetime_to_gregorian_seconds(Start) +
                        list_to_integer(Secs))
            end,
        body =
            case Name of
                note ->
                    exmpp_xml:get_cdata_as_list(XM);
                _ ->
                    exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(XM, body))
            end,
        name = exmpp_xml:get_attribute_as_list(XM, name, undefined),
        jid = exmpp_xml:get_attribute_as_list(XM, jid, undefined)}.

%%--------------------------------------------------------------------
%% Conversion utility functions.
%%--------------------------------------------------------------------

jid_to_string(#archive_collection{} = C) ->
    exmpp_jid:prep_to_list(
        exmpp_jid:make(C#archive_collection.with_user,
                       C#archive_collection.with_server,
                       C#archive_collection.with_resource)).

datetime_to_utc_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
        io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",
                      [Year, Month, Day, Hour, Minute, Second, 0])).

datetime_from_xml(undefined) -> undefined;

datetime_from_xml(TimeStr) ->
    calendar:now_to_datetime(jlib:datetime_string_to_timestamp(TimeStr)).
