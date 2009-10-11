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
-export([collection_from_xml/2, collection_to_xml/2, datetime_from_xml/1]).

-include("mod_archive2.hrl").

%%--------------------------------------------------------------------
%% Conversions to/from XML.
%%--------------------------------------------------------------------

collection_to_xml(_, undefined) ->
    undefined;

collection_to_xml(Name, #archive_collection{} = C) ->
    [PrevLink, NextLink] =
        get_collection_links([C#archive_collection.prev_id,
                              C#archive_collection.next_id]),
    PrevLinkXML = collection_to_xml(previous, PrevLink),
    NextLinkXML = collection_to_xml(next, NextLink),
    ExtraXML =
        case C#archive_collection.extra of
            #xmlel{} = R -> R;
            _ ->
                undefined
        end,
    exmpp_xml:element(undefined, Name,
        [A ||
         A <-[exmpp_xml:attribute(with, jid_to_string(C)),
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
              end],
         A =/= undefined],
        [E || E <- [PrevLinkXML, NextLinkXML, ExtraXML], E =/= undefined]).

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
        prev_id = get_collection_id(collection_from_xml(From, LinkPrevXML)),
        next_id = get_collection_id(collection_from_xml(From, LinkNextXML)),
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
        crypt = mod_archive2_utils:list_to_bool(
            exmpp_xml:get_attribute_as_list(XC, crypt, undefined)),
        extra = Extra}.

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

get_collection_links(undefined) ->
    undefined;

get_collection_links(LinksIDs) when is_list(LinksIDs) ->
    [get_collection_links(ID) || ID <- LinksIDs];

get_collection_links(ID) ->
    case mod_archive2_storage:select(
        ets:fun2ms(fun(#archive_collection{id = ID1,
                                           with_user = WithUser,
                                           with_server = WithServer,
                                           with_resource = WithResource,
                                           utc = UTC}) when ID1 =:= ID ->
                        {WithUser, WithServer, WithResource, UTC}
                   end)) of
        {selected, [{WithUser, WithServer, WithResource, UTC}]} ->
            #archive_collection{with_user = WithUser,
                                with_server = WithServer,
                                with_resource = WithResource,
                                utc = UTC};
        _ ->
            undefined
    end.

get_collection_id(undefined) -> undefined;

get_collection_id(#archive_collection{} = C) ->
    case mod_archive2_storage:select(
        ets:fun2ms(fun(#archive_collection{id = ID,
                       with_user = WithUser,
                       with_server = WithServer,
                       with_resource = WithResource,
                       utc = UTC}) when
                           WithUser =:= C#archive_collection.with_user,
                           WithServer =:= C#archive_collection.with_server,
                           WithResource =:= C#archive_collection.with_resource,
                           UTC =:= C#archive_collection.utc ->
                       {ID}
                   end)) of
        {selected, [{ID}]} ->
            ID;
        _ ->
            undefined
    end.
