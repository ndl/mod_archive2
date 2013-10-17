%%%----------------------------------------------------------------------
%%% File    : mod_archive2_xml.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : Support XEP-136 for messages archiving
%%% Created : 07 Oct 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
%%%
%%% mod_archive2, Copyright (C) 2009 Alexander Tsvyashchenko
%%%
%%% Based on earlier works by:
%%%  - Olivier Goffart <ogoffar@kde.org> (mnesia version)
%%%  - Alexey Shchepin <alexey@process-one.net> (PostgreSQL version)
%%%  - Alexander Tsvyashchenko <xmpp@endl.ch> (ODBC version)
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
-author('xmpp@endl.ch').

%% Our hooks
-export([collection_from_xml/2, collection_to_xml/2,
         message_from_xml/2, message_to_xml/3,
         external_message_from_xml/2,
         thread_from_external_message/1,
         global_prefs_from_xml/2, global_prefs_to_xml/3,
         jid_prefs_from_xml/2, jid_prefs_to_xml/1,
         session_prefs_from_xml/1, session_prefs_to_xml/2,
         modified_to_xml/1,
         datetime_from_xml/1]).

-include("mod_archive2.hrl").

%%--------------------------------------------------------------------
%% Collections conversion to/from XML.
%%--------------------------------------------------------------------

collection_to_xml(_, undefined) ->
    undefined;

collection_to_xml(Name, #archive_collection{} = C) ->
    [PrevLink, NextLink] =
        [case ID of
             undefined ->
                undefined;
             null ->
                undefined;
             _ ->
                mod_archive2_storage:get_collection(
                     #archive_collection{id = ID}, by_id, existing,
                     [with_user, with_server, with_resource, utc])
         end ||
         ID <- [C#archive_collection.prev_id, C#archive_collection.next_id]],
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
            exmpp_xml:attribute(<<"with">>, jid_to_string(C)),
            exmpp_xml:attribute(<<"start">>,
                datetime_to_utc_string(C#archive_collection.utc)),
            if C#archive_collection.subject =/= undefined ->
                exmpp_xml:attribute(<<"subject">>, C#archive_collection.subject);
               true ->
                undefined
            end,
            if C#archive_collection.thread =/= undefined ->
                exmpp_xml:attribute(<<"thread">>, C#archive_collection.thread);
               true ->
                undefined
            end,
            if C#archive_collection.crypt =:= true ->
                exmpp_xml:attribute(<<"crypt">>, true);
               C#archive_collection.crypt =:= false ->
                exmpp_xml:attribute(<<"crypt">>, false);
               true ->
                undefined
            end,
            if C#archive_collection.version =/= undefined ->
                exmpp_xml:attribute(<<"version">>, C#archive_collection.version);
               true ->
                undefined
            end]),
        filter_undef([PrevLinkXML, NextLinkXML, ExtraXML])).

collection_from_xml(_From, undefined) ->
    undefined;

collection_from_xml(From, XC) ->
    LinkPrevXML = exmpp_xml:get_element(XC, previous),
    LinkNextXML = exmpp_xml:get_element(XC, next),
    Extra =
        % Empty 'x' element is used to clear 'extra' value from storage,
        % so we return 'null' here to allow this.
        case exmpp_xml:get_element(XC, x) of
            undefined -> undefined;
            #xmlel{attrs = [], children = []} -> null;
            XML -> XML
        end,
    FromUser = exmpp_jid:prep_node_as_list(From),
    FromServer = exmpp_jid:prep_domain_as_list(From),
    % We assume that empty 'with' indicates empty previous/next element,
    % therefore we return 'null' so that we clear these elements from storage.
    case exmpp_xml:get_attribute_as_list(XC, <<"with">>, undefined) of
        undefined ->
            null;
        JID ->
            With = exmpp_jid:parse(JID),
            #archive_collection{
                prev_id =
                    get_collection_id(collection_from_xml(From, LinkPrevXML)),
                next_id =
                    get_collection_id(collection_from_xml(From, LinkNextXML)),
                us = exmpp_jid:prep_to_list(
                    exmpp_jid:make(FromUser, FromServer)),
                with_user = exmpp_jid:prep_node_as_list(With),
                with_server = exmpp_jid:prep_domain_as_list(With),
                with_resource = exmpp_jid:prep_resource_as_list(With),
                utc = datetime_from_xml(
                    exmpp_xml:get_attribute_as_list(XC, <<"start">>, undefined)),
                change_utc = mod_archive2_utils:now_to_datetime(mod_archive2_time:now()),
                version =
                    case exmpp_xml:get_attribute_as_list(XC, <<"version">>, undefined) of
                        undefined -> undefined;
                        R -> list_to_integer(R)
                    end,
                deleted = false,
                subject = exmpp_xml:get_attribute_as_list(XC, <<"subject">>, undefined),
                thread = exmpp_xml:get_attribute_as_list(XC, <<"thread">>, undefined),
                crypt = list_to_bool(
                    exmpp_xml:get_attribute_as_list(XC, <<"crypt">>, undefined)),
                extra = Extra}
        end.

get_collection_id(undefined) ->
    undefined;

get_collection_id(null) ->
    null;

get_collection_id(C) ->
    case mod_archive2_storage:get_collection(C, by_link, existing, [id]) of
        #archive_collection{id = ID} when ID =/= undefined -> ID;
        _ -> null
    end.

%%--------------------------------------------------------------------
%% Messages conversion to/from XML.
%%--------------------------------------------------------------------

message_to_xml(#archive_message{} = M, Start, ForceUtc) ->
    MicroSecs =
        mod_archive2_utils:datetime_to_microseconds(M#archive_message.utc) -
        mod_archive2_utils:datetime_to_microseconds(Start),
    exmpp_xml:element(
        undefined,
        M#archive_message.direction,
        filter_undef([
            if M#archive_message.direction =:= note orelse MicroSecs < 0 orelse ForceUtc =:= true ->
                exmpp_xml:attribute(<<"utc">>,
                     datetime_to_utc_string(M#archive_message.utc));
               true ->
                exmpp_xml:attribute(<<"secs">>, MicroSecs div 1000000)
            end,
            case M#archive_message.name of
                undefined -> undefined;
                Name -> exmpp_xml:attribute(<<"name">>, Name)
            end,
            case M#archive_message.jid of
                undefined -> undefined;
                JID -> exmpp_xml:attribute(<<"jid">>, JID)
            end]),
        case exmpp_xml:normalize_cdata_in_list(M#archive_message.body) of
            [#xmlcdata{} = Text] ->
                if M#archive_message.direction =/= note ->
                    [exmpp_xml:element(undefined, body, [], [Text])];
                   true ->
                    M#archive_message.body
                end;
            _ ->
                M#archive_message.body
        end).

message_from_xml(#xmlel{name = Name, children = Children} = XM, Start) ->
    #archive_message{
        direction = Name,
        utc =
            case exmpp_xml:get_attribute_as_list(XM, <<"secs">>, undefined) of
                undefined ->
                    datetime_from_xml(
                        exmpp_xml:get_attribute_as_list(XM, <<"utc">>, undefined));
                Secs ->
                    mod_archive2_utils:microseconds_to_datetime(
                        mod_archive2_utils:datetime_to_microseconds(Start) +
                        1000000 * list_to_integer(Secs))
            end,
        body = get_body_from_children(Children),
        name = exmpp_xml:get_attribute_as_list(XM, <<"name">>, undefined),
        jid = exmpp_xml:get_attribute_as_list(XM, <<"jid">>, undefined)}.

%%--------------------------------------------------------------------
%% External messages conversion from XML.
%%--------------------------------------------------------------------
external_message_from_xml(#xmlel{name = message} = M, FullMsg) ->
    Type = list_to_atom(exmpp_xml:get_attribute_as_list(M, <<"type">>, [])),
    Nick =
        case Type of
            groupchat ->
                exmpp_jid:resource_as_list(
                    exmpp_jid:parse(
                        exmpp_xml:get_attribute_as_list(M, <<"from">>, undefined)));
             _ ->
                undefined
        end,
        Children =
            if FullMsg ->
                exmpp_xml:get_child_elements(M);
               true ->
                exmpp_xml:get_elements(M, body)
            end,
        Body = get_body_from_children(Children),
    #external_message{
        type = Type,
        thread = get_cdata(exmpp_xml:get_element(M, thread)),
        subject = get_cdata(exmpp_xml:get_element(M, subject)),
        nick = Nick,
        % Currently I see no way to get it easily :-(
        jid = undefined,
        body = Body};
external_message_from_xml(_, _) ->
    undefined.

get_cdata(undefined) ->
    undefined;
get_cdata(Element) ->
    exmpp_xml:get_cdata_as_list(Element).

thread_from_external_message(Packet) ->
    case exmpp_xml:get_cdata(exmpp_xml:get_element(Packet, thread)) of
        Thread when is_binary(Thread) -> Thread;
        _ -> undefined
    end.

%%--------------------------------------------------------------------
%% Preferences conversion to/from XML.
%%--------------------------------------------------------------------

session_prefs_to_xml({Thread, AutoSave}, TimeOut) ->
    exmpp_xml:element(undefined, session,
        [exmpp_xml:attribute(<<"thread">>, Thread),
         exmpp_xml:attribute(<<"save">>, AutoSave),
         exmpp_xml:attribute(<<"timeout">>, TimeOut)], []).

session_prefs_from_xml(PrefsXML) ->
    {exmpp_xml:get_attribute_as_binary(PrefsXML, <<"thread">>, undefined),
     list_to_atom(exmpp_xml:get_attribute_as_list(PrefsXML, <<"save">>, "undefined"))}.

jid_prefs_to_xml(Prefs) ->
    ExactMatch = Prefs#archive_jid_prefs.exactmatch,
    Save = Prefs#archive_jid_prefs.save,
    Expire = Prefs#archive_jid_prefs.expire,
    OTR = Prefs#archive_jid_prefs.otr,
    exmpp_xml:element(undefined, item,
        filter_undef([
            exmpp_xml:attribute(<<"jid">>, jid_to_string(Prefs)),
            if ExactMatch =/= undefined ->
                exmpp_xml:attribute(<<"exactmatch">>, ExactMatch);
               true ->
                undefined
            end,
            if Save =/= undefined ->
                exmpp_xml:attribute(<<"save">>, Save);
               true ->
                undefined
            end,
            if Expire =/= undefined ->
                exmpp_xml:attribute(<<"expire">>, Expire);
               true ->
                undefined
            end,
            if OTR =/= undefined ->
                exmpp_xml:attribute(<<"otr">>, OTR);
               true ->
                undefined
            end]), []).

jid_prefs_from_xml(From, PrefsXML) ->
    JID =
        exmpp_jid:parse(
            exmpp_xml:get_attribute_as_list(PrefsXML, <<"jid">>, undefined)),
    #archive_jid_prefs{
        us = exmpp_jid:prep_bare_to_list(From),
        with_user = exmpp_jid:prep_node_as_list(JID),
        with_server = exmpp_jid:prep_domain_as_list(JID),
        with_resource = exmpp_jid:prep_resource_as_list(JID),
        exactmatch = list_to_bool(
            exmpp_xml:get_attribute_as_list(PrefsXML, <<"exactmatch">>, "false")),
        save = list_to_atom(
            exmpp_xml:get_attribute_as_list(PrefsXML, <<"save">>, "undefined")),
        expire =
            case exmpp_xml:get_attribute_as_list(PrefsXML, <<"expire">>, undefined) of
                undefined ->
                    undefined;
                Value ->
                    list_to_integer(Value)
            end,
        otr = list_to_atom(
            exmpp_xml:get_attribute_as_list(PrefsXML, <<"otr">>, "undefined"))}.

global_prefs_to_xml(Prefs, UnSet, AutoState) ->
    filter_undef(
        [exmpp_xml:element(undefined, default,
            filter_undef([
                if Prefs#archive_global_prefs.save =/= undefined ->
                    exmpp_xml:attribute(<<"save">>, Prefs#archive_global_prefs.save);
                   true ->
                    undefined
                end,
                if Prefs#archive_global_prefs.expire =/= infinity ->
                    exmpp_xml:attribute(<<"expire">>, Prefs#archive_global_prefs.expire);
                   true ->
                    undefined
                end,
                if Prefs#archive_global_prefs.otr =/= undefined ->
                    exmpp_xml:attribute(<<"otr">>, Prefs#archive_global_prefs.otr);
                   true ->
                    undefined
                end,
                exmpp_xml:attribute(<<"unset">>, UnSet)]), []),
         exmpp_xml:element(undefined, method,
            [exmpp_xml:attribute(<<"type">>, auto),
             exmpp_xml:attribute(<<"use">>, Prefs#archive_global_prefs.method_auto)],
            []),
         exmpp_xml:element(undefined, method,
            [exmpp_xml:attribute(<<"type">>, local),
             exmpp_xml:attribute(<<"use">>, Prefs#archive_global_prefs.method_local)],
            []),
         exmpp_xml:element(undefined, method,
            [exmpp_xml:attribute(<<"type">>, manual),
             exmpp_xml:attribute(<<"use">>, Prefs#archive_global_prefs.method_manual)],
            []),
         if Prefs#archive_global_prefs.auto_save =/= undefined ->
             exmpp_xml:element(undefined, auto,
                 [exmpp_xml:attribute(<<"save">>,
                    Prefs#archive_global_prefs.auto_save),
                  exmpp_xml:attribute(<<"scope">>, global)],
                 []);
            true ->
                undefined
         end,
         if AutoState =/= undefined ->
             exmpp_xml:element(undefined, auto,
                 [exmpp_xml:attribute(<<"save">>, AutoState),
                  exmpp_xml:attribute(<<"scope">>, stream)],
                 []);
            true ->
             undefined
         end]).

global_prefs_from_xml(From, PrefsXML) ->
    lists:foldl(
        fun(PartialPrefs, Prefs) ->
            list_to_tuple(
                    lists:zipwith(
                            fun(Item1, Item2) ->
                                    if Item1 =/= undefined ->
                            Item1;
                                       true ->
                            Item2
                                    end
                            end,
                            tuple_to_list(PartialPrefs),
                            tuple_to_list(Prefs)))
        end,
        #archive_global_prefs{us = exmpp_jid:prep_bare_to_list(From)},
        [global_prefs_from_xml2(Element) ||
         Element <- exmpp_xml:get_child_elements(PrefsXML)]).

global_prefs_from_xml2(#xmlel{name = default} = Element) ->
    #archive_global_prefs{
        save = list_to_atom(
            exmpp_xml:get_attribute_as_list(Element, <<"save">>, "undefined")),
        expire =
            case exmpp_xml:get_attribute_as_list(Element, <<"expire">>, undefined) of
                undefined ->
                    undefined;
                Value ->
                    list_to_integer(Value)
            end,
        otr = list_to_atom(
            exmpp_xml:get_attribute_as_list(Element, <<"otr">>, "undefined"))};

global_prefs_from_xml2(#xmlel{name = method} = Element) ->
    Use = list_to_atom(
        exmpp_xml:get_attribute_as_list(Element, <<"use">>, "undefined")),
    case exmpp_xml:get_attribute_as_list(Element, <<"type">>, undefined) of
       "auto" ->
           #archive_global_prefs{method_auto = Use};
       "local" ->
           #archive_global_prefs{method_local = Use};
       "manual" ->
           #archive_global_prefs{method_manual = Use}
    end;

global_prefs_from_xml2(_) ->
    #archive_global_prefs{}.

%%--------------------------------------------------------------------
%% Replication information conversion to XML.
%%--------------------------------------------------------------------

modified_to_xml(#archive_collection{} = C) ->
    Name =
        case C#archive_collection.deleted of
            true -> removed;
            false -> changed
        end,
    exmpp_xml:element(undefined, Name,
        [exmpp_xml:attribute(<<"with">>, jid_to_string(C)),
         exmpp_xml:attribute(<<"start">>,
            datetime_to_utc_string(C#archive_collection.utc)),
         exmpp_xml:attribute(<<"version">>, C#archive_collection.version)], []).

%%--------------------------------------------------------------------
%% Conversion utility functions.
%%--------------------------------------------------------------------

jid_to_string(#archive_collection{} = C) ->
    exmpp_jid:prep_to_list(
        exmpp_jid:make(C#archive_collection.with_user,
                       C#archive_collection.with_server,
                       C#archive_collection.with_resource));

jid_to_string(#archive_jid_prefs{} = Prefs) ->
    exmpp_jid:prep_to_list(
        exmpp_jid:make(Prefs#archive_jid_prefs.with_user,
                       Prefs#archive_jid_prefs.with_server,
                       Prefs#archive_jid_prefs.with_resource)).

datetime_to_utc_string({{Year, Month, Day}, {Hour, Minute, Second, MicroSecond}}) ->
    lists:flatten(
        io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",
                      [Year, Month, Day, Hour, Minute, Second, MicroSecond])).

datetime_from_xml(undefined) -> undefined;

datetime_from_xml(TimeStr) ->
    mod_archive2_utils:now_to_datetime(mod_archive2_utils:datetime_string_to_timestamp(TimeStr)).

get_body_from_children(Children) ->
    NormChildren = exmpp_xml:normalize_cdata_in_list(Children),
    case NormChildren of
        % Directly embedded text, i.e. for <note> element.
        [#xmlcdata{}] ->
            NormChildren;
        _ ->
            NoCdataChildren = exmpp_xml:remove_cdata_from_list(NormChildren),
            case NoCdataChildren of
                % Common case of single <body> element:
                % extract & store underlying cdata.
                [#xmlel{name = body, children = Text}] ->
                    exmpp_xml:normalize_cdata_in_list(Text);
                _ ->
                % Everything else is stored "as is".
                    NoCdataChildren
            end
    end.
