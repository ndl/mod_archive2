%%%----------------------------------------------------------------------
%%% File    : exporter.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : Exports history from at least somewhat XEP-136 compliant server
%%% Created : 20 Aug 2012 by Alexander Tsvyashchenko <xmpp@endl.ch>
%%%
%%% mod_archive2, Copyright (C) 2009,2012 Alexander Tsvyashchenko
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

-module(exporter).
-author('xmpp@endl.ch').

-export([start/0]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-include("exporter.hrl").

%-define(DEBUG_FMT, io:format).
-define(DEBUG_FMT(Format, Value), ok).

create_session() ->
    Session = exmpp_session:start(),
    JID = exmpp_jid:make(?USER_NAME, ?SERVER_HOST, random),
    exmpp_session:auth_basic(Session, JID, ?USER_PASSWORD),
    %exmpp_session:auth_basic_digest(Session, JID, ?USER_PASSWORD),
    exmpp_session:connect_TCP(Session, ?SERVER_HOST, ?SERVER_PORT, [{starttls, enabled}, {compression, enabled}]),
    exmpp_session:login(Session),
    exmpp_session:send_packet(Session,
                exmpp_presence:set_status(
                exmpp_presence:available(), "History exporter bot is ready.")),
    {Session, JID}.

start() ->
    application:start(exmpp),
    {Session, _JID} = create_session(),
    OrigRoster =
        lists:foldl(
            fun(Item, Items) ->
                dict:store(exmpp_xml:get_attribute_as_binary(Item, <<"jid">>, undefined), 1, Items)
            end,
            dict:new(),
            exmpp_xml:get_elements(exmpp_xml:get_element(get_roster(Session), 'query'), 'item')),
    Roster =
        lists:foldl(
            fun(JID, D) ->
                dict:erase(list_to_binary(JID), D)
            end,
            OrigRoster,
            ?JIDS_TO_SKIP),
    Colls = get_collections(Session, [], undefined),
    ?DEBUG_FMT("Initial colls: ~p~n", [length(Colls)]),
    ValidColls =
        lists:filter(
            fun(C) ->
                With = exmpp_jid:parse(exmpp_xml:get_attribute(C, <<"with">>, undefined)),
                case dict:find(exmpp_jid:bare_to_binary(With), Roster) of
                    {ok, _} -> true;
                    _ -> false
                end
            end,
            Colls),
    ?DEBUG_FMT("Left colls: ~p~n", [length(ValidColls)]),
    lists:foreach(
        fun(C) ->
            dump_messages(Session, C, undefined)
        end,
        ValidColls),
    exmpp_session:stop(Session).

get_roster(Session) ->
    exmpp_session:send_packet(Session, exmpp_client_roster:get_roster()),
    get_roster2().

get_roster2() ->
    case response() of
        #received_packet{packet_type = 'iq', type_attr="result", raw_packet = IQ} -> IQ;
        _ -> get_roster2()
    end.

get_collections(Session, Collections, LastRsm) ->
    Rsm =
        exmpp_xml:element(
            ?NS_RSM,
            "set",
            [],
            [exmpp_xml:element(
                 undefined,
                 "max",
                 [],
                 [{xmlcdata, <<?MAX_COLLECTIONS>>}])]),
    RsmFull =
        if LastRsm =/= undefined ->
            exmpp_xml:append_child(
                Rsm,
                exmpp_xml:element(undefined, "after", [], [{xmlcdata, LastRsm}]));
           true ->
            Rsm
        end,
    Req =
        exmpp_iq:get(undefined,
                    exmpp_xml:element(
                        ?SERVER_NS_ARCHIVING,
                        "list",
                        [],
                        [RsmFull])),
    ?DEBUG_FMT("Sending: ~p~n", [exmpp_xml:document_to_list(Req)]),
    ID = exmpp_xml:get_attribute(Req, <<"id">>, undefined),
    exmpp_session:send_packet(Session, Req),
    get_collections2(Session, Collections, ID).

get_collections2(Session, Collections, ID) ->
    case response_iq(ID) of
        #received_packet{packet_type = 'iq', type_attr="error", raw_packet = IQ} ->
            io:format("ERROR: ~p~n", [IQ]),
            erlang:error(IQ);
        #received_packet{packet_type = 'iq', type_attr="result", raw_packet = IQ} ->
            ?DEBUG_FMT("Receiving: ~p~n", [IQ]),
            List = exmpp_xml:get_element(IQ, 'list'),
            NewColls = exmpp_xml:get_elements(List, 'chat'),
            if NewColls =/= [] ->
                % If there are less collections than our max threshold - first / last elements
                % will not be there and there's no need for further paging.
                HasLastRsm = exmpp_xml:has_element(exmpp_xml:get_element(List, 'set'), 'last'),
                if HasLastRsm ->
                    LastRsm = exmpp_xml:get_cdata(exmpp_xml:get_element(exmpp_xml:get_element(List, 'set'), 'last')),
                    get_collections(Session, Collections ++ NewColls, LastRsm);
                   true ->
                    Collections ++ NewColls
                end;
               true ->
                Collections
            end
    end.

dump_messages(Session, Collection, LastRsm) ->
    Rsm =
        exmpp_xml:element(
            ?NS_RSM,
            "set",
            [],
            [exmpp_xml:element(
                 undefined,
                 "max",
                 [],
                 [{xmlcdata, <<?MAX_MESSAGES>>}])]),
    RsmFull =
        if LastRsm =/= undefined ->
            exmpp_xml:append_child(
                Rsm,
                exmpp_xml:element(undefined, "after", [], [{xmlcdata, LastRsm}]));
           true ->
            Rsm
        end,
    Req =
        exmpp_iq:get(undefined,
                    exmpp_xml:element(
                        ?SERVER_NS_ARCHIVING,
                        "retrieve",
                        [exmpp_xml:attribute(<<"with">>, exmpp_xml:get_attribute(Collection, <<"with">>, undefined)),
                         exmpp_xml:attribute(<<"start">>, exmpp_xml:get_attribute(Collection, <<"start">>, undefined))],
                        [RsmFull])),
    ?DEBUG_FMT("Sending: ~p~n", [exmpp_xml:document_to_list(Req)]),
    ID = exmpp_xml:get_attribute(Req, <<"id">>, undefined),
    exmpp_session:send_packet(Session, Req),
    dump_messages2(Session, Collection, LastRsm, ID).

dump_messages2(Session, Collection, PrevLastRsm, ID) ->
    case response_iq(ID) of
        #received_packet{packet_type = 'iq', type_attr="error", raw_packet = IQ} ->
            io:format("ERROR: ~p~n", [IQ]),
            erlang:error(IQ);
        #received_packet{packet_type = 'iq', type_attr="result", raw_packet = IQ} ->
            ?DEBUG_FMT("Received: ~p~n", [IQ]),
            Chat = exmpp_xml:get_element(IQ, 'chat'),
            NewMsgs = exmpp_xml:get_child_elements(Chat),
            if NewMsgs =/= [] ->
                if PrevLastRsm =:= undefined ->
                    {xmlel, _, _, Name, Attrs, _} = Chat,
                    BareChat = {xmlel, undefined, [], Name, Attrs, []},
                    io:format("~s~n", [exmpp_xml:node_to_iolist(BareChat, ['jabber:client'], [])]);
                   true -> ok
                end,
                ?DEBUG_FMT("Msgs: ~p~n", [NewMsgs]),
                lists:foreach(
                    fun(Msg) ->
                        case exmpp_xml:get_name_as_atom(Msg) of
                            'set' ->
                                ok;
                            _ ->
                                io:format("~s~n", [exmpp_xml:node_to_iolist(Msg, ['jabber:client'], [])])
                        end
                    end,
                    NewMsgs),
                % If there are less messages than our max threshold - first / last elements
                % will not be there and there's no need for further paging.
                HasLastRsm = exmpp_xml:has_element(exmpp_xml:get_element(Chat, 'set'), 'last'),
                if HasLastRsm ->
                    LastRsm = exmpp_xml:get_cdata(exmpp_xml:get_element(exmpp_xml:get_element(Chat, 'set'), 'last')),
                    dump_messages(Session, Collection, LastRsm);
                   true ->
                    ok
                end;
               true ->
                ok
            end
    end.

response() ->
    receive
        Response -> Response
    after ?SERVER_TIMEOUT ->
        throw("No response from server!")
    end.

response_iq(ID) ->
    R = response(),
    case R of
        #received_packet{packet_type = 'iq', raw_packet = IQ} ->
            NewID = exmpp_xml:get_attribute(IQ, <<"id">>, undefined),
            if NewID =:= ID -> R;
               true -> response_iq(ID)
            end;
        _ -> response_iq(ID)
    end.
