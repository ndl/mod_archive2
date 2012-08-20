%%%----------------------------------------------------------------------
%%% File    : exporter.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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
    Roster =
        lists:foldl(
            fun(Item, Items) ->
              dict:store(exmpp_xml:get_attribute_as_binary(Item, <<"jid">>, undefined), 1, Items)
            end,
            dict:new(),
            exmpp_xml:get_elements(exmpp_xml:get_element(get_roster(Session), 'query'), 'item')),
    Colls = get_collections(Session, [], undefined),
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
    exmpp_session:send_packet(Session, Req),
    get_collections2(Session, Collections).

get_collections2(Session, Collections) ->
    case response() of
        #received_packet{packet_type = 'iq', type_attr=_, raw_packet = IQ} ->
            List = exmpp_xml:get_element(IQ, 'list'),
            NewColls = exmpp_xml:get_elements(List, 'chat'),
            LastRsm = exmpp_xml:get_cdata(exmpp_xml:get_element(exmpp_xml:get_element(List, 'set'), 'last')),
            if NewColls =/= [] andalso length(Collections) < 10 ->
                get_collections(Session, Collections ++ NewColls, LastRsm);
               true ->
                Collections
            end;
        _ ->
            get_collections2(Session, Collections)
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
    exmpp_session:send_packet(Session, Req),
    dump_messages2(Session, Collection, LastRsm).

dump_messages2(Session, Collection, PrevLastRsm) ->
    case response() of
        #received_packet{packet_type = 'iq', type_attr=_, raw_packet = IQ} ->
            Chat = exmpp_xml:get_element(IQ, 'chat'),
            NewMsgs = exmpp_xml:get_child_elements(Chat),
            LastRsm = exmpp_xml:get_cdata(exmpp_xml:get_element(exmpp_xml:get_element(Chat, 'set'), 'last')),
            if NewMsgs =/= [] ->
                if PrevLastRsm =:= undefined ->
                    BareChat =
                        exmpp_xml:remove_elements(
                            exmpp_xml:remove_elements(
                                exmpp_xml:remove_elements(
                                    exmpp_xml:remove_elements(Chat, "set"),
                                    "from"),
                                "to"),
                            "note"),
                    {xmlel, _, _, Name, Attrs, Children} = BareChat,
                    NoNsChat = {xmlel, undefined, [], Name, Attrs, Children},
                    io:format("~s~n", [exmpp_xml:document_to_list(NoNsChat)]);
                   true -> ok
                end,
                ?DEBUG_FMT("Msgs: ~p~n", [NewMsgs]),
                lists:foreach(
                    fun(Msg) ->
                        case exmpp_xml:get_name_as_atom(Msg) of
                            'set' ->
                                ok;
                            _ ->
                                {xmlel, _, _, MsgName, MsgAttrs, MsgChildren} = Msg,
                                NoNsMsg = {xmlel, undefined, [], MsgName, MsgAttrs, MsgChildren},
                                io:format("~s~n", [exmpp_xml:document_to_list(NoNsMsg)])
                        end
                    end,
                    NewMsgs),
                dump_messages(Session, Collection, LastRsm);
               true ->
                ok
            end;
        _ ->
            dump_messages2(Session, Collection, PrevLastRsm)
    end.

response() ->
    receive
        Response -> Response
    after ?SERVER_TIMEOUT ->
        throw("No response from server!")
    end.
