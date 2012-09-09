%%%----------------------------------------------------------------------
%%% File    : auto.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 automatic archiving regression testing
%%% Created : 05 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%%
%%% mod_archive2, Copyright (C) 2009 Alexander Tsvyashchenko
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

-module(auto).
-author('ejabberd@ndl.kiev.ua').

-include_lib("eunit/include/eunit.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-include("testing.hrl").
-include("auto.hrl").

-define(THREAD, "thread-12345").
-define(MESSAGE_INTERVAL, 2000).

auto_test_() ->
    ?test_foreach(
        tests_setup,
        tests_teardown,
        [
            ?test_gen1(test_auto)
        ]).

tests_setup() ->
    application:start(exmpp),
    {client:create_session(?CLIENTNAME), client:create_session(?CLIENTNAME2)}.

tests_teardown({{Session, _JID}, {Session2, _JID2}}) ->
    exmpp_session:stop(Session),
    exmpp_session:stop(Session2).

%%
%% Scenario is as follows:
%% 1) client sends message to client2 bare JID
%% 2) client2 replies to client bare JID: the message is added to the same
%%    collection and collection is adjusted to reflect client2 full JID.
%% 3) client sends message to client2 bare JID but with thread attribute:
%%    new collection is created, client2 bare JID is used.
%% 4) client2 replies from the full JID with the same "thread" attribute:
%%    messages goes to the same (2nd) collection, collection full JID is
%%    adjusted.
%% 5) client sends "composing" message to client2 without 'body' element,
%%    the message is ignored.
%%
%% Note that we use sleeps between message sending: this is done to make sure
%% start times for different collections differ (as we check for that during
%% tests) and also to leave mod_archive2 some time to actually record those
%% messages.
%%
test_auto({{Session, _JID} = F1, {Session2, _JID2}}) ->
    Msg =
        exmpp_stanza:set_recipient(
            exmpp_message:chat("Test",
                "This is test message."),
            ?CLIENTJID2),
    exmpp_session:send_packet(Session, Msg),
    client:skip(1),
    timer:sleep(?MESSAGE_INTERVAL),
    ?AUTO_TC1_RETRIEVE_RESULT =
        client:response(F1,
            exmpp_iq:get(undefined,
                exmpp_xml:element(?NS_ARCHIVING,
                    "list",
                    [exmpp_xml:attribute(<<"with">>, ?CLIENTJID2)],
                    []))),
    ?assert(With =:= <<?CLIENTJID2>>),
    ?AUTO_TC1_RETRIEVE_RESULT2 =
        client:response(F1,
            exmpp_iq:get(undefined,
                exmpp_xml:element(?NS_ARCHIVING,
                    "retrieve",
                    [exmpp_xml:attribute(<<"with">>, With),
	                 exmpp_xml:attribute(<<"start">>, Start)],
                    []))),
    Msg2 =
        exmpp_stanza:set_recipient(
            exmpp_message:chat("Test2",
                "This is test message reply."),
            ?CLIENTJID),
    exmpp_session:send_packet(Session2, Msg2),
    client:skip(1),
    timer:sleep(?MESSAGE_INTERVAL),
    ?AUTO_TC1_RETRIEVE_RESULT3 =
        client:response(F1,
            exmpp_iq:get(undefined,
                exmpp_xml:element(?NS_ARCHIVING,
                    "list",
                    [exmpp_xml:attribute(<<"with">>, ?CLIENTJID2)],
                    []))),
    ?assert(With3 =/= <<?CLIENTJID2>>),
    ?assert(Start =:= Start3),
    ?AUTO_TC1_RETRIEVE_RESULT4 =
        client:response(F1,
            exmpp_iq:get(undefined,
                exmpp_xml:element(?NS_ARCHIVING,
                    "retrieve",
                    [exmpp_xml:attribute(<<"with">>, With3),
	                 exmpp_xml:attribute(<<"start">>, Start3)],
                    []))),
    Msg3 =
        exmpp_message:set_thread(
            exmpp_stanza:set_recipient(
                exmpp_message:chat("Test",
                    "This is test message 2."),
                ?CLIENTJID2),
            ?THREAD),
    exmpp_session:send_packet(Session, Msg3),
    client:skip(1),
    timer:sleep(?MESSAGE_INTERVAL),
    ?AUTO_TC1_RETRIEVE_RESULT5 =
        client:response(F1,
            exmpp_iq:get(undefined,
                exmpp_xml:element(?NS_ARCHIVING,
                    "list",
                    [exmpp_xml:attribute(<<"with">>, ?CLIENTJID2)],
                    []))),
    ?assert(With5 =:= <<?CLIENTJID2>>),
    ?assert(Start3 =/= Start5),
    ?AUTO_TC1_RETRIEVE_RESULT6 =
        client:response(F1,
            exmpp_iq:get(undefined,
                exmpp_xml:element(?NS_ARCHIVING,
                    "retrieve",
                    [exmpp_xml:attribute(<<"with">>, With5),
	                 exmpp_xml:attribute(<<"start">>, Start5)],
                    []))),
    Msg4 =
        exmpp_message:set_thread(
                exmpp_stanza:set_recipient(
                    exmpp_message:chat("Test2",
                        "This is test message reply 2."),
                    ?CLIENTJID),
            ?THREAD),
    exmpp_session:send_packet(Session2, Msg4),
    client:skip(1),
    timer:sleep(?MESSAGE_INTERVAL),
    ?AUTO_TC1_RETRIEVE_RESULT7 =
        client:response(F1,
            exmpp_iq:get(undefined,
                exmpp_xml:element(?NS_ARCHIVING,
                    "list",
                    [exmpp_xml:attribute(<<"with">>, ?CLIENTJID2)],
                    []))),
    ?assert(With3 =:= With7),
    ?assert(Start5 =:= Start7),
    ?AUTO_TC1_RETRIEVE_RESULT8 =
        client:response(F1,
            exmpp_iq:get(undefined,
                exmpp_xml:element(?NS_ARCHIVING,
                    "retrieve",
                    [exmpp_xml:attribute(<<"with">>, With7),
	                 exmpp_xml:attribute(<<"start">>, Start7)],
                    []))),
    % Turn on 'message' level saving.
    client:responses(F1, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref", [],
    [
        exmpp_xml:element(undefined, "default",
        [exmpp_xml:attribute(<<"save">>, "message")], [])
    ])), 2),
    Msg5 =
        exmpp_stanza:set_recipient(
            exmpp_xml:append_child(
                exmpp_message:chat(),
                exmpp_xml:element('http://jabber.org/protocol/chatstates', 'composing')),
            ?CLIENTJID2),
    exmpp_session:send_packet(Session, Msg5),
    client:skip(1),
    timer:sleep(?MESSAGE_INTERVAL),
    ?AUTO_TC1_RETRIEVE_RESULT8 =
        client:response(F1,
            exmpp_iq:get(undefined,
                exmpp_xml:element(?NS_ARCHIVING,
                    "retrieve",
                    [exmpp_xml:attribute(<<"with">>, With7),
                         exmpp_xml:attribute(<<"start">>, Start7)],
                    []))),
    Msg6 =
        exmpp_stanza:set_recipient(
            exmpp_xml:append_child(
                exmpp_message:chat("Test3",
                    "This is test message reply 3."),
                {xmlel,'http://jabber.org/protocol/xhtml-im',[],html,[],
                 [{xmlel,'http://www.w3.org/1999/xhtml',[],body,[],
                   [{xmlel,undefined,[],p,[],
                     [{xmlcdata,
                       <<"Neither, fair saint, if either thee dislike.">>}]}]}]}),
            ?CLIENTJID2),
    exmpp_session:send_packet(Session, Msg6),
    client:skip(1),
    timer:sleep(?MESSAGE_INTERVAL),
    ?AUTO_TC1_RETRIEVE_RESULT9 =
        client:response(F1,
            exmpp_iq:get(undefined,
                exmpp_xml:element(?NS_ARCHIVING,
                    "retrieve",
                    [exmpp_xml:attribute(<<"with">>, With7),
                         exmpp_xml:attribute(<<"start">>, Start7)],
                    []))).
