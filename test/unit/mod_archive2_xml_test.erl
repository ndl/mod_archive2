%%%----------------------------------------------------------------------
%%% File    : mod_archive2_xml_test.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 xml conversion functionality unit testing
%%% Created : 03 Oct 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-module(mod_archive2_xml_test).
-author('xmpp@endl.ch').

-include("mod_archive2.hrl").
%-include("mod_archive2_xml_test.hrl").
-include("mod_archive2_common_test_data.hrl").
-include("testing.hrl").

-export([eunit_xml_report/1]).

-define(JID, "client@localhost").
-define(HOST, "localhost").

-define(ARCHIVE_COLLECTION_NO_LINKS,
        #archive_collection{
            us = ?JID,
            with_user = "juliet",
            with_server = "capulet.com",
            with_resource = "chamber",
            utc = {{1469, 07, 21}, {02, 56, 15, 123}},
            version = 1,
            deleted = false,
            subject = "Subject",
            thread = "12345",
            crypt = true,
            extra = #xmlel{name =x,children=[#xmlel{name=test}]}}).

-define(ARCHIVE_COLLECTION_NO_LINKS_XML,
        {xmlel,'urn:xmpp:archive',[],chat,
         [{xmlattr,undefined,<<"with">>,<<"juliet@capulet.com/chamber">>},
          {xmlattr,undefined,<<"start">>,<<"1469-07-21T02:56:15.000123Z">>},
          {xmlattr,undefined,<<"subject">>,<<"Subject">>},
          {xmlattr,undefined,<<"thread">>,<<"12345">>},
          {xmlattr,undefined,<<"crypt">>,<<"true">>},
          {xmlattr,undefined,<<"version">>,<<"1">>}],
         [{xmlel,undefined,[],x,[],[{xmlel,undefined,[],test,[],[]}]}]}).

-define(ARCHIVE_COLLECTION_WITH_LINKS_XML,
        {xmlel,'urn:xmpp:archive',[],chat,
         [{xmlattr,undefined,<<"with">>,<<"juliet@capulet.com/chamber">>},
          {xmlattr,undefined,<<"start">>,<<"1469-07-21T02:56:15.000123Z">>},
          {xmlattr,undefined,<<"subject">>,<<"Subject">>},
          {xmlattr,undefined,<<"thread">>,<<"12345">>},
          {xmlattr,undefined,<<"crypt">>,<<"true">>},
          {xmlattr,undefined,<<"version">>,<<"1">>}],
         [{xmlel,'urn:xmpp:archive',[],previous,[{xmlattr,undefined,<<"with">>,
                                         <<"balcony@house.capulet.com">>},
                                        {xmlattr,undefined,<<"start">>,
                                         <<"1469-07-21T03:16:37.000123Z">>}], []},
          {xmlel,'urn:xmpp:archive',[],next,[{xmlattr,undefined,<<"with">>,
                                     <<"benvolio@montague.net">>},
                                    {xmlattr,undefined,<<"start">>,
                                     <<"1469-07-21T03:01:54.000123Z">>}], []},
          {xmlel,undefined,[],x,[],[{xmlel,undefined,[],test,[],[]}]}]}).

-define(ARCHIVE_COLLECTION_WITH_EMPTY_LINKS_XML,
        {xmlel,'urn:xmpp:archive',[],chat,
         [{xmlattr,undefined,<<"with">>,<<"juliet@capulet.com/chamber">>},
          {xmlattr,undefined,<<"start">>,<<"1469-07-21T02:56:15.000123Z">>},
          {xmlattr,undefined,<<"subject">>,<<"Subject">>},
          {xmlattr,undefined,<<"thread">>,<<"12345">>},
          {xmlattr,undefined,<<"crypt">>,<<"true">>},
          {xmlattr,undefined,<<"version">>,<<"1">>}],
         [{xmlel,'urn:xmpp:archive',[],previous,[], []},
          {xmlel,'urn:xmpp:archive',[],next,[], []},
          {xmlel,undefined,[],x,[],[]}]}).

-define(ARCHIVE_MESSAGE1,
        #archive_message{
            direction = from,
            utc = {{1469, 07, 21}, {02, 56, 15, 123}},
            body = [{xmlcdata, <<"Art thou not Romeo, and a Montague?">>}]}).

-define(ARCHIVE_MESSAGE1_XML,
        {xmlel,'urn:xmpp:archive',[],from,
         [{xmlattr,undefined,<<"secs">>,<<"0">>}],
         [{xmlel,'urn:xmpp:archive',[],body,[],
              [{xmlcdata,<<"Art thou not Romeo, and a Montague?">>}]}]}).

-define(ARCHIVE_MESSAGE2,
        #archive_message{
            direction = to,
            utc = {{1469, 07, 21}, {02, 56, 26, 123}},
            name = "romeo",
            jid = "romeo@montague.net",
            body = [{xmlcdata, <<"Neither, fair saint, if either thee dislike.">>}]}).

-define(ARCHIVE_MESSAGE2_XML,
        {xmlel,'urn:xmpp:archive',[],to,
         [{xmlattr,undefined,<<"secs">>,<<"11">>},
          {xmlattr,undefined,<<"name">>,<<"romeo">>},
          {xmlattr,undefined,<<"jid">>,<<"romeo@montague.net">>}],
         [{xmlel,'urn:xmpp:archive',[],body,[],
               [{xmlcdata,<<"Neither, fair saint, if either thee dislike.">>}]}]}).

-define(ARCHIVE_MESSAGE2_UTC_XML,
        {xmlel,'urn:xmpp:archive',[],to,
         [{xmlattr,undefined,<<"utc">>,<<"1469-07-21T02:56:26.000123Z">>},
          {xmlattr,undefined,<<"name">>,<<"romeo">>},
          {xmlattr,undefined,<<"jid">>,<<"romeo@montague.net">>}],
         [{xmlel,'urn:xmpp:archive',[],body,[],
               [{xmlcdata,<<"Neither, fair saint, if either thee dislike.">>}]}]}).

-define(ARCHIVE_MESSAGE3,
        #archive_message{
            direction = note,
            utc = {{1469, 07, 21}, {03, 04, 35, 123}},
            body = [{xmlcdata, <<"I think she might fancy me.">>}]}).

-define(ARCHIVE_MESSAGE3_XML,
        {xmlel,'urn:xmpp:archive',[],note,
         [{xmlattr,undefined,<<"utc">>,<<"1469-07-21T03:04:35.000123Z">>}],
         [{xmlcdata,<<"I think she might fancy me.">>}]}).

-define(ARCHIVE_MESSAGE4,
        #archive_message{
            direction = to,
            utc = {{1469, 07, 21}, {02, 56, 26, 123}},
            name = "romeo",
            jid = "romeo@montague.net",
            body = [{xmlel,'urn:xmpp:archive',[],body,[],[{xmlcdata, <<"Neither, fair saint, if either thee dislike.">>}]},
                    {xmlel,'http://jabber.org/protocol/xhtml-im',[],html,[],
                        [{xmlel,'http://www.w3.org/1999/xhtml',[],body,[],
                            [{xmlel,undefined,[],p,[],[{xmlcdata,<<"Neither, fair saint, if either thee dislike.">>}]}]}]}]}).

-define(ARCHIVE_MESSAGE4_XML,
    {xmlel,'urn:xmpp:archive',[],to,
       [{xmlattr,undefined,<<"secs">>,<<"11">>},
        {xmlattr,undefined,<<"name">>,<<"romeo">>},
        {xmlattr,undefined,<<"jid">>,<<"romeo@montague.net">>}],
       [{xmlel,'urn:xmpp:archive',[],body,[],
            [{xmlcdata,<<"Neither, fair saint, if either thee dislike.">>}]},
        {xmlel,'http://jabber.org/protocol/xhtml-im',[],html,[],
            [{xmlel,'http://www.w3.org/1999/xhtml',[],body,[],
                 [{xmlel,undefined,[],p,[],
                      [{xmlcdata,
                           <<"Neither, fair saint, if either thee dislike.">>}]}]}]}]}).

-define(ARCHIVE_MESSAGE5_XML,
    "<to    secs   =   '11'  >\n"
    "    <body>Test</body>\n"
    "</to>\n").

-define(ARCHIVE_MESSAGE5,
    {archive_message,
     undefined,
     undefined,
     {{1,2,3},{4,5,17, 0}},
     to,
     [{xmlcdata,<<"Test">>}],
     undefined,undefined}).

-define(START, {{1469, 07, 21}, {02, 56, 15, 123}}).

-define(EXTERNAL_MESSAGE1_XML,
    {xmlel,'jabber:client',
        [{'jabber:client',none}],
        message,
        [{xmlattr,undefined,<<"type">>,<<"chat">>},
         {xmlattr,undefined,<<"to">>,<<"client2@ndl-server">>},
         {xmlattr,undefined,<<"id">>,<<"session-182531630">>}],
        [{xmlel,'jabber:client',[],subject,[],
                [{xmlcdata,<<"Test">>}]},
         {xmlel,'jabber:client',[],body,[],
                [{xmlcdata,<<"This is test message.">>}]}]}).

-define(EXTERNAL_MESSAGE2_XML,
    {xmlel,'jabber:client',
        [{'jabber:client',none}],
        message,
        [{xmlattr,undefined,<<"type">>,<<"groupchat">>},
         {xmlattr,undefined,<<"from">>,<<"darkcave@chat.shakespeare.lit/thirdwitch">>},
         {xmlattr,undefined,<<"to">>,<<"crone1@shakespeare.lit/desktop">>},
         {xmlattr,undefined,<<"id">>,<<"session-182531630">>}],
        [{xmlel,'jabber:client',[],body,[],
                [{xmlcdata,<<"Harpier cries: 'tis time, 'tis time.">>}]}]}).

-define(EXTERNAL_MESSAGE3_XML,
        {xmlel,'jabber:client',[],iq,
                 [{xmlattr,undefined,<<"type">>,<<"get">>},
                  {xmlattr,undefined,<<"id">>,<<"stanza-396429316">>}],
                 [{xmlel,"http://www.xmpp.org/extensions/xep-0136.html#ns",
                      [{"http://www.xmpp.org/extensions/xep-0136.html#ns",
                        none}],
                      <<"list">>,
                      [{xmlattr,undefined,<<"with">>,<<"client2@ndl-server">>}],
                      []}]}).

-define(EXTERNAL_MESSAGE4_XML,
    {xmlel,'jabber:client',
        [{'jabber:client',none}],
        message,
        [{xmlattr,undefined,<<"type">>,<<"groupchat">>},
         {xmlattr,undefined,<<"from">>,<<"darkcave@chat.shakespeare.lit/thirdwitch">>},
         {xmlattr,undefined,<<"to">>,<<"crone1@shakespeare.lit/desktop">>},
         {xmlattr,undefined,<<"id">>,<<"session-182531630">>}],
        [{xmlel,'jabber:client',[],body,[],
             [{xmlcdata,<<"Neither, fair saint, if either thee dislike.">>}]},
         {xmlel,'http://jabber.org/protocol/xhtml-im',[],html,[],
             [{xmlel,'http://www.w3.org/1999/xhtml',[],body,[],
                  [{xmlel,undefined,[],p,[],
                       [{xmlcdata,
                            <<"Neither, fair saint, if either thee dislike.">>}]}]}]}]}).

-define(EXTERNAL_MESSAGE4_BODY,
    {external_message,groupchat,undefined,undefined,"thirdwitch",undefined,
        [{xmlcdata,<<"Neither, fair saint, if either thee dislike.">>}]}).

-define(EXTERNAL_MESSAGE4_ALL,
    {external_message,groupchat,undefined,undefined,"thirdwitch",undefined,
        [{xmlel,'jabber:client',[],body,[],
             [{xmlcdata,<<"Neither, fair saint, if either thee dislike.">>}]},
         {xmlel,'http://jabber.org/protocol/xhtml-im',[],html,[],
             [{xmlel,'http://www.w3.org/1999/xhtml',[],body,[],
                  [{xmlel,undefined,[],p,[],
                       [{xmlcdata,
                            <<"Neither, fair saint, if either thee dislike.">>}]}]}]}]}).

-define(GLOBAL_PREFS1_XML,
    "<pref xmlns='urn:xmpp:archive'>"
    "    <default otr='approve' expire='3600' save='stream'/>"
    "    <method type='auto' use='concede'/>"
    "    <method type='local' use='forbid'/>"
    "    <method type='manual' use='prefer'/>"
    "</pref>").

-define(GLOBAL_PREFS2_XML,
[{xmlel,'urn:xmpp:archive',[],default,
        [{xmlattr,undefined,<<"save">>,<<"message">>},
         {xmlattr,undefined,<<"expire">>,<<"3600">>},
         {xmlattr,undefined,<<"otr">>,<<"require">>},
         {xmlattr,undefined,<<"unset">>,<<"false">>}],
        []},
 {xmlel,'urn:xmpp:archive',[],method,
        [{xmlattr,undefined,<<"type">>,<<"auto">>},
         {xmlattr,undefined,<<"use">>,<<"prefer">>}],
        []},
 {xmlel,'urn:xmpp:archive',[],method,
        [{xmlattr,undefined,<<"type">>,<<"local">>},
         {xmlattr,undefined,<<"use">>,<<"forbid">>}],
        []},
 {xmlel,'urn:xmpp:archive',[],method,
        [{xmlattr,undefined,<<"type">>,<<"manual">>},
         {xmlattr,undefined,<<"use">>,<<"concede">>}],
        []},
 {xmlel,'urn:xmpp:archive',[],auto,
        [{xmlattr,undefined,<<"save">>,<<"true">>},
         {xmlattr,undefined,<<"scope">>,<<"global">>}],
        []},
 {xmlel,'urn:xmpp:archive',[],auto,
        [{xmlattr,undefined,<<"save">>,<<"false">>},
         {xmlattr,undefined,<<"scope">>,<<"stream">>}],
        []}]).

-define(GLOBAL_PREFS3_XML,
[{xmlel,'urn:xmpp:archive',[],default,
        [{xmlattr,undefined,<<"save">>,<<"false">>},
         {xmlattr,undefined,<<"expire">>,<<"3600">>},
         {xmlattr,undefined,<<"otr">>,<<"forbid">>},
         {xmlattr,undefined,<<"unset">>,<<"true">>}],
        []},
 {xmlel,'urn:xmpp:archive',[],method,
        [{xmlattr,undefined,<<"type">>,<<"auto">>},
         {xmlattr,undefined,<<"use">>,<<"concede">>}],
        []},
 {xmlel,'urn:xmpp:archive',[],method,
        [{xmlattr,undefined,<<"type">>,<<"local">>},
         {xmlattr,undefined,<<"use">>,<<"concede">>}],
        []},
 {xmlel,'urn:xmpp:archive',[],method,
        [{xmlattr,undefined,<<"type">>,<<"manual">>},
         {xmlattr,undefined,<<"use">>,<<"prefer">>}],
        []},
 {xmlel,'urn:xmpp:archive',[],auto,
        [{xmlattr,undefined,<<"save">>,<<"false">>},
         {xmlattr,undefined,<<"scope">>,<<"global">>}],
        []}]).

-define(JID_PREFS1_XML,
        "<item expire='630720000' jid='benvolio@montague.net/res' "
        "exactmatch='true' otr='forbid' save='message'/>").

-define(JID_PREFS2_XML, "<item jid='benvolio@montague.net'/>").

-define(SESSION_PREFS1_XML,
        "<session thread='123' save='message'/>").

-define(MODIFIED1_XML,
    {xmlel,'urn:xmpp:archive',[],changed,
       [{xmlattr,undefined,<<"with">>,<<"juliet@capulet.com/chamber">>},
        {xmlattr,undefined,<<"start">>,<<"1469-07-21T02:56:15.000123Z">>},
        {xmlattr,undefined,<<"version">>,<<"1">>}],
       []}).

-define(MODIFIED2_XML,
    {xmlel,'urn:xmpp:archive',[],removed,
       [{xmlattr,undefined,<<"with">>,<<"juliet@capulet.com/chamber">>},
        {xmlattr,undefined,<<"start">>,<<"1469-07-21T02:56:15.000123Z">>},
        {xmlattr,undefined,<<"version">>,<<"1">>}],
       []}).

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

mod_archive2_xml_test_() ->
{
    foreach,
    local,
    fun xml_tests_setup/0,
    fun xml_tests_teardown/1,
    [
        ?test_gen1(test_collection_to_xml),
        ?test_gen1(test_collection_from_xml),
        ?test_gen1(test_collection_xml_each),
        ?test_gen1(test_collection_from_xml_empty_links),
        ?test_gen1(test_message1_to_xml),
        ?test_gen1(test_message1_from_xml),
        ?test_gen1(test_message2_to_xml),
        ?test_gen1(test_message2_to_xml_utc),
        ?test_gen1(test_message2_from_xml),
        ?test_gen1(test_message3_to_xml),
        ?test_gen1(test_message3_from_xml),
        ?test_gen1(test_message4_to_xml),
        ?test_gen1(test_message4_from_xml),
        ?test_gen1(test_external_message1_from_xml),
        ?test_gen1(test_external_message2_from_xml),
        ?test_gen1(test_external_message3_from_xml),
        ?test_gen1(test_external_message4_from_xml_body),
        ?test_gen1(test_external_message4_from_xml_all),
        ?test_gen1(test_global_prefs1_from_xml),
        ?test_gen1(test_global_prefs1_to_xml),
        ?test_gen1(test_global_prefs2_to_xml),
        ?test_gen1(test_jid_prefs1_from_xml),
        ?test_gen1(test_jid_prefs2_from_xml),
        ?test_gen1(test_jid_prefs1_to_xml),
        ?test_gen1(test_jid_prefs2_to_xml),
        ?test_gen1(test_modified1_to_xml),
        ?test_gen1(test_modified2_to_xml),
        ?test_gen1(test_session_prefs1_from_xml),
        ?test_gen1(test_session_prefs1_to_xml),
        ?test_gen1(test_archive_message5_from_xml)
    ]
 }.

mod_archive2_xml_mysql_test_() ->
{
    foreach,
    local,
    fun testing:mysql_tests_setup/0,
    fun testing:mysql_tests_teardown/1,
    [
        ?test_gen1(mysql_test_links)
    ]
}.

mod_archive2_xml_mnesia_test_() ->
{
    foreach,
    local,
    fun testing:mnesia_tests_setup/0,
    fun testing:mnesia_tests_teardown/1,
    [
        ?test_gen1(common_test_links)
    ]
}.

xml_tests_setup() -> exmpp:start().

xml_tests_teardown(_) -> ok.

test_collection_to_xml(_) ->
    ?ARCHIVE_COLLECTION_NO_LINKS_XML =
        mod_archive2_xml:collection_to_xml(chat, ?ARCHIVE_COLLECTION_NO_LINKS).

test_collection_from_xml(_) ->
    ?ARCHIVE_COLLECTION_NO_LINKS = (mod_archive2_xml:collection_from_xml(
        exmpp_jid:parse(?JID),
        ?ARCHIVE_COLLECTION_NO_LINKS_XML,
        microseconds))#archive_collection{change_utc = undefined}.

test_collection_from_xml_empty_links(_) ->
    {archive_collection,undefined,null,null,"client@localhost","juliet",
     "capulet.com","chamber", {{1469,7,21},{2,56,15, 123}},
     _,1,false,"Subject","12345",true,null} =
    mod_archive2_xml:collection_from_xml(
        exmpp_jid:parse(?JID),
        ?ARCHIVE_COLLECTION_WITH_EMPTY_LINKS_XML,
        microseconds).

test_collection_xml_each(_) ->
    lists:foreach(
        fun(Index) ->
            if Index =/= #archive_collection.with_server andalso
               Index =/= #archive_collection.us andalso
               Index =/= #archive_collection.utc andalso
               Index =/= #archive_collection.deleted ->
                R = setelement(Index, ?ARCHIVE_COLLECTION_NO_LINKS, undefined),
                OutR = (mod_archive2_xml:collection_from_xml(exmpp_jid:parse(?JID),
                     mod_archive2_xml:collection_to_xml(chat, R),
                     microseconds))#archive_collection{
                        change_utc = undefined},
                R = OutR;
               true ->
                   ok
            end
        end,
        lists:seq(5, tuple_size(?ARCHIVE_COLLECTION_NO_LINKS))).

test_message1_to_xml(_) ->
    ?ARCHIVE_MESSAGE1_XML =
        mod_archive2_xml:message_to_xml(?ARCHIVE_MESSAGE1, ?START, false).

test_message1_from_xml(_) ->
    ?ARCHIVE_MESSAGE1 =
        mod_archive2_xml:message_from_xml(?ARCHIVE_MESSAGE1_XML, ?START, microseconds).

test_message2_to_xml(_) ->
    ?ARCHIVE_MESSAGE2_XML =
        mod_archive2_xml:message_to_xml(?ARCHIVE_MESSAGE2, ?START, false).

test_message2_to_xml_utc(_) ->
    ?ARCHIVE_MESSAGE2_UTC_XML =
        mod_archive2_xml:message_to_xml(?ARCHIVE_MESSAGE2, ?START, true).

test_message2_from_xml(_) ->
    ?ARCHIVE_MESSAGE2 =
        mod_archive2_xml:message_from_xml(?ARCHIVE_MESSAGE2_XML, ?START, microseconds).

test_message3_to_xml(_) ->
    ?ARCHIVE_MESSAGE3_XML =
        mod_archive2_xml:message_to_xml(?ARCHIVE_MESSAGE3, ?START, false).

test_message3_from_xml(_) ->
    ?ARCHIVE_MESSAGE3 =
        mod_archive2_xml:message_from_xml(?ARCHIVE_MESSAGE3_XML, ?START, microseconds).

test_message4_to_xml(_) ->
    ?ARCHIVE_MESSAGE4_XML =
        mod_archive2_xml:message_to_xml(?ARCHIVE_MESSAGE4, ?START, false).

test_message4_from_xml(_) ->
    ?ARCHIVE_MESSAGE4 =
        mod_archive2_xml:message_from_xml(?ARCHIVE_MESSAGE4_XML, ?START, microseconds).

test_external_message1_from_xml(_) ->
    {external_message,chat,undefined,"Test",undefined,undefined,
     [{xmlcdata, <<"This is test message.">>}]} =
    mod_archive2_xml:external_message_from_xml(?EXTERNAL_MESSAGE1_XML, false).

test_external_message2_from_xml(_) ->
    {external_message,groupchat,undefined,undefined,"thirdwitch",undefined,
     [{xmlcdata, <<"Harpier cries: 'tis time, 'tis time.">>}]} =
    mod_archive2_xml:external_message_from_xml(?EXTERNAL_MESSAGE2_XML, false).

test_external_message3_from_xml(_) ->
    undefined = mod_archive2_xml:external_message_from_xml(?EXTERNAL_MESSAGE3_XML, false).

test_external_message4_from_xml_body(_) ->
    ?EXTERNAL_MESSAGE4_BODY =
        mod_archive2_xml:external_message_from_xml(?EXTERNAL_MESSAGE4_XML, false).

test_external_message4_from_xml_all(_) ->
    ?EXTERNAL_MESSAGE4_ALL =
        mod_archive2_xml:external_message_from_xml(?EXTERNAL_MESSAGE4_XML, true).

test_global_prefs1_from_xml(_) ->
    [PrefsXML] =
        exmpp_xml:parse_document_fragment(?GLOBAL_PREFS1_XML, [{root_depth, 0}]),
    {archive_global_prefs, "client@localhost", stream, 3600, approve,
     concede, forbid, prefer, undefined} =
         mod_archive2_xml:global_prefs_from_xml(exmpp_jid:parse(?JID), PrefsXML).

test_global_prefs1_to_xml(_) ->
    ?GLOBAL_PREFS2_XML =
        mod_archive2_xml:global_prefs_to_xml(
            #archive_global_prefs{
                us = "client@localhost",
                save = message,
                expire = 3600,
                otr = require,
                method_auto = prefer,
                method_local = forbid,
                method_manual = concede,
                auto_save = true},
            false,
            false).

test_global_prefs2_to_xml(_) ->
    ?GLOBAL_PREFS3_XML =
        mod_archive2_xml:global_prefs_to_xml(
            (mod_archive2_prefs:default_global_prefs(false, 3600))#archive_global_prefs{
                us = "client@localhost"},
            true,
            undefined).

test_jid_prefs1_from_xml(_) ->
    [PrefsXML] =
        exmpp_xml:parse_document_fragment(?JID_PREFS1_XML, [{root_depth, 0}]),
    {archive_jid_prefs, "client@localhost", "benvolio", "montague.net", "res",
     true, message, 630720000, forbid} =
        mod_archive2_xml:jid_prefs_from_xml(exmpp_jid:parse(?JID), PrefsXML).

test_jid_prefs2_from_xml(_) ->
    [PrefsXML] =
        exmpp_xml:parse_document_fragment(?JID_PREFS2_XML, [{root_depth, 0}]),
    {archive_jid_prefs, "client@localhost", "benvolio", "montague.net", undefined,
     false, undefined, undefined, undefined} =
        mod_archive2_xml:jid_prefs_from_xml(exmpp_jid:parse(?JID), PrefsXML).

test_jid_prefs1_to_xml(_) ->
    {xmlel,'urn:xmpp:archive',[],item,
       [{xmlattr,undefined,<<"jid">>,<<"benvolio@montague.net/res">>},
        {xmlattr,undefined,<<"exactmatch">>,<<"true">>},
        {xmlattr,undefined,<<"save">>,<<"stream">>},
        {xmlattr,undefined,<<"expire">>,<<"3600">>},
        {xmlattr,undefined,<<"otr">>,<<"require">>}],
       []} =
        mod_archive2_xml:jid_prefs_to_xml(
            #archive_jid_prefs{
                us = "client@localhost",
                with_user = "benvolio",
                with_server = "montague.net",
                with_resource = "res",
                exactmatch = true,
                save = stream,
                expire = 3600,
                otr = require}).

test_jid_prefs2_to_xml(_) ->
    {xmlel,'urn:xmpp:archive',[],item,
     [{xmlattr,undefined,<<"jid">>,<<"montague.net">>},
      {xmlattr,undefined,<<"exactmatch">>,<<"true">>}],
     []} =
        mod_archive2_xml:jid_prefs_to_xml(
            #archive_jid_prefs{
                us = "client@localhost",
                with_server = "montague.net",
                exactmatch = true}).

test_session_prefs1_from_xml(_) ->
    [PrefsXML] =
        exmpp_xml:parse_document_fragment(?SESSION_PREFS1_XML, [{root_depth, 0}]),
    {<<"123">>, message} =
        mod_archive2_xml:session_prefs_from_xml(PrefsXML).

test_session_prefs1_to_xml(_) ->
    {xmlel,'urn:xmpp:archive',[],session,
       [{xmlattr,undefined,<<"thread">>,<<"123">>},
        {xmlattr,undefined,<<"save">>,<<"message">>},
        {xmlattr,undefined,<<"timeout">>,<<"1800">>}],
       []} =
        mod_archive2_xml:session_prefs_to_xml({<<"123">>, message}, 1800).

test_modified1_to_xml(_) ->
    ?MODIFIED1_XML =
        mod_archive2_xml:modified_to_xml(
            #archive_collection{
                us = ?JID,
                with_user = "juliet",
                with_server = "capulet.com",
                with_resource = "chamber",
                utc = {{1469, 07, 21}, {02, 56, 15, 123}},
                change_utc = {{2000, 12, 31}, {23, 59, 59, 7890}},
                version = 1,
                deleted = false}).

test_modified2_to_xml(_) ->
    ?MODIFIED2_XML =
        mod_archive2_xml:modified_to_xml(
            #archive_collection{
                us = ?JID,
                with_user = "juliet",
                with_server = "capulet.com",
                with_resource = "chamber",
                utc = {{1469, 07, 21}, {02, 56, 15, 123}},
                change_utc = {{2000, 12, 31}, {23, 59, 59, 7890}},
                version = 1,
                deleted = true}).

test_archive_message5_from_xml(_) ->
    [MsgXML] =
        exmpp_xml:parse_document_fragment(?ARCHIVE_MESSAGE5_XML, [{root_depth, 0}]),
    ?ARCHIVE_MESSAGE5 = mod_archive2_xml:message_from_xml(MsgXML, {{1, 2, 3}, {4, 5, 6, 0}}, seconds).

mysql_test_links(Pid) ->
    dbms_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"insert into archive_collection (prev_id, next_id, us, with_user, "
                 "with_server, with_resource, utc, change_utc, version, deleted, "
                 "subject, thread, crypt, extra) values (null, null, "
                 "'client@localhost', 'juliet', 'capulet.com', 'chamber', "
                 "'1469-07-21 02:56:15.000123', '2002-12-31 23:59:59.007890', 1, 0, null, "
                 "null, null, null), "
                 "(null, null, "
                 "'client@localhost', 'balcony', 'house.capulet.com', null, "
                 "'1469-07-21 03:16:37.000123', '2001-12-31 23:59:59.007890', 1, 0, null, "
                 "null, null, null), "
                 "(null, null, "
                 "'client@localhost', 'benvolio', 'montague.net', null, "
                 "'1469-07-21 03:01:54.000123', '2000-12-31 23:59:59.007890', 1, 0, null, "
                 "null, null, null)",
                 {updated, 3}},
                {"select LAST_INSERT_ID()", {selected, [], [{3}]}},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'balcony') "
                 "and (with_server = 'house.capulet.com') and (with_resource is null) "
                 "and (utc = '1469-07-21 03:16:37.000123') and (deleted <> 1)",
                 {selected, [], [{2}]}},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'benvolio') "
                 "and (with_server = 'montague.net') and (with_resource is null) "
                 "and (utc = '1469-07-21 03:01:54.000123') and (deleted <> 1)",
                 {selected, [], [{3}]}},
                {"select with_user, with_server, with_resource, utc from "
                 "archive_collection where (id = 2) and (deleted <> 1)",
                 {selected, [], [{"balcony", "house.capulet.com", undefined,
                                  "1469-07-21 03:16:37.000123"}]}},
                {"select with_user, with_server, with_resource, utc from "
                 "archive_collection where (id = 3) and (deleted <> 1)",
                 {selected, [], [{"benvolio", "montague.net", undefined,
                                  "1469-07-21 03:01:54.000123"}]}},
                {}])
        end),
    common_test_links(Pid).

common_test_links(_Pid) ->
    {atomic, ?ARCHIVE_COLLECTION_WITH_LINKS_XML} =
        dbms_storage:transaction(?HOST,
            fun() ->
                dbms_storage:insert([?ARCHIVE_COLLECTION1,
                                             ?ARCHIVE_COLLECTION2,
                                             ?ARCHIVE_COLLECTION3]),
                mod_archive2_xml:collection_to_xml(chat,
                    mod_archive2_xml:collection_from_xml(
                        exmpp_jid:parse(?JID),
                        ?ARCHIVE_COLLECTION_WITH_LINKS_XML,
                        microseconds))
            end).
