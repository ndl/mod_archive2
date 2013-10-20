%%%----------------------------------------------------------------------
%%% File    : mod_archive2_manual_test.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 manual collections uploading unit testing
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

-module(mod_archive2_manual_test).
-author('xmpp@endl.ch').

-include("mod_archive2.hrl").
-include("mod_archive2_manual_test.hrl").
-include("testing.hrl").

-export([eunit_xml_report/1]).

-define(JID, "client@localhost").
-define(HOST, "localhost").

-define(ARCHIVE_COLLECTION_COMPLETE,
        {xmlel,'urn:xmpp:archive',[],chat,
         [{xmlattr,undefined,<<"with">>,<<"juliet@capulet.com/chamber">>},
          {xmlattr,undefined,<<"start">>,<<"1469-07-21T02:56:15.000123Z">>},
          {xmlattr,undefined,<<"subject">>,<<"Subject">>},
          {xmlattr,undefined,<<"thread">>,<<"12345">>},
          {xmlattr,undefined,<<"crypt">>,<<"true">>},
          {xmlattr,undefined,<<"version">>,<<"3">>}],
         [{xmlel,'urn:xmpp:archive',[],previous,[{xmlattr,undefined,<<"with">>,
                                         <<"balcony@house.capulet.com">>},
                                        {xmlattr,undefined,<<"start">>,
                                         <<"1469-07-21T03:16:37.000123Z">>}], []},
          {xmlel,'urn:xmpp:archive',[],next,[{xmlattr,undefined,<<"with">>,
                                     <<"benvolio@montague.net">>},
                                    {xmlattr,undefined,<<"start">>,
                                     <<"1469-07-21T03:01:54.000123Z">>}], []},
          {xmlel,undefined,[],x,[],[{xmlel,undefined,[],test,[],[]}]},
          {xmlel,'urn:xmpp:archive',[],from,[{xmlattr,undefined,<<"secs">>,<<"0">>}],
           [{xmlel,'urn:xmpp:archive',[],body,[],
             [{xmlcdata,<<"Art thou not Romeo, and a Montague?">>}]}]},
          {xmlel,'urn:xmpp:archive',[],to,[{xmlattr,undefined,<<"secs">>,<<"11">>}],
           [{xmlel,'urn:xmpp:archive',[],body,[],
             [{xmlcdata,<<"Neither, fair saint, if either thee dislike.">>}]}]},
          {xmlel,'urn:xmpp:archive',[],to,
           [{xmlattr,undefined,<<"secs">>,<<"12">>},
            {xmlattr,undefined,<<"name">>,<<"romeo">>},
            {xmlattr,undefined,<<"jid">>,<<"romeo@montague.net">>}],
           [{xmlel,'urn:xmpp:archive',[],body,[],
             [{xmlcdata,<<"Neither, fair saint, if either thee dislike.">>}]},
            {xmlel,'http://jabber.org/protocol/xhtml-im',[],html,[],
             [{xmlel,'http://www.w3.org/1999/xhtml',[],body,[],
               [{xmlel,undefined,[],p,[],
                 [{xmlcdata,
                   <<"Neither, fair saint, if either thee dislike.">>}]}]}]}]},
          {xmlel,'urn:xmpp:archive',[],note,[{xmlattr,undefined,<<"utc">>,<<"1469-07-21T03:04:35.000123Z">>}],
           [{xmlcdata,<<"I think she might fancy me.">>}]}]}).

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

mod_archive2_manual_mysql_test_() ->
{
    foreach,
    local,
    fun testing:mysql_tests_setup/0,
    fun testing:mysql_tests_teardown/1,
    [
        [
            ?test_gen0(mysql_test_upload),
            ?test_gen0(mysql_test_retrieve_all),
            ?test_gen0(mysql_test_retrieve_all_utc),
            ?test_gen0(mysql_test_update),
            ?test_gen0(mysql_test_retrieve_max),
            ?test_gen0(mysql_test_retrieve_empty)
        ]
    ]
}.

mod_archive2_manual_mnesia_test_() ->
{
    foreach,
    local,
    fun testing:mnesia_tests_setup/0,
    fun testing:mnesia_tests_teardown/1,
    [
        [
            ?test_gen0(common_test_upload),
            ?test_gen0(common_test_retrieve_all),
            ?test_gen0(common_test_retrieve_all_utc),
            ?test_gen0(common_test_update),
            ?test_gen0(common_test_retrieve_max),
            ?test_gen0(common_test_retrieve_empty)
        ]
    ]
}.

mysql_test_upload() ->
    dbms_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'balcony') and (with_server = 'house.capulet.com') "
                 "and (with_resource is null) and (utc = '1469-07-21 03:16:37.000123') "
                 "and (deleted <> 1)",
                 {selected, [], []}},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'benvolio') "
                 "and (with_server = 'montague.net') and (with_resource is null) "
                 "and (utc = '1469-07-21 03:01:54.000123') "
                 "and (deleted <> 1)",
                 {selected, [], []}},
                {"select id, version from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') "
                 "and (with_server = 'capulet.com') and (with_resource = 'chamber') "
                 "and (utc = '1469-07-21 02:56:15.000123')",
                 {selected, [], []}},
                {ignore, {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{1}]}},
                {"insert into archive_message (coll_id, utc, direction, body, "
                 "name, jid) values (1, '1469-07-21 02:56:15.000123', "
                 "0, 'Art thou not Romeo, and a Montague?', null, null), "
                 "(1, '1469-07-21 02:56:26.000123', "
                 "1, 'Neither, fair saint, if either thee dislike.', null, null), "
                 "(1, '1469-07-21 02:56:27.000123', "
                 "1, '<body>Neither, fair saint, if either thee dislike.</body>"
                 "<html xmlns=\\\"http://jabber.org/protocol/xhtml-im\\\">"
                 "<body xmlns=\\\"http://www.w3.org/1999/xhtml\\\">"
                 "<p xmlns=\\\"\\\">Neither, fair saint, if either thee dislike.</p>"
                 "</body></html>', 'romeo', 'romeo@montague.net'), "
                 "(1, '1469-07-21 03:04:35.000123', "
                 "2, 'I think she might fancy me.', null, null)",
                 {updated, 4}},
                {"select LAST_INSERT_ID()", {selected, [], [{4}]}},
                {}])
        end),
    common_test_upload().

common_test_upload() ->
    ?MANUAL_TC1_SAVE_RESULT =
        mod_archive2_manual:save(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, save,
                        [],
                        [?ARCHIVE_COLLECTION_COMPLETE]))),
            microseconds).

mysql_test_retrieve_all_setup() ->
    dbms_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') and (with_server = 'capulet.com') and "
                 "(with_resource = 'chamber') and (utc = '1469-07-21 02:56:15.000123') "
                 "and (deleted <> 1)",
                 {selected, [],
                    [{1, null, null, "client@localhost",
                      "juliet", "capulet.com", "chamber", "1469-07-21 02:56:15.000123",
                      "2009-10-15 07:53:19", 0, "0", "Subject", "12345", 1,
                      "<x><test/></x>"}]}},
                {"select count(*) from archive_message where (coll_id = 1)",
                 {selected, [], [{4}]}},
                {"select * from archive_message where (coll_id = 1) "
                 "order by utc asc, id asc",
                 {selected, [],
                  [{1, 1, "1469-07-21 02:56:15.000123", 0,
                     "Art thou not Romeo, and a Montague?", undefined,
                     undefined},
                   {2, 1, "1469-07-21 02:56:26.000123", 1,
                    "Neither, fair saint, if either thee dislike.", null, null},  
                   {3, 1, "1469-07-21 02:56:27.000123", 1,
                    "<body>Neither, fair saint, if either thee dislike.</body>"
                    "<html xmlns=\"http://jabber.org/protocol/xhtml-im\">"
                    "<body xmlns=\"http://www.w3.org/1999/xhtml\">"
                    "<p xmlns=\"\">Neither, fair saint, if either thee dislike.</p>"
                    "</body></html>", "romeo", "romeo@montague.net"},
                   {4, 1, "1469-07-21 03:04:35.000123", "2",
                    "I think she might fancy me.", null, null}]}},
                {"select count(*) from archive_message where (coll_id = 1) and "
                 "((utc < '1469-07-21 02:56:15.000123') or "
                 "((utc = '1469-07-21 02:56:15.000123') and (id < 1)))",
                 {selected, [], [{0}]}},
                {}])
        end).

common_test_retrieve_all_iq() ->
    exmpp_iq:xmlel_to_iq(
       exmpp_iq:get(?NS_JABBER_CLIENT,
           exmpp_xml:element(?NS_ARCHIVING, retrieve,
               [exmpp_xml:attribute(<<"with">>, "juliet@capulet.com/chamber"),
                exmpp_xml:attribute(<<"start">>, "1469-07-21T02:56:15.000123Z")],
               []))).

mysql_test_retrieve_all() ->
    mysql_test_retrieve_all_setup(),
    common_test_retrieve_all().

common_test_retrieve_all() ->
    ?MANUAL_TC2_RETRIEVE_RESULT =
        mod_archive2_management:retrieve(
            exmpp_jid:parse(?JID),
            common_test_retrieve_all_iq(),
            false,
            microseconds).

mysql_test_retrieve_all_utc() ->
    mysql_test_retrieve_all_setup(),
    common_test_retrieve_all_utc().

common_test_retrieve_all_utc() ->
    ?MANUAL_TC5_RETRIEVE_RESULT =
        mod_archive2_management:retrieve(
            exmpp_jid:parse(?JID),
            common_test_retrieve_all_iq(),
            true,
            microseconds).

mysql_test_update() ->
    dbms_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select id from archive_collection where (us = 'client@localhost') "
                "and (with_user = 'balcony') "
                 "and (with_server = 'house.capulet.com') and (with_resource is null) "
                 "and (utc = '1469-07-21 03:16:37.000123') and (deleted <> 1)",
                 {selected, [], []}},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'benvolio') "
                 "and (with_server = 'montague.net') and (with_resource is null) "
                 "and (utc = '1469-07-21 03:01:54.000123') and (deleted <> 1)",
                 {selected, [], []}},
                {},
                {"select id, version from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') "
                 "and (with_server = 'capulet.com') and (with_resource = 'chamber') "
                 "and (utc = '1469-07-21 02:56:15.000123')",
                 {selected, [], [{1, 0}]}},
                {ignore, {updated, 1}},
                {}])
        end),
    common_test_update().

common_test_update() ->
    F = fun() ->
        C = mod_archive2_xml:collection_from_xml(exmpp_jid:parse(?JID),
            ?ARCHIVE_COLLECTION_COMPLETE, microseconds),
        mod_archive2_xml:collection_to_xml(chat,
            C#archive_collection{
                subject = "Subject2",
                crypt = false,
                extra = exmpp_xml:element(undefined, x, [], [])})
    end,
    {atomic, XC} =
        dbms_storage:transaction(?HOST, F),
    ?MANUAL_TC3_UPDATE_RESULT =
        mod_archive2_manual:save(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, save,
                        [],
                        [XC]))),
            microseconds).

mysql_test_retrieve_max() ->
    dbms_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') "
                 "and (with_server = 'capulet.com') and (with_resource = 'chamber') "
                 "and (utc = '1469-07-21 02:56:15.000123') and (deleted <> 1)",
                 {selected, [],
                    [{1, null, null, "client@localhost",
                      "juliet", "capulet.com", "chamber", "1469-07-21 02:56:15.000123",
                      "2009-10-15 07:53:19", 1, "0", "Subject2", "12345", 0,
                      undefined}]}},
                {"select count(*) from archive_message where (coll_id = 1)",
                 {selected, [], [{4}]}},
                {"select * from archive_message where (coll_id = 1) "
                 "order by utc asc, id asc offset 1 limit 2",
                 {selected, [],
                  [{2, 1, "1469-07-21 02:56:26.000123", 1,
                    "Neither, fair saint, if either thee dislike.", null, null},
                   {3, 1, "1469-07-21 02:56:27.000123", 1,
                    "<body>Neither, fair saint, if either thee dislike.</body>"
                    "<html xmlns=\"http://jabber.org/protocol/xhtml-im\">"
                    "<body xmlns=\"http://www.w3.org/1999/xhtml\">"
                    "<p xmlns=\"\">Neither, fair saint, if either thee dislike.</p>"
                    "</body></html>", "romeo", "romeo@montague.net"}]}},
                {"select count(*) from archive_message where (coll_id = 1) and "
                 "((utc < '1469-07-21 02:56:26.000123') or "
                 "((utc = '1469-07-21 02:56:26.000123') and (id < 2)))",
                 {selected, [], [{1}]}},
                {}])
        end),
    common_test_retrieve_max().

common_test_retrieve_max() ->
    ?MANUAL_TC4_RETRIEVE_RESULT =
        mod_archive2_management:retrieve(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, retrieve,
                        [exmpp_xml:attribute(<<"with">>, "juliet@capulet.com/chamber"),
                         exmpp_xml:attribute(<<"start">>, "1469-07-21T02:56:15.000123Z")],
                        [exmpp_xml:element("http://jabber.org/protocol/rsm",
                         set,
                             [],
                         [exmpp_xml:element(undefined, index, [], [exmpp_xml:cdata(1)]),
                          exmpp_xml:element(undefined, max, [], [exmpp_xml:cdata(2)])])]))),
            false,
            microseconds).

mysql_test_retrieve_empty() ->
    dbms_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') and (with_server = 'capulet.com') and "
                 "(with_resource = 'NOT_EXISTING') and (utc = '1469-07-21 02:56:15.000123') "
                 "and (deleted <> 1)",
                 {selected, [], []}},
                {}])
        end),
    common_test_retrieve_empty().

common_test_retrieve_empty() ->
    {aborted, {throw, {error,'item-not-found'}}} =
        mod_archive2_management:retrieve(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, retrieve,
                        [exmpp_xml:attribute(<<"with">>, "juliet@capulet.com/NOT_EXISTING"),
                         exmpp_xml:attribute(<<"start">>, "1469-07-21T02:56:15.000123Z")],
                        []))),
            false,
            microseconds).
