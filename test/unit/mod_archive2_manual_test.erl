%%%----------------------------------------------------------------------
%%% File    : mod_archive2_manual_test.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 manual collections uploading unit testing
%%% Created : 03 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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
-author('ejabberd@ndl.kiev.ua').

-include("mod_archive2.hrl").
-include("mod_archive2_manual_test.hrl").
-include("testing.hrl").

-export([eunit_xml_report/1]).

-define(JID, "client@localhost").
-define(HOST, "localhost").

-define(ARCHIVE_COLLECTION_COMPLETE,
        {xmlel,undefined,[],chat,
         [{xmlattr,undefined,with,<<"juliet@capulet.com/chamber">>},
          {xmlattr,undefined,start,<<"1469-07-21T02:56:15.000000Z">>},
          {xmlattr,undefined,subject,<<"Subject">>},
          {xmlattr,undefined,thread,<<"12345">>},
          {xmlattr,undefined,crypt,<<"true">>},
          {xmlattr,undefined,version,<<"3">>}],
         [{xmlel,undefined,[],previous,[{xmlattr,undefined,with,
                                         <<"balcony@house.capulet.com">>},
                                        {xmlattr,undefined,start,
                                         <<"1469-07-21T03:16:37.000000Z">>}], []},
          {xmlel,undefined,[],next,[{xmlattr,undefined,with,
                                     <<"benvolio@montague.net">>},
                                    {xmlattr,undefined,start,
                                     <<"1469-07-21T03:01:54.000000Z">>}], []},
          {xmlel,undefined,[],x,[],[{xmlel,undefined,[],test,[],[]}]},
          {xmlel,undefined,[],from,[{xmlattr,undefined,secs,<<"0">>}],
           [{xmlel,undefined,[],body,[],
             [{xmlcdata,<<"Art thou not Romeo, and a Montague?">>}]}]},
          {xmlel,undefined,[],to,[{xmlattr,undefined,secs,<<"11">>}],
           [{xmlel,undefined,[],body,[],
             [{xmlcdata,<<"Neither, fair saint, if either thee dislike.">>}]}]},
          {xmlel,undefined,[],note,[{xmlattr,undefined,utc,<<"1469-07-21T03:04:35Z">>}],
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
            ?test_gen0(common_test_update),
            ?test_gen0(common_test_retrieve_max),
            ?test_gen0(common_test_retrieve_empty)
        ]
    ]
}.

mysql_test_upload() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'balcony') and (with_server = 'house.capulet.com') "
                 "and (with_resource is null) and (utc = '1469-07-21 03:16:37') "
                 "and (deleted <> 1)",
                 {selected, [], []}},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'benvolio') "
                 "and (with_server = 'montague.net') and (with_resource is null) "
                 "and (utc = '1469-07-21 03:01:54') "
                 "and (deleted <> 1)",
                 {selected, [], []}},
                {"select id, version from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') "
                 "and (with_server = 'capulet.com') and (with_resource = 'chamber') "
                 "and (utc = '1469-07-21 02:56:15')",
                 {selected, [], []}},
                {ignore, {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{1}]}},
                {"insert into archive_message (coll_id, utc, direction, body, "
                 "name, jid) values (1, '1469-07-21 02:56:15', "
                 "0, 'Art thou not Romeo, and a Montague?', null, null), "
                 "(1, '1469-07-21 02:56:26', "
                 "1, 'Neither, fair saint, if either thee dislike.', null, null), "
                 "(1, '1469-07-21 03:04:35', "
                 "2, 'I think she might fancy me.', null, null)",
                 {updated, 3}},
                {"select LAST_INSERT_ID()", {selected, [], [{3}]}},
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
                        [?ARCHIVE_COLLECTION_COMPLETE])))).

mysql_test_retrieve_all() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') and (with_server = 'capulet.com') and "
                 "(with_resource = 'chamber') and (utc = '1469-07-21 02:56:15') "
                 "and (deleted <> 1)",
                 {selected, [],
                    [{1, null, null, "client@localhost",
                      "juliet", "capulet.com", "chamber", "1469-07-21 02:56:15",
                      "2009-10-15 07:53:19", 0, "0", "Subject", "12345", 1,
                      "<x><test/></x>"}]}},
                {"select count(*) from archive_message where (coll_id = 1)",
                 {selected, [], [{3}]}},
                {"select * from archive_message where (coll_id = 1) "
                 "order by utc asc",
                 {selected, [],
                  [{1, 1, "1469-07-21 02:56:15", 0,
                     "Art thou not Romeo, and a Montague?", undefined,
                     undefined},
                   {2, 1, "1469-07-21 02:56:26", 1,
                    "Neither, fair saint, if either thee dislike.", null, null},
                   {3, 1, "1469-07-21 03:04:35", "2",
                    "I think she might fancy me.", null, null}]}},
                {"select count(*) from archive_message where (coll_id = 1) and "
                 "((utc < '1469-07-21 02:56:15') or "
                 "((utc = '1469-07-21 02:56:15') and (id < 1)))",
                 {selected, [], [{0}]}},
                {}])
        end),
    common_test_retrieve_all().

common_test_retrieve_all() ->
    ?MANUAL_TC2_RETRIEVE_RESULT =
        mod_archive2_management:retrieve(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, retrieve,
                        [exmpp_xml:attribute(with, "juliet@capulet.com/chamber"),
                         exmpp_xml:attribute(start, "1469-07-21T02:56:15Z")],
                        [])))).

mysql_test_update() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select id from archive_collection where (us = 'client@localhost') "
                "and (with_user = 'balcony') "
                 "and (with_server = 'house.capulet.com') and (with_resource is null) "
                 "and (utc = '1469-07-21 03:16:37') and (deleted <> 1)",
                 {selected, [], []}},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'benvolio') "
                 "and (with_server = 'montague.net') and (with_resource is null) "
                 "and (utc = '1469-07-21 03:01:54') and (deleted <> 1)",
                 {selected, [], []}},
                {},
                {"select id, version from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') "
                 "and (with_server = 'capulet.com') and (with_resource = 'chamber') "
                 "and (utc = '1469-07-21 02:56:15')",
                 {selected, [], [{1, 0}]}},
                {ignore, {updated, 1}},
                {}])
        end),
    common_test_update().

common_test_update() ->
    F = fun() ->
        C = mod_archive2_xml:collection_from_xml(exmpp_jid:parse(?JID),
            ?ARCHIVE_COLLECTION_COMPLETE),
        mod_archive2_xml:collection_to_xml(chat,
            C#archive_collection{
                subject = "Subject2",
                crypt = false,
                extra = exmpp_xml:element(undefined, x, [], [])})
    end,
    {atomic, XC} =
        ejabberd_storage:transaction(?HOST, F),
    ?MANUAL_TC3_UPDATE_RESULT =
        mod_archive2_manual:save(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, save,
                        [],
                        [XC])))).

mysql_test_retrieve_max() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') "
                 "and (with_server = 'capulet.com') and (with_resource = 'chamber') "
                 "and (utc = '1469-07-21 02:56:15') and (deleted <> 1)",
                 {selected, [],
                    [{1, null, null, "client@localhost",
                      "juliet", "capulet.com", "chamber", "1469-07-21 02:56:15",
                      "2009-10-15 07:53:19", 1, "0", "Subject2", "12345", 0,
                      undefined}]}},
                {"select count(*) from archive_message where (coll_id = 1)",
                 {selected, [], [{3}]}},
                {"select * from archive_message where (coll_id = 1) "
                 "order by utc asc offset 1 limit 2",
                 {selected, [],
                  [{2, 1, "1469-07-21 02:56:26", 1,
                    "Neither, fair saint, if either thee dislike.", null, null},
                   {3, 1, "1469-07-21 03:04:35", "2",
                    "I think she might fancy me.", null, null}]}},
                {"select count(*) from archive_message where (coll_id = 1) and "
                 "((utc < '1469-07-21 02:56:26') or "
                 "((utc = '1469-07-21 02:56:26') and (id < 2)))",
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
                        [exmpp_xml:attribute(with, "juliet@capulet.com/chamber"),
                         exmpp_xml:attribute(start, "1469-07-21T02:56:15Z")],
                        [exmpp_xml:element("http://jabber.org/protocol/rsm",
                         set,
	                     [],
                         [exmpp_xml:element(undefined, index, [], [exmpp_xml:cdata(1)]),
                          exmpp_xml:element(undefined, max, [], [exmpp_xml:cdata(2)])])])))).

mysql_test_retrieve_empty() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') and (with_server = 'capulet.com') and "
                 "(with_resource = 'NOT_EXISTING') and (utc = '1469-07-21 02:56:15') "
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
                        [exmpp_xml:attribute(with, "juliet@capulet.com/NOT_EXISTING"),
                         exmpp_xml:attribute(start, "1469-07-21T02:56:15Z")],
                        [])))).