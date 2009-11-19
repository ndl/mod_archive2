%%%----------------------------------------------------------------------
%%% File    : mod_archive2_management_test.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 collections management commands unit testing
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

-module(mod_archive2_management_test).
-author('ejabberd@ndl.kiev.ua').

-include("mod_archive2.hrl").
-include("mod_archive2_management_test.hrl").
-include("mod_archive2_common_test_data.hrl").
-include("testing.hrl").

-export([eunit_xml_report/1]).

-define(JID, "client@localhost").
-define(HOST, "localhost").


eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

mod_archive2_management_mysql_test_() ->
{
    foreach,
    local,
    fun testing:mysql_tests_setup/0,
    fun testing:mysql_tests_teardown/1,
    [
        ?test_gen1(mysql_test_list_empty),
        [
            ?test_gen0(mysql_test_insert1),
            ?test_gen0(mysql_test_list_all),
            ?test_gen0(mysql_test_list_max),
            ?test_gen0(mysql_test_list_index),
            ?test_gen0(mysql_test_list_after),
            ?test_gen0(mysql_test_list_before),
            ?test_gen0(mysql_test_list_start_end),
            ?test_gen0(mysql_test_list_with),
            ?test_gen0(mysql_test_list_exactmatch)
        ],
        ?test_gen1(mysql_test_remove_single),
        ?test_gen1(mysql_test_remove_single_non_existing),
        ?test_gen1(mysql_test_remove_range)
    ]
}.

mod_archive2_management_mnesia_test_() ->
{
    foreach,
    local,
    fun testing:mnesia_tests_setup/0,
    fun testing:mnesia_tests_teardown/1,
    [
        ?test_gen1(common_test_list_empty),
        [
            ?test_gen0(common_test_insert1),
            ?test_gen0(common_test_list_all),
            ?test_gen0(common_test_list_max),
            ?test_gen0(common_test_list_index),
            ?test_gen0(common_test_list_after),
            ?test_gen0(common_test_list_before),
            ?test_gen0(common_test_list_start_end),
            ?test_gen0(common_test_list_with),
            ?test_gen0(common_test_list_exactmatch)
        ],
        ?test_gen1(mnesia_test_remove_single),
        ?test_gen1(mnesia_test_remove_single_non_existing),
        ?test_gen1(mnesia_test_remove_range)
    ]
}.

% Note: to ensure that we set mock data in the same process we're going to read
% them all calls to ejabberd_odbc:start should be placed in transaction.

mysql_test_list_empty(Pid) ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (with_user = 'juliet') and "
                 "(with_server = 'capulet.com') and (deleted <> 1)",
                 {selected, [], [{0}]}},
                {}])
        end),
    common_test_list_empty(Pid).

common_test_list_empty(_Pid) ->
    ?MGMT_TC1_RETRIEVE_RESULT =
        mod_archive2_management:list(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, list,
                        [exmpp_xml:attribute("with", "juliet@capulet.com")],
                        [])))).

mysql_test_insert1() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"insert into archive_collection (prev_id, next_id, us, with_user, "
                 "with_server, with_resource, utc, change_utc, version, deleted, "
                 "subject, thread, crypt, extra) values (null, null, "
                 "'client@localhost', 'juliet', 'capulet.com', 'chamber', "
                 "'1469-07-21 02:56:15', '1469-07-21 02:56:15', 1, 0, null, "
                 "null, null, null)",
                 {updated, 1}},
                {"insert into archive_collection (prev_id, next_id, us, with_user, "
                 "with_server, with_resource, utc, change_utc, version, deleted, "
                 "subject, thread, crypt, extra) values (null, null, "
                 "'client@localhost', 'balcony', 'house.capulet.com', null, "
                 "'1469-07-21 03:16:37', '1469-07-21 03:16:37', 1, 0, null, "
                 "null, null, null)",
                 {updated, 1}},
                {"insert into archive_collection (prev_id, next_id, us, with_user, "
                 "with_server, with_resource, utc, change_utc, version, deleted, "
                 "subject, thread, crypt, extra) values (null, null, "
                 "'client@localhost', 'benvolio', 'montague.net', null, "
                 "'1469-07-21 03:01:54', '1469-07-21 03:01:54', 1, 0, null, "
                 "null, null, null)",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{3}]}},
                {}])
        end),
    common_test_insert1().

common_test_insert1() ->
    {atomic, {inserted, 3, _Key}} =
        ejabberd_storage:transaction(?HOST,
            fun() ->
                ejabberd_storage:insert([?ARCHIVE_COLLECTION1,
                                             ?ARCHIVE_COLLECTION2,
                                             ?ARCHIVE_COLLECTION3])
            end).

mysql_test_list_all() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1)",
                 {selected, [], [{3}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') "
                 "and (deleted <> 1) order by utc asc",
                 {selected, [], [{1, "juliet", "capulet.com", "chamber",
                                  "1469-07-21 02:56:15", 1},
                                 {3, "benvolio", "montague.net", undefined,
                                  "1469-07-21 03:01:54", 1},
                                 {2, "balcony", "house.capulet.com", undefined,
                                  "1469-07-21 03:16:37", 1}]}},
                 {"select count(*) from archive_collection where "
                  "(us = 'client@localhost') and (deleted <> 1) and "
                  "((utc < '1469-07-21 02:56:15') "
                  "or ((utc = '1469-07-21 02:56:15') and (id < 1)))",
                  {selected, [], [{0}]}},
                {}])
        end),
    common_test_list_all().

common_test_list_all() ->
    ?MGMT_TC2_RETRIEVE_RESULT =
        mod_archive2_management:list(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, list, [], [])))).

mysql_test_list_max() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1)",
                 {selected, [], [{3}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') and "
                 "(deleted <> 1) order by utc asc limit 1",
                 {selected, [], [{1, "juliet", "capulet.com", "chamber",
                                  "1469-07-21 02:56:15", 1}]}},
                 {"select count(*) from archive_collection where "
                  "(us = 'client@localhost') and (deleted <> 1) "
                  "and ((utc < '1469-07-21 02:56:15') "
                  "or ((utc = '1469-07-21 02:56:15') and (id < 1)))",
                  {selected, [], [{0}]}},
                {}])
        end),
    common_test_list_max().

common_test_list_max() ->
    ?MGMT_TC3_RETRIEVE_RESULT = get_list_range(1, undefined).

get_list_range(Max, Index) ->
    mod_archive2_management:list(
        exmpp_jid:parse(?JID),
        exmpp_iq:xmlel_to_iq(
            exmpp_iq:get(?NS_JABBER_CLIENT,
                exmpp_xml:element(?NS_ARCHIVING, list,
                    [],
                    [exmpp_xml:element("http://jabber.org/protocol/rsm",
                        set,
	                    [],
                        filter_undef([
                            if Max =/= undefined ->
                                exmpp_xml:element(undefined, max, [],
                                    [exmpp_xml:cdata(Max)]);
                               true ->
                                undefined
                            end,
                            if Index =/= undefined ->
                                exmpp_xml:element(undefined, index, [],
                                    [exmpp_xml:cdata(Index)]);
                               true ->
                                undefined
                            end]))])))).

mysql_test_list_index() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1)",
                 {selected, [], [{3}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') "
                 "and (deleted <> 1) order by utc asc offset 1 limit 1",
                 {selected, [], [{3, "benvolio", "montague.net", undefined,
                                  "1469-07-21 03:01:54", 1}]}},
                 {"select count(*) from archive_collection where "
                  "(us = 'client@localhost') and (deleted <> 1) and "
                  "((utc < '1469-07-21 03:01:54') "
                  "or ((utc = '1469-07-21 03:01:54') and (id < 3)))",
                  {selected, [], [{1}]}},
                {}])
        end),
    common_test_list_index().

common_test_list_index() ->
    ?MGMT_TC4_RETRIEVE_RESULT = get_list_range(1, 1).

mysql_test_list_after() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1)",
                 {selected, [], [{3}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') "
                 "and (deleted <> 1) order by utc asc limit 1",
                 {selected, [], [{1, "juliet", "capulet.com", "chamber",
                                  "1469-07-21 02:56:15", 1}]}},
                 {"select count(*) from archive_collection where "
                  "(us = 'client@localhost') and (deleted <> 1) and "
                  "((utc < '1469-07-21 02:56:15') "
                  "or ((utc = '1469-07-21 02:56:15') and (id < 1)))",
                  {selected, [], [{0}]}},
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1)",
                 {selected, [], [{3}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') and "
                 "(deleted <> 1) and "
                 "((utc > '1469-07-21 02:56:15') or ((utc = '1469-07-21 02:56:15') "
                 "and (id > 1))) order by utc asc limit 2",
                 {selected, [], [{3, "benvolio", "montague.net", undefined,
                                  "1469-07-21 03:01:54", 1},
                                 {2, "balcony", "house.capulet.com", undefined,
                                  "1469-07-21 03:16:37", 1}]}},
                 {"select count(*) from archive_collection where "
                  "(us = 'client@localhost') and (deleted <> 1) "
                  "and ((utc < '1469-07-21 03:01:54') "
                  "or ((utc = '1469-07-21 03:01:54') and (id < 3)))",
                  {selected, [], [{1}]}},
                {}])
        end),
    common_test_list_after().

common_test_list_after() ->
    {atomic, #iq{payload = Items}} = get_list_range(1, undefined),
    Last = exmpp_xml:get_cdata(get_child(Items, 'last')),
    ?MGMT_TC5_RETRIEVE_RESULT =
        mod_archive2_management:list(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, list,
                        [],
                        [exmpp_xml:element("http://jabber.org/protocol/rsm",
                            set,
	                        [],
                            [exmpp_xml:element(undefined, 'after', [],
                                               [exmpp_xml:cdata(Last)]),
                             exmpp_xml:element(undefined, max, [],
                                               [exmpp_xml:cdata("2")])])])))).

mysql_test_list_before() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1)",
                 {selected, [], [{3}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') "
                 "and (deleted <> 1) order by utc asc offset 2 limit 1",
                 {selected, [], [{2, "balcony", "house.capulet.com", undefined,
                                  "1469-07-21 03:16:37", 1}]}},
                 {"select count(*) from archive_collection where "
                  "(us = 'client@localhost') and (deleted <> 1) and "
                  "((utc < '1469-07-21 03:16:37') "
                  "or ((utc = '1469-07-21 03:16:37') and (id < 2)))",
                  {selected, [], [{1}]}},
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1)",
                 {selected, [], [{3}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') and "
                 "(deleted <> 1) and "
                 "((utc < '1469-07-21 03:16:37') or ((utc = '1469-07-21 03:16:37') "
                 "and (id < 2))) order by utc desc limit 2",
                 {selected, [], [{3, "benvolio", "montague.net", undefined,
                                  "1469-07-21 03:01:54", 1},
                                 {1, "juliet", "capulet.com", "chamber",
                                  "1469-07-21 02:56:15", 1}]}},
                 {"select count(*) from archive_collection where "
                  "(us = 'client@localhost') and (deleted <> 1) and "
                  "((utc < '1469-07-21 02:56:15') "
                  "or ((utc = '1469-07-21 02:56:15') and (id < 1)))",
                  {selected, [], [{0}]}},
                {}])
        end),
    common_test_list_before().

common_test_list_before() ->
    {atomic, #iq{payload = Items}} = get_list_range(1, 2),
    First = exmpp_xml:get_cdata(get_child(Items, 'first')),
    ?MGMT_TC6_RETRIEVE_RESULT =
        mod_archive2_management:list(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, list,
                        [],
                        [exmpp_xml:element("http://jabber.org/protocol/rsm",
                            set,
	                        [],
                            [exmpp_xml:element(undefined, 'before', [],
                                               [exmpp_xml:cdata(First)]),
                             exmpp_xml:element(undefined, max, [],
                                               [exmpp_xml:cdata("2")])])])))).

mysql_test_list_start_end() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1) and "
                 "(utc >= '1469-07-21 03:01:54') "
                 "and (utc < '1469-07-21 03:16:37')",
                 {selected, [], [{1}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') and "
                 "(deleted <> 1) and "
                 "(utc >= '1469-07-21 03:01:54') and (utc < '1469-07-21 03:16:37') "
                 "order by utc asc",
                 {selected, [], [{3, "benvolio", "montague.net", undefined,
                                  "1469-07-21 03:01:54", 1}]}},
                 {"select count(*) from archive_collection where "
                  "(us = 'client@localhost') and (deleted <> 1) and "
                  "(utc >= '1469-07-21 03:01:54') "
                  "and ((utc < '1469-07-21 03:01:54') or "
                  "((utc = '1469-07-21 03:01:54') and (id < 3)))",
                  {selected, [], [{0}]}},
                {}])
        end),
    common_test_list_start_end().

common_test_list_start_end() ->
    ?MGMT_TC7_RETRIEVE_RESULT =
        mod_archive2_management:list(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, list,
                        [exmpp_xml:attribute(start, "1469-07-21T03:01:54Z"),
                         exmpp_xml:attribute('end', "1469-07-21T03:16:37Z")], [])))).

mysql_test_list_with() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') "
                 "and (with_user = 'juliet') and "
                 "(with_server = 'capulet.com') and (deleted <> 1) and "
                 "(utc < '1469-07-21 03:16:37')",
                 {selected, [], [{1}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') and "
                 "(with_user = 'juliet') and (with_server = 'capulet.com') and "
                 "(deleted <> 1) and "
                 "(utc < '1469-07-21 03:16:37') order by utc asc",
                 {selected, [], [{1, "juliet", "capulet.com", "chamber",
                                  "1469-07-21 02:56:15", 1}]}},
                 {"select count(*) from archive_collection where "
                  "(us = 'client@localhost') and "
                  "(with_user = 'juliet') and "
                  "(with_server = 'capulet.com') and (deleted <> 1) and "
                  "((utc < '1469-07-21 02:56:15') "
                  "or ((utc = '1469-07-21 02:56:15') and (id < 1)))",
                  {selected, [], [{0}]}},
                {}])
        end),
    common_test_list_with().

common_test_list_with() ->
    ?MGMT_TC8_RETRIEVE_RESULT =
        mod_archive2_management:list(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, list,
                        [exmpp_xml:attribute('end', "1469-07-21T03:16:37Z"),
                         exmpp_xml:attribute(with, "juliet@capulet.com")], [])))).

mysql_test_list_exactmatch() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and "
                 "(with_user = 'juliet') and (with_server = 'capulet.com') "
                 "and (with_resource is null) and (deleted <> 1) and "
                 "(utc < '1469-07-21 03:16:37')",
                 {selected, [], [{0}]}},
                {}])
        end),
    common_test_list_exactmatch().

common_test_list_exactmatch() ->
    ?MGMT_TC9_RETRIEVE_RESULT =
        mod_archive2_management:list(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, list,
                        [exmpp_xml:attribute('end', "1469-07-21T03:16:37Z"),
                         exmpp_xml:attribute(with, "juliet@capulet.com"),
                         exmpp_xml:attribute(exactmatch, "1")], [])))).

mysql_test_remove_single(_) ->
    mysql_test_insert1(),
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"update archive_collection set deleted = 1 where "
                 "(utc = '1469-07-21 02:56:15') and (us = 'client@localhost') "
                 "and (with_user = 'juliet') and (with_server = 'capulet.com') "
                 "and (with_resource = 'chamber') and (deleted <> 1)", {updated, 1}},
                 {"select id from archive_collection where "
                  "(utc = '1469-07-21 02:56:15') and (us = 'client@localhost') "
                  "and (with_user = 'juliet') and (with_server = 'capulet.com') "
                  "and (with_resource = 'chamber') and (deleted <> 1)", {selected, [], [{1}]}},
                 {"update archive_collection set prev_id = null where (prev_id = 1)",
                  {updated, 0}},
                 {"update archive_collection set next_id = null where (next_id = 1)",
                  {updated, 0}},
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1)",
                 {selected, [], [{2}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') "
                 "and (deleted <> 1) order by utc asc",
                 {selected, [], [{3, "benvolio", "montague.net", undefined,
                                  "1469-07-21 03:01:54", 1},
                                 {2, "balcony", "house.capulet.com", undefined,
                                  "1469-07-21 03:16:37", 1}]}},
                 {"select count(*) from archive_collection where "
                  "(us = 'client@localhost') and (deleted <> 1) and "
                  "((utc < '1469-07-21 03:01:54') "
                  "or ((utc = '1469-07-21 03:01:54') and (id < 3)))",
                  {selected, [], [{0}]}},
                {}])
        end),
    common_test_remove_single(mysql).

mnesia_test_remove_single(_) ->
    common_test_insert1(),
    common_test_remove_single(mnesia).

common_test_remove_single(RDBMS) ->
    {atomic, _} =
        mod_archive2_management:remove(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, remove,
                        [exmpp_xml:attribute('start', "1469-07-21T02:56:15Z"),
                         exmpp_xml:attribute(with, "juliet@capulet.com/chamber")], []))),
            RDBMS,
            dict:new()),
    ?MGMT_TC10_RETRIEVE_RESULT =
        mod_archive2_management:list(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, list, [], [])))).

mysql_test_remove_single_non_existing(_) ->
    mysql_test_insert1(),
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"update archive_collection set deleted = 1 where "
                 "(utc = '1469-07-21 02:56:14') and (us = 'client@localhost') "
                 "and (deleted <> 1)", {updated, 0}},
                {}])
        end),
    common_test_remove_single_non_existing(mysql),
    mysql_test_list_all().

mnesia_test_remove_single_non_existing(_) ->
    common_test_insert1(),
    common_test_remove_single_non_existing(mnesia),
    common_test_list_all().

common_test_remove_single_non_existing(RDBMS) ->
    {aborted, {throw, {error, 'item-not-found'}}} =
        mod_archive2_management:remove(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, remove,
                        [exmpp_xml:attribute('start', "1469-07-21T02:56:14Z")], []))),
            RDBMS,
            dict:new()).

mysql_test_remove_range(_) ->
    % No need to call it, as no actual DB interaction occurs.
    % mysql_test_insert1(),
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"update archive_collection set deleted = 1 where "
                 "(utc >= '1469-07-21 02:56:15') and (utc < '1469-07-21 03:16:37') "
                 "and (us = 'client@localhost') and (deleted <> 1)", {updated, 2}},
                 {"select id from archive_collection where "
                  "(utc >= '1469-07-21 02:56:15') and (utc < '1469-07-21 03:16:37') "
                  "and (us = 'client@localhost') "
                  "and (deleted <> 1)", {selected, [], [{1}, {3}]}},
                 {"update archive_collection set prev_id = null where (prev_id = 1)",
                  {updated, 0}},
                 {"update archive_collection set next_id = null where (next_id = 1)",
                  {updated, 0}},
                 {"update archive_collection set prev_id = null where (prev_id = 3)",
                  {updated, 0}},
                 {"update archive_collection set next_id = null where (next_id = 3)",
                  {updated, 0}},
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1)",
                 {selected, [], [{1}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') "
                 "and (deleted <> 1) order by utc asc",
                 {selected, [], [{2, "balcony", "house.capulet.com", undefined,
                                  "1469-07-21 03:16:37", 1}]}},
                 {"select count(*) from archive_collection where "
                  "(us = 'client@localhost') and (deleted <> 1) and "
                  "((utc < '1469-07-21 03:16:37') "
                  "or ((utc = '1469-07-21 03:16:37') and (id < 2)))",
                  {selected, [], [{0}]}},
                {}])
        end),
    common_test_remove_range(mysql).

mnesia_test_remove_range(_) ->
    common_test_insert1(),
    common_test_remove_range(mnesia).

common_test_remove_range(RDBMS) ->
    {atomic, _} =
        mod_archive2_management:remove(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, remove,
                        [exmpp_xml:attribute('start', "1469-07-21T02:56:15Z"),
                         exmpp_xml:attribute('end', "1469-07-21T03:16:37Z")], []))),
            RDBMS,
            dict:new()),
    ?MGMT_TC12_RETRIEVE_RESULT =
        mod_archive2_management:list(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, list, [], [])))).

get_child(undefined, _) ->
    undefined;

get_child([], _) ->
    undefined;

get_child([Element | Tail], Name) ->
    case get_child(Element, Name) of
        undefined -> get_child(Tail, Name);
        R -> R
    end;

get_child(#xmlel{children = SubEl} = Items, Name) ->
    case exmpp_xml:get_element(Items, Name) of
        undefined -> get_child(SubEl, Name);
        R -> R
    end.
