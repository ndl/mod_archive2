%%%----------------------------------------------------------------------
%%% File    : mod_archive2_auto_test.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 auto collections archiving unit testing
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

-module(mod_archive2_auto_test).
-author('ejabberd@ndl.kiev.ua').

-include("mod_archive2.hrl").
-include("testing.hrl").

-export([eunit_xml_report/1]).

-define(XML_MESSAGE_1,
    "<message"
    "   from='client@localhost'"
    "   to='juliet@example.com'"
    "   type='chat'"
    "   xml:lang='en'>"
    "  <body>Art thou not Romeo, and a Montague?</body>"
    "</message>").

-define(XML_MESSAGE_2,
    "<message"
    "   from='juliet@example.com/balcony'"
    "   to='client@localhost'"
    "   type='chat'"
    "   xml:lang='en'>"
    "  <subject>Incompetence</subject>"
    "  <body>That&apos;s my line, idiot!!!</body>"
    "</message>").

-define(XML_MESSAGE_3,
    "<message"
    "   from='client@localhost'"
    "   to='juliet@example.com'"
    "   type='chat'"
    "   xml:lang='en'>"
    "  <subject>Sorry</subject>"
    "  <body>Sorry, that&apos;s due to that extra beer ...</body>"
    "</message>").

-define(XML_MESSAGE_4,
    "<message"
    "   from='client@localhost'"
    "   to='juliet@example.com'"
    "   type='chat'"
    "   xml:lang='en'>"
    "  <thread>thread123</thread>"
    "  <body>Art thou not Romeo, and a Montague?</body>"
    "</message>").

-define(XML_MESSAGE_5,
    "<message"
    "   from='juliet@example.com/balcony'"
    "   to='client@localhost'"
    "   type='chat'"
    "   xml:lang='en'>"
    "  <thread>thread123</thread>"
    "  <subject>Incompetence</subject>"
    "  <body>That&apos;s my line, idiot!!!</body>"
    "</message>").

-define(XML_MESSAGE_6,
    "<message"
    "   from='client@localhost'"
    "   to='juliet@example.com/chamber'"
    "   type='chat'"
    "   xml:lang='en'>"
    "  <thread>thread123</thread>"
    "  <subject>Sorry</subject>"
    "  <body>Sorry, that&apos;s due to that extra beer ...</body>"
    "</message>").

-define(HOST, "localhost").

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

mod_archive2_auto_mysql_test_() ->
{
    foreach,
    local,
    fun testing:mysql_tests_setup/0,
    fun testing:mysql_tests_teardown/1,
    [
        [
            ?test_gen0(mysql_test_auto_new_and_update),
            ?test_gen0(mysql_test_auto_thread_new_and_update)
        ]
    ]
}.

mod_archive2_auto_mnesia_test_() ->
{
    foreach,
    local,
    fun testing:mnesia_tests_setup/0,
    fun testing:mnesia_tests_teardown/1,
    [
        [
            ?test_gen0(common_test_auto_new_and_update),
            ?test_gen0(common_test_auto_thread_new_and_update)
        ]
    ]
}.

mysql_test_auto_new_and_update() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') and (with_server = 'example.com') "
                 "and (with_resource is null) and (utc = '2010-01-02 03:04:05') "
                 "and (deleted <> 1)", {selected, [], []}},
                {"insert into archive_collection (prev_id, next_id, us, with_user, "
                 "with_server, with_resource, utc, change_utc, version, deleted, "
                 "subject, thread, crypt, extra) values (null, null, 'client@localhost', "
                 "'juliet', 'example.com', null, '2010-01-02 03:04:05', "
                 "'2010-01-02 03:04:05', 0, 0, null, null, null, null)",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{1}]}},
                {"insert into archive_message (coll_id, utc, direction, body, "
                 "name, jid) values (1, '2010-01-02 03:04:05', 1, 'Art thou not "
                 "Romeo, and a Montague?', null, null)", {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{1}]}},
                {},
                {},
                {"update archive_collection set with_resource = 'balcony', "
                 "change_utc = '2010-01-02 03:04:06', version = 1, "
                 "subject = 'Incompetence', thread = null where id = 1",
                 {updated, 1}},
                {"insert into archive_message (coll_id, utc, direction, body, "
                 "name, jid) values (1, '2010-01-02 03:04:06', 0, "
                 "'That\\'s my line, idiot!!!', null, null)",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{2}]}},
                {},
                {},
                {"update archive_collection set with_resource = 'balcony', "
                 "change_utc = '2010-01-02 03:04:07', version = 2, "
                 "subject = 'Sorry', thread = null where id = 1",
                 {updated, 1}},
                {"insert into archive_message (coll_id, utc, direction, body, "
                 "name, jid) values (1, '2010-01-02 03:04:07', 1, "
                 "'Sorry, that\\'s due to that extra beer ...', null, null)",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{3}]}},
                {},
                {},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') and (with_server = 'example.com') "
                 "and (with_resource is null) and (utc = '2010-01-02 04:04:07') "
                 "and (deleted <> 1)", {selected, [], []}},
                {"insert into archive_collection (prev_id, next_id, us, with_user, "
                 "with_server, with_resource, utc, change_utc, version, deleted, "
                 "subject, thread, crypt, extra) values (null, null, 'client@localhost', "
                 "'juliet', 'example.com', null, '2010-01-02 04:04:07', "
                 "'2010-01-02 04:04:07', 0, 0, null, null, null, null)",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{1}]}},
                {"insert into archive_message (coll_id, utc, direction, body, "
                 "name, jid) values (1, '2010-01-02 04:04:07', 1, 'Art thou not "
                 "Romeo, and a Montague?', null, null)", {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{1}]}},
                {}])
        end),
    common_test_auto_new_and_update().

common_test_auto_new_and_update() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            NewTS = {{2010, 1, 2}, {3, 4, 5}},
            mod_archive2_time:start(lists:duplicate(10, NewTS))
        end),
    [Msg] =
        exmpp_xml:parse_document_fragment(?XML_MESSAGE_1, [{root_depth, 0}]),
    Sessions =
        mod_archive2_auto:add_message({to,
            exmpp_jid:make("client", "localhost"),
            exmpp_jid:make("juliet", "example.com"),
            Msg}, 1800, dict:new()),
    [KeyWith] = dict:fetch_keys(Sessions),
    ?assert(KeyWith =:=
        {{jid,<<"client@localhost">>,<<"client">>,<<"localhost">>,undefined},
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads = dict:to_list(dict:fetch(KeyWith, Sessions)),
    [{{no_thread, undefined}, {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 5}}, _, 0, undefined}}] = Threads,
    ejabberd_storage:transaction(?HOST,
        fun() ->
            NewTS = {{2010, 1, 2}, {3, 4, 6}},
            mod_archive2_time:start(lists:duplicate(10, NewTS))
        end),
    [Msg2] =
        exmpp_xml:parse_document_fragment(?XML_MESSAGE_2, [{root_depth, 0}]),
    NewSessions =
        mod_archive2_auto:add_message({from,
            exmpp_jid:make("client", "localhost"),
            exmpp_jid:make("juliet", "example.com", "balcony"),
            Msg2}, 1800, Sessions),
    [KeyWith2] = dict:fetch_keys(NewSessions),
    ?assert(KeyWith2 =:=
        {{jid,<<"client@localhost">>,<<"client">>,<<"localhost">>,undefined},
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads2 = dict:to_list(dict:fetch(KeyWith2, NewSessions)),
    [{{no_thread, "balcony"}, {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 6}}, _, 1, "balcony"}}] = Threads2,
    ejabberd_storage:transaction(?HOST,
        fun() ->
            NewTS = {{2010, 1, 2}, {3, 4, 7}},
            mod_archive2_time:start(lists:duplicate(10, NewTS))
        end),
    [Msg3] =
        exmpp_xml:parse_document_fragment(?XML_MESSAGE_3, [{root_depth, 0}]),
    NewSessions2 =
        mod_archive2_auto:add_message({to,
            exmpp_jid:make("client", "localhost"),
            exmpp_jid:make("juliet", "example.com", "balcony"),
            Msg3}, 1800, NewSessions),
    [KeyWith3] = dict:fetch_keys(NewSessions2),
    ?assert(KeyWith3 =:=
        {{jid,<<"client@localhost">>,<<"client">>,<<"localhost">>,undefined},
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads3 = dict:to_list(dict:fetch(KeyWith3, NewSessions2)),
    [{{no_thread, "balcony"}, {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 7}}, _, 2, "balcony"}}] = Threads3,
    ejabberd_storage:transaction(?HOST,
        fun() ->
            NewTS = {{2010, 1, 2}, {4, 4, 7}},
            mod_archive2_time:start(lists:duplicate(10, NewTS))
        end),
    NewSessions3 =
        mod_archive2_auto:add_message({to,
            exmpp_jid:make("client", "localhost"),
            exmpp_jid:make("juliet", "example.com", undefined),
            Msg}, 1800, NewSessions2),
    [KeyWith4] = dict:fetch_keys(NewSessions3),
    ?assert(KeyWith4 =:=
        {{jid,<<"client@localhost">>,<<"client">>,<<"localhost">>,undefined},
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads4 = dict:to_list(dict:fetch(KeyWith4, NewSessions3)),
    [{{no_thread, undefined}, {session, {{2010, 1, 2}, {4, 4, 7}},
        {{2010, 1, 2}, {4, 4, 7}}, _, 0, undefined}}] = Threads4.

mysql_test_auto_thread_new_and_update() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'juliet') and (with_server = 'example.com') "
                 "and (with_resource is null) and (utc = '2010-01-02 03:04:05') "
                 "and (deleted <> 1)", {selected, [], []}},
                {"insert into archive_collection (prev_id, next_id, us, with_user, "
                 "with_server, with_resource, utc, change_utc, version, deleted, "
                 "subject, thread, crypt, extra) values (null, null, 'client@localhost', "
                 "'juliet', 'example.com', null, '2010-01-02 03:04:05', "
                 "'2010-01-02 03:04:05', 0, 0, null, 'thread123', null, null)",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{1}]}},
                {"insert into archive_message (coll_id, utc, direction, body, "
                 "name, jid) values (1, '2010-01-02 03:04:05', 1, 'Art thou not "
                 "Romeo, and a Montague?', null, null)", {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{1}]}},
                {},
                {},
                {"update archive_collection set with_resource = 'balcony', "
                 "change_utc = '2010-01-02 03:04:06', version = 1, "
                 "subject = 'Incompetence', thread = 'thread123' where id = 1",
                 {updated, 1}},
                {"insert into archive_message (coll_id, utc, direction, body, "
                 "name, jid) values (1, '2010-01-02 03:04:06', 0, "
                 "'That\\'s my line, idiot!!!', null, null)",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{2}]}},
                {},
                {},
                {"update archive_collection set with_resource = 'chamber', "
                 "change_utc = '2010-01-02 03:04:07', version = 2, "
                 "subject = 'Sorry', thread = 'thread123' where id = 1",
                 {updated, 1}},
                {"insert into archive_message (coll_id, utc, direction, body, "
                 "name, jid) values (1, '2010-01-02 03:04:07', 1, "
                 "'Sorry, that\\'s due to that extra beer ...', null, null)",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{3}]}},
                {}])
        end),
    common_test_auto_thread_new_and_update().

common_test_auto_thread_new_and_update() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            NewTS = {{2010, 1, 2}, {3, 4, 5}},
            mod_archive2_time:start(lists:duplicate(10, NewTS))
        end),
    [Msg] =
        exmpp_xml:parse_document_fragment(?XML_MESSAGE_4, [{root_depth, 0}]),
    Sessions =
        mod_archive2_auto:add_message({to,
            exmpp_jid:make("client", "localhost"),
            exmpp_jid:make("juliet", "example.com"),
            Msg}, 1800, dict:new()),
    [KeyWith] = dict:fetch_keys(Sessions),
    ?assert(KeyWith =:=
        {{jid,<<"client@localhost">>,<<"client">>,<<"localhost">>,undefined},
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads = dict:to_list(dict:fetch(KeyWith, Sessions)),
    [{"thread123", {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 5}}, _, 0, undefined}}] = Threads,
    ejabberd_storage:transaction(?HOST,
        fun() ->
            NewTS = {{2010, 1, 2}, {3, 4, 6}},
            mod_archive2_time:start(lists:duplicate(10, NewTS))
        end),
    [Msg2] =
        exmpp_xml:parse_document_fragment(?XML_MESSAGE_5, [{root_depth, 0}]),
    NewSessions =
        mod_archive2_auto:add_message({from,
            exmpp_jid:make("client", "localhost"),
            exmpp_jid:make("juliet", "example.com", "balcony"),
            Msg2}, 1800, Sessions),
    [KeyWith2] = dict:fetch_keys(NewSessions),
    ?assert(KeyWith2 =:=
        {{jid,<<"client@localhost">>,<<"client">>,<<"localhost">>,undefined},
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads2 = dict:to_list(dict:fetch(KeyWith2, NewSessions)),
    [{"thread123", {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 6}}, _, 1, "balcony"}}] = Threads2,
    ejabberd_storage:transaction(?HOST,
        fun() ->
            NewTS = {{2010, 1, 2}, {3, 4, 7}},
            mod_archive2_time:start(lists:duplicate(10, NewTS))
        end),
    [Msg3] =
        exmpp_xml:parse_document_fragment(?XML_MESSAGE_6, [{root_depth, 0}]),
    NewSessions2 =
        mod_archive2_auto:add_message({to,
            exmpp_jid:make("client", "localhost"),
            exmpp_jid:make("juliet", "example.com", "chamber"),
            Msg3}, 1800, NewSessions),
    [KeyWith3] = dict:fetch_keys(NewSessions2),
    ?assert(KeyWith3 =:=
        {{jid,<<"client@localhost">>,<<"client">>,<<"localhost">>,undefined},
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads3 = dict:to_list(dict:fetch(KeyWith3, NewSessions2)),
    [{"thread123", {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 7}}, _, 2, "chamber"}}] = Threads3.
