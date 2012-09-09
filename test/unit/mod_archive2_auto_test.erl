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

-define(JID, "client@localhost").

-define(SESSIONS_KEY, auto_sessions).

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

-define(MESSAGE7,
    {xmlel,'jabber:client',
        [{'jabber:client',none}],
        message,
        [{xmlattr,undefined,<<"type">>,<<"groupchat">>},
         {xmlattr,undefined,<<"from">>,<<"darkcave@chat.shakespeare.lit/thirdwitch">>},
         {xmlattr,undefined,<<"to">>,<<"client@localhost/desktop">>},
         {xmlattr,undefined,<<"id">>,<<"session-182531630">>}],
        [{xmlel,'jabber:client',[],body,[],
                [{xmlcdata,<<"Harpier cries: 'tis time, 'tis time.">>}]}]}).

-define(SESSIONS_TO_EXPIRE,
    {dict,2,16,16,8,80,48,
           {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
           {{[],[],[],[],[],[],
             [[{"client@localhost",
                {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>,
                 undefined}}|
               {dict,1,16,16,8,80,48,
                {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
                {{[],[],
                  [[{no_thread,undefined}|
                    {session,
                     {{2010,1,2},{4,4,7}},
                     {{2010,1,2},{4,4,7}},
                     1,0,undefined}]],
                  [],[],[],[],[],[],[],[],[],[],[],[],[]}}}]],
             [],
             [[{"client@localhost",
                {jid,<<"darkcave@chat.shakespeare.lit">>,<<"darkcave">>,
                 <<"chat.shakespeare.lit">>,undefined}}|
               {dict,1,16,16,8,80,48,
                {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
                {{[],[],
                  [[{no_thread,undefined}|
                    {session,
                     {{2010,1,2},{5,4,7}},
                     {{2010,1,2},{5,4,7}},
                     2,0,undefined}]],
                  [],[],[],[],[],[],[],[],[],[],[],[],[]}}}]],
             [],[],[],[],[],[],[]}}}).

-define(EXPIRED_SESSIONS,
    {dict,1,16,16,8,80,48,
       {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
       {{[],[],[],[],[],[],[],[],
         [[{"client@localhost",
            {jid,<<"darkcave@chat.shakespeare.lit">>,<<"darkcave">>,
             <<"chat.shakespeare.lit">>,undefined}}|
           {dict,1,16,16,8,80,48,
            {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
            {{[],[],
              [[{no_thread,undefined}|
                {session,
                 {{2010,1,2},{5,4,7}},
                 {{2010,1,2},{5,4,7}},
                 2,0,undefined}]],
              [],[],[],[],[],[],[],[],[],[],[],[],[]}}}]],
         [],[],[],[],[],[],[]}}}).


-define(REMOVE_RESULT,
{atomic,
 {dict,1,16,16,8,80,48,
  {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
  {{[],[],[],[],[],[],[],[],[],[],[],
    [[{"client@localhost",
       {jid,<<"darkcave@chat.shakespeare.lit">>,<<"darkcave">>,
        <<"chat.shakespeare.lit">>,undefined}}|
      {dict,1,16,16,8,80,48,
       {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
       {{[],[],
         [[{no_thread,undefined}|
           {session,{{2010,1,2},{5,4,7}},{{2010,1,2},{5,4,7}},_,0,undefined}]],
         [],[],[],[],[],[],[],[],[],[],[],[],[]}}}]],
    [],[],[],[]}}}}).

-define(RETRIEVE_RESULT,
    {atomic,
        {iq,response,result,<<"stanza-",_/binary>>,?NS_ARCHIVING,
            {xmlel,?NS_ARCHIVING,[],list,[],
                [{xmlel,undefined,[],chat,
                     [{xmlattr,undefined,<<"with">>,
                          <<"juliet@example.com/chamber">>},
                      {xmlattr,undefined,<<"start">>,
                          _},
                      {xmlattr,undefined,<<"version">>,<<"2">>}],
                     []},
                 {xmlel,undefined,[],chat,
                     [{xmlattr,undefined,<<"with">>,
                          <<"darkcave@chat.shakespeare.lit">>},
                      {xmlattr,undefined,<<"start">>,
                          _},
                      {xmlattr,undefined,<<"version">>,<<"0">>}],
                     []},
                 {xmlel,'http://jabber.org/protocol/rsm',[],set,[],
                     [{xmlel,'http://jabber.org/protocol/rsm',[],first,
                          [{xmlattr,undefined,<<"index">>,<<"0">>}],
                          [{xmlcdata,
                               _}]},
                      {xmlel,'http://jabber.org/protocol/rsm',[],last,
                          [],
                          [{xmlcdata,
                               _}]},
                      {xmlel,'http://jabber.org/protocol/rsm',[],count,
                          [],
                          [{xmlcdata,<<"2">>}]}]}]},
            undefined,undefined,'jabber:client'}}).

-define(HOST, "localhost").

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

mod_archive2_auto_common_test_() ->
    [
        ?test_gen0(common_test_expire)
    ].

mod_archive2_auto_mysql_test_() ->
{
    foreach,
    local,
    fun testing:mysql_tests_setup/0,
    fun testing:mysql_tests_teardown/1,
    [
        [
            ?test_gen0(mysql_test_auto_new_and_update),
            ?test_gen0(mysql_test_auto_thread_new_and_update),
            ?test_gen0(mysql_test_remove_open)
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
            ?test_gen0(common_test_auto_thread_new_and_update),
            ?test_gen0(mnesia_test_remove_open)
        ]
    ]
}.

mysql_test_auto_new_and_update() ->
    dbms_storage:transaction(?HOST,
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
                {"select LAST_INSERT_ID()", {selected, [], [{2}]}},
                {"insert into archive_message (coll_id, utc, direction, body, "
                 "name, jid) values (2, '2010-01-02 04:04:07', 1, 'Art thou not "
                 "Romeo, and a Montague?', null, null)", {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{4}]}},
                {},
                {},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'darkcave') and (with_server = 'chat.shakespeare.lit') "
                 "and (with_resource is null) and (utc = '2010-01-02 05:04:07') "
                 "and (deleted <> 1)", {selected, [], []}},
                {"insert into archive_collection (prev_id, next_id, us, with_user, "
                 "with_server, with_resource, utc, change_utc, version, deleted, "
                 "subject, thread, crypt, extra) values (null, null, "
                 "'client@localhost', 'darkcave', 'chat.shakespeare.lit', null, "
                 "'2010-01-02 05:04:07', '2010-01-02 05:04:07', 0, 0, null, "
                 "null, null, null)", {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{3}]}},
                {"insert into archive_message (coll_id, utc, direction, body, "
                 "name, jid) values (3, '2010-01-02 05:04:07', 0, "
                 "'Harpier cries: \\'tis time, \\'tis time.', 'thirdwitch', null)",
                 {selected, [], []}},
                {"select LAST_INSERT_ID()", {selected, [], [{5}]}},
                {}])
        end),
    common_test_auto_new_and_update().

common_test_auto_new_and_update() ->
    dbms_storage:transaction(?HOST,
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
            Msg}, 1800, body, dict:new()),
    [KeyWith] = dict:fetch_keys(Sessions),
    ?assert(KeyWith =:=
        {"client@localhost",
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads = dict:to_list(dict:fetch(KeyWith, Sessions)),
    [{{no_thread, undefined}, {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 5}}, _, 0, undefined}}] = Threads,
    dbms_storage:transaction(?HOST,
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
            Msg2}, 1800, body, Sessions),
    [KeyWith2] = dict:fetch_keys(NewSessions),
    ?assert(KeyWith2 =:=
        {"client@localhost",
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads2 = dict:to_list(dict:fetch(KeyWith2, NewSessions)),
    [{{no_thread, "balcony"}, {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 6}}, _, 1, "balcony"}}] = Threads2,
    dbms_storage:transaction(?HOST,
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
            Msg3}, 1800, body, NewSessions),
    [KeyWith3] = dict:fetch_keys(NewSessions2),
    ?assert(KeyWith3 =:=
        {"client@localhost",
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads3 = dict:to_list(dict:fetch(KeyWith3, NewSessions2)),
    [{{no_thread, "balcony"}, {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 7}}, _, 2, "balcony"}}] = Threads3,
    dbms_storage:transaction(?HOST,
        fun() ->
            NewTS = {{2010, 1, 2}, {4, 4, 7}},
            mod_archive2_time:start(lists:duplicate(10, NewTS))
        end),
    NewSessions3 =
        mod_archive2_auto:add_message({to,
            exmpp_jid:make("client", "localhost"),
            exmpp_jid:make("juliet", "example.com", undefined),
            Msg}, 1800, body, NewSessions2),
    [KeyWith4] = dict:fetch_keys(NewSessions3),
    ?assert(KeyWith4 =:=
        {"client@localhost",
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads4 = dict:to_list(dict:fetch(KeyWith4, NewSessions3)),
    [{{no_thread, undefined}, {session, {{2010, 1, 2}, {4, 4, 7}},
        {{2010, 1, 2}, {4, 4, 7}}, _, 0, undefined}}] = Threads4,
    dbms_storage:transaction(?HOST,
        fun() ->
            NewTS = {{2010, 1, 2}, {5, 4, 7}},
            mod_archive2_time:start(lists:duplicate(10, NewTS))
        end),
    NewSessions4 =
        mod_archive2_auto:add_message({from,
            exmpp_jid:make("client", "localhost", undefined),
            exmpp_jid:make("darkcave", "chat.shakespeare.lit", "thirdwitch"),
            ?MESSAGE7}, 1800, body, NewSessions3),
    put(?SESSIONS_KEY, NewSessions4),
    KeysWith5 = dict:fetch_keys(NewSessions4),
    ?assert(KeysWith5 =:=
        [{"client@localhost",
          {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>,undefined}},
         {"client@localhost",
          {jid,<<"darkcave@chat.shakespeare.lit">>,<<"darkcave">>,<<"chat.shakespeare.lit">>,undefined}}]),
    [KeyWith4 | [KeyWith5]] = KeysWith5,
    Threads5 = dict:to_list(dict:fetch(KeyWith5, NewSessions4)),
    [{{no_thread, undefined}, {session, {{2010, 1, 2}, {5, 4, 7}},
      {{2010, 1, 2}, {5, 4, 7}}, _, 0, undefined}}] = Threads5.

mysql_test_auto_thread_new_and_update() ->
    dbms_storage:transaction(?HOST,
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
    dbms_storage:transaction(?HOST,
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
            Msg}, 1800, body, dict:new()),
    [KeyWith] = dict:fetch_keys(Sessions),
    ?assert(KeyWith =:=
        {"client@localhost",
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads = dict:to_list(dict:fetch(KeyWith, Sessions)),
    [{"thread123", {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 5}}, _, 0, undefined}}] = Threads,
    dbms_storage:transaction(?HOST,
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
            Msg2}, 1800, body, Sessions),
    [KeyWith2] = dict:fetch_keys(NewSessions),
    ?assert(KeyWith2 =:=
        {"client@localhost",
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads2 = dict:to_list(dict:fetch(KeyWith2, NewSessions)),
    [{"thread123", {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 6}}, _, 1, "balcony"}}] = Threads2,
    dbms_storage:transaction(?HOST,
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
            Msg3}, 1800, body, NewSessions),
    [KeyWith3] = dict:fetch_keys(NewSessions2),
    ?assert(KeyWith3 =:=
        {"client@localhost",
         {jid,<<"juliet@example.com">>,<<"juliet">>,<<"example.com">>, undefined}}),
    Threads3 = dict:to_list(dict:fetch(KeyWith3, NewSessions2)),
    [{"thread123", {session, {{2010, 1, 2}, {3, 4, 5}},
        {{2010, 1, 2}, {3, 4, 7}}, _, 2, "chamber"}}] = Threads3.

mysql_test_remove_open() ->
    dbms_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"update archive_collection set deleted = 1 where (id = 2)",
                 {updated, 1}},
                 {"select id from archive_collection where (id = 2)",
                  {selected, [], [{2}]}},
                 {"update archive_collection set prev_id = null where (prev_id = 2)",
                  {updated, 1}},
                 {"update archive_collection set next_id = null where (next_id = 2)",
                  {updated, 1}},
                {},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1)",
                 {selected, [], [{2}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') and "
                 "(deleted <> 1) order by utc asc",
                 {selected, [],
                  [{1, "juliet", "example.com", "chamber", "2010-01-02 03:04:05", 2},
                   {3, "darkcave", "chat.shakespeare.lit", null, "2010-01-02 05:04:07", 0}]}},
                {"select count(*) from archive_collection where (us = 'client@localhost') "
                 "and (deleted <> 1) and ((utc < '2010-01-02 03:04:05') or "
                 "((utc = '2010-01-02 03:04:05') and (id < 1)))",
                 {selected, [], [{0}]}},
                {}])
        end),
    common_test_remove_open(mysql).

mnesia_test_remove_open() ->
    common_test_remove_open(mnesia).

common_test_remove_open(RDBMS) ->
    Sessions = get(?SESSIONS_KEY),
    ?REMOVE_RESULT =
        mod_archive2_management:remove(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, remove,
                        [exmpp_xml:attribute(<<"open">>, "true"),
                         exmpp_xml:attribute(<<"with">>, "juliet@example.com")], []))),
            RDBMS,
            Sessions),
    ?RETRIEVE_RESULT =
        mod_archive2_management:list(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, list, [], [])))).

common_test_expire() ->
    NewTS = {{2010, 1, 2}, {5, 33, 7}},
    mod_archive2_time:start(lists:duplicate(10, NewTS)),
    NewSessions = mod_archive2_auto:expire_sessions(?SESSIONS_TO_EXPIRE, 1800),
    ?EXPIRED_SESSIONS =  NewSessions.
