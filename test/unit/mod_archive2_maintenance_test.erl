%%%----------------------------------------------------------------------
%%% File    : mod_archive2_maintenance_test.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 maintenance functionality unit testing
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

-module(mod_archive2_maintenance_test).
-author('xmpp@endl.ch').

-include("mod_archive2.hrl").
-include("testing.hrl").
-include("mod_archive2_common_test_data.hrl").

-export([eunit_xml_report/1]).

-define(JID, "client@localhost").
-define(HOST, "localhost").

-define(MAINTENANCE_TC1_RETRIEVE_RESULT,
{atomic,
    {iq,response,result,<<"stanza-",_/binary>>,?NS_ARCHIVING,
        {xmlel,?NS_ARCHIVING,[],list,[],
            [{xmlel,undefined,[],chat,
                 [{xmlattr,undefined,<<"with">>,<<"juliet@capulet.com/chamber">>},
                  {xmlattr,undefined,<<"start">>,_},
                  {xmlattr,undefined,<<"version">>,<<"1">>}],
                 []},
             {xmlel,'http://jabber.org/protocol/rsm',[],set,[],
                 [{xmlel,'http://jabber.org/protocol/rsm',[],first,
                      [{xmlattr,undefined,<<"index">>,<<"0">>}],
                      [{xmlcdata,
                           _}]},
                  {xmlel,'http://jabber.org/protocol/rsm',[],last,[],
                      [{xmlcdata,
                           _}]},
                  {xmlel,'http://jabber.org/protocol/rsm',[],count,[],
                      [{xmlcdata,<<"1">>}]}]}]},
        undefined,undefined,'jabber:client'}}).


eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

mod_archive2_maintenance_mysql_test_() ->
{
    foreach,
    local,
    fun testing:mysql_tests_setup/0,
    fun testing:mysql_tests_teardown/1,
    [
        [
            ?test_gen0(mysql_test_expire)
        ]
    ]
}.

mod_archive2_maintenance_mnesia_test_() ->
{
    foreach,
    local,
    fun testing:mnesia_tests_setup/0,
    fun testing:mnesia_tests_teardown/1,
    [
        [
            ?test_gen0(mnesia_test_expire)
        ]
    ]
}.

mysql_test_expire() ->
    dbms_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {},
                {"insert into archive_collection (prev_id, next_id, us, with_user, "
                 "with_server, with_resource, utc, change_utc, version, deleted, "
                 "subject, thread, crypt, extra) values (null, null, "
                 "'client@localhost', 'juliet', 'capulet.com', 'chamber', "
                 "'2003-02-01 00:00:00.000000', '2002-12-31 23:59:59.007890', 1, 0, null, "
                 "null, null, null), (null, null, "
                 "'client@localhost', 'balcony', 'house.capulet.com', null, "
                 "'1469-07-21 03:16:37.000123', '2001-12-31 23:59:59.007890', 1, 1, null, "
                 "null, null, null), (null, null, "
                 "'client@localhost', 'benvolio', 'montague.net', null, "
                 "'1469-07-21 03:01:54.000123', '2000-12-31 23:59:59.007890', 1, 0, null, "
                 "null, null, null)",
                 {updated, 3}},
                {"select LAST_INSERT_ID()", {selected, [], [{3}]}},
                {},
                {"update archive_collection as ac left join archive_jid_prefs as "
                 "full_jid_prefs on ac.with_user = full_jid_prefs.with_user and "
                 "ac.with_server = full_jid_prefs.with_server and "
                 "ac.with_resource = full_jid_prefs.with_resource left join "
                 "archive_jid_prefs as bare_jid_prefs on ac.with_user = "
                 "bare_jid_prefs.with_user and ac.with_server = "
                 "bare_jid_prefs.with_server and bare_jid_prefs.with_resource = "
                 "'' and bare_jid_prefs.exactmatch = 0 left join "
                 "archive_jid_prefs as domain_jid_prefs on ac.with_server = "
                 "domain_jid_prefs.with_server and domain_jid_prefs.with_user = "
                 "'' and domain_jid_prefs.with_resource = '' and "
                 "domain_jid_prefs.exactmatch = 0 left join archive_global_prefs "
                 "on ac.us = archive_global_prefs.us set deleted = 1, "
                 "change_utc = '2004-01-01 00:00:00.000000', version = ac.version + 1 "
                 "where ac.deleted = 0 and not full_jid_prefs.expire is null and "
                 "timestampadd(second, full_jid_prefs.expire, ac.utc) < "
                 "'2004-01-01 00:00:00.000000' or not bare_jid_prefs.expire is null and "
                 "full_jid_prefs.expire is null and timestampadd(second, "
                 "bare_jid_prefs.expire, ac.utc) < '2004-01-01 00:00:00.000000' or not "
                 "domain_jid_prefs.expire is null and full_jid_prefs.expire is "
                 "null and bare_jid_prefs.expire is null and timestampadd(second, "
                 "domain_jid_prefs.expire, ac.utc) < '2004-01-01 00:00:00.000000' or "
                 "not archive_global_prefs.expire is null and "
                 "domain_jid_prefs.expire is null and full_jid_prefs.expire is "
                 "null and bare_jid_prefs.expire is null and timestampadd(second, "
                 "archive_global_prefs.expire, ac.utc) < '2004-01-01 00:00:00.000000'",
                 {updated, 1}},
                {},
                {"update archive_collection as ac left join archive_jid_prefs as "
                 "full_jid_prefs on ac.with_user = full_jid_prefs.with_user and "
                 "ac.with_server = full_jid_prefs.with_server and "
                 "ac.with_resource = full_jid_prefs.with_resource left join "
                 "archive_jid_prefs as bare_jid_prefs on ac.with_user = "
                 "bare_jid_prefs.with_user and ac.with_server = "
                 "bare_jid_prefs.with_server and bare_jid_prefs.with_resource = "
                 "'' and bare_jid_prefs.exactmatch = 0 left join "
                 "archive_jid_prefs as domain_jid_prefs on ac.with_server = "
                 "domain_jid_prefs.with_server and domain_jid_prefs.with_user = "
                 "'' and domain_jid_prefs.with_resource = '' and "
                 "domain_jid_prefs.exactmatch = 0 left join archive_global_prefs "
                 "on ac.us = archive_global_prefs.us set deleted = 1, "
                 "change_utc = '2004-01-01 00:00:01.000000', version = ac.version + 1 "
                 "where ac.deleted = 0 and not full_jid_prefs.expire is null and "
                 "timestampadd(second, full_jid_prefs.expire, ac.utc) < "
                 "'2004-01-01 00:00:01.000000' or not bare_jid_prefs.expire is null and "
                 "full_jid_prefs.expire is null and timestampadd(second, "
                 "bare_jid_prefs.expire, ac.utc) < '2004-01-01 00:00:01.000000' or not "
                 "domain_jid_prefs.expire is null and full_jid_prefs.expire is "
                 "null and bare_jid_prefs.expire is null and timestampadd(second, "
                 "domain_jid_prefs.expire, ac.utc) < '2004-01-01 00:00:01.000000' or "
                 "not archive_global_prefs.expire is null and "
                 "domain_jid_prefs.expire is null and full_jid_prefs.expire is "
                 "null and bare_jid_prefs.expire is null and timestampadd(second, "
                 "archive_global_prefs.expire, ac.utc) < '2004-01-01 00:00:01.000000' "
                 "or archive_global_prefs.expire is null and domain_jid_prefs.expire is "
                 "null and full_jid_prefs.expire is null and bare_jid_prefs.expire is "
                 "null and timestampadd(second, 31536000, ac.utc) < "
                 "'2004-01-01 00:00:01.000000'",
                 {updated, 1}},
                {"delete from archive_collection where deleted = 1 and "
                 "timestampadd(second, 31536000, archive_collection.change_utc) < "
                 "'2004-01-01 00:00:01.000000'",
                 {updated, 1}},
                {},
                {"select count(*) from archive_collection where (us = 'client@localhost') "
                 "and (deleted <> 1)",
                 {selected, [], [{1}]}},
                {"select id, with_user, with_server, with_resource, utc, version "
                 "from archive_collection where (us = 'client@localhost') and "
                 "(deleted <> 1) order by utc, id asc",
                 {selected, [], [{1, "juliet", "capulet.com", "chamber",
                                 "2003-02-01 00:00:00.000000", 1}]}},
                {"select count(*) from archive_collection where "
                 "(us = 'client@localhost') and (deleted <> 1) and "
                 "((utc < '2003-02-01 00:00:00.000000') or ((utc = '2003-02-01 00:00:00.000000') "
                 "and (id < 1)))",
                 {selected, [], [{0}]}},
                {}])
        end),
    common_test_expire(mysql).

mnesia_test_expire() ->
    common_test_expire(mnesia).

common_test_expire(RDBMS) ->
    dbms_storage:transaction(?HOST,
        fun() ->
            NewTS = {{2004, 1, 1}, {0, 0, 0, 0}},
            mod_archive2_time:start([NewTS])
        end),
    {atomic, {inserted, 3, _Key}} =
        dbms_storage:transaction(?HOST,
            fun() ->
                dbms_storage:insert([
                    (?ARCHIVE_COLLECTION1)#archive_collection{
                      utc = {{2003, 02, 01}, {0, 0, 0, 0}}},
                    (?ARCHIVE_COLLECTION2)#archive_collection{
                      deleted = true},
                    ?ARCHIVE_COLLECTION3])
            end),
    {atomic, ok} =
        mod_archive2_maintenance:expire_collections(
            ?HOST, infinity, infinity, RDBMS),
    {atomic, ok} =
        mod_archive2_maintenance:expire_collections(
            ?HOST, 365 * 24 * 3600, 365 * 24 * 3600, RDBMS),
    ?MAINTENANCE_TC1_RETRIEVE_RESULT =
        mod_archive2_management:list(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, list, [], [])))).
