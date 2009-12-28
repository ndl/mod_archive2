%%%----------------------------------------------------------------------
%%% File    : mod_archive2_prefs_test.erl
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

-module(mod_archive2_prefs_test).
-author('ejabberd@ndl.kiev.ua').

-include("mod_archive2.hrl").
-include("testing.hrl").

-export([eunit_xml_report/1]).

-define(JID, "client@localhost/res").
-define(HOST, "localhost").

-define(PREFS_TC1_RETRIEVE_RESULT,
    {atomic,{iq,response,result,<<"stanza-",_/binary>>,?NS_ARCHIVING,
            {xmlel,?NS_ARCHIVING,[],pref,[],
                   [{xmlel,undefined,[],default,
                           [{xmlattr,undefined,save,<<"false">>},
                            {xmlattr,undefined,expire,<<"3600">>},
                            {xmlattr,undefined,otr,<<"forbid">>},
                            {xmlattr,undefined,unset,<<"true">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"auto">>},
                            {xmlattr,undefined,use,<<"concede">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"local">>},
                            {xmlattr,undefined,use,<<"concede">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"manual">>},
                            {xmlattr,undefined,use,<<"prefer">>}],
                           []},
                    {xmlel,undefined,[],auto,
                           [{xmlattr,undefined,save,<<"false">>},
                            {xmlattr,undefined,scope,<<"global">>}],
                           []},
                    {xmlel,undefined,[],auto,
                           [{xmlattr,undefined,save,<<"true">>},
                            {xmlattr,undefined,scope,<<"session">>}],
                           []}]},
            undefined,undefined,'jabber:client'}}).

-define(PREFS_TC2_RETRIEVE_RESULT,
    {atomic,{iq,response,result,<<"stanza-",_/binary>>,?NS_ARCHIVING,
            {xmlel,?NS_ARCHIVING,[],pref,[],
                   [{xmlel,undefined,[],item,
                           [{xmlattr,undefined,jid,<<"romeo@montague.net">>},
                            {xmlattr,undefined,exactmatch,<<"true">>},
                            {xmlattr,undefined,save,<<"body">>},
                            {xmlattr,undefined,expire,<<"3600">>},
                            {xmlattr,undefined,otr,<<"approve">>}],
                           []},
                    {xmlel,undefined,[],default,
                           [{xmlattr,undefined,save,<<"body">>},
                            {xmlattr,undefined,expire,<<"1800">>},
                            {xmlattr,undefined,otr,<<"prefer">>},
                            {xmlattr,undefined,unset,<<"false">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"auto">>},
                            {xmlattr,undefined,use,<<"forbid">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"local">>},
                            {xmlattr,undefined,use,<<"prefer">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"manual">>},
                            {xmlattr,undefined,use,<<"concede">>}],
                           []},
                    {xmlel,undefined,[],auto,
                           [{xmlattr,undefined,save,<<"false">>},
                            {xmlattr,undefined,scope,<<"global">>}],
                           []},
                    {xmlel,undefined,[],auto,
                           [{xmlattr,undefined,save,<<"true">>},
                            {xmlattr,undefined,scope,<<"session">>}],
                           []}]},
            undefined,undefined,'jabber:client'}}).

-define(PREFS_TC3_RETRIEVE_RESULT,
    {atomic,{iq,response,result,<<"stanza-",_/binary>>,?NS_ARCHIVING,
            {xmlel,?NS_ARCHIVING,[],pref,[],
                   [{xmlel,undefined,[],item,
                           [{xmlattr,undefined,jid,<<"romeo@montague.net">>},
                            {xmlattr,undefined,exactmatch,<<"true">>},
                            {xmlattr,undefined,save,<<"false">>},
                            {xmlattr,undefined,expire,<<"3600">>},
                            {xmlattr,undefined,otr,<<"concede">>}],
                           []},
                    {xmlel,undefined,[],default,
                           [{xmlattr,undefined,save,<<"false">>},
                            {xmlattr,undefined,expire,<<"1800">>},
                            {xmlattr,undefined,otr,<<"forbid">>},
                            {xmlattr,undefined,unset,<<"false">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"auto">>},
                            {xmlattr,undefined,use,<<"prefer">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"local">>},
                            {xmlattr,undefined,use,<<"prefer">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"manual">>},
                            {xmlattr,undefined,use,<<"concede">>}],
                           []},
                    {xmlel,undefined,[],auto,
                           [{xmlattr,undefined,save,<<"false">>},
                            {xmlattr,undefined,scope,<<"global">>}],
                           []},
                    {xmlel,undefined,[],auto,
                           [{xmlattr,undefined,save,<<"true">>},
                            {xmlattr,undefined,scope,<<"session">>}],
                           []}]},
            undefined,undefined,'jabber:client'}}).

-define(PREFS_TC4_RESULT,
    {atomic,
     {dict,1,16,16,8,80,48,
          {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
          {{[],[],[],[],
            [["client@localhost/res"|true]],
            [],[],[],[],[],[],[],[],[],[],[]}}}}).

-define(PREFS_TC5_RETRIEVE_RESULT,
    {atomic,{iq,response,result,<<"stanza-",_/binary>>,?NS_ARCHIVING,
            {xmlel,?NS_ARCHIVING,[],pref,[],
                   [{xmlel,undefined,[],default,
                           [{xmlattr,undefined,save,<<"false">>},
                            {xmlattr,undefined,expire,<<"1800">>},
                            {xmlattr,undefined,otr,<<"forbid">>},
                            {xmlattr,undefined,unset,<<"false">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"auto">>},
                            {xmlattr,undefined,use,<<"prefer">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"local">>},
                            {xmlattr,undefined,use,<<"prefer">>}],
                           []},
                    {xmlel,undefined,[],method,
                           [{xmlattr,undefined,type,<<"manual">>},
                            {xmlattr,undefined,use,<<"concede">>}],
                           []},
                    {xmlel,undefined,[],auto,
                           [{xmlattr,undefined,save,<<"true">>},
                            {xmlattr,undefined,scope,<<"global">>}],
                           []},
                    {xmlel,undefined,[],auto,
                           [{xmlattr,undefined,save,<<"true">>},
                            {xmlattr,undefined,scope,<<"session">>}],
                           []}]},
            undefined,undefined,'jabber:client'}}).

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

mod_archive2_prefs_mysql_test_() ->
{
    foreach,
    local,
    fun testing:mysql_tests_setup/0,
    fun testing:mysql_tests_teardown/1,
    [
        ?test_gen1(mysql_test_default_prefs),
        [
            ?test_gen0(mysql_test_set_prefs1),
            ?test_gen0(mysql_test_get_prefs1),
            ?test_gen0(mysql_test_should_auto_archive1),
            ?test_gen0(mysql_test_update_prefs1),
            ?test_gen0(mysql_test_get_prefs2),
            ?test_gen0(mysql_test_should_auto_archive2),
            ?test_gen0(mysql_test_itemremove_prefs1),
            ?test_gen0(mysql_test_auto1),
            ?test_gen0(mysql_test_auto2),
            ?test_gen0(mysql_test_get_prefs3),
            ?test_gen0(mysql_test_set_not_implemented_prefs1),
            ?test_gen0(mysql_test_set_not_implemented_prefs2),
            ?test_gen0(mysql_test_set_not_allowed_expire_prefs1)
        ]
    ]
}.

mod_archive2_prefs_mnesia_test_() ->
{
    foreach,
    local,
    fun testing:mnesia_tests_setup/0,
    fun testing:mnesia_tests_teardown/1,
    [
        ?test_gen1(common_test_default_prefs),
        [
            ?test_gen0(common_test_set_prefs1),
            ?test_gen0(common_test_get_prefs1),
            ?test_gen0(common_test_should_auto_archive1),
            ?test_gen0(common_test_update_prefs1),
            ?test_gen0(common_test_get_prefs2),
            ?test_gen0(common_test_should_auto_archive2),
            ?test_gen0(common_test_itemremove_prefs1),
            ?test_gen0(common_test_auto1),
            ?test_gen0(common_test_auto2),
            ?test_gen0(common_test_get_prefs3),
            ?test_gen0(common_test_set_not_implemented_prefs1),
            ?test_gen0(common_test_set_not_implemented_prefs2),
            ?test_gen0(common_test_set_not_allowed_expire_prefs1)
        ]
    ]
}.

mysql_test_default_prefs(Pid) ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_jid_prefs where (us = 'client@localhost')",
                 {selected, [], []}},
                {"select * from archive_global_prefs where us = 'client@localhost'",
                 {selected, [], []}},
                {}])
        end),
    common_test_default_prefs(Pid).

common_test_default_prefs(_Pid) ->
    ?PREFS_TC1_RETRIEVE_RESULT =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [], []))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            dict:from_list([{?JID, true}]),
            {0, infinity}).

mysql_test_set_prefs1() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_global_prefs where us = 'client@localhost'",
                 {selected, [], []}},
                {"insert into archive_global_prefs (us, save, expire, otr, "
                 "method_auto, method_local, method_manual, auto_save) values "
                 "('client@localhost', 0, 1800, 4, 1, 2, 0, null)",
                 {updated, 1}},
                {"select * from archive_jid_prefs where us = 'client@localhost' "
                 "and with_user = 'romeo' and with_server = 'montague.net' "
                 "and with_resource is null and exactmatch = 1",
                 {selected, [], []}},
                {"insert into archive_jid_prefs (us, with_user, with_server, "
                 "with_resource, exactmatch, save, expire, otr) values ('client@localhost', "
                 "'romeo', 'montague.net', null, 1, 0, 3600, 0)",
                 {updated, 1}},
                {}])
        end),
    common_test_set_prefs1().

common_test_set_prefs1() ->
    {atomic, ok} =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [],
                        [
                            exmpp_xml:element(undefined, default,
                                [exmpp_xml:attribute(otr, prefer),
                                 exmpp_xml:attribute(expire, 1800),
                                 exmpp_xml:attribute(save, body)], []),
                            exmpp_xml:element(undefined, method,
                                [exmpp_xml:attribute(type, auto),
                                 exmpp_xml:attribute(use, forbid)], []),
                            exmpp_xml:element(undefined, method,
                                [exmpp_xml:attribute(type, local),
                                 exmpp_xml:attribute(use, prefer)], []),
                            exmpp_xml:element(undefined, method,
                                [exmpp_xml:attribute(type, manual),
                                 exmpp_xml:attribute(use, concede)], []),
                            exmpp_xml:element(undefined, item,
                                [exmpp_xml:attribute(jid, "romeo@montague.net"),
                                 exmpp_xml:attribute(exactmatch, true),
                                 exmpp_xml:attribute(save, body),
                                 exmpp_xml:attribute(expire, 3600),
                                 exmpp_xml:attribute(otr, approve)], [])
                        ]))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            dict:from_list([{?JID, true}]),
            {0, infinity}).

mysql_test_get_prefs1() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_jid_prefs where (us = 'client@localhost')",
                 {selected, [], [{"client@localhost", "romeo", "montague.net",
                                  null, 1, 0, 3600, 0}]}},
                {"select * from archive_global_prefs where us = 'client@localhost'",
                 {selected, [], [{"client@localhost", 0, 1800, 4, 1, 2, 0, null}]}},
                {}])
        end),
    common_test_get_prefs1().

common_test_get_prefs1() ->
    ?PREFS_TC2_RETRIEVE_RESULT =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [], []))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            dict:from_list([{?JID, true}]),
            {0, infinity}).

mysql_test_should_auto_archive1() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_global_prefs where us = 'client@localhost'",
                 {selected, [], [{"client@localhost", 0, 1800, 4, 1, 2, 0, null}]}},
                {"select * from archive_jid_prefs where us = 'client@localhost' "
                 "and with_user = 'romeo' and with_server = 'montague.net' and "
                 "with_resource is null and exactmatch = 0",
                 {selected, [], []}},
                {"select * from archive_jid_prefs where us = 'client@localhost' "
                 "and with_user = 'romeo' and with_server = 'montague.net' and "
                 "with_resource is null and exactmatch = 1",
                 {selected, [], [{"client@localhost", "romeo", "montague.net",
                                  null, 1, 0, 3600, 0}]}},
                {}])
        end),
    common_test_should_auto_archive1().

common_test_should_auto_archive1() ->
    true =
        mod_archive2_prefs:should_auto_archive(
            exmpp_jid:parse(?JID),
            exmpp_jid:parse("romeo@montague.net"),
            dict:from_list([{?JID, true}]),
            mod_archive2_prefs:default_global_prefs(true, 3600)).

mysql_test_update_prefs1() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_global_prefs where us = 'client@localhost'",
                 {selected, [], [{"client@localhost", 0, 1800, 4, 1, 2, 0, null}]}},
                {"update archive_global_prefs set save = 1, otr = 2, "
                 "method_auto = 2 where us = 'client@localhost'",
                 {updated, 1}},
                {"select * from archive_jid_prefs where us = 'client@localhost' "
                 "and with_user = 'romeo' and with_server = 'montague.net' "
                 "and with_resource is null and exactmatch = 1",
                 {selected, [], [{"client@localhost", "romeo", "montague.net",
                                  null, 1, 0, 3600, 0}]}},
                {"update archive_jid_prefs set save = 1, otr = 1 where "
                 "us = 'client@localhost' and with_user = 'romeo' and "
                 "with_server = 'montague.net' and with_resource is null "
                 "and exactmatch = 1",
                 {updated, 1}},
                {}])
        end),
    common_test_update_prefs1().

common_test_update_prefs1() ->
    {atomic, ok} =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [],
                        [
                            exmpp_xml:element(undefined, default,
                                [exmpp_xml:attribute(otr, forbid),
                                 exmpp_xml:attribute(save, false)], []),
                            exmpp_xml:element(undefined, method,
                                [exmpp_xml:attribute(type, auto),
                                 exmpp_xml:attribute(use, prefer)], []),
                            exmpp_xml:element(undefined, item,
                                [exmpp_xml:attribute(jid, "romeo@montague.net"),
                                 exmpp_xml:attribute(exactmatch, true),
                                 exmpp_xml:attribute(save, false),
                                 exmpp_xml:attribute(otr, concede)], [])
                        ]))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            dict:from_list([{?JID, false}]),
            {0, infinity}).

mysql_test_get_prefs2() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_jid_prefs where (us = 'client@localhost')",
                 {selected, [], [{"client@localhost", "romeo", "montague.net",
                                  null, 1, 1, 3600, 1}]}},
                {"select * from archive_global_prefs where us = 'client@localhost'",
                 {selected, [], [{"client@localhost", 1, 1800, 2, 2, 2, 0, null}]}},
                {}])
        end),
    common_test_get_prefs2().

common_test_get_prefs2() ->
    ?PREFS_TC3_RETRIEVE_RESULT =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [], []))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            dict:from_list([{?JID, true}]),
            {0, infinity}).

mysql_test_should_auto_archive2() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_global_prefs where us = 'client@localhost'",
                 {selected, [], [{"client@localhost", 1, 1800, 2, 2, 2, 0, null}]}},
                {"select * from archive_jid_prefs where us = 'client@localhost' "
                 "and with_user = 'romeo' and with_server = 'montague.net' and "
                 "with_resource is null and exactmatch = 0",
                 {selected, [], []}},
                {"select * from archive_jid_prefs where us = 'client@localhost' "
                 "and with_user = 'romeo' and with_server = 'montague.net' and "
                 "with_resource is null and exactmatch = 1",
                 {selected, [], [{"client@localhost", "romeo", "montague.net",
                                  null, 1, 1, 3600, 1}]}},
                {}])
        end),
    common_test_should_auto_archive2().

common_test_should_auto_archive2() ->
    false =
        mod_archive2_prefs:should_auto_archive(
            exmpp_jid:parse(?JID),
            exmpp_jid:parse("romeo@montague.net"),
            dict:from_list([{?JID, true}]),
            mod_archive2_prefs:default_global_prefs(true, 3600)).

mysql_test_itemremove_prefs1() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"delete from archive_jid_prefs where us = 'client@localhost' "
                 "and with_user = 'romeo' and with_server = 'montague.net' "
                 "and with_resource is null and exactmatch = 1",
                 {updated, 1}},
                {}])
        end),
    common_test_itemremove_prefs1().

common_test_itemremove_prefs1() ->
    {atomic, ok} =
        mod_archive2_prefs:itemremove(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, itemremove, [],
                        [
                            exmpp_xml:element(undefined, item,
                                [exmpp_xml:attribute(jid, "romeo@montague.net"),
                                 exmpp_xml:attribute(exactmatch, "true")], [])
                        ])))).

mysql_test_auto1() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_global_prefs where us = 'client@localhost'",
                 {selected, [], [{"client@localhost", 1, 1800, 2, 2, 2, 0, null}]}},
                {"update archive_global_prefs set auto_save = 1 where "
                 "us = 'client@localhost'",
                 {updated, 1}},
                {}])
        end),
    common_test_auto1().

common_test_auto1() ->
    AutoStates = dict:from_list([{?JID, true}]),
    {atomic, AutoStates} =
        mod_archive2_prefs:auto(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, auto,
                        [exmpp_xml:attribute(save, "true"),
                         exmpp_xml:attribute(scope, "global")], []))),
            AutoStates).

mysql_test_auto2() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_global_prefs where us = 'client@localhost'",
                 {selected, [], [{"client@localhost", 1, 1800, 2, 2, 2, 0, null}]}},
                {"update archive_global_prefs set auto_save = 1 where "
                 "us = 'client@localhost'",
                 {updated, 1}},
                {}])
        end),
    common_test_auto2().

common_test_auto2() ->
    ?PREFS_TC4_RESULT =
        mod_archive2_prefs:auto(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, auto,
                        [exmpp_xml:attribute(save, "true"),
                         exmpp_xml:attribute(scope, "session")], []))),
            dict:new()).

mysql_test_get_prefs3() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_jid_prefs where (us = 'client@localhost')",
                 {selected, [], []}},
                {"select * from archive_global_prefs where us = 'client@localhost'",
                 {selected, [], [{"client@localhost", 1, 1800, 2, 2, 2, 0, 1}]}},
                {}])
        end),
    common_test_get_prefs3().

common_test_get_prefs3() ->
    ?PREFS_TC5_RETRIEVE_RESULT =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [], []))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            dict:from_list([{?JID, true}]),
            {0, infinity}).

mysql_test_set_not_implemented_prefs1() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select * from archive_global_prefs where us = 'client@localhost'",
                 {selected, [], []}},
                {"insert into archive_global_prefs (us, save, expire, otr, "
                 "method_auto, method_local, method_manual, auto_save) values "
                 "('client@localhost', 1, 1800, 2, 2, 2, 0, null)",
                 {updated, 1}},
                {"select * from archive_jid_prefs where us = 'client@localhost' "
                 "and with_user = 'romeo' and with_server = 'montague.net' "
                 "and with_resource is null and exactmatch = 1",
                 {selected, [], []}},
                {"insert into archive_jid_prefs (us, with_user, with_server, "
                 "with_resource, exactmatch, save, expire, otr) values ('client@localhost', "
                 "'romeo', 'montague.net', null, 1, 0, 3600, 0)",
                 {updated, 1}},
                {}])
        end),
    common_test_set_not_implemented_prefs1().

common_test_set_not_implemented_prefs1() ->
    {aborted, {throw, {error, 'not-implemented'}}} =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [],
                        [
                            exmpp_xml:element(undefined, default,
                                 [exmpp_xml:attribute(save, stream)], [])
                        ]))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            dict:from_list([{?JID, true}]),
            {0, infinity}).

mysql_test_set_not_implemented_prefs2() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {}])
        end),
    common_test_set_not_implemented_prefs2().

common_test_set_not_implemented_prefs2() ->
    {aborted, {throw, {error, 'not-implemented'}}} =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [],
                        [
                            exmpp_xml:element(undefined, item,
                                [exmpp_xml:attribute(jid, "romeo@montague.net"),
                                 exmpp_xml:attribute(save, message)], [])
                        ]))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            dict:from_list([{?JID, true}]),
            {0, infinity}).

mysql_test_set_not_allowed_expire_prefs1() ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {}])
        end),
    common_test_set_not_allowed_expire_prefs1().

common_test_set_not_allowed_expire_prefs1() ->
    {aborted, {throw, {error, 'not-allowed'}}} =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [],
                        [
                            exmpp_xml:element(undefined, item,
                                [exmpp_xml:attribute(jid, "romeo@montague.net"),
                                 exmpp_xml:attribute(expire, 1)], [])
                        ]))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            dict:from_list([{?JID, true}]),
            {0, 0}).