%%%----------------------------------------------------------------------
%%% File    : mod_archive2_prefs_test.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 collections management commands unit testing
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

-module(mod_archive2_prefs_test).
-author('xmpp@endl.ch').

-include("mod_archive2.hrl").
-include("testing.hrl").

-export([eunit_xml_report/1]).

-define(XMPP_API_MOCK, xmpp_api_mock).
-define(JID, "client@localhost/res").
-define(HOST, "localhost").
-define(PREFS_THREADS_EXPIRATION, 1800).

-define(PREFS_TC1_RETRIEVE_RESULT,
    {atomic,{iq,response,result,<<"stanza-",_/binary>>,?NS_ARCHIVING,
            {xmlel,?NS_ARCHIVING,[],pref,[],
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
                           []},
                    {xmlel,'urn:xmpp:archive',[],auto,
                           [{xmlattr,undefined,<<"save">>,<<"true">>},
                            {xmlattr,undefined,<<"scope">>,<<"stream">>}],
                           []}]},
            undefined,undefined,'jabber:client'}}).

-define(PREFS_TC2_RETRIEVE_RESULT,
    {atomic,{iq,response,result,<<"stanza-",_/binary>>,?NS_ARCHIVING,
            {xmlel,?NS_ARCHIVING,[],pref,[],
                   [{xmlel,'urn:xmpp:archive',[],item,
                           [{xmlattr,undefined,<<"jid">>,<<"romeo@montague.net">>},
                            {xmlattr,undefined,<<"exactmatch">>,<<"true">>},
                            {xmlattr,undefined,<<"save">>,<<"body">>},
                            {xmlattr,undefined,<<"expire">>,<<"3600">>},
                            {xmlattr,undefined,<<"otr">>,<<"approve">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],default,
                           [{xmlattr,undefined,<<"save">>,<<"body">>},
                            {xmlattr,undefined,<<"expire">>,<<"1800">>},
                            {xmlattr,undefined,<<"otr">>,<<"prefer">>},
                            {xmlattr,undefined,<<"unset">>,<<"false">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],method,
                           [{xmlattr,undefined,<<"type">>,<<"auto">>},
                            {xmlattr,undefined,<<"use">>,<<"forbid">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],method,
                           [{xmlattr,undefined,<<"type">>,<<"local">>},
                            {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],method,
                           [{xmlattr,undefined,<<"type">>,<<"manual">>},
                            {xmlattr,undefined,<<"use">>,<<"concede">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],auto,
                           [{xmlattr,undefined,<<"save">>,<<"false">>},
                            {xmlattr,undefined,<<"scope">>,<<"global">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],auto,
                           [{xmlattr,undefined,<<"save">>,<<"true">>},
                            {xmlattr,undefined,<<"scope">>,<<"stream">>}],
                           []}]},
            undefined,undefined,'jabber:client'}}).

-define(PREFS_TC3_RETRIEVE_RESULT,
    {atomic,{iq,response,result,<<"stanza-",_/binary>>,?NS_ARCHIVING,
            {xmlel,?NS_ARCHIVING,[],pref,[],
                   [{xmlel,'urn:xmpp:archive',[],item,
                           [{xmlattr,undefined,<<"jid">>,<<"romeo@montague.net">>},
                            {xmlattr,undefined,<<"exactmatch">>,<<"true">>},
                            {xmlattr,undefined,<<"save">>,<<"false">>},
                            {xmlattr,undefined,<<"expire">>,<<"3600">>},
                            {xmlattr,undefined,<<"otr">>,<<"concede">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],default,
                           [{xmlattr,undefined,<<"save">>,<<"false">>},
                            {xmlattr,undefined,<<"expire">>,<<"1800">>},
                            {xmlattr,undefined,<<"otr">>,<<"forbid">>},
                            {xmlattr,undefined,<<"unset">>,<<"false">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],method,
                           [{xmlattr,undefined,<<"type">>,<<"auto">>},
                            {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],method,
                           [{xmlattr,undefined,<<"type">>,<<"local">>},
                            {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],method,
                           [{xmlattr,undefined,<<"type">>,<<"manual">>},
                            {xmlattr,undefined,<<"use">>,<<"concede">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],auto,
                           [{xmlattr,undefined,<<"save">>,<<"false">>},
                            {xmlattr,undefined,<<"scope">>,<<"global">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],auto,
                           [{xmlattr,undefined,<<"save">>,<<"true">>},
                            {xmlattr,undefined,<<"scope">>,<<"stream">>}],
                           []}]},
            undefined,undefined,'jabber:client'}}).

-define(PREFS_TC4_RESULT,
    {atomic,
     {auto_states,
      {dict,1,16,16,8,80,48,
       {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
       {{[],[],[],[],[],[],[],
         [["client@localhost"|
           {dict,1,16,16,8,80,48,
            {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
            {{[],
              [["res"|{auto_state,true,
              {dict,0,16,16,8,80,48,
               {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
               {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}},
              {dict,0,16,16,8,80,48,
               {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
               {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}}]],
              [],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}]],
         [],[],[],[],[],[],[],[]}}}}}).

-define(SHOULD_AUTO_ARCHIVE1,
{body,
 {dict,1,16,16,8,80,48,
  {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
  {{[],[],[],[],[],[],[],
    [["client@localhost"|
      {dict,1,16,16,8,80,48,
       {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
       {{[],
         [["res"|
           {auto_state,true,
            {dict,1,16,16,8,80,48,
             {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
             {{[],[],[],[],[],[],[],[],
               [[{jid,<<"romeo@montague.net">>,<<"romeo">>,
                  <<"montague.net">>,undefined}|
                 body]],
               [],[],[],[],[],[],[]}}},
            {dict,0,16,16,8,80,48,
             {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
             {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}}]],
         [],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}]],
    [],[],[],[],[],[],[],[]}}}}).

-define(SHOULD_AUTO_ARCHIVE_NOT_IN_ROSTER,
{false,
    {dict,1,16,16,8,80,48,
     {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
     {{[],[],[],[],[],[],[],
       [["client@localhost"|
         {dict,1,16,16,8,80,48,
          {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
          {{[],
            [["res"|
              {auto_state,true,
               {dict,0,16,16,8,80,48,
                {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
                {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}},
               {dict,0,16,16,8,80,48,
                {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
                {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}}]],
            [],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}]],
       [],[],[],[],[],[],[],[]}}}}).

-define(SHOULD_AUTO_ARCHIVE2,
    {false,
     {dict,1,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],
        [["client@localhost"|
          {dict,1,16,16,8,80,48,
           {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
           {{[],
             [["res"|
               {auto_state,true,
                {dict,1,16,16,8,80,48,
                 {[],[],[],[],[],[],[],[],[],[],[],[],[],[],
                  [],[]},
                 {{[],[],[],[],[],[],[],[],
                   [[{jid,<<"romeo@montague.net">>,
                      <<"romeo">>,<<"montague.net">>,
                      undefined}|
                     false]],
                   [],[],[],[],[],[],[]}}},
                {dict,0,16,16,8,80,48,
                 {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
                 {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}}]],
             [],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}]],
        [],[],[],[],[],[],[],[]}}}}).

-define(PREFS_TC5_RETRIEVE_RESULT,
    {atomic,{iq,response,result,<<"stanza-",_/binary>>,?NS_ARCHIVING,
            {xmlel,?NS_ARCHIVING,[],pref,[],
                   [{xmlel,'urn:xmpp:archive',[],default,
                           [{xmlattr,undefined,<<"save">>,<<"false">>},
                            {xmlattr,undefined,<<"expire">>,<<"1800">>},
                            {xmlattr,undefined,<<"otr">>,<<"forbid">>},
                            {xmlattr,undefined,<<"unset">>,<<"false">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],method,
                           [{xmlattr,undefined,<<"type">>,<<"auto">>},
                            {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                           []},
                    {xmlel,'urn:xmpp:archive',[],method,
                           [{xmlattr,undefined,<<"type">>,<<"local">>},
                            {xmlattr,undefined,<<"use">>,<<"prefer">>}],
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
                           [{xmlattr,undefined,<<"save">>,<<"true">>},
                            {xmlattr,undefined,<<"scope">>,<<"stream">>}],
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
            ?test_gen0(mysql_test_should_auto_archive_not_in_roster),
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
            ?test_gen0(common_test_should_auto_archive_not_in_roster),
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

mod_archive2_prefs_test_() ->
{
    foreach,
    local,
    fun prefs_tests_setup/0,
    fun prefs_tests_teardown/1,
    [
        ?test_gen1(test_prefs_cache_expire),
        ?test_gen1(test_prefs_threads_expire),
        ?test_gen1(test_prefs_threads_expiration_reset),
        ?test_gen1(test_prefs_thread_stream_override)
    ]
}.

prefs_tests_setup() -> ok.
prefs_tests_teardown(_) -> ok.

create_auto_states(Value) ->
    dict:from_list([{exmpp_jid:bare_to_list(exmpp_jid:parse(?JID)),
       dict:from_list([{exmpp_jid:resource_as_list(exmpp_jid:parse(?JID)),
                        {auto_state, Value, dict:new(), dict:new()}}])}]).

create_auto_states_with(Value) ->
    dict:from_list([{exmpp_jid:bare_to_list(exmpp_jid:parse(?JID)),
       dict:from_list([{exmpp_jid:resource_as_list(exmpp_jid:parse(?JID)),
                        {auto_state, Value,
                         dict:from_list([{exmpp_jid:parse(
                             "romeo@montague.net/romeo"), Value}]),
                         dict:new()}}])}]).

create_auto_states_threads(StreamAutoSave, Threads) ->
    dict:from_list([{exmpp_jid:bare_to_list(exmpp_jid:parse(?JID)),
       dict:from_list([{exmpp_jid:resource_as_list(exmpp_jid:parse(?JID)),
                        {auto_state, StreamAutoSave,
                         dict:new(),
                         dict:from_list(Threads)}}])}]).

mysql_test_default_prefs(Pid) ->
    dbms_storage:transaction(?HOST,
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
            create_auto_states(true),
            {0, infinity},
            ?PREFS_THREADS_EXPIRATION,
            ?XMPP_API_MOCK).

mysql_test_set_prefs1() ->
    dbms_storage:transaction(?HOST,
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
    CleanedAutoStates = create_auto_states(true),
    {atomic, {auto_states, CleanedAutoStates}} =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [],
                        [
                            exmpp_xml:element(undefined, default,
                                [exmpp_xml:attribute(<<"otr">>, prefer),
                                 exmpp_xml:attribute(<<"expire">>, 1800),
                                 exmpp_xml:attribute(<<"save">>, body)], []),
                            exmpp_xml:element(undefined, method,
                                [exmpp_xml:attribute(<<"type">>, auto),
                                 exmpp_xml:attribute(<<"use">>, forbid)], []),
                            exmpp_xml:element(undefined, method,
                                [exmpp_xml:attribute(<<"type">>, local),
                                 exmpp_xml:attribute(<<"use">>, prefer)], []),
                            exmpp_xml:element(undefined, method,
                                [exmpp_xml:attribute(<<"type">>, manual),
                                 exmpp_xml:attribute(<<"use">>, concede)], []),
                            exmpp_xml:element(undefined, item,
                                [exmpp_xml:attribute(<<"jid">>, "romeo@montague.net"),
                                 exmpp_xml:attribute(<<"exactmatch">>, true),
                                 exmpp_xml:attribute(<<"save">>, body),
                                 exmpp_xml:attribute(<<"expire">>, 3600),
                                 exmpp_xml:attribute(<<"otr">>, approve)], [])
                        ]))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            create_auto_states_with(true),
            {0, infinity},
            ?PREFS_THREADS_EXPIRATION,
            ?XMPP_API_MOCK).

mysql_test_get_prefs1() ->
    dbms_storage:transaction(?HOST,
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
            create_auto_states(true),
            {0, infinity},
            ?PREFS_THREADS_EXPIRATION,
            ?XMPP_API_MOCK).

mysql_test_should_auto_archive1() ->
    dbms_storage:transaction(?HOST,
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
    ?SHOULD_AUTO_ARCHIVE1 =
        mod_archive2_prefs:should_auto_archive(
            exmpp_jid:parse(?JID),
            exmpp_jid:parse("romeo@montague.net"),
            create_auto_states(true),
            mod_archive2_prefs:default_global_prefs(true, 3600),
            true,
            xmpp_api_mock,
            false,
            undefined).

mysql_test_should_auto_archive_not_in_roster() ->
    dbms_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {}])
        end),
    common_test_should_auto_archive_not_in_roster().

common_test_should_auto_archive_not_in_roster() ->
    ?SHOULD_AUTO_ARCHIVE_NOT_IN_ROSTER =
        mod_archive2_prefs:should_auto_archive(
            exmpp_jid:parse(?JID),
            exmpp_jid:parse("romeo@montague.net"),
            create_auto_states(true),
            mod_archive2_prefs:default_global_prefs(true, 3600),
            true,
            xmpp_api_mock,
            true,
            undefined).

mysql_test_update_prefs1() ->
    dbms_storage:transaction(?HOST,
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
    CleanedAutoStates = create_auto_states(false),
    {atomic, {auto_states, CleanedAutoStates}} =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [],
                        [
                            exmpp_xml:element(undefined, default,
                                [exmpp_xml:attribute(<<"otr">>, forbid),
                                 exmpp_xml:attribute(<<"save">>, false)], []),
                            exmpp_xml:element(undefined, method,
                                [exmpp_xml:attribute(<<"type">>, auto),
                                 exmpp_xml:attribute(<<"use">>, prefer)], []),
                            exmpp_xml:element(undefined, item,
                                [exmpp_xml:attribute(<<"jid">>, "romeo@montague.net"),
                                 exmpp_xml:attribute(<<"exactmatch">>, true),
                                 exmpp_xml:attribute(<<"save">>, false),
                                 exmpp_xml:attribute(<<"otr">>, concede)], [])
                        ]))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            create_auto_states_with(false),
            {0, infinity},
            ?PREFS_THREADS_EXPIRATION,
            ?XMPP_API_MOCK).

mysql_test_get_prefs2() ->
    dbms_storage:transaction(?HOST,
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
            create_auto_states(true),
            {0, infinity},
            ?PREFS_THREADS_EXPIRATION,
            ?XMPP_API_MOCK).

mysql_test_should_auto_archive2() ->
    dbms_storage:transaction(?HOST,
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
    ?SHOULD_AUTO_ARCHIVE2 =
        mod_archive2_prefs:should_auto_archive(
            exmpp_jid:parse(?JID),
            exmpp_jid:parse("romeo@montague.net"),
            create_auto_states(true),
            mod_archive2_prefs:default_global_prefs(true, 3600),
            true,
            xmpp_api_mock,
            false,
            undefined).

mysql_test_itemremove_prefs1() ->
    dbms_storage:transaction(?HOST,
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
    CleanedAutoStates = create_auto_states(true),
    {atomic, {auto_states, CleanedAutoStates}} =
        mod_archive2_prefs:itemremove(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, itemremove, [],
                        [
                            exmpp_xml:element(undefined, item,
                                [exmpp_xml:attribute(<<"jid">>, "romeo@montague.net"),
                                 exmpp_xml:attribute(<<"exactmatch">>, "true")], [])
                        ]))),
            create_auto_states_with(true),
            ?XMPP_API_MOCK).

mysql_test_auto1() ->
    dbms_storage:transaction(?HOST,
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
    AutoStates = create_auto_states(true),
    {atomic, {auto_states, AutoStates}} =
        mod_archive2_prefs:auto(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, auto,
                        [exmpp_xml:attribute(<<"save">>, "true"),
                         exmpp_xml:attribute(<<"scope">>, "global")], []))),
            AutoStates,
            ?XMPP_API_MOCK).

mysql_test_auto2() ->
    dbms_storage:transaction(?HOST,
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
                        [exmpp_xml:attribute(<<"save">>, "true"),
                         exmpp_xml:attribute(<<"scope">>, "stream")], []))),
            dict:new(),
            ?XMPP_API_MOCK).

mysql_test_get_prefs3() ->
    dbms_storage:transaction(?HOST,
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
            create_auto_states(true),
            {0, infinity},
            ?PREFS_THREADS_EXPIRATION,
            ?XMPP_API_MOCK).

mysql_test_set_not_implemented_prefs1() ->
    dbms_storage:transaction(?HOST,
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
    {aborted, {throw, {error, 'feature-not-implemented'}}} =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [],
                        [
                            exmpp_xml:element(undefined, default,
                                 [exmpp_xml:attribute(<<"save">>, stream)], [])
                        ]))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            create_auto_states(true),
            {0, infinity},
            ?PREFS_THREADS_EXPIRATION,
            ?XMPP_API_MOCK).

mysql_test_set_not_implemented_prefs2() ->
    dbms_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {}])
        end),
    common_test_set_not_implemented_prefs2().

common_test_set_not_implemented_prefs2() ->
    {aborted, {throw, {error, 'feature-not-implemented'}}} =
        mod_archive2_prefs:pref(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:set(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, pref, [],
                        [
                            exmpp_xml:element(undefined, item,
                                [exmpp_xml:attribute(<<"jid">>, "romeo@montague.net"),
                                 exmpp_xml:attribute(<<"save">>, stream)], [])
                        ]))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            create_auto_states(true),
            {0, infinity},
            ?PREFS_THREADS_EXPIRATION,
            ?XMPP_API_MOCK).

mysql_test_set_not_allowed_expire_prefs1() ->
    dbms_storage:transaction(?HOST,
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
                                [exmpp_xml:attribute(<<"jid">>, "romeo@montague.net"),
                                 exmpp_xml:attribute(<<"expire">>, 1)], [])
                        ]))),
            mod_archive2_prefs:default_global_prefs(false, 3600),
            create_auto_states(true),
            {0, 0},
            ?PREFS_THREADS_EXPIRATION,
            ?XMPP_API_MOCK).

test_prefs_cache_expire(_) ->
    AutoStates = create_auto_states(true),
    AutoStates =
        mod_archive2_prefs:expire_prefs_cache(create_auto_states_with(true)).

test_prefs_threads_expire(_) ->
    Time1 = {{2010, 1, 2}, {3, 4, 5, 0}},
    Time2 = {{2010, 1, 2}, {3, 4, 7, 0}},
    Time3 = {{2010, 1, 2}, {3, 34, 6, 0}},
    AutoStates = create_auto_states_threads(
        true,
        [{<<"123">>, {thread_info, Time1, body}},
         {<<"456">>, {thread_info, Time2, message}}]),
    AutoStates2 = create_auto_states_threads(true, [{<<"456">>, {thread_info, Time2, message}}]),
    mod_archive2_time:start([Time3]),
    % Should expire first thread but not the second one.
    AutoStates2 =
        mod_archive2_prefs:expire_threads(AutoStates, 1800).

test_prefs_threads_expiration_reset(_) ->
    Time1 = {{2010, 1, 2}, {3, 4, 5, 0}},
    Time2 = {{2010, 1, 2}, {3, 4, 7, 0}},
    Time3 = {{2010, 1, 2}, {3, 34, 6, 0}},
    AutoStates = create_auto_states_threads(
        true,
        [{<<"123">>, {thread_info, Time1, body}},
         {<<"456">>, {thread_info, Time2, message}}]),
    AutoStates2 = create_auto_states_threads(
        true,
        [{<<"123">>, {thread_info, Time3, body}},
         {<<"456">>, {thread_info, Time2, message}}]),
    mod_archive2_time:start([Time3]),
    % Should update the expiration timestamp for the first thread.
    AutoStates2 =
        mod_archive2_prefs:reset_thread_expiration(exmpp_jid:parse(?JID), <<"123">>, AutoStates).

test_prefs_thread_stream_override(_) ->
    Time1 = {{2010, 1, 2}, {3, 4, 5, 0}},
    {message, _} =
        mod_archive2_prefs:should_auto_archive(
            exmpp_jid:parse(?JID),
            exmpp_jid:parse("romeo@montague.net"),
            create_auto_states_threads(
                false,
                [{<<"123">>, {thread_info, Time1, body}},
                 {<<"456">>, {thread_info, Time1, message}}]),
            mod_archive2_prefs:default_global_prefs(true, 3600),
            true,
            xmpp_api_mock,
            false,
            <<"456">>).
