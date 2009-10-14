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
        ?test_gen1(mysql_test_upload)
    ]
}.

mod_archive2_manual_mnesia_test_() ->
{
    foreach,
    local,
    fun testing:mnesia_tests_setup/0,
    fun testing:mnesia_tests_teardown/1,
    [
        ?test_gen1(common_test_upload)
    ]
}.

mysql_test_upload(Pid) ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"select id from archive_collection where (with_user = 'balcony') "
                 "and (with_server = 'house.capulet.com') and (with_resource = null) "
                 "and (utc = '1469-07-21 03:16:37')",
                 {selected, [], []}},
                {"select id from archive_collection where (with_user = 'benvolio') "
                 "and (with_server = 'montague.net') and (with_resource = null) "
                 "and (utc = '1469-07-21 03:01:54')",
                 {selected, [], []}},
                {"select id from archive_collection where (with_user = 'juliet') "
                 "and (with_server = 'capulet.com') and (with_resource = 'chamber') "
                 "and (utc = '1469-07-21 02:56:15')",
                 {selected, [], []}},
                {ignore, {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{1}]}},
                {"insert into archive_message values (null, 1, '1469-07-21 02:56:15', "
                 "0, 'Art thou not Romeo, and a Montague?', null, null)",
                 {updated, 1}},
                {"insert into archive_message values (null, 1, '1469-07-21 02:56:26', "
                 "1, 'Neither, fair saint, if either thee dislike.', null, null)",
                 {updated, 1}},
                {"insert into archive_message values (null, 1, '1469-07-21 03:04:35', "
                 "2, 'I think she might fancy me.', null, null)",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{3}]}},
                {}])
        end),
    common_test_upload(Pid).

common_test_upload(_Pid) ->
    ?MANUAL_TC1_SAVE_RESULT =
        mod_archive2_manual:save(
            exmpp_jid:parse(?JID),
            exmpp_iq:xmlel_to_iq(
                exmpp_iq:get(?NS_JABBER_CLIENT,
                    exmpp_xml:element(?NS_ARCHIVING, save,
                        [],
                        [?ARCHIVE_COLLECTION_COMPLETE])))).