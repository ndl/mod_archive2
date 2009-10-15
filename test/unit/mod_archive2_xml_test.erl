%%%----------------------------------------------------------------------
%%% File    : mod_archive2_xml_test.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 xml conversion functionality unit testing
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

-module(mod_archive2_xml_test).
-author('ejabberd@ndl.kiev.ua').

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
            utc = {{1469, 07, 21}, {02, 56, 15}},
            version = 1,
            deleted = 0,
            subject = "Subject",
            thread = "12345",
            crypt = true,
            extra = #xmlel{name=x,children=[#xmlel{name=test}]}}).

-define(ARCHIVE_COLLECTION_NO_LINKS_XML,
        {xmlel,undefined,[],chat,
         [{xmlattr,undefined,with,<<"juliet@capulet.com/chamber">>},
          {xmlattr,undefined,start,<<"1469-07-21T02:56:15.000000Z">>},
          {xmlattr,undefined,subject,<<"Subject">>},
          {xmlattr,undefined,thread,<<"12345">>},
          {xmlattr,undefined,crypt,<<"true">>},
          {xmlattr,undefined,version,<<"1">>}],
         [{xmlel,undefined,[],x,[],[{xmlel,undefined,[],test,[],[]}]}]}).

-define(ARCHIVE_COLLECTION_WITH_LINKS_XML,
        {xmlel,undefined,[],chat,
         [{xmlattr,undefined,with,<<"juliet@capulet.com/chamber">>},
          {xmlattr,undefined,start,<<"1469-07-21T02:56:15.000000Z">>},
          {xmlattr,undefined,subject,<<"Subject">>},
          {xmlattr,undefined,thread,<<"12345">>},
          {xmlattr,undefined,crypt,<<"true">>},
          {xmlattr,undefined,version,<<"1">>}],
         [{xmlel,undefined,[],previous,[{xmlattr,undefined,with,
                                         <<"balcony@house.capulet.com">>},
                                        {xmlattr,undefined,start,
                                         <<"1469-07-21T03:16:37.000000Z">>}], []},
          {xmlel,undefined,[],next,[{xmlattr,undefined,with,
                                     <<"benvolio@montague.net">>},
                                    {xmlattr,undefined,start,
                                     <<"1469-07-21T03:01:54.000000Z">>}], []},
          {xmlel,undefined,[],x,[],[{xmlel,undefined,[],test,[],[]}]}]}).

-define(ARCHIVE_MESSAGE1,
        #archive_message{
            direction = from,
            utc = {{1469, 07, 21}, {02, 56, 15}},
            body = "Art thou not Romeo, and a Montague?"}).

-define(ARCHIVE_MESSAGE1_XML,
        {xmlel,undefined,[],from,
         [{xmlattr,undefined,secs,<<"0">>}],
         [{xmlel,undefined,[],body,[],
              [{xmlcdata,<<"Art thou not Romeo, and a Montague?">>}]}]}).

-define(ARCHIVE_MESSAGE2,
        #archive_message{
            direction = to,
            utc = {{1469, 07, 21}, {02, 56, 26}},
            name = "romeo",
            jid = "romeo@montague.net",
            body = "Neither, fair saint, if either thee dislike."}).

-define(ARCHIVE_MESSAGE2_XML,
        {xmlel,undefined,[],to,
         [{xmlattr,undefined,secs,<<"11">>},
          {xmlattr,undefined,name,<<"romeo">>},
          {xmlattr,undefined,jid,<<"romeo@montague.net">>}],
         [{xmlel,undefined,[],body,[],
               [{xmlcdata,<<"Neither, fair saint, if either thee dislike.">>}]}]}).

-define(ARCHIVE_MESSAGE3,
        #archive_message{
            direction = note,
            utc = {{1469, 07, 21}, {03, 04, 35}},
            body = "I think she might fancy me."}).

-define(ARCHIVE_MESSAGE3_XML,
        {xmlel,undefined,[],note,
         [{xmlattr,undefined,utc,<<"1469-07-21T03:04:35.000000Z">>}],
         [{xmlcdata,<<"I think she might fancy me.">>}]}).

-define(START, {{1469, 07, 21}, {02, 56, 15}}).

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
        ?test_gen1(test_message1_to_xml),
        ?test_gen1(test_message1_from_xml),
        ?test_gen1(test_message2_to_xml),
        ?test_gen1(test_message2_from_xml),
        ?test_gen1(test_message3_to_xml),
        ?test_gen1(test_message3_from_xml)
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
        ?ARCHIVE_COLLECTION_NO_LINKS_XML))#archive_collection{change_utc = undefined}.

test_collection_xml_each(_) ->
    lists:foreach(
        fun(Index) ->
            if Index =/= #archive_collection.with_server andalso
               Index =/= #archive_collection.us andalso
               Index =/= #archive_collection.utc andalso
               Index =/= #archive_collection.deleted ->
                R = setelement(Index, ?ARCHIVE_COLLECTION_NO_LINKS, undefined),
                OutR = (mod_archive2_xml:collection_from_xml(exmpp_jid:parse(?JID),
                     mod_archive2_xml:collection_to_xml(chat, R)))#archive_collection{
                        change_utc = undefined},
                R = OutR;
               true ->
                   ok
            end
        end,
        lists:seq(5, tuple_size(?ARCHIVE_COLLECTION_NO_LINKS))).

test_message1_to_xml(_) ->
    ?ARCHIVE_MESSAGE1_XML =
        mod_archive2_xml:message_to_xml(?ARCHIVE_MESSAGE1, ?START).

test_message1_from_xml(_) ->
    ?ARCHIVE_MESSAGE1 =
        mod_archive2_xml:message_from_xml(?ARCHIVE_MESSAGE1_XML, ?START).

test_message2_to_xml(_) ->
    ?ARCHIVE_MESSAGE2_XML =
        mod_archive2_xml:message_to_xml(?ARCHIVE_MESSAGE2, ?START).

test_message2_from_xml(_) ->
    ?ARCHIVE_MESSAGE2 =
        mod_archive2_xml:message_from_xml(?ARCHIVE_MESSAGE2_XML, ?START).

test_message3_to_xml(_) ->
    ?ARCHIVE_MESSAGE3_XML =
        mod_archive2_xml:message_to_xml(?ARCHIVE_MESSAGE3, ?START).

test_message3_from_xml(_) ->
    ?ARCHIVE_MESSAGE3 =
        mod_archive2_xml:message_from_xml(?ARCHIVE_MESSAGE3_XML, ?START).

mysql_test_links(Pid) ->
    ejabberd_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"insert into archive_collection values (null, null, null, "
                 "'client@localhost', 'juliet', 'capulet.com', 'chamber', "
                 "'1469-07-21 02:56:15', '1469-07-21 02:56:15', 1, 0, null, "
                 "null, null, null)",
                 {updated, 1}},
                {"insert into archive_collection values (null, null, null, "
                 "'client@localhost', 'balcony', 'house.capulet.com', null, "
                 "'1469-07-21 03:16:37', '1469-07-21 03:16:37', 1, 0, null, "
                 "null, null, null)",
                 {updated, 1}},
                {"insert into archive_collection values (null, null, null, "
                 "'client@localhost', 'benvolio', 'montague.net', null, "
                 "'1469-07-21 03:01:54', '1469-07-21 03:01:54', 1, 0, null, "
                 "null, null, null)",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{3}]}},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'balcony') "
                 "and (with_server = 'house.capulet.com') and (with_resource = null) "
                 "and (utc = '1469-07-21 03:16:37')",
                 {selected, [], [{2}]}},
                {"select id from archive_collection where (us = 'client@localhost') "
                 "and (with_user = 'benvolio') "
                 "and (with_server = 'montague.net') and (with_resource = null) "
                 "and (utc = '1469-07-21 03:01:54')",
                 {selected, [], [{3}]}},
                {"select with_user, with_server, with_resource, utc from "
                 "archive_collection where (id = 2)",
                 {selected, [], [{"balcony", "house.capulet.com", undefined,
                                  "1469-07-21 03:16:37"}]}},
                {"select with_user, with_server, with_resource, utc from "
                 "archive_collection where (id = 3)",
                 {selected, [], [{"benvolio", "montague.net", undefined,
                                  "1469-07-21 03:01:54"}]}},
                {}])
        end),
    common_test_links(Pid).

common_test_links(_Pid) ->
    {atomic, ?ARCHIVE_COLLECTION_WITH_LINKS_XML} =
        ejabberd_storage:transaction(?HOST,
            fun() ->
                ejabberd_storage:insert([?ARCHIVE_COLLECTION1,
                                             ?ARCHIVE_COLLECTION2,
                                             ?ARCHIVE_COLLECTION3]),
                mod_archive2_xml:collection_to_xml(chat,
                    mod_archive2_xml:collection_from_xml(
                        exmpp_jid:parse(?JID),
                        ?ARCHIVE_COLLECTION_WITH_LINKS_XML))
            end).
