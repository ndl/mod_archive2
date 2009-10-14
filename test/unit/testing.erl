%%%----------------------------------------------------------------------
%%% File    : testing.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 unit testing helper functions
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

-module(testing).
-author('ejabberd@ndl.kiev.ua').

-export([eunit_xml_report/1, mnesia_tests_setup/0, mnesia_tests_teardown/1,
         mysql_tests_setup/0, mysql_tests_teardown/1]).

-include("testing.hrl").
-include("mod_archive2.hrl").
-include("mod_archive2_storage.hrl").

-define(HOST, "localhost").

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

common_tests_setup(RDBMS) ->
    exmpp:start(),
    case lists:member(ejabberd_sup, registered()) of
        false -> ejabberd_sup:start_link();
        true -> ok
    end,
    {ok, Pid} =
        ejabberd_storage:start(
            ?HOST, [{rdbms, RDBMS}, {schema, ?MOD_ARCHIVE2_SCHEMA}]),
    Pid.

common_tests_teardown(_Pid) ->
    ejabberd_storage:stop(?HOST).

mysql_tests_setup() ->
    common_tests_setup(mysql).

mysql_tests_teardown(Pid) ->
    common_tests_teardown(Pid).

mnesia_tests_setup() ->
    mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:create_table(archive_message,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, archive_message)}]),
    mnesia:create_table(archive_collection,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, archive_collection)}]),
    mnesia:create_table(archive_jid_prefs,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, archive_jid_prefs)}]),
    common_tests_setup(mnesia).

mnesia_tests_teardown(Pid) ->
    mnesia:clear_table(archive_message),
    mnesia:clear_table(archive_jid_prefs),
    common_tests_teardown(Pid).
