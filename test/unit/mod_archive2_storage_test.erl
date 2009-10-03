%%%----------------------------------------------------------------------
%%% File    : mod_archive2_odbc_test.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Message Archiving (XEP-136) Storage Support
%%% Created : 3 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_archive2_storage_test).
-author('ejabberd@ndl.kiev.ua').

-include_lib("stdlib/include/ms_transform.hrl").
-include("mod_archive2.hrl").
-include("config.hrl").

-export([eunit_xml_report/1]).

-define(HOST, "localhost").

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

mod_archive2_mysql_test_() ->
{
    foreach,
    local,
    fun mysql_tests_setup/0,
    fun common_tests_teardown/1,
    [
        ?test_gen(mysql_test_insert_and_read)
    ]
}.

mod_archive2_mnesia_test_() ->
{
    foreach,
    local,
    fun mnesia_tests_setup/0,
    fun mnesia_tests_teardown/1,
    [
        ?test_gen(mnesia_test_insert_and_read)
    ]
}.

common_tests_setup(RDBMS) ->
    {ok, Pid} =
        gen_server:start(
            mod_archive2_storage,
            [{host, ?HOST},
             [{rdbms, RDBMS}, {schema, ?MOD_ARCHIVE2_SCHEMA}]], []),
    Pid.

common_tests_teardown(Pid) ->
    gen_server:call(Pid, stop).

mysql_tests_setup() ->
    common_tests_setup(mysql).

mnesia_tests_setup() ->
    mnesia:start(),
    mnesia:create_schema([node()]),
    mnesia:create_table(archive_message,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, archive_message)}]),
    common_tests_setup(mnesia).

mnesia_tests_teardown(Pid) ->
    mnesia:clear_table(archive_message),
    common_tests_teardown(Pid).

mysql_test_insert_and_read(Pid) ->
    ejabberd_odbc:start(["select", {selected, [], [{"0", "1", "2"}]}]),
    common_test_insert_and_read(Pid).

mnesia_test_insert_and_read(Pid) ->
    common_test_insert_and_read(Pid).

common_test_insert_and_read(_Pid) ->
    {atomic, {inserted, 1, Key}} =
        mod_archive2_storage:transaction(?HOST,
            fun() ->
                mod_archive2_storage:insert(
                    [#archive_message{
                        direction = from,
                        body = "Hi!",
                        name = "me"}])
            end),
    {atomic, {selected, [#archive_message{body = "Hi!", name = "me"}]}} =
        mod_archive2_storage:transaction(?HOST,
            fun() ->
                mod_archive2_storage:read(#archive_message{id = Key})
            end).
