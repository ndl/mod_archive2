%%%----------------------------------------------------------------------
%%% File    : mod_archive2_odbc_test.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Message Archiving (XEP-136) Storage Support
%%% Created : 3 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_archive2_odbc_test).
-author('ejabberd@ndl.kiev.ua').

-include_lib("stdlib/include/ms_transform.hrl").
-include("mod_archive2.hrl").
-include("config.hrl").

-export([eunit_xml_report/1]).

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

mod_archive2_odbc_test_() ->
{
    foreach,
    local,
    fun tests_setup/0,
    fun tests_teardown/1,
    [
        ?test_gen(test_insert_and_read)
    ]
}.

tests_setup() ->
    {ok, Pid} = gen_server:start(mod_archive2_odbc, [{host, "localhost"}, []], []),
    Opts =
    	{mysql,
	 dict:from_list(
	     [{archive_jid_prefs, record_info(fields, archive_jid_prefs)},
              {archive_global_prefs, record_info(fields, archive_global_prefs)},
              {archive_collection, record_info(fields, archive_collection)},
              {archive_message, record_info(fields, archive_message)}])},
    {Pid, Opts}.

tests_teardown({Pid, _Opts}) ->
    gen_server:call(Pid, stop),
    mnesia:clear_table(archive_message).

test_insert_and_read({_Pid, Opts}) ->
    ejabberd_odbc:start(["select", {selected, [], [{"0", "1", "2"}]}]),
    {atomic, {inserted, 1, Key}} =
        ejabberd_odbc:sql_transaction("localhost",
            fun() ->
                mod_archive2_odbc:handle_query(
                    {insert,
                     [#archive_message{direction = from, body = "Hi!", name = "me"}]}, Opts)
            end),
    {atomic, {selected, [#archive_message{body = "Hi!", name = "me"}]}} =
        ejabberd_odbc:sql_transaction("localhost",
            fun() ->
                mod_archive2_odbc:handle_query(
                    {read,
                     #archive_message{id = Key}}, Opts)
            end).
