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
        ?test_gen(mysql_test_read),
        ?test_gen(mysql_test_select),
        ?test_gen(mysql_test_update)
    ]
}.

mod_archive2_mnesia_test_() ->
{
    foreach,
    local,
    fun mnesia_tests_setup/0,
    fun mnesia_tests_teardown/1,
    [
        ?test_gen(common_test_read),
        ?test_gen(common_test_select),
        ?test_gen(common_test_update)
    ]
}.

common_tests_setup(RDBMS) ->
    case lists:member(ejabberd_sup, registered()) of
        false -> ejabberd_sup:start_link();
        true -> ok
    end,
    {ok, Pid} =
        mod_archive2_storage:start(
            ?HOST, [{rdbms, RDBMS}, {schema, ?MOD_ARCHIVE2_SCHEMA}]),
    Pid.

common_tests_teardown(_Pid) ->
    mod_archive2_storage:stop(?HOST).

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

-define(RECORD1, #archive_message{utc = {{2000, 12, 31}, {23, 59, 59}},
                                  direction = from,
                                  body = "Hi!",
                                  name = "me"}).

-define(RECORD2, #archive_message{utc = {{1999, 11, 30}, {19, 01, 02}},
                                  direction = from,
                                  body = "Hi there!",
                                  name = "smb"}).

mysql_test_read(Pid) ->
    % Ensure we set mock data in the same process we're going to read them.
    mod_archive2_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"insert into archive_message values (null, null, "
                 "'2000-12-31 23:59:59', 0, 'Hi!', 'me')",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{1}]}},
                {},
                {"select * from archive_message where id = 1",
                 {selected, [], [{1, null, "2000-12-31 23:59:59", "0", "Hi!", "me"}]}},
                {},
                {"delete from archive_message where id = 1",
                 {updated, 1}},
                {}])
        end),
    common_test_read(Pid).

common_test_read(_Pid) ->
    {atomic, {inserted, 1, Key}} =
        mod_archive2_storage:transaction(?HOST,
            fun() ->
                mod_archive2_storage:insert(
                    [?RECORD1])
            end),
    {atomic, {selected, [#archive_message{
        direction = from, body = "Hi!", name = "me"}]}} =
        mod_archive2_storage:transaction(?HOST,
            fun() ->
                mod_archive2_storage:read(#archive_message{id = Key})
            end),
    {atomic, {deleted, 1}} =
        mod_archive2_storage:transaction(?HOST,
            fun() ->
                mod_archive2_storage:delete(#archive_message{id = Key})
            end).

mysql_test_select(Pid) ->
    % Ensure we set mock data in the same process we're going to read them.
    mod_archive2_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"insert into archive_message values (null, null, "
                 "'2000-12-31 23:59:59', 0, 'Hi!', 'me')",
                 {updated, 1}},
                {"insert into archive_message values (null, null, "
                 "'1999-11-30 19:01:02', 0, 'Hi there!', 'smb')",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{2}]}},
                {"select * from archive_message where (direction = 0)",
                 {selected, [], [{1, null, "2000-12-31 23:59:59", "0",
                                  "Hi!", "me"},
                                 {2, null, "1999-11-30 19:01:02", "0",
                                  "Hi there!", "smb"}]}},
                {"select * from archive_message where (direction = 0) order by "
                 "name, asc offset 1 limit 2",
                 {selected, [], [{1, null, "1999-11-30 19:01:02", "0",
                                  "Hi there!", "smb"}]}},
                {"select * from archive_message where (direction = 0) order by "
                 "name, desc offset 1 limit 2",
                 {selected, [], [{1, null, "2000-12-31 23:59:59", "0",
                                  "Hi!", "me"}]}},
                {"select count(*) from archive_message where (direction = 0)",
                 {selected, [], [{2}]}},
                {"select count(*) from archive_message where (direction = 0) "
                "order by name, asc offset 1",
                 {selected, [], [{1}]}},
                {"select min(body) from archive_message where (direction = 0) "
                "order by name, desc limit 1",
                 {selected, [], [{"Hi there!"}]}},
                {"select body, name from archive_message order by "
                 "name, asc",
                 {selected, [], [{"Hi!", "me"}, {"Hi there!", "smb"}]}},
                {"select body, name from archive_message where (name = 'me')",
                 {selected, [], [{"Hi!", "me"}]}},
                {"delete from archive_message where (direction = 0)",
                 {updated, 2}},
                {}])
        end),
    common_test_select(Pid).

common_test_select(_Pid) ->
    MS = ets:fun2ms(
             fun(#archive_message{direction = from} = R) ->
                 R
             end),
    {atomic, {deleted, 2}} =
        mod_archive2_storage:transaction(?HOST,
            fun() ->
                {inserted, 2, _Key} =
                    mod_archive2_storage:insert([?RECORD1, ?RECORD2]),
                case mod_archive2_storage:select(MS) of
                    {selected, [?RECORD1, ?RECORD2]} -> ok;
                    {selected, [?RECORD2, ?RECORD1]} -> ok;
                    _ -> throw({error, badmatch})
                end,
                {selected, [?RECORD2]} =
                    mod_archive2_storage:select(MS,
                        [{order_by, {#archive_message.name, asc}},
                         {offset, 1}, {limit, 2}]),
                {selected, [?RECORD1]} =
                    mod_archive2_storage:select(MS,
                        [{order_by, {#archive_message.name, desc}},
                         {offset, 1}, {limit, 2}]),
                {selected, [{2}]} =
                    mod_archive2_storage:select(MS, [{aggregate, count}]),
                {selected, [{1}]} =
                    mod_archive2_storage:select(MS,
                        [{order_by, {#archive_message.name, asc}},
                         {offset, 1}, {aggregate, count}]),
                {selected, [{"Hi there!"}]} =
                    mod_archive2_storage:select(MS,
                        [{order_by, {#archive_message.name, desc}},
                         {limit, 1},
                         {aggregate, {min, #archive_message.body}}]),
                {selected, [{"Hi!", "me"}, {"Hi there!", "smb"}]} =
                    mod_archive2_storage:select(
                        ets:fun2ms(
                            fun(#archive_message{body = Body, name = Name}) ->
                                {Body, Name}
                            end), [{order_by, {#archive_message.name, asc}}]),
                {selected, [{"Hi!", "me"}]} =
                    mod_archive2_storage:select(
                        ets:fun2ms(
                            fun(#archive_message{body = Body, name = Name})
                                when Name =:= "me" ->
                                {Body, Name}
                            end)),
                {deleted, 2} =
                    mod_archive2_storage:delete(MS)
            end).

mysql_test_update(Pid) ->
    % Ensure we set mock data in the same process we're going to read them.
    mod_archive2_storage:transaction(?HOST,
        fun() ->
            ejabberd_odbc:start([
                {},
                {"insert into archive_message values (null, null, "
                 "'2000-12-31 23:59:59', 0, 'Hi!', 'me')",
                 {updated, 1}},
                {"insert into archive_message values (null, null, "
                 "'1999-11-30 19:01:02', 0, 'Hi there!', 'smb')",
                 {updated, 1}},
                {"select LAST_INSERT_ID()", {selected, [], [{2}]}},
                {"select * from archive_message where (name = 'me')",
                 {selected, [], [{1, null, "2000-12-31 23:59:59", "0",
                                  "Hi!", "me"}]}},
                {"update archive_message set name = 'other' where id = 1",
                 {updated, 1}},
                {"update archive_message set direction = 1 where (direction = 0)",
                 {updated, 2}},
                {"select direction, name from archive_message order by "
                 "name, asc",
                 {selected, [], [{"1", "other"}, {"1", "smb"}]}},
                {"delete from archive_message",
                 {updated, 2}},
                {}])
        end),
    common_test_update(Pid).

common_test_update(_Pid) ->
    {atomic, {deleted, 2}} =
        mod_archive2_storage:transaction(?HOST,
            fun() ->
                {inserted, 2, _Key} =
                    mod_archive2_storage:insert([?RECORD1, ?RECORD2]),
                {selected, [Record]} =
                    mod_archive2_storage:select(
                        ets:fun2ms(fun(#archive_message{name = "me"} = R) ->
                                        R
                                   end)),
                ?RECORD1 = Record,
                {updated, 1} =
                    mod_archive2_storage:update(
                        #archive_message{id = Record#archive_message.id,
                                         name = "other"}),
                {updated, 2} =
                    mod_archive2_storage:update(
                        #archive_message{direction = to},
                        ets:fun2ms(
                            fun(#archive_message{direction = from} = R) ->
                                R
                            end)),
                {selected, [{to, "other"}, {to, "smb"}]} =
                    mod_archive2_storage:select(
                        ets:fun2ms(
                            fun(#archive_message{name = Name, direction = Dir}) ->
                                {Dir, Name}
                            end), [{order_by, {#archive_message.name, asc}}]),
                {deleted, 2} =
                    mod_archive2_storage:delete(
                        ets:fun2ms(fun(#archive_message{} = R) -> R end))
            end).