%%%----------------------------------------------------------------------
%%% File    : mod_archive2_storage_test.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Message Archiving (XEP-136) Storage Support
%%% Created : 27 Sep 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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

mod_archive2_match_to_sql_test_() ->
{
    foreach,
    local,
    fun tests_setup/0,
    fun tests_teardown/1,
    [
        ?test_gen(test_match_to_sql)
    ]
}.

tests_setup() -> ok.

tests_teardown(_) -> ok.

test_match_to_sql(_) ->
    Where1Clause = "((id = 1) or ((id = 2) and (utc = '2000-01-01')))",
    Query1Res =  {Where1Clause, [id, utc, body]},
    TableSchema =
        lists:keyfind(archive_message, #table.name, ?MOD_ARCHIVE2_SCHEMA),
    TableInfo = TableSchema#table{rdbms = mysql},
    ?assert(
        mod_archive2_odbc:ms_to_sql(
            [{#archive_message{id = '$1', coll_id = '_', utc = '$2', direction = '_', body = '$3', name = '_'},
             [{'orelse',{'==','$1',1},
	                {'andalso',{'==','$1', 2},{'==',utc,"2000-01-01"}}}],
	     [{{'$1', '$2', '$3'}}]}], TableInfo)
	=:= Query1Res),
    % The same query, but now using matching transform:
    ?assert(
        mod_archive2_odbc:ms_to_sql(
	    ets:fun2ms(
	        fun(#archive_message{id = ID, utc = UTC, body = Body}) when
		    ID =:= 1 orelse (ID =:= 2 andalso UTC =:= "2000-01-01") ->
		    {ID, UTC, Body}
		end), TableInfo)
	=:= Query1Res),
    % The same query, but with full record returned.
    ?assert(
        mod_archive2_odbc:ms_to_sql(
	    ets:fun2ms(
	        fun(#archive_message{id = ID, utc = UTC} = R) when
		    ID =:= 1 orelse (ID =:= 2 andalso UTC =:= "2000-01-01") ->
		    R
		end), TableInfo)
	=:= {Where1Clause, TableInfo#table.fields}),
    % Test head matching.
    ?assert(
        mod_archive2_odbc:ms_to_sql(
	    ets:fun2ms(
	        fun(#archive_message{id = 1, utc = "2000-01-01"} = R) ->
		    R
		end), TableInfo)
	=:= {"(id = 1) and (utc = '2000-01-01')", TableInfo#table.fields}).
