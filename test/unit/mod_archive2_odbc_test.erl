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
-include("testing.hrl").

-export([eunit_xml_report/1]).

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

mod_archive2_match_to_sql_test_() ->
    [
        ?test_gen0(test_match_to_sql1),
        ?test_gen0(test_match_to_sql2),
        ?test_gen0(test_match_to_sql3),
        ?test_gen0(test_match_to_sql4),
        ?test_gen0(test_match_to_sql5),
        ?test_gen0(test_match_to_sql6)
     ].

-define(WHERE_CLAUSE1, "((id = 1) or ((id = 2) and (utc = '2000-01-01')))").
-define(QUERY_RES1, {?WHERE_CLAUSE1, [id, utc, body]}).

get_table_info(Table) ->
    TableSchema =
        lists:keyfind(Table, #table.name, ?MOD_ARCHIVE2_SCHEMA),
    TableSchema#table{rdbms = mysql}.

%% Direct MS test.
test_match_to_sql1() ->
    TableInfo = get_table_info(archive_message),
    ?assert(
        mod_archive2_odbc:ms_to_sql(
            [{#archive_message{id = '$1', coll_id = '_', utc = '$2',
                               direction = '_', body = '$3', name = '_'},
             [{'orelse',{'==','$1',1},
	                {'andalso',{'==','$1', 2},{'==',utc,"2000-01-01"}}}],
	     [{{'$1', '$2', '$3'}}]}], TableInfo)
	=:= ?QUERY_RES1).

%% fun2ms MS-generated test.
test_match_to_sql2() ->
    TableInfo = get_table_info(archive_message),
    ?assert(
        mod_archive2_odbc:ms_to_sql(
	    ets:fun2ms(
	        fun(#archive_message{id = ID, utc = UTC, body = Body}) when
		    ID =:= 1 orelse (ID =:= 2 andalso UTC =:= "2000-01-01") ->
		    {ID, UTC, Body}
		end), TableInfo)
	=:= ?QUERY_RES1).

%% full record test.
test_match_to_sql3() ->
    TableInfo = get_table_info(archive_message),
    ?assert(
        mod_archive2_odbc:ms_to_sql(
	    ets:fun2ms(
	        fun(#archive_message{id = ID, utc = UTC} = R) when
		    ID =:= 1 orelse (ID =:= 2 andalso UTC =:= "2000-01-01") ->
		    R
		end), TableInfo)
	=:= {?WHERE_CLAUSE1, TableInfo#table.fields}).

%% Head matching test.
test_match_to_sql4() ->
    TableInfo = get_table_info(archive_message),
    ?assert(
        mod_archive2_odbc:ms_to_sql(
	    ets:fun2ms(
	        fun(#archive_message{id = 1, utc = "2000-01-01"} = R) ->
		    R
		end), TableInfo)
	=:= {"(id = 1) and (utc = '2000-01-01')", TableInfo#table.fields}).

%% Partial body plus head matching test.
test_match_to_sql5() ->
    TableInfo = get_table_info(archive_jid_prefs),
    ?assert(
        mod_archive2_odbc:ms_to_sql(
            ets:fun2ms(
                fun(#archive_jid_prefs{with_user = "juliet",
                                       save = Save, otr = OTR}) ->
                    {Save, OTR}
                end), TableInfo)
    =:= {"(with_user = 'juliet')",[save,otr]}).

%% All supported functions test.
test_match_to_sql6() ->
    TableInfo = get_table_info(archive_jid_prefs),
    ?assert(
        mod_archive2_odbc:ms_to_sql(
            ets:fun2ms(
                fun(#archive_jid_prefs{save = Save, expire = Expire, otr = Otr} = R)
                    when (not (not Save)) =:= Save andalso
                         abs(Expire) == Expire andalso
                         Expire < Otr orelse
                         Expire =< Otr orelse
                         Expire > Otr andalso
                         Expire >= Otr andalso
                         Expire =/= Otr andalso
                         Expire /= Otr andalso
                         Expire and Otr =:= Otr andalso
                         Expire or Otr == Otr orelse
                         Expire + Otr > Expire orelse
                         Expire - Otr < Expire orelse
                         Expire * Otr =:= Expire orelse
                         Expire / Otr =/= Expire ->
                        R
                end), TableInfo)
    =:=
        {"(((not(not(save)) = save) and ((abs(expire) = expire) and "
         "(expire < otr))) or ((expire <= otr) or (((expire > otr) and "
         "((expire >= otr) and ((expire <> otr) and ((expire <> otr) and "
         "(((expire & otr) = otr) and ((expire | otr) = otr)))))) or "
         "(((expire + otr) > expire) or (((expire - otr) < expire) or "
         "(((expire * otr) = expire) or ((expire / otr) <> expire)))))))",
         [us,with_user,with_server,with_resource,save,expire,otr]}).
