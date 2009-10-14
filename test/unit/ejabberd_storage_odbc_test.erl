%%%----------------------------------------------------------------------
%%% File    : ejabberd_storage_odbc_test.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 ODBC storage unit testing
%%% Created : 30 Sep 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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

-module(ejabberd_storage_odbc_test).
-author('ejabberd@ndl.kiev.ua').

-include("mod_archive2.hrl").
-include("mod_archive2_storage.hrl").
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
        ?test_gen0(test_match_to_sql6),
        ?test_gen0(test_match_to_sql7)
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
        ejabberd_storage_odbc:ms_to_sql(
            [{#archive_message{id = '$1', coll_id = '_', utc = '$2',
                               direction = '_', body = '$3', name = '_',
                               jid = '_'},
             [{'orelse',{'==','$1',1},
	                {'andalso',{'==','$1', 2},{'==',utc,"2000-01-01"}}}],
	     [{{'$1', '$2', '$3'}}]}], TableInfo)
	=:= ?QUERY_RES1).

%% fun2ms MS-generated test.
test_match_to_sql2() ->
    TableInfo = get_table_info(archive_message),
    ?assert(
        ejabberd_storage_odbc:ms_to_sql(
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
        ejabberd_storage_odbc:ms_to_sql(
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
        ejabberd_storage_odbc:ms_to_sql(
	    ets:fun2ms(
	        fun(#archive_message{id = 1, utc = "2000-01-01"} = R) ->
		    R
		end), TableInfo)
	=:= {"(id = 1) and (utc = '2000-01-01')", TableInfo#table.fields}).

%% Partial body plus head matching test.
test_match_to_sql5() ->
    TableInfo = get_table_info(archive_jid_prefs),
    ?assert(
        ejabberd_storage_odbc:ms_to_sql(
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
        ejabberd_storage_odbc:ms_to_sql(
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
         TableInfo#table.fields}).

test_match_to_sql7() ->
    TableInfo = get_table_info(archive_collection),
    ?assert(
        ejabberd_storage_odbc:ms_to_sql(
            ets:fun2ms(fun(#archive_collection{id = ID, utc = UTC, us = US}) when
                           US =:= "client@localhost",
                           UTC < {{1469,7,21},{2,56,15}} orelse
                           UTC =:= {{1469,7,21},{2,56,15}} andalso
                           ID < 1 -> ok
                       end), TableInfo)
    =:=
        {"(us = 'client@localhost') and ((utc < '1469-07-21 02:56:15') or "
         "((utc = '1469-07-21 02:56:15') and (id < 1)))", []}).