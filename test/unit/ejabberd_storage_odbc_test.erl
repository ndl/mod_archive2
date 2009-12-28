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
        ?test_gen0(test_match_to_sql7),
        ?test_gen0(test_match_to_sql8)
     ].

-define(WHERE_CLAUSE1, "((id = 1) or ((id = 2) and (utc = '2000-01-01 00:00:00')))").

-define(QUERY_DATE, {{2000, 1, 1}, {0, 0, 0}}).

-define(QUERY_DATE_ENCODED, {{{{2000, 1, 1}}, {{0, 0, 0}}}}).

-define(QUERY_RES1,
    {?WHERE_CLAUSE1,
        {[id, utc, body],
            [{field, id}, {field, utc}, {field, body}]}}).

query_body_full(TableInfo) ->
    Fields = TableInfo#table.fields,
    {Fields, [{value, TableInfo#table.name} | [{field, Field} || Field <- Fields]]}.

get_table_info(TableName) ->
    TableSchema =
        lists:keyfind(TableName, #table.name, ?MOD_ARCHIVE2_SCHEMA),
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
	                {'andalso',{'==','$1', 2},{'==',utc, ?QUERY_DATE_ENCODED}}}],
	     [{{'$1', '$2', '$3'}}]}], TableInfo)
	=:= ?QUERY_RES1).

%% fun2ms MS-generated test.
test_match_to_sql2() ->
    TableInfo = get_table_info(archive_message),
    ?assert(
        ejabberd_storage_odbc:ms_to_sql(
	    ets:fun2ms(
	        fun(#archive_message{id = ID, utc = UTC, body = Body}) when
		    ID =:= 1 orelse (ID =:= 2 andalso UTC =:= ?QUERY_DATE) ->
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
		    ID =:= 1 orelse (ID =:= 2 andalso UTC =:= ?QUERY_DATE) ->
		    R
		end), TableInfo)
	=:= {?WHERE_CLAUSE1, query_body_full(TableInfo)}).

%% Head matching test.
test_match_to_sql4() ->
    TableInfo = get_table_info(archive_message),
    ?assert(
        ejabberd_storage_odbc:ms_to_sql(
	    ets:fun2ms(
	        fun(#archive_message{id = 1, utc = ?QUERY_DATE} = R) ->
		    R
		end), TableInfo)
	=:= {"(id = 1) and (utc = '2000-01-01 00:00:00')", query_body_full(TableInfo)}).

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
    =:= {"(with_user = 'juliet')",{[save,otr], [{field, save}, {field, otr}]}}).

%% All supported functions test.
test_match_to_sql6() ->
    TableInfo = get_table_info(archive_jid_prefs),
    QueryBodyFull = query_body_full(TableInfo),
    {"(((not(not(exactmatch)) = exactmatch) and "
     "((1 = exactmatch) and ((exactmatch = 0) and ((exactmatch <> (exactmatch = 1)) "
     "and (((exactmatch = 0) <> exactmatch) and (((1 | 0) = exactmatch) and "
     "((exactmatch <> (0 & 1)) and ((abs(expire) = expire) and (expire < expire))))))))) "
     "or ((expire <= expire) or (((expire > expire) and ((expire >= expire) and "
     "((expire <> expire) and ((expire <> expire) and (((expire & expire) = expire) "
     "and ((expire | expire) = expire)))))) or (((expire + expire) > expire) or "
     "(((expire - expire) < expire) or (((expire * expire) = expire) or "
     "((expire / expire) <> expire)))))))",
     QueryBodyFull} =
        ejabberd_storage_odbc:ms_to_sql(
            ets:fun2ms(
                fun(#archive_jid_prefs{exactmatch = ExactMatch, expire = Expire} = R)
                    when (not (not ExactMatch)) =:= ExactMatch andalso
                         true =:= ExactMatch andalso
                         ExactMatch =:= false andalso
                         (ExactMatch =/= (ExactMatch =:= true)) andalso
                         ((ExactMatch =:= false) =/= ExactMatch) andalso
                         ((true or false) =:= ExactMatch) andalso
                         (ExactMatch =/= (false and true)) andalso
                         abs(Expire) == Expire andalso
                         Expire < Expire orelse
                         Expire =< Expire orelse
                         Expire > Expire andalso
                         Expire >= Expire andalso
                         Expire =/= Expire andalso
                         Expire /= Expire andalso
                         Expire and Expire =:= Expire andalso
                         Expire or Expire == Expire orelse
                         Expire + Expire > Expire orelse
                         Expire - Expire < Expire orelse
                         Expire * Expire =:= Expire orelse
                         Expire / Expire =/= Expire ->
                        R
                end), TableInfo).

%% Tuples decoding test.
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
         "((utc = '1469-07-21 02:56:15') and (id < 1)))", {[], [{value, ok}]}}).

%% Partial records in body result test.
test_match_to_sql8() ->
    TableInfo = get_table_info(archive_collection),
    ?assert(
        ejabberd_storage_odbc:ms_to_sql(
            ets:fun2ms(fun(#archive_collection{id = ID, utc = UTC, us = US}) when
                           US =:= "client@localhost",
                           UTC < {{1469,7,21},{2,56,15}} orelse
                           UTC =:= {{1469,7,21},{2,56,15}} andalso
                           ID < 1 ->
                            #archive_collection{id = ID, utc = UTC}
                       end), TableInfo)
    =:=
        {"(us = 'client@localhost') and ((utc < '1469-07-21 02:56:15') or "
         "((utc = '1469-07-21 02:56:15') and (id < 1)))",
         {[id,utc],
          [{value,archive_collection},
           {field,id},
           {value,undefined},
           {value,undefined},
           {value,undefined},
           {value,undefined},
           {value,undefined},
           {value,undefined},
           {field,utc},
           {value,undefined},
           {value,undefined},
           {value,undefined},
           {value,undefined},
           {value,undefined},
           {value,undefined},
           {value,undefined}]}}).