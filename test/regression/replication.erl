%%%----------------------------------------------------------------------
%%% File    : replication.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 archiving replication regression testing
%%% Created : 27 Sep 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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

-module(replication).
-author('ejabberd@ndl.kiev.ua').

-include_lib("eunit/include/eunit.hrl").
-include_lib("exmpp/include/exmpp.hrl").

-include("testing.hrl").
-include("replication.hrl").

replication_test_() ->
    ?test_foreach(
        client:session_setup,
        client:session_teardown,
        [
            ?test_gen1(test_retrieve_replication)
        ]).

test_retrieve_replication(F) ->
    ?REPLICATION_TC1_UPLOAD_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "save", [],
    [
        exmpp_xml:element(undefined, "chat",
	[
	    exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	    exmpp_xml:attribute("start", "1469-07-21T02:56:15Z"),
	    exmpp_xml:attribute("thread", "damduoeg09"),
	    exmpp_xml:attribute("subject", "She speaks!")
	],
	[
	    exmpp_xml:element(undefined, "from",
	    [
	        exmpp_xml:attribute("secs", "0")
	    ],
	    [ exmpp_xml:element(undefined, "body", [], [exmpp_xml:cdata("Art thou not Romeo, and a Montague?")]) ])
	])
    ]))),
    ?REPLICATION_TC1_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "modified",
        [exmpp_xml:attribute("start", "1469-07-21T01:14:47Z")], []))).
    %?REPLICATION_TC1_RETRIEVE_ELEMENTS = lists:sort(ReplicationElements).
