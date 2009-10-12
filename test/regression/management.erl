%%%----------------------------------------------------------------------
%%% File    : management.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 collections management commands regression testing
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

-module(management).
-author('ejabberd@ndl.kiev.ua').

-include_lib("eunit/include/eunit.hrl").
-include_lib("exmpp/include/exmpp.hrl").

-include("testing.hrl").
-include("management.hrl").

management_test_() ->
    ?test_foreach(
        client:session_setup,
        client:session_teardown,
        [
%            ?test_gen1(test_retrieve_list),
            ?test_gen1(test_retrieve_empty_list)
%            ?test_gen1(test_retrieve_collection),
%            ?test_gen1(test_retrieve_non_existing_collection),
%            ?test_gen1(test_remove_collections),
%            ?test_gen1(test_remove_all_collections),
%            ?test_gen1(test_remove_non_existing_collection)
        ]).

test_retrieve_list(F) ->
%    ?MANAGEMENT_TC1_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "list",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com")
    ], []))),
%    ?MANAGEMENT_TC1_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "list",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com"),
	exmpp_xml:attribute("start", "1469-07-21T02:00:00Z"),
	exmpp_xml:attribute("end", "1479-07-21T04:00:00Z")
    ], []))),
%    ?MANAGEMENT_TC1_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "list",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com"),
	    exmpp_xml:attribute("start", "1469-07-21T02:00:00Z")
    ], []))).

test_retrieve_empty_list(F) ->
    ?MANAGEMENT_TC2_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "list",
    [
        exmpp_xml:attribute("with", "NOTEXISTING@capulet.com")
    ], []))),
    ?MANAGEMENT_TC2_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "list",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com"),
	exmpp_xml:attribute("start", "2000-07-21T02:00:00Z")
    ], []))),
    ?MANAGEMENT_TC2_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "list",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com"),
	exmpp_xml:attribute("end", "1468-07-21T02:00:00Z")
    ], []))),
    ?MANAGEMENT_TC2_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "list",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com"),
	exmpp_xml:attribute("start", "1467-07-21T02:00:00Z"),
	exmpp_xml:attribute("end", "1468-07-21T02:00:00Z")
    ], []))).

test_retrieve_collection(F) ->
    ?MANAGEMENT_TC3_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "retrieve",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	exmpp_xml:attribute("start", "1469-07-21T02:56:15Z")
    ],
    [
        exmpp_xml:element("http://jabber.org/protocol/rsm", "set",
	[
	    exmpp_xml:attribute("max", "100")
	], [])
    ]))).

test_retrieve_non_existing_collection(F) ->
    ?MANAGEMENT_TC4_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "retrieve",
    [
        exmpp_xml:attribute("with", "NOTEXISTING@capulet.com/chamber"),
	exmpp_xml:attribute("start", "1469-07-21T02:56:15Z")
    ],
    [
        exmpp_xml:element("http://jabber.org/protocol/rsm", "set",
	[
	    exmpp_xml:attribute("max", "100")
	], [])
    ]))).

test_remove_collections(F) ->
    % Upload one more collection with date after the one we're going to remove so that we can check if "remove" removes only one collection, not all after date.
   ?MANAGEMENT_TC5_UPLOAD_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "save", [],
    [
        exmpp_xml:element(undefined, "chat",
	[
	    exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	    exmpp_xml:attribute("start", "1470-07-21T02:56:15Z"),
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
    ?MANAGEMENT_TC5_REMOVE_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "remove",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	exmpp_xml:attribute("start", "1469-07-21T02:56:15Z")
    ], []))),
    ?MANAGEMENT_TC5_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "list",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com"),
	exmpp_xml:attribute("start", "1469-07-21T02:00:00Z")
    ],
    [
        exmpp_xml:element("http://jabber.org/protocol/rsm", "set",
	[
	    exmpp_xml:attribute("max", "30")
	], [])
    ]))),
   ?MANAGEMENT_TC5_RETRIEVE_RESULT2 =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "list",
    [
	exmpp_xml:attribute("start", "1469-07-21T03:16:37Z"),
	exmpp_xml:attribute("end", "1470-07-21T03:16:37Z")
    ],
    [
        exmpp_xml:element("http://jabber.org/protocol/rsm", "set",
	[
	    exmpp_xml:attribute("max", "30")
	], [])
    ]))),
    ?MANAGEMENT_TC5_REMOVE_RESULT2 =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "remove",
    [
	exmpp_xml:attribute("start", "1469-07-21T03:16:37Z"),
	exmpp_xml:attribute("end", "2038-01-01T00:00:00Z")
    ], []))),
   ?MANAGEMENT_TC5_RETRIEVE_RESULT3 =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "list",
    [
	exmpp_xml:attribute("start", "1469-07-21T03:16:37Z")
    ],
    [
        exmpp_xml:element("http://jabber.org/protocol/rsm", "set",
	[
	    exmpp_xml:attribute("max", "30")
	], [])
    ]))).

test_remove_all_collections(F) ->
    ?MANAGEMENT_TC6_REMOVE_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "remove", [], []))),
    ?MANAGEMENT_TC6_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "list",
    [],
    [
        exmpp_xml:element("http://jabber.org/protocol/rsm", "set",
	[
	    exmpp_xml:attribute("max", "30")
	], [])
    ]))).

test_remove_non_existing_collection(F) ->
    ?MANAGEMENT_TC7_REMOVE_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "remove",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com"),
	exmpp_xml:attribute("start", "1469-07-21T02:56:15Z")
    ],[]))).
