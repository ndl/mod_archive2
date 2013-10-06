%%%----------------------------------------------------------------------
%%% File    : prefs.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 archiving preferences regression testing
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

-module(prefs).
-author('ejabberd@ndl.kiev.ua').

-include_lib("eunit/include/eunit.hrl").
-include_lib("exmpp/include/exmpp.hrl").

-include("testing.hrl").
-include("prefs.hrl").

prefs_test_() ->
    ?test_foreach(
        client:session_setup,
        client:session_teardown,
        [
            ?test_gen1(test_default_prefs),
            ?test_gen1(test_global_prefs_change),
            ?test_gen1(test_prefs_methods_change),
            ?test_gen1(test_jid_prefs_change),
            ?test_gen1(test_jid_prefs_remove),
            ?test_gen1(test_auto_prefs_change1),
            ?test_gen1(test_auto_prefs_change2),
	    ?test_gen1(test_auto_refs_session_expire),
            ?test_gen1(test_session_prefs_change)
        ]).

test_default_prefs(F) ->
    ?PREFS_TC1_DEFAULT =
        client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref"))).

test_global_prefs_change(F) ->
    [?PREFS_TC2_CHANGE_PUSH, ?PREFS_TC2_CHANGE_RESULT] =
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref", [],
    [
        exmpp_xml:element(undefined, "default",
	[
	    exmpp_xml:attribute(<<"otr">>, "prefer"),
	    exmpp_xml:attribute(<<"save">>, "false")
	], [])
    ])), 2),
    ?PREFS_TC2_CHANGED =
        client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref"))).

test_prefs_methods_change(F) ->
    [?PREFS_TC3_CHANGE_PUSH, ?PREFS_TC3_CHANGE_RESULT] =
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref", [],
    [
        exmpp_xml:element(undefined, "method",
	[
	    exmpp_xml:attribute(<<"type">>, "auto"),
	    exmpp_xml:attribute(<<"use">>, "concede")
	], []),
        exmpp_xml:element(undefined, "method",
	[
	    exmpp_xml:attribute(<<"type">>, "local"),
	    exmpp_xml:attribute(<<"use">>, "forbid")
	], []),
        exmpp_xml:element(undefined, "method",
	[
	    exmpp_xml:attribute(<<"type">>, "manual"),
	    exmpp_xml:attribute(<<"use">>, "prefer")
	], [])
    ])), 2),
    ?PREFS_TC3_CHANGED =
        client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref"))).

test_jid_prefs_change(F) ->
    [?PREFS_TC4_CHANGE_PUSH, ?PREFS_TC4_CHANGE_RESULT] =
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref", [],
    [
        exmpp_xml:element(undefined, "item",
	[
	    exmpp_xml:attribute(<<"jid">>, "romeo@montague.net"),
	    exmpp_xml:attribute(<<"save">>, "body"),
	    exmpp_xml:attribute(<<"expire">>, "604800"),
	    exmpp_xml:attribute(<<"otr">>, "concede")
	], [])
    ])), 2),
    ?PREFS_TC4_CHANGED =
        client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref"))).

test_jid_prefs_remove(F) ->
    [?PREFS_TC5_CHANGE_PUSH, ?PREFS_TC5_CHANGE_RESULT] =
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "itemremove", [],
    [
        exmpp_xml:element(undefined, "item",
	[
	    exmpp_xml:attribute(<<"jid">>, "romeo@montague.net")
	], [])
    ])), 2),
    ?PREFS_TC5_CHANGED =
        client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref"))).

test_auto_prefs_change1(F) ->
    ?PREFS_TC6_CHANGE_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "auto",
	[
	    exmpp_xml:attribute(<<"save">>, "false"),
	    exmpp_xml:attribute(<<"scope">>, "stream")
	], []))),
    ?PREFS_TC6_CHANGED =
        client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref"))),
    % Restore it back so that further tests continue to use auto-archiving.
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "auto",
	[
	    exmpp_xml:attribute(<<"save">>, "true"),
	    exmpp_xml:attribute(<<"scope">>, "stream")
	], []))).

test_auto_prefs_change2(F) ->
    [?PREFS_TC7_CHANGE_PUSH, ?PREFS_TC7_CHANGE_RESULT] =
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "auto",
	[
	    exmpp_xml:attribute(<<"save">>, "false"),
	    exmpp_xml:attribute(<<"scope">>, "global")
	], [])), 2),
    ?PREFS_TC7_CHANGED =
        client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref"))),
    % Restore it back so that further tests continue to use auto-archiving.
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "auto",
	[
	    exmpp_xml:attribute(<<"save">>, "true"),
	    exmpp_xml:attribute(<<"scope">>, "global")
	], [])), 2),
    % Restore global 'save' setting back so that subsequent tests work properly.
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref", [],
    [
        exmpp_xml:element(undefined, "default",
	[exmpp_xml:attribute(<<"save">>, "body")], [])
    ])), 2).

test_auto_refs_session_expire(_) ->
    % Using separate session to avoid screwing the default one
    % as we need to close & re-open it. Also using fixed resource to make sure
    % the stream settings expiration is not masked by different random resource use.
    {Session2, JID2} = client:create_session(?CLIENTNAME2, "test"),
    F2 = {Session2, JID2},
    client:response(F2, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "auto",
	[
	    exmpp_xml:attribute(<<"save">>, "false"),
	    exmpp_xml:attribute(<<"scope">>, "stream")
	], []))),
    exmpp_session:stop(Session2),
    {NewSession2, NewJID2} = client:create_session(?CLIENTNAME2, "test"),
    NewF2 = {NewSession2, NewJID2},
    ?PREFS_TC8_RESULT =
        client:response(NewF2, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref"))),
    exmpp_session:stop(NewSession2).

test_session_prefs_change(F) ->
    [?PREFS_TC9_CHANGE_PUSH, ?PREFS_TC9_CHANGE_RESULT] =
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref", [],
    [
        exmpp_xml:element(undefined, "session",
	[
	    exmpp_xml:attribute(<<"thread">>, "123"),
	    exmpp_xml:attribute(<<"save">>, "false")
	], [])
    ])), 2),
    ?PREFS_TC9_GET_RESULT =
        client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref"))),
    ?PREFS_TC9_CHANGE_RESULT2 =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS_ARCHIVING, "sessionremove", [],
    [
        exmpp_xml:element(undefined, "session",
	[
	    exmpp_xml:attribute(<<"thread">>, "123")
	], [])
    ]))),
    ?PREFS_TC9_GET_RESULT2 =
        client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS_ARCHIVING, "pref"))).
