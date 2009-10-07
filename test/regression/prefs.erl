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
	        ?test_gen1(test_jid_prefs_change)
%	        ?test_gen(test_jid_prefs_remove)
        ]).

test_default_prefs(F) ->
    ?PREFS_TC1_DEFAULT = client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).

test_global_prefs_change(F) ->
    [?PREFS_TC2_CHANGE_PUSH, ?PREFS_TC2_CHANGE_RESULT] =
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "pref", [],
    [
        exmpp_xml:element(undefined, "default",
	[
	    exmpp_xml:attribute("otr", "prefer"),
	    exmpp_xml:attribute("save", "false")
	], [])
    ])), 2),
    ?PREFS_TC2_CHANGED = client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).

test_prefs_methods_change(F) ->
    [?PREFS_TC3_CHANGE_PUSH, ?PREFS_TC3_CHANGE_RESULT] =
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "pref", [],
    [
        exmpp_xml:element(undefined, "method",
	[
	    exmpp_xml:attribute("type", "auto"),
	    exmpp_xml:attribute("use", "concede")
	], []),
        exmpp_xml:element(undefined, "method",
	[
	    exmpp_xml:attribute("type", "local"),
	    exmpp_xml:attribute("use", "forbid")
	], []),
        exmpp_xml:element(undefined, "method",
	[
	    exmpp_xml:attribute("type", "manual"),
	    exmpp_xml:attribute("use", "prefer")
	], [])
    ])), 2),
    ?PREFS_TC3_CHANGED = client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).

test_jid_prefs_change(F) ->
    [?PREFS_TC4_CHANGE_PUSH, ?PREFS_TC4_CHANGE_RESULT] =
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "pref", [],
    [
        exmpp_xml:element(undefined, "item",
	[
	    exmpp_xml:attribute("jid", "romeo@montague.net"),
	    exmpp_xml:attribute("save", "body"),
	    exmpp_xml:attribute("expire", "604800"),
	    exmpp_xml:attribute("otr", "concede")
	], [])
    ])), 2),
    ?PREFS_TC4_CHANGED = client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).

%test_jid_prefs_remove(F) ->
%    [?PREFS_TC5_CHANGE_PUSH, ?PREFS_TC5_CHANGE_RESULT] =
%    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "pref", [],
%    [
%        exmpp_xml:element(undefined, "itemremove",
%	[
%	    exmpp_xml:attribute("jid", "romeo@montague.net")
%	], [])
%    ])), 2).
%    ?PREFS_TC5_CHANGED = client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).
