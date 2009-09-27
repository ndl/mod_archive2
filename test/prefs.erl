-module(prefs).

-include_lib("eunit/include/eunit.hrl").

-include("config.hrl").
-include("prefs.hrl").

prefs_test_() ->
{
    foreach,
    local, 
    fun client:session_setup/0,
    fun client:session_teardown/1, 
    [
        ?test_gen(test_default_prefs),
	?test_gen(test_global_prefs_change),
        ?test_gen(test_prefs_methods_change),
	?test_gen(test_jid_prefs_change)
%	?test_gen(test_jid_prefs_remove)
    ]
}.

test_default_prefs(F) ->
    ?DEFAULT_PREFS = client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).

test_global_prefs_change(F) ->
    [?PREFS_CHANGE1_PUSH, ?PREFS_CHANGE1_RESULT] =
    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "pref", [],
    [
        exmpp_xml:element(undefined, "default",
	[
	    exmpp_xml:attribute("otr", "prefer"),
	    exmpp_xml:attribute("save", "false")
	], [])
    ])), 2),
    ?PREFS_CHANGED1 = client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).

test_prefs_methods_change(F) ->
    [?PREFS_CHANGE2_PUSH, ?PREFS_CHANGE2_RESULT] =
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
    ?PREFS_CHANGED2 = client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).

test_jid_prefs_change(F) ->
    [?PREFS_CHANGE3_PUSH, ?PREFS_CHANGE3_RESULT] =
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
    ?PREFS_CHANGED3 = client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).

%test_jid_prefs_remove(F) ->
%    [?PREFS_CHANGE4_PUSH, ?PREFS_CHANGE4_RESULT] =
%    client:responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "pref", [],
%    [
%        exmpp_xml:element(undefined, "itemremove",
%	[
%	    exmpp_xml:attribute("jid", "romeo@montague.net")
%	], [])
%    ])), 2).
%    ?PREFS_CHANGED4 = client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).
