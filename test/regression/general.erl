-module(general).

-include_lib("eunit/include/eunit.hrl").

-include("config.hrl").
-include("general.hrl").

general_test_() ->
{
    foreach,
    local, 
    fun client:session_setup/0,
    fun client:session_teardown/1, 
    [
        ?test_gen1(test_disco),
        ?test_gen1(test_remove_user),
        ?test_gen1(test_remove_user2)
    ]
}.

test_disco(F) ->
    ?GENERAL_TC1_QUERY_RESULT =
    client:response(F, exmpp_xml:element(undefined, "iq",
    [
        exmpp_xml:attribute("from", ?CLIENTJID),
	exmpp_xml:attribute("to", ?SERVERHOST),
	exmpp_xml:attribute("type", "get")
    ],
    [
        exmpp_xml:element("http://jabber.org/protocol/disco#info", "query", [], [])
    ])),
    DiscoSorted = lists:sort(lists:filter(
        fun({xmlel, _, _, _, [{xmlattr,_,_,NS}], []}) -> string:str(binary_to_list(NS), "xep-0136") /= 0;
	(_) -> false end, DiscoElements)),
    ?GENERAL_TC1_DISCO_ELEMENTS = DiscoSorted.

test_remove_user(F) ->
    ?GENERAL_TC2_QUERY_RESULT =
    client:response(F, exmpp_client_register:remove_account()).

test_remove_user2(F) ->
    ?GENERAL_TC3_RETRIEVE_RESULT =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "modified",
    [
	exmpp_xml:attribute("start", "1469-07-21T01:14:47Z")
    ],
    [
        exmpp_xml:element("http://jabber.org/protocol/rsm", "set",
	[
	    exmpp_xml:attribute("max", "30")
	], [])
    ]))).
