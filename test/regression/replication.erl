-module(replication).

-include_lib("eunit/include/eunit.hrl").

-include("config.hrl").
-include("replication.hrl").

replication_test_() ->
{
    foreach,
    local, 
    fun client:session_setup/0,
    fun client:session_teardown/1, 
    [
        ?test_gen1(test_retrieve_replication)
    ]
}.

test_retrieve_replication(F) ->
    ?REPLICATION_TC1_UPLOAD_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "save", [],
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
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "modified",
    [
	exmpp_xml:attribute("start", "1469-07-21T01:14:47Z")
    ],
    [
        exmpp_xml:element("http://jabber.org/protocol/rsm", "set",
	[
	    exmpp_xml:attribute("max", "30")
	], [])
    ]))),
    ?REPLICATION_TC1_RETRIEVE_ELEMENTS = lists:sort(ReplicationElements).
