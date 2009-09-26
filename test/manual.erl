-module(manual).

-include_lib("eunit/include/eunit.hrl").

-include("config.hrl").
-include("manual.hrl").

manual_test_() ->
{
    foreach,
    local, 
    fun client:session_setup/0,
    fun client:session_teardown/1, 
    [
        client:gen(fun test_upload/1),
        client:gen(fun test_change_subject/1),
        client:gen(fun test_utc_attribute/1)
    ]
}.

test_upload(F) ->
    ?MANUAL_UPLOAD1_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "save", [],
    [
        exmpp_xml:element(undefined, "chat",
	[
	    exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	    exmpp_xml:attribute("start", "1469-07-21T02:56:15Z"),
	    exmpp_xml:attribute("thread", "damduoeg08"),
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
    ?MANUAL_UPLOADED1 =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "retrieve",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	exmpp_xml:attribute("start", "1469-07-21T02:56:15Z")
    ], []))).

test_change_subject(F) ->
    ?MANUAL_CHANGE1_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "save", [],
    [
        exmpp_xml:element(undefined, "chat",
	[
	    exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	    exmpp_xml:attribute("start", "1469-07-21T02:56:15Z"),
	    exmpp_xml:attribute("subject", "She speaks twice!")
	], [])
    ]))),
    ?MANUAL_CHANGED1 =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "retrieve",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	exmpp_xml:attribute("start", "1469-07-21T02:56:15Z")
    ], []))).

test_utc_attribute(F) ->
    ?MANUAL_CHANGE2_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "save", [],
    [
        exmpp_xml:element(undefined, "chat",
	[
	    exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	    exmpp_xml:attribute("start", "1469-07-21T02:56:15Z"),
	    exmpp_xml:attribute("subject", "She speaks!")
	],
	[
	    exmpp_xml:element(undefined, "from",
	    [
	        exmpp_xml:attribute("utc", "1469-07-21T00:32:29Z")
	    ],
	    [ exmpp_xml:element(undefined, "body", [], [exmpp_xml:cdata("Art thou not Romeo, and a Montague?")]) ]),
	    exmpp_xml:element(undefined, "to",
	    [
	        exmpp_xml:attribute("secs", "11")
	    ],
	    [ exmpp_xml:element(undefined, "body", [], [exmpp_xml:cdata("Neither, fair saint, if either thee dislike.")]) ]),
	    exmpp_xml:element(undefined, "from",
	    [
	        exmpp_xml:attribute("secs", "7")
	    ],
	    [ exmpp_xml:element(undefined, "body", [], [exmpp_xml:cdata("How cam'st thou hither, tell me, and wherefore?")]) ])
	])
    ]))),
    ?MANUAL_CHANGED2 =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "retrieve",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	exmpp_xml:attribute("start", "1469-07-21T02:56:15Z")
    ], []))).
