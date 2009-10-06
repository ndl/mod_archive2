-module(manual).

-include_lib("eunit/include/eunit.hrl").

-include("testing.hrl").
-include("manual.hrl").

manual_test_() ->
    ?test_foreach(
        client:session_setup,
        client:session_teardown,
        [
            ?test_gen1(test_upload),
            ?test_gen1(test_change_subject),
            ?test_gen1(test_utc_attribute),
            ?test_gen1(test_groupchat),
            ?test_gen1(test_linking),
            ?test_gen1(test_link_remove),
            ?test_gen1(test_add_attributes)
        ]).

test_upload(F) ->
    ?MANUAL_TC1_UPLOAD_RESULT =
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
    ?MANUAL_TC1_UPLOADED =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "retrieve",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	exmpp_xml:attribute("start", "1469-07-21T02:56:15Z")
    ], []))).

test_change_subject(F) ->
    ?MANUAL_TC2_CHANGE_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "save", [],
    [
        exmpp_xml:element(undefined, "chat",
	[
	    exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	    exmpp_xml:attribute("start", "1469-07-21T02:56:15Z"),
	    exmpp_xml:attribute("subject", "She speaks twice!")
	], [])
    ]))),
    ?MANUAL_TC2_CHANGED =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "retrieve",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	exmpp_xml:attribute("start", "1469-07-21T02:56:15Z")
    ], []))).

test_utc_attribute(F) ->
    ?MANUAL_TC3_CHANGE_RESULT =
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
    ?MANUAL_TC3_CHANGED =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "retrieve",
    [
        exmpp_xml:attribute("with", "juliet@capulet.com/chamber"),
	exmpp_xml:attribute("start", "1469-07-21T02:56:15Z")
    ], []))).

test_groupchat(F) ->
    ?MANUAL_TC4_UPLOAD_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "save", [],
    [
        exmpp_xml:element(undefined, "chat",
	[
	    exmpp_xml:attribute("with", "balcony@house.capulet.com"),
	    exmpp_xml:attribute("start", "1469-07-21T03:16:37Z")
	],
	[
	    exmpp_xml:element(undefined, "from",
	    [
	        exmpp_xml:attribute("secs", "0"),
		exmpp_xml:attribute("name", "benvolio")
	    ],
	    [ exmpp_xml:element(undefined, "body", [], [exmpp_xml:cdata("She will invite him to some supper.")]) ]),
	    exmpp_xml:element(undefined, "from",
	    [
	        exmpp_xml:attribute("secs", "6"),
		exmpp_xml:attribute("name", "mercutio")
	    ],
	    [ exmpp_xml:element(undefined, "body", [], [exmpp_xml:cdata("A bawd, a bawd, a bawd! So ho!")]) ]),
	    exmpp_xml:element(undefined, "from",
	    [
	        exmpp_xml:attribute("secs", "3"),
		exmpp_xml:attribute("name", "romeo"),
		exmpp_xml:attribute("jid", "romeo@montague.net")
	    ],
	    [ exmpp_xml:element(undefined, "body", [], [exmpp_xml:cdata("What hast thou found?")]) ])
	])
    ]))),
    ?MANUAL_TC4_UPLOADED =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "retrieve",
    [
        exmpp_xml:attribute("with", "balcony@house.capulet.com"),
	exmpp_xml:attribute("start", "1469-07-21T03:16:37Z")
    ], []))).

test_linking(F) ->
    ?MANUAL_TC5_UPLOAD_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "save", [],
    [
        exmpp_xml:element(undefined, "chat",
	[
	    exmpp_xml:attribute("with", "benvolio@montague.net"),
	    exmpp_xml:attribute("start", "1469-07-21T03:01:54Z")
	],
	[
	    exmpp_xml:element(undefined, "next",
	    [
	        exmpp_xml:attribute("with", "balcony@house.capulet.com"),
		exmpp_xml:attribute("start", "1469-07-21T03:16:37Z")
	    ], []),
	    exmpp_xml:element(undefined, "to",
	    [
	        exmpp_xml:attribute("secs", "0")
	    ],
	    [ exmpp_xml:element(undefined, "body", [], [exmpp_xml:cdata("O, I am fortune's fool!")]) ]),
	    exmpp_xml:element(undefined, "from",
	    [
	        exmpp_xml:attribute("secs", "4")
	    ],
	    [ exmpp_xml:element(undefined, "body", [], [exmpp_xml:cdata("Why dost thou stay?")]) ])
	])
    ]))),
    ?MANUAL_TC5_UPLOADED =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "retrieve",
    [
        exmpp_xml:attribute("with", "benvolio@montague.net"),
	exmpp_xml:attribute("start", "1469-07-21T03:01:54Z")
    ], []))),
    ?MANUAL_TC5_CHANGE_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "save", [],
    [
        exmpp_xml:element(undefined, "chat",
	[
	    exmpp_xml:attribute("with", "balcony@house.capulet.com"),
	    exmpp_xml:attribute("start", "1469-07-21T03:16:37Z")
	],
	[
	    exmpp_xml:element(undefined, "previous",
	    [
	        exmpp_xml:attribute("with", "benvolio@montague.net"),
		exmpp_xml:attribute("start", "1469-07-21T03:01:54Z")
	    ], [])
	])
    ]))),
    ?MANUAL_TC5_CHANGED =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "retrieve",
    [
        exmpp_xml:attribute("with", "balcony@house.capulet.com"),
	exmpp_xml:attribute("start", "1469-07-21T03:16:37Z")
    ], []))).

test_link_remove(F) ->
    ?MANUAL_TC6_CHANGE_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "save", [],
    [
        exmpp_xml:element(undefined, "chat",
	[
	    exmpp_xml:attribute("with", "balcony@house.capulet.com"),
	    exmpp_xml:attribute("start", "1469-07-21T03:16:37Z")
	],
	[
	    exmpp_xml:element(undefined, "previous", [], []),
	    exmpp_xml:element(undefined, "next", [], [])
	])
    ]))),
    ?MANUAL_TC6_CHANGED =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "retrieve",
    [
        exmpp_xml:attribute("with", "balcony@house.capulet.com"),
	exmpp_xml:attribute("start", "1469-07-21T03:16:37Z")
    ], []))).

test_add_attributes(F) ->
    ?MANUAL_TC7_CHANGE_RESULT =
    client:response(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "save", [],
    [
        exmpp_xml:element(undefined, "chat",
	[
	    exmpp_xml:attribute("with", "benvolio@montague.net"),
	    exmpp_xml:attribute("start", "1469-07-21T03:01:54Z")
	],
	[
	    exmpp_xml:element("jabber:x:data", "x",
	    [
	        exmpp_xml:attribute("type", "submit")
	    ],
	    [
	        exmpp_xml:element(undefined, "field",
		[
		    exmpp_xml:attribute("var", "FORM_TYPE")
		],
		[
		    exmpp_xml:element(undefined, "value", [],
		    [
		        exmpp_xml:cdata("http://example.com/archiving")
		    ])
		])
	    ])
	])
    ]))),
    ?MANUAL_TC7_CHANGED =
    client:response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "retrieve",
    [
        exmpp_xml:attribute("with", "benvolio@montague.net"),
	exmpp_xml:attribute("start", "1469-07-21T03:01:54Z")
    ], []))).
