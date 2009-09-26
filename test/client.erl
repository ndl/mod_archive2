-module(client).
-export([xml_report/1]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVERHOST, "ndl-server").
-define(SERVERPORT, 6222).
-define(CLIENTNAME, "client").
-define(PASSWORD, "password").
-define(NS, "http://www.xmpp.org/extensions/xep-0136.html#ns").
-define(TIMEOUT, 5000).

-include("global_prefs.hrl").

xml_report(OutDir) ->
    eunit:test(client, [{report,{eunit_surefire,[{dir,OutDir}]}}]).

client_test_() ->
{
    setup,
    local, 
    fun session_setup/0,
    fun session_teardown/1, 
    {
        with,
        [
	    fun test_default_global_prefs/1,
	    fun test_default_global_prefs_change/1
	]
    }
}.

session_setup() ->
    application:start(exmpp),
    Session = exmpp_session:start(),
    JID = exmpp_jid:make(?CLIENTNAME, ?SERVERHOST, random),
    exmpp_session:auth_basic_digest(Session, JID, ?PASSWORD),
    exmpp_session:connect_TCP(Session, ?SERVERHOST, ?SERVERPORT),
    try exmpp_session:login(Session)
    catch
	throw:{auth_error, 'not-authorized'} ->
	    exmpp_session:register_account(Session, ?PASSWORD),
	    exmpp_session:login(Session)
    end,
    {Session, JID}.

session_teardown({Session, _JID}) ->
    exmpp_session:stop(Session).


response({Session, _JID}, Query) ->
    exmpp_session:send_packet(Session, Query),
    receive
        Response -> Response
    after ?TIMEOUT ->
        throw("No response from server!")
    end.

responses({Session, _JID}, Query, Count) ->
    exmpp_session:send_packet(Session, Query),
    responses([], Count).

responses(Responses, 0) ->
    lists:reverse(Responses);

responses(Responses, Count) ->
    receive
        Response -> responses([Response | Responses], Count - 1)
    after ?TIMEOUT ->
        throw("No response from server!")
    end.

test_default_global_prefs(F) ->
    ?DEFAULT_GLOBAL_PREFS = response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).

test_default_global_prefs_change(F) ->
    [?DEFAULT_GLOBAL_PREFS_CHANGE_PUSH, ?DEFAULT_GLOBAL_PREFS_CHANGE_RESULT] =
    responses(F, exmpp_iq:set(undefined, exmpp_xml:element(?NS, "pref", [],
    [
        exmpp_xml:element(undefined, "default",
	[
	    exmpp_xml:attribute("otr", "prefer"),
	    exmpp_xml:attribute("save", "false")
	], [])
    ])), 2),
    ?DEFAULT_GLOBAL_PREFS_CHANGED = response(F, exmpp_iq:get(undefined, exmpp_xml:element(?NS, "pref"))).
