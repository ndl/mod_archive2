-module(client).
-export([xml_report/1, session_setup/0, session_teardown/1, response/2, responses/3]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("config.hrl").

xml_report(OutDir) ->
    lists:foreach(
        fun(Module) ->
            eunit:test(Module, [{report, {eunit_surefire, [{dir, OutDir}]}}])
	end,
	modules_to_test()).

client_test_() -> modules_to_test().

modules_to_test() -> [prefs, manual, management, replication, general].

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

responses({Session, _JID}, Query, Count) ->
    ?DEBUG_FMT("Request (XML): ~p~n", [exmpp_xml:document_to_list(Query)]),
    exmpp_session:send_packet(Session, Query),
    Responses = responses([], Count),
    lists:map(fun(_Response) -> ?DEBUG_FMT("Response (XML): ~p~nResponse (XMLEL): ~p~n", [exmpp_xml:document_to_list(_Response#received_packet.raw_packet), _Response]) end, Responses),
    Responses.

responses(Responses, 0) ->
    lists:reverse(Responses);

responses(Responses, Count) ->
    receive
        Response -> responses([Response | Responses], Count - 1)
    after ?TIMEOUT ->
        throw("No response from server!")
    end.

response(F, Query) ->
    [Response] = responses(F, Query, 1),
    Response.
