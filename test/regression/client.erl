-module(client).
-export([eunit_xml_report/1, session_setup/0, session_teardown/1,
         create_session/1,response/2, responses/3, skip/1]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("testing.hrl").

eunit_xml_report(OutDir) ->
    lists:foreach(
        fun(Module) ->
	    ?EUNIT_XML_REPORT(Module, OutDir)
	end,
	modules_to_test()).

client_test_() -> modules_to_test().

modules_to_test() -> [prefs, manual, management, replication, general, auto].

create_session(ClientName) ->
    Session = exmpp_session:start(),
    JID = exmpp_jid:make(ClientName, ?SERVERHOST, random),
    exmpp_session:auth_basic_digest(Session, JID, ?PASSWORD),
    exmpp_session:connect_TCP(Session, ?SERVERHOST, ?SERVERPORT),
    try exmpp_session:login(Session)
    catch
	throw:{auth_error, 'not-authorized'} ->
	    exmpp_session:register_account(Session, ?PASSWORD),
	    exmpp_session:login(Session)
    end,
    exmpp_session:send_packet(Session,
		exmpp_presence:set_status(
		exmpp_presence:available(), "Test Client Is Ready")),
    % Skip presense.
    Resp = skip(1),
    % That was not presense but welcome message? Attempt to skip presense
    % once more.
    case Resp of
        {received_packet,message, _, _, _, _, _} -> skip(1);
        _ -> ok
    end,
    {Session, JID}.

session_setup() ->
    application:start(exmpp),
    create_session(?CLIENTNAME).

session_teardown({Session, _JID}) ->
    exmpp_session:stop(Session).

responses({Session, _JID}, Query, Count) ->
    ?DEBUG_FMT("Request (XML): ~p~n", [exmpp_xml:document_to_list(Query)]),
    exmpp_session:send_packet(Session, Query),
    Responses = responses([], Count),
    lists:map(
        fun(_Response) ->
                ?DEBUG_FMT("Response (XML): ~p~nResponse (XMLEL): ~p~n",
                           [exmpp_xml:document_to_list(
                               _Response#received_packet.raw_packet),
                            _Response])
        end, Responses),
    Responses.

responses(Responses, 0) ->
    lists:reverse(Responses);

responses(Responses, Count) ->
    receive
        Response -> responses([Response | Responses], Count - 1)
    after ?SERVER_TIMEOUT ->
        throw("No response from server!")
    end.

skip(Count) ->
    skip(Count, undefined).

skip(0, LastResponse) -> LastResponse;

skip(Count, _LastResponse) ->
    receive
        Response ->
            ?DEBUG_FMT("Skipped response: ~p~n", [Response]),
            skip(Count - 1, Response)
    after ?SERVER_TIMEOUT ->
        throw("No response from server!")
    end.

response(F, Query) ->
    [Response] = responses(F, Query, 1),
    Response.
