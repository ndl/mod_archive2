%%%----------------------------------------------------------------------
%%% File    : client.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 testing common functionality
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

-module(client).
-author('ejabberd@ndl.kiev.ua').

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

%modules_to_test() -> [prefs, manual, management, replication, general, auto].
modules_to_test() -> [management].

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
