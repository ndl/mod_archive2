%%%----------------------------------------------------------------------
%%% File    : importer.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : Imports history to at least somewhat XEP-136 compliant server
%%% Created : 20 Aug 2012 by Alexander Tsvyashchenko <xmpp@endl.ch>
%%%
%%% mod_archive2, Copyright (C) 2009,2012 Alexander Tsvyashchenko
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

-module(importer).
-author('xmpp@endl.ch').

-export([start/0]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-include("importer.hrl").

%-define(DEBUG_FMT, io:format).
-define(DEBUG_FMT(Format, Value), ok).

create_session() ->
    Session = exmpp_session:start(),
    JID = exmpp_jid:make(?USER_NAME, ?SERVER_HOST, random),
    exmpp_session:auth_basic(Session, JID, ?USER_PASSWORD),
    %exmpp_session:auth_basic_digest(Session, JID, ?USER_PASSWORD),
    exmpp_session:connect_TCP(Session, ?SERVER_HOST, ?SERVER_PORT, [{starttls, enabled}, {compression, enabled}]),
    try exmpp_session:login(Session)
    catch
        throw:{auth_error, 'not-authorized'} ->
            exmpp_session:register_account(Session, ?USER_PASSWORD),
            exmpp_session:login(Session)
    end,
    exmpp_session:send_packet(Session,
		exmpp_presence:set_status(
		exmpp_presence:available(), "History importer bot is ready.")),
    {Session, JID}.

start() ->
    application:start(exmpp),
    {Session, _JID} = create_session(),
    Parser = exmpp_xml:start_parser([{root_depth, 1}]),
    % Fake root element so that parsing doesn't fail.
    exmpp_xml:parse(Parser, "<root>"),
    {Chat, Msgs} = get_elements(Session, Parser, {undefined, []}),
    if length(Msgs) =/= 0 ->
        send_chat(Session, Chat, Msgs);
       true ->
        ok
    end,
    exmpp_session:stop(Session).

get_elements(Session, Parser, {Chat, Msgs}) ->
    case file:read(standard_io, ?READ_BLOCK_SIZE) of
        eof ->
            send_chat(Session, Chat, Msgs);
        {ok, Data} ->
            ?DEBUG_FMT("Parsing this data: ~s~n", [Data]),
            case exmpp_xml:parse(Parser, Data) of
                continue ->
                    get_elements(Session, Parser, {Chat, Msgs});
                NewEls ->
                    get_elements(Session, Parser, process_elements(Session, Chat, Msgs, NewEls))
            end
    end.

process_elements(Session, undefined, Msgs, [#xmlel{name = 'chat'} = El | Tail]) ->
    process_elements(Session, El, Msgs, Tail);

process_elements(Session, Chat, Msgs, [#xmlel{name = Name} = El | Tail]) when Name =/= 'chat' ->
    if length(Msgs) >= ?MAX_MESSAGES ->
        send_chat(Session, Chat, Msgs),
        process_elements(Session, Chat, [El], Tail);
       true ->
        process_elements(Session, Chat, Msgs ++ [El], Tail)
    end;

% Parser might insert xmlcdata for newlines.
process_elements(Session, Chat, Msgs, [{xmlcdata, <<"\n">>} | Tail]) ->
    process_elements(Session, Chat, Msgs, Tail);

process_elements(_Session, Chat, Msgs, []) ->
    {Chat, Msgs};

process_elements(Session, Chat, Msgs, [#xmlel{name = 'chat'} = El | Tail]) ->
    send_chat(Session, Chat, Msgs),
    process_elements(Session, El, [], Tail).

send_chat(_, undefined, _) ->
    ok;

send_chat(Session, Chat, Msgs) ->
    FullChat = exmpp_xml:append_children(Chat, Msgs),
    Req =
        exmpp_iq:set(undefined,
                    exmpp_xml:element(
                        ?SERVER_NS_ARCHIVING,
                        "save",
                        [],
                        [FullChat])),
    ?DEBUG_FMT("Sending: ~s~n", [exmpp_xml:document_to_iolist(Req)]),
    ID = exmpp_xml:get_attribute(Req, <<"id">>, undefined),
    exmpp_session:send_packet(Session, Req),
    case response_iq(ID) of
        #received_packet{packet_type = 'iq', type_attr="error", raw_packet = IQ} ->
            io:format("ERROR: ~p~n", [IQ]),
            erlang:error(IQ);
        #received_packet{packet_type = 'iq', type_attr="result", raw_packet = _IQ} ->
            ok
    end.

response() ->
    receive
        Response -> Response
    after ?SERVER_TIMEOUT ->
        throw("No response from server!")
    end.

response_iq(ID) ->
    R = response(),
    case R of
        #received_packet{packet_type = 'iq', raw_packet = IQ} ->
            NewID = exmpp_xml:get_attribute(IQ, <<"id">>, undefined),
            if NewID =:= ID -> R;
               true -> response_iq(ID)
            end;
        _ -> response_iq(ID)
    end.
