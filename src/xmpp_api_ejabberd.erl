%%%----------------------------------------------------------------------
%%% File    : xmpp_api_ejabberd.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : ejabberd-specific functionality
%%% Created : 18 Aug 2012 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-module(xmpp_api_ejabberd).
-author('xmpp@endl.ch').

-include("ejabberd.hrl").
-include_lib("exmpp/include/exmpp.hrl").

-export([get_valid_hosts/0,
         info_msg/2,
         error_msg/2,
         broadcast_iq/2,
         is_in_roster/3,
         jid_to_exmpp/1,
         jid_from_exmpp/1,
         iq_to_exmpp/1,
         iq_from_exmpp/1]).

get_valid_hosts() ->
    ?MYHOSTS.

info_msg(Format, Opts) ->
    ?INFO_MSG(Format, Opts).

error_msg(Format, Opts) ->
    ?ERROR_MSG(Format, Opts).

%% Broadcasts IQ to all active sessions.
%% FIXME: ASSUMES ejabberd 2.x !!!
broadcast_iq(From, IQ) ->
    lists:foreach(
        fun(Resource) ->
            ejabberd_router:route(
                jid_from_exmpp(exmpp_jid:make(exmpp_jid:domain(From))),
                jid_from_exmpp(exmpp_jid:make(exmpp_jid:node(From), exmpp_jid:domain(From),
                    Resource)),
                exmpp_xml:xmlel_to_xmlelement(exmpp_iq:iq_to_xmlel(IQ#iq{id = <<"push">>})))
            end,
        ejabberd_sm:get_user_resources(
            exmpp_jid:prep_node_as_list(From),
            exmpp_jid:prep_domain_as_list(From))).

% FIXME: ASSUMES ejabberd 2.x !!!
is_in_roster(User, Server, JID) ->
    case mod_roster:get_jid_info(undefined, User, Server, jid_from_exmpp(JID)) of
        {S, _} when S =/= none -> true;
        _ -> false
    end.

% ejabberd 2.x -> exmpp JID conversion
% If this function is called - we know we're under
% ejabberd 2.x, so using its specific jlib calls is fine.
jid_to_exmpp(Jid) ->
    exmpp_jid:parse(jlib:jid_to_string(Jid)).

jid_from_exmpp(Jid) ->
    jlib:string_to_jid(exmpp_jid:to_list(Jid)).

iq_to_exmpp(IQ) ->
    exmpp_iq:xmlel_to_iq(
        exmpp_xml:xmlelement_to_xmlel(iq_to_xml(IQ), [?NS_JABBER_CLIENT], [])).

iq_from_exmpp(IQ) ->
    jlib:iq_query_or_response_info(
        exmpp_xml:xmlel_to_xmlelement(
            exmpp_iq:iq_to_xmlel(IQ))).

% Fixed version of jlib iq_to_xml function

iq_type_to_string(set) -> "set";
iq_type_to_string(get) -> "get";
iq_type_to_string(result) -> "result";
iq_type_to_string(error) -> "error";
iq_type_to_string(_) -> invalid.

iq_to_xml({iq, ID, Type, _XmlNS, _Lang, SubEl}) ->
    SubElAsList =
        case SubEl of
          L when is_list(L) -> L;
          R -> [R]
        end,
    if
        ID =/= "" ->
            {xmlelement, "iq",
             [{"id", ID}, {"type", iq_type_to_string(Type)}], SubElAsList};
        true ->
            {xmlelement, "iq",
             [{"type", iq_type_to_string(Type)}], SubElAsList}
    end.
