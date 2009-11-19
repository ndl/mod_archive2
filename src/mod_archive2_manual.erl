%%%----------------------------------------------------------------------
%%% File    : mod_archive2_manual.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 manual collections upload support
%%% Created : 14 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%%
%%% mod_archive2, Copyright (C) 2009 Alexander Tsvyashchenko
%%%
%%% Based on earlier works by:
%%%  - Olivier Goffart <ogoffar@kde.org> (mnesia version)
%%%  - Alexey Shchepin <alexey@process-one.net> (PostgreSQL version)
%%%  - Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua> (ODBC version)
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

-module(mod_archive2_manual).
-author('ejabberd@ndl.kiev.ua').

%% Our hooks
-export([save/2]).

-include("mod_archive2.hrl").

%%--------------------------------------------------------------------
%% Uploads given collections
%%--------------------------------------------------------------------

save(From, #iq{type = Type, payload = SubEl} = IQ) ->
    mod_archive2_utils:verify_iq_type(Type, set),
    F =
        fun() ->
            XC = exmpp_xml:get_element(SubEl, chat),
            InC = mod_archive2_xml:collection_from_xml(From, XC),
            InMessages =
                [mod_archive2_xml:message_from_xml(M,
                    InC#archive_collection.utc) ||
                 M <- exmpp_xml:get_child_elements(XC),
                 M#xmlel.name =:= to orelse
                 M#xmlel.name =:= from orelse
                 M#xmlel.name =:= note],
            OutC =
                case mod_archive2_storage:get_collection(InC, by_link,
                    all, [id, version]) of
                    undefined ->
                        NewC = InC#archive_collection{version = 0, deleted = 0},
                        {inserted, 1, ID} = ejabberd_storage:insert([NewC]),
                        NewC#archive_collection{id = ID};
                    #archive_collection{id = ID, version = OldVersion} ->
                        NewC =
                            InC#archive_collection{
                                id = ID,
                                version = OldVersion + 1},
                        ejabberd_storage:update(NewC),
                        NewC
                end,
            Messages =
                [M#archive_message{coll_id = OutC#archive_collection.id} ||
                 M <- InMessages],
            ejabberd_storage:insert(Messages),
            exmpp_iq:result(IQ,
                exmpp_xml:element(?NS_ARCHIVING, save, [],
                    [mod_archive2_xml:collection_to_xml(chat, OutC)]))
        end,
        ejabberd_storage:transaction(exmpp_jid:prep_domain_as_list(From), F).