%%%----------------------------------------------------------------------
%%% File    : mod_archive2_storage.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2-specific storage functionality
%%% Created : 07 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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

-module(mod_archive2_storage).
-author('ejabberd@ndl.kiev.ua').

%% Our hooks
-export([get_collection_links/1, get_collection_id/1]).

-include("mod_archive2.hrl").

%%--------------------------------------------------------------------
%% Retrieve collection links from minimally filled list of archive_collection
%% records. Required fields are 'with_*' and 'utc'.
%%--------------------------------------------------------------------

get_collection_links(LinksIDs) when is_list(LinksIDs) ->
    [get_collection_link(ID) || ID <- LinksIDs].

get_collection_link(undefined) ->
    undefined;

get_collection_link(ID) ->
    case ejabberd_storage:select(
        ets:fun2ms(fun(#archive_collection{id = ID1,
                                           with_user = WithUser,
                                           with_server = WithServer,
                                           with_resource = WithResource,
                                           utc = UTC}) when ID1 =:= ID ->
                        {WithUser, WithServer, WithResource, UTC}
                   end)) of
        {selected, [{WithUser, WithServer, WithResource, UTC}]} ->
            #archive_collection{with_user = WithUser,
                                with_server = WithServer,
                                with_resource = WithResource,
                                utc = UTC};
        _ ->
            undefined
    end.

%%--------------------------------------------------------------------
%% Retrieve collection ID from minimally filled archive_collection
%% record. Required fields are 'with_*' and 'utc'.
%%--------------------------------------------------------------------

get_collection_id(#archive_collection{} = C) ->
    case ejabberd_storage:select(
        ets:fun2ms(fun(#archive_collection{id = ID,
                       with_user = WithUser,
                       with_server = WithServer,
                       with_resource = WithResource,
                       utc = UTC}) when
                           WithUser =:= C#archive_collection.with_user,
                           WithServer =:= C#archive_collection.with_server,
                           WithResource =:= C#archive_collection.with_resource,
                           UTC =:= C#archive_collection.utc ->
                       {ID}
                   end)) of
        {selected, [{ID}]} ->
            ID;
        _ ->
            undefined
    end;

get_collection_id(undefined) -> undefined.
