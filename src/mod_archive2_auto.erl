%%%----------------------------------------------------------------------
%%% File    : mod_archive2_auto.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 automatic archiving
%%% Created : 17 Oct 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
%%%
%%% mod_archive2, Copyright (C) 2009 Alexander Tsvyashchenko
%%%
%%% Based on earlier works by:
%%%  - Olivier Goffart <ogoffar@kde.org> (mnesia version)
%%%  - Alexey Shchepin <alexey@process-one.net> (PostgreSQL version)
%%%  - Alexander Tsvyashchenko <xmpp@endl.ch> (ODBC version)
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

-module(mod_archive2_auto).
-author('xmpp@endl.ch').

-export([expire_sessions/2, filter_sessions/2, add_message/6]).

-include("mod_archive2.hrl").

-define(ZERO_DATETIME, {{0, 0, 0}, {0, 0, 0}}).

%%--------------------------------------------------------------------
%% Filters given sessions according to specified filter function
%%--------------------------------------------------------------------

filter_sessions(Filter, Sessions) ->
    F =
        fun(WithKey, Threads) ->
                    dict:filter(
                fun(_Thread, Session) ->
                                    Filter(WithKey, Session)
                            end,
                Threads)
            end,
    FilteredSessions = dict:map(F, Sessions),
    % Remove all dictionaries with empty 2nd-level dict
    dict:filter(
        fun(_WithKey, Threads) ->
                   dict:size(Threads) > 0
        end,
        FilteredSessions).

expire_sessions(Sessions, TimeOut) ->
    TS = mod_archive2_utils:current_datetime(microseconds),
    filter_sessions(
        fun(_WithKey, Session) ->
            not is_session_expired(Session, TS, TimeOut)
        end,
        Sessions).

add_message({_Direction, _From, _With, Packet} = Args,
            TimeOut, TimeAccuracy, AutoSave, Filters, Sessions) ->
    case mod_archive2_xml:external_message_from_xml(Packet, AutoSave =:= message, Filters) of
        #external_message{} = EM ->
            case exmpp_xml:get_element(Packet, body) of
                undefined ->
                    Sessions;
                _ ->
                    add_message2(Args, EM, TimeOut, TimeAccuracy, Sessions)
            end;
        _ -> Sessions
    end.

add_message2({Direction, From, With, _Packet}, EM, TimeOut, TimeAccuracy, Sessions) ->
    F =
        fun() ->
            {NewSessions, Session} =
                get_session(From, With, EM, TimeOut, TimeAccuracy, Sessions),
            case Session#session.version of
                0 ->
                    % Nothing to update, collection was just created.
                    ok;
                _ ->
                    dbms_storage:update(
                        #archive_collection{
                            id = Session#session.id,
                            change_utc = Session#session.last_access,
                            version = Session#session.version,
                            with_resource = Session#session.resource,
                            subject = EM#external_message.subject,
                            thread =
                                if EM#external_message.thread =/=
                                    undefined ->
                                    EM#external_message.thread;
                                   true ->
                                    null
                                end})
            end,
            M = #archive_message{
                coll_id = Session#session.id,
                utc = Session#session.last_access,
                direction = Direction,
                name =
                    if EM#external_message.type =:= groupchat ->
                        if EM#external_message.nick =/= undefined ->
                                        EM#external_message.nick;
                           true ->
                            exmpp_jid:prep_resource_as_list(With)
                        end;
                       true ->
                        undefined
                    end,
                body = EM#external_message.body},
            dbms_storage:insert([M]),
            NewSessions
        end,
    case dbms_storage:transaction(
        exmpp_jid:prep_domain_as_list(From), F) of
        {atomic, Result} ->
            Result;
        _ ->
            Sessions
    end.

%%
%% In fact there's small problem with resources: we can send the message
%% to recepient without specifying resource (typically, when sending the
%% first message), but reply will come from the full JID and subsequent
%% messages will go to this full JID.
%%
%% This means that we either should strip resouce completely from JID when
%% creating the key (which is easy, but not nice, as then all messages will
%% be put into single collection without treating resources at all), or use
%% more intelligent approach to match the appropriate collection to our
%% message.
%%
%% Additionally we'd like to use "thread" to differentiate between different
%% conversations, to put them into different collections.
%%
%% Here is the approach we use:
%%
%% 1) There's two levels key schema: first key is bare JID, second-level
%%    key is the thread. If thread is not present, {no_thread, Resource} is
%%    used instead.
%% 2) If thread is specified in the message - just use both-levels keys
%%    normally, reusing existing collections if there's a match or creating
%%    new one if no matching collection found.
%% 3) Otherwise use first-level key to get all sub-items, then
%%      * If resource IS specified: search for matching resource:
%%        - if found - use it.
%%        - if not, search for sub-item with empty resource. If found, use
%%          it and rewrite its resource to ours, notifying the caller to
%%          store collection. If not - create new one.
%%      * If resource IS NOT specified: use the most recent sub-item or
%%        create new if none exists, notifying the caller about change of
%%        resource, if needed.
%%
get_session(From, With, EM, TimeOut, TimeAccuracy, InSessions) ->
    % Assume empty resource for groupchat messages so that they're recorded
    % to the same collection.
    Type = EM#external_message.type,
    Resource =
        case Type of
            groupchat ->
                undefined;
            _ ->
                exmpp_jid:prep_resource_as_list(With)
        end,
    WithKey = {exmpp_jid:prep_bare_to_list(From), exmpp_jid:bare(With)},
    TS = mod_archive2_utils:current_datetime(TimeAccuracy),
    Thread = EM#external_message.thread,
    % Make sure the session is removed if it is timed out, as otherwise
    % our tracking logic below might go wrong. We do not use all sessions
    % expiration function here for performance reasons.
    Sessions =
        expire_sessions_subset(WithKey, TS, TimeOut, InSessions),
    case dict:find(WithKey, Sessions) of
        error ->
            new_session(WithKey, TS, Resource, Thread, Sessions);
        {ok, Threads} ->
            if Thread =/= undefined ->
                        case dict:find(Thread, Threads) of
                                error ->
                                    new_session(WithKey, TS, Resource, Thread, Sessions);
                                {ok, Session} ->
                                    updated_session(WithKey, TS, Thread,
                            Session#session{resource = Resource}, Sessions)
                        end;
                   true ->
                        if Resource =/= undefined ->
                                case dict:find({no_thread, Resource}, Threads) of
                                        {ok, Session} ->
                                            updated_session(WithKey, TS, Thread,
                                Session, Sessions);
                                        error ->
                                            case dict:find({no_thread, undefined}, Threads) of
                                                    error ->
                                                        new_session(WithKey, TS, Resource, Thread,
                                        Sessions);
                                                    {ok, Session} ->
                                    NewThreads = dict:erase(
                                        {no_thread, undefined}, Threads),
                                    NewSessions = dict:store(WithKey,
                                        NewThreads, Sessions),
                                                        updated_session(WithKey, TS,
                                        Thread,
                                        Session#session{resource = Resource},
                                        NewSessions)
                                            end
                                end;
                           true ->
                                F =
                        fun(_,
                            #session{last_access = Last,
                                resource = SessionResource} = Value,
                            #session{last_access = MaxLast} = PrevValue) ->
                                                if ((Type =/= groupchat) andalso
                                (Last > MaxLast)) orelse
                               ((Type =:= groupchat) andalso
                                (SessionResource =:= undefined)) ->
                               Value;
                                                  true ->
                               PrevValue
                                                end
                                        end,
                                case dict:fold(F, #session{
                        last_access = ?ZERO_DATETIME,
                        resource = undefined}, Threads) of
                                        #session{last_access = ?ZERO_DATETIME} ->
                                            new_session(WithKey, TS, Resource, Thread, Sessions);
                                        #session{resource = NewResource} = Session ->
                                            updated_session(WithKey, TS, Thread,
                                Session#session{resource = NewResource},
                                Sessions)
                                end
                        end
            end
    end.

updated_session(WithKey, TS, Thread, Session, Sessions) ->
    UpdatedSession = Session#session{
        last_access = TS,
        version = Session#session.version + 1},
    {updated_sessions(WithKey, Thread, UpdatedSession, Sessions),
        UpdatedSession}.

new_session({US, BareJID} = WithKey, TS, Resource, Thread, Sessions) ->
    C = #archive_collection{
        us = US,
        with_user = exmpp_jid:prep_node_as_list(BareJID),
        with_server = exmpp_jid:prep_domain_as_list(BareJID),
        with_resource = Resource,
        utc = TS},
    CID =
        case mod_archive2_storage:get_collection(C, by_link, existing, [id]) of
            undefined ->
                NewC = C#archive_collection{
                    change_utc = TS,
                    version = 0,
                    deleted = false,
                    thread = Thread},
                {inserted, 1, ID} = dbms_storage:insert([NewC]),
                ID;
            #archive_collection{id = ID} ->
                ID
        end,
    Session = #session{
        utc = TS,
        last_access = TS,
        id = CID,
        resource = Resource,
        version = 0},
    {updated_sessions(WithKey, Thread, Session, Sessions), Session}.

updated_sessions(WithKey, Thread, Session, Sessions) ->
    Threads = case dict:find(WithKey, Sessions) of
               error -> dict:new();
               {ok, Result} -> Result
           end,
    ThreadKey =
        case Thread of
            undefined ->
                {no_thread, Session#session.resource};
            _ ->
                Thread
        end,
    NewThreads = dict:store(ThreadKey, Session, Threads),
    dict:store(WithKey, NewThreads, Sessions).

expire_sessions_subset(WithKey, TS, TimeOut, Sessions) ->
    case dict:find(WithKey, Sessions) of
        error ->
            Sessions;
        {ok, Threads} ->
            NewThreads =
                dict:filter(
                        fun(_WithKey, Session) ->
                            not is_session_expired(Session, TS, TimeOut)
                    end,
                        Threads),
            dict:store(WithKey, NewThreads, Sessions)
    end.

is_session_expired(Session, TS, TimeOut) ->
    TimeDiff =
        mod_archive2_utils:datetime_to_microseconds(TS) -
        mod_archive2_utils:datetime_to_microseconds(Session#session.last_access),
    TimeDiff > 1000000 * TimeOut.
