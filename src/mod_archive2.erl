%%%----------------------------------------------------------------------
%%% File    : mod_archive2.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Support XEP-136 for messages archiving
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

%%--------------------------------------------------------------------
%% Options:
%%
%% default_auto_save -> true | false - is auto-save turned on by default or not;
%%     if true, default 'save' attribute will be set to 'body'.
%%
%% read_only -> true | false - if true, no client modifications either to
%%     collections or to archiving settings are allowed.
%%
%%  default_expire -> default time in seconds before collections are wiped out -
%%      or infinity atom.
%%
%%  enforce_min_expire -> minimal time in seconds before collections are wiped out
%%      that the user is allowed to set.
%%
%%  enforce_max_expire -> maximal time in seconds before collections are wiped out
%%      that the user is allowed to set - or infinity atom.
%%
%%  replication_expire -> time in seconds before 'removed' replication
%%      information if wiped out or infinity atom to disable.
%%
%%  session_duration -> time in secondes before the timeout of a session.
%%
%%  wipeout_interval -> time in seconds between wipeout runs or infinity atom
%%                      to disable.
%%
%% Default values:
%% - default_auto_save = false
%% - read_only = false
%% - default_expire = infinity
%% - enforce_min_expire = 0
%% - enforce_max_expire = infinity
%% - replication_expire = 31536000 (= 1 year)
%% - session_duration = 1800
%% - wipeout_interval = 86400 (= 1 day)
%%--------------------------------------------------------------------

-module(mod_archive2).
-author('ejabberd@ndl.kiev.ua').

-behaviour(gen_server).
-behaviour(gen_mod).

%% gen_mod callbacks
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Our hooks
-export([remove_user/2, send_packet/3, receive_packet/3, receive_packet/4,
         iq_archive/3]).

-include("mod_archive2.hrl").
-include("mod_archive2_storage.hrl").

-record(state, {host,
                options,
                default_global_prefs,
                dbinfo,
                auto_states,
                sessions,
                sessions_expiration_timer,
                collections_expiration_timer}).

-define(PROCNAME, ejabberd_mod_archive2).
-define(HOOK_SEQ, 50).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
    % Start our storage manager first
    RDBMS = gen_mod:get_opt(rdbms, Opts, mnesia),
    ejabberd_storage:start(Host,
        [{rdbms, RDBMS}, {schema, ?MOD_ARCHIVE2_SCHEMA}]),
    % Now start ourselves
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
        {Proc,
         {?MODULE, start_link, [Host, Opts]},
         permanent,
         1000,
         worker,
         [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    % Stop ourselves
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc),
    % Stop our storage manager
    ejabberd_storage:stop(Host).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Opts]) ->
    HostB = list_to_binary(Host),
    % Get options necessary for initialization
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    SessionDuration = gen_mod:get_opt(session_duration, Opts, 1800),
    WipeOutInterval = gen_mod:get_opt(wipeout_interval, Opts, 86400),
    AutoSave = gen_mod:get_opt(default_auto_save, Opts, false),
    Expire = gen_mod:get_opt(default_expire, Opts, infinity),
    GlobalPrefs = mod_archive2_prefs:default_global_prefs(AutoSave, Expire),
    % Initialize mnesia tables, if needed.
    case gen_mod:get_opt(rdbms, Opts, mnesia) of
        mnesia ->
            init_mnesia_tables();
        _ ->
            ok
    end,
    % Add all necessary hooks
    gen_iq_handler:add_iq_handler(ejabberd_sm, HostB, ?NS_ARCHIVING, ?MODULE,
                                  iq_archive, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, HostB, ?NS_ARCHIVING, ?MODULE,
                                  iq_archive, IQDisc),
    ejabberd_hooks:add(remove_user, HostB, ?MODULE, remove_user,
                       ?HOOK_SEQ),
    ejabberd_hooks:add(user_send_packet, HostB, ?MODULE, send_packet,
                       ?HOOK_SEQ),
    ejabberd_hooks:add(user_receive_packet, HostB, ?MODULE, receive_packet,
                       ?HOOK_SEQ),
    ejabberd_hooks:add(offline_message_hook, HostB, ?MODULE, receive_packet,
                       ?HOOK_SEQ),
    % Register our provided features
    mod_disco:register_feature(HostB, ?NS_ARCHIVING),
    mod_disco:register_feature(HostB, ?NS_ARCHIVING_AUTO),
    mod_disco:register_feature(HostB, ?NS_ARCHIVING_MANAGE),
    mod_disco:register_feature(HostB, ?NS_ARCHIVING_MANUAL),
    mod_disco:register_feature(HostB, ?NS_ARCHIVING_PREF),
    % Setup timers for auto-archived sessions cleaning and collections
    % expiration
    {ok, SessionsExpirationTimer} =
        timer:send_interval(1000 * SessionDuration div 2, expire_sessions),
    {ok, CollectionsExpirationTimer} =
        case WipeOutInterval of
            infinity ->
                {ok, undefined};
            N when is_integer(N) ->
                timer:send_interval(1000 * N, expire_collections)
        end,
    % We're done - return our state
    {ok, #state{host = Host,
                options = Opts,
                default_global_prefs = GlobalPrefs,
                auto_states = dict:new(),
                sessions = dict:new(),
                sessions_expiration_timer = SessionsExpirationTimer,
                collections_expiration_timer = CollectionsExpirationTimer}}.

init_mnesia_tables() ->
    mnesia:create_table(archive_collection,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, archive_collection)}]),
    mnesia:add_table_index(archive_collection, prev_id),
    mnesia:add_table_index(archive_collection, next_id),
    mnesia:add_table_index(archive_collection, us),
    mnesia:add_table_index(archive_collection, with_user),
    mnesia:add_table_index(archive_collection, with_server),
    mnesia:add_table_index(archive_collection, with_resource),
    mnesia:add_table_index(archive_collection, utc),
    mnesia:add_table_index(archive_collection, change_utc),
    mnesia:create_table(archive_message,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, archive_message)}]),
    mnesia:add_table_index(archive_message, coll_id),
    mnesia:add_table_index(archive_message, utc),
    mnesia:create_table(archive_jid_prefs,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, archive_jid_prefs)}]),
    mnesia:add_table_index(archive_jid_prefs, with_user),
    mnesia:add_table_index(archive_jid_prefs, with_server),
    mnesia:add_table_index(archive_jid_prefs, with_resource),
    mnesia:create_table(archive_global_prefs,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, archive_global_prefs)}]).

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    HostB = list_to_binary(State#state.host),
    % Cancel timers, if present
    timer:cancel(State#state.sessions_expiration_timer),
    timer:cancel(State#state.collections_expiration_timer),
    % Unregister our provided features
    mod_disco:unregister_feature(HostB, ?NS_ARCHIVING),
    mod_disco:unregister_feature(HostB, ?NS_ARCHIVING_AUTO),
    mod_disco:unregister_feature(HostB, ?NS_ARCHIVING_MANAGE),
    mod_disco:unregister_feature(HostB, ?NS_ARCHIVING_MANUAL),
    mod_disco:unregister_feature(HostB, ?NS_ARCHIVING_PREF),
    % Unregister all hooks
    gen_iq_handler:remove_iq_handler(ejabberd_local, HostB, ?NS_ARCHIVING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, HostB, ?NS_ARCHIVING),
    ejabberd_hooks:delete(remove_user, HostB, ?MODULE, remove_user,
        ?HOOK_SEQ),
    ejabberd_hooks:delete(user_send_packet, HostB, ?MODULE, send_packet,
        ?HOOK_SEQ),
    ejabberd_hooks:delete(user_receive_packet, HostB, ?MODULE, receive_packet,
        ?HOOK_SEQ),
    ejabberd_hooks:delete(offline_message_hook, HostB, ?MODULE, receive_packet,
        ?HOOK_SEQ),
    ok.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({From, _To, #iq{type = Type, payload = SubEl} = IQ}, _, State) ->
    #xmlel{name = Name} = SubEl,
    F =
        fun() ->
            case gen_mod:get_opt(read_only, State#state.options, false) of
                false ->
                    ok;
                true ->
                    if Type =:= set ->
                        throw({error, 'not-allowed'});
                       true ->
                        ok
                    end
            end,
	        case Name of
                'pref' ->
                    EnforceMinExpire =
                        gen_mod:get_opt(enforce_min_expire,
                            State#state.options, 0),
                    EnforceMaxExpire =
                        gen_mod:get_opt(enforce_max_expire,
                            State#state.options, infinity),
                    mod_archive2_prefs:pref(From, IQ,
                        State#state.default_global_prefs,
                        State#state.auto_states,
                        {EnforceMinExpire, EnforceMaxExpire});
                'itemremove' ->
                    mod_archive2_prefs:itemremove(From, IQ);
			    'auto' ->
                    case mod_archive2_prefs:auto(From, IQ,
                        State#state.auto_states) of
                        {atomic, AutoStates} ->
                            {reply, exmpp_iq:result(IQ),
                             State#state{auto_states = AutoStates}};
                        Result ->
                            Result
                    end;
			    'list' ->
                    mod_archive2_management:list(From, IQ);
			    'modified' ->
                    mod_archive2_management:modified(From, IQ);
			    'retrieve' ->
                    mod_archive2_management:retrieve(From, IQ);
			    'save' ->
                    mod_archive2_manual:save(From, IQ);
			    'remove' ->
                    RDBMS = gen_mod:get_opt(rdbms, State#state.options, mnesia),
                    case mod_archive2_management:remove(From, IQ, RDBMS,
                        State#state.sessions) of
                        {atomic, Sessions} ->
                            {reply, exmpp_iq:result(IQ),
                                State#state{sessions = Sessions}};
                        Result ->
                            Result
                    end;
			    _ -> exmpp_iq:error(IQ, 'bad-request')
		    end
        end,
    case catch F() of
        {reply, _, _} = Reply ->
            Reply;
        {atomic, ok} ->
            {reply, exmpp_iq:result(IQ), State};
        {atomic, R} ->
            {reply, R, State};
        {error, Error} ->
            {reply, handle_error(Error, IQ), State};
        {aborted, {error, Error}} ->
            {reply, handle_error(Error, IQ), State};
        {aborted, {throw, {error, Error}}} ->
            {reply, handle_error(Error, IQ), State};
        {aborted, Error} ->
            {reply, handle_error(Error, IQ), State};
        {'EXIT', Ex} ->
            ?ERROR_MSG("catched exit: ~p", [Ex]),
            {reply, exmpp_iq:error(IQ, 'internal-server-error'), State};
        Result ->
            ?ERROR_MSG("unexpected result: ~p", [Result]),
            {reply, exmpp_iq:error(IQ, 'internal-server-error'), State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add_message, {_Direction, From, With, _Packet} = Args}, State) ->
    AutoStates = State#state.auto_states,
    Prefs = State#state.default_global_prefs,
    case mod_archive2_prefs:should_auto_archive(From, With, AutoStates, Prefs) of
        true ->
            Sessions =
                State#state.sessions,
            TimeOut =
                gen_mod:get_opt(session_duration, State#state.options, 1800),
            NewSessions =
                mod_archive2_auto:add_message(Args, TimeOut, Sessions),
            {noreply, State#state{sessions = NewSessions}};
        _ ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(expire_sessions, State) ->
    Sessions = State#state.sessions,
    TimeOut = gen_mod:get_opt(session_duration, State#state.options, 1800),
    NewSessions = mod_archive2_auto:expire_sessions(Sessions, TimeOut),
    {noreply, State#state{sessions = NewSessions}};

handle_info(expire_collections, State) ->
    Host = State#state.host,
    RDBMS = gen_mod:get_opt(rdbms, State#state.options, mnesia),
    expire_collections(Host, State#state.options, RDBMS),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

handle_error(Error, IQ) ->
    ?INFO_MSG("error while executing archiving request: ~p", [Error]),
    if is_atom(Error) ->
        exmpp_iq:error(IQ, Error);
       true ->
        exmpp_iq:error(IQ, 'bad-request')
    end.

iq_archive(From, To, IQ) ->
    LServer = exmpp_jid:prep_domain_as_list(From),
    case lists:member(LServer, ?MYHOSTS) of
        false ->
            exmpp_iq:error(IQ, 'not-allowed');
        true ->
            Proc = gen_mod:get_module_proc(LServer, ?PROCNAME),
            gen_server:call(Proc, {From, To, IQ})
    end.

send_packet(From, To, Packet) ->
    add_packet(to, From, To, Packet).

receive_packet(From, To, Packet) ->
    add_packet(from, To, From, Packet).

receive_packet(_JID, From, To, Packet) ->
    receive_packet(From, To, Packet).

add_packet(Direction, OurJID, With, Packet) ->
    Host = exmpp_jid:prep_domain_as_list(OurJID),
    case lists:member(Host, ?MYHOSTS) of
        true ->
            Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
            gen_server:cast(
                Proc, {add_message, {Direction, OurJID, With, Packet}});
        false ->
            false
    end.

%%--------------------------------------------------------------------
%%% Maintenance functions
%%--------------------------------------------------------------------

remove_user(User, Server) ->
    JID = exmpp_jid:make(User, Server),
    Host = exmpp_jid:prep_domain_as_list(JID),
    US = exmpp_jid:prep_bare_to_list(JID),
    ejabberd_storage:transaction(
        Host,
        fun() ->
            ejabberd_storage:delete(
                ets:fun2ms(fun(#archive_jid_prefs{us = US1}) when US1 =:= US -> ok end)),
            ejabberd_storage:delete(
                ets:fun2ms(fun(#archive_global_prefs{us = US1}) when US1 =:= US -> ok end)),
            ejabberd_storage:delete(
                ets:fun2ms(fun(#archive_collection{us = US1}) when US1 =:= US -> ok end))
        end).

%% TODO: implement!!!
expire_collections(_Host, _Opts, mnesia) ->
    ok;

expire_collections(Host, Opts, RDBMS) ->
    UTCField = "archive_collection.utc",
    Now = ejabberd_storage_odbc:encode(
        calendar:now_to_datetime(mod_archive2_time:now()),
        ejabberd_storage_utils:get_table_info(archive_collection,
            ?MOD_ARCHIVE2_SCHEMA)),
    ExpireByDefault =
    	case gen_mod:get_opt(default_expire, Opts, infinity) of
            infinity ->
	            "";
            N when is_integer(N) ->
         	    ["or
		          archive_global_prefs.expire is null and
                  domain_jid_prefs.expire is null and
                  full_jid_prefs.expire is null and
                  bare_jid_prefs.expire is null and ",
	             get_expired_condition(Host, integer_to_list(N), UTCField), " < ", Now]
        end,
    Query =
    ["update archive_collection
             left join archive_jid_prefs as full_jid_prefs
                 on archive_collection.with_user = full_jid_prefs.with_user and
                    archive_collection.with_server = full_jid_prefs.with_server and
                    archive_collection.with_resource = full_jid_prefs.with_resource
             left join archive_jid_prefs as bare_jid_prefs
                 on archive_collection.with_user = bare_jid_prefs.with_user and
                    archive_collection.with_server = bare_jid_prefs.with_server and
                    bare_jid_prefs.with_resource = '' and
                    bare_jid_prefs.exactmatch = 0
             left join archive_jid_prefs as domain_jid_prefs
                 on archive_collection.with_server = domain_jid_prefs.with_server and
                    domain_jid_prefs.with_user = '' and
                    domain_jid_prefs.with_resource = '' and
                    domain_jid_prefs.exactmatch = 0
             left join archive_global_prefs
                 on archive_collection.us = archive_global_prefs.us
      set deleted = 1
      where
          deleted = 0 and
          not full_jid_prefs.expire is null and ",
      get_expired_condition(RDBMS, "full_jid_prefs.expire", UTCField), " < ", Now, " or
          not bare_jid_prefs.expire is null and
              full_jid_prefs.expire is null and ",
      get_expired_condition(RDBMS, "bare_jid_prefs.expire", UTCField), " < ", Now, " or
          not domain_jid_prefs.expire is null and
              full_jid_prefs.expire is null and
              bare_jid_prefs.expire is null and ",
      get_expired_condition(RDBMS, "domain_jid_prefs.expire", UTCField), " < ", Now, " or
          not archive_global_prefs.expire is null and
              domain_jid_prefs.expire is null and
              full_jid_prefs.expire is null and
              bare_jid_prefs.expire is null and ",
      get_expired_condition(RDBMS, "archive_global_prefs.expire", UTCField), " < ", Now,
      ExpireByDefault],
    ejabberd_storage:transaction(Host,
        fun() ->
	        ejabberd_storage:sql_query(Query),
            case gen_mod:get_opt(replication_expire, Opts, 31536000) of
		        infinity ->
                    ok;
		        N1 when is_integer(N1) ->
			        ejabberd_storage:sql_query(
                        ["delete from archive_collection "
				         "where deleted = 1 "
				         "and ",
                         get_expired_condition(RDBMS, integer_to_list(N1),
                             "archive_collection.change_utc"), " < ", Now])
            end
        end).

get_expired_condition(RDBMS, ExpireField, UTCField) ->
    case RDBMS of
        mysql ->
            ["timestampadd(second, ", ExpireField, ", ", UTCField, ")"];
        sqlite ->
            ["datetime(", UTCField, ", '+' || ", ExpireField,
             " || ' seconds')"];
        pgsql ->
            ["timestamp ", UTCField, " + interval ", ExpireField,
             " || ' seconds'"];
        _ ->
            throw({error, 'unknown-rdbms'})
    end.
