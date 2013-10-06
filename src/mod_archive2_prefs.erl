%%%----------------------------------------------------------------------
%%% File    : mod_archive2_prefs.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 archiving preferences management
%%% Created : 16 Nov 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-module(mod_archive2_prefs).
-author('xmpp@endl.ch').

-export([pref/6, auto/4, itemremove/4, default_global_prefs/2,
         should_auto_archive/8, expire_prefs_cache/1,
         get_effective_jid_prefs/2, get_global_prefs/2,
	 remove_session/2, reset_thread_expiration/3, expire_threads/2]).

-include("mod_archive2.hrl").
-include("mod_archive2_storage.hrl").

-record(auto_state, {stream_auto_save, with, threads}).
-record(thread_info, {last_access, auto_save}).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

%% Processes 'pref' requests from clients
pref(From, #iq{type = Type, payload = SubEl} = IQ, GlobalPrefs, AutoStates,
     EnforceExpire, XmppApi) ->
    case Type of
	    set ->
            case pref_set(From, SubEl, EnforceExpire) of
                {atomic, ok} ->
                    XmppApi:broadcast_iq(From, IQ),
                    {atomic, {auto_states, clear_with_auto_states(
                        From, AutoStates)}};
                Result ->
                    Result
            end;
	    get ->
	        pref_get(From, IQ, GlobalPrefs, AutoStates)
    end.

%% Processes 'auto' requests from clients
auto(From, #iq{type = Type, payload = AutoEl} = IQ, AutoStates, XmppApi) ->
    mod_archive2_utils:verify_iq_type(Type, set),
    AutoSave = list_to_atom(
        exmpp_xml:get_attribute_as_list(AutoEl, <<"save">>, "false")),
    case AutoSave of
        true -> ok;
        false -> ok;
        _ -> throw({error, 'bad-request'})
    end,
    Scope =
        list_to_atom(
            exmpp_xml:get_attribute_as_list(AutoEl, <<"scope">>, "stream")),
    case Scope of
        global ->
            F =
                fun() ->
                    GlobalPrefs =
                        #archive_global_prefs{
                            us = exmpp_jid:prep_bare_to_list(From),
                            auto_save = AutoSave},
                    store_global_prefs(GlobalPrefs),
                    XmppApi:broadcast_iq(From, IQ),
                    {auto_states, clear_with_auto_states(From, AutoStates)}
                end,
            dbms_storage:transaction(
                exmpp_jid:prep_domain_as_list(From), F);
        stream ->
            {atomic,
             {auto_states,
              update_stream_auto_states(From, AutoSave,
                    clear_with_auto_states(From, AutoStates))}};
        _ ->
            {aborted, {throw, {error, 'bad-request'}}}
    end.

%% Processes 'itemremove' requests from clients
itemremove(From, #iq{type = Type, payload = SubEl} = IQ, AutoStates, XmppApi) ->
    mod_archive2_utils:verify_iq_type(Type, set),
    F = fun() ->
            lists:foreach(
                fun(Item) ->
                    JidPrefs = mod_archive2_xml:jid_prefs_from_xml(From, Item),
                    remove_jid_prefs(JidPrefs)
                end,
                exmpp_xml:get_child_elements(SubEl)),
            ok
        end,
    case dbms_storage:transaction(exmpp_jid:prep_domain_as_list(From), F) of
        {atomic, ok} ->
            XmppApi:broadcast_iq(From, IQ),
            {atomic, {auto_states, clear_with_auto_states(
               From, AutoStates)}};
        Result ->
            Result
    end.

%% Returns true if collections for given From JID with given With JID
%% should be auto-archived.
should_auto_archive(From, With, AutoStates, DefaultGlobalPrefs,
    ShouldCachePrefs, XmppApi, OnlyFromRoster, Thread) ->
    AutoArchiveCheckPrefsFun =
        fun(AutoSave) ->
            should_auto_archive2(From, With, AutoStates, AutoSave,
	        DefaultGlobalPrefs, ShouldCachePrefs, XmppApi,
	        OnlyFromRoster)
        end,
    case dict:find(exmpp_jid:bare_to_list(From), AutoStates) of
        {ok, Resources} ->
            case dict:find(exmpp_jid:resource_as_list(From), Resources) of
                {ok, AutoState} ->
                    case dict:find(Thread, AutoState#auto_state.threads) of
		        {ok, ThreadInfo} ->
		            {ThreadInfo#thread_info.auto_save, AutoStates};
		        _ ->
                            case AutoState#auto_state.stream_auto_save of
                                false ->
                                    {false, AutoStates};
                                AutoSave ->
                                    case dict:find(With, AutoState#auto_state.with) of
                                        {ok, Result} ->
                                            {Result, AutoStates};
                                        _ ->
		                	    AutoArchiveCheckPrefsFun(AutoSave)
		                    end
                            end
		    end;
		_ ->
		    AutoArchiveCheckPrefsFun(undefined)
	    end;
	_ ->
	    AutoArchiveCheckPrefsFun(undefined)
    end.

should_auto_archive2(From, With, AutoStates, SessionAutoSave,
    DefaultGlobalPrefs, ShouldCachePrefs, XmppApi, OnlyFromRoster) ->
    ShouldArchive =
        case OnlyFromRoster of
            true -> XmppApi:is_in_roster(
                        exmpp_jid:prep_node_as_list(From),
                        exmpp_jid:prep_domain_as_list(From),
                        With);
            _ -> true
        end,
    case ShouldArchive of
        true ->
            F =
                fun() ->
                    GlobalPrefs = get_global_prefs(From, DefaultGlobalPrefs),
                    if SessionAutoSave =/= false orelse
                        GlobalPrefs#archive_global_prefs.auto_save =/= false ->
                        case get_effective_jid_prefs(From, With) of
                            undefined ->
                                GlobalPrefs#archive_global_prefs.save;
                            JidPrefs ->
                                case JidPrefs#archive_jid_prefs.save of
                                    false -> false;
                                    JidSave -> JidSave
                                end
                        end;
                       true ->
                        false
                    end
                end,
            case dbms_storage:transaction(exmpp_jid:prep_domain_as_list(From), F) of
                {atomic, Value} ->
                    IsMethodOk = is_save_method_supported(Value),
                    if IsMethodOk andalso Value =/= undefined ->
                        case ShouldCachePrefs of
                            true ->
                                {Value,
                                 update_with_auto_states(From, With, Value, AutoStates)};
                            false ->
                                {Value, AutoStates}
                        end;
                       true ->
                        XmppApi:error_msg("Unexpected 'save' method in should_auto_archive: ~p~n", [Value]),
                        throw({error, 'internal-server-error'})
                    end;
                Result ->
                    XmppApi:error_msg("should_auto_archive failed; ~p~n", [Result]),
                    throw({error, 'internal-server-error'})
            end;
        _ -> {false, AutoStates}
    end.

expire_prefs_cache(AutoStates) ->
    dict:map(
        fun(_US, Resources) ->
            dict:map(
                fun(_Resource, AutoState) ->
                    AutoState#auto_state{with = dict:new()}
                end,
                Resources)
        end,
        AutoStates).

remove_session(From, AutoStates) ->
    US = exmpp_jid:bare_to_list(From),
    ResourceToRemove = exmpp_jid:resource_as_list(From),
    case dict:find(US, AutoStates) of
        {ok, Resources} ->
            dict:store(
                US,
		dict:erase(ResourceToRemove, Resources),
                AutoStates);
        _ ->
            AutoStates
    end.

reset_thread_expiration(_From, undefined, AutoStates) -> AutoStates;

reset_thread_expiration(From, Thread, AutoStates) ->
    US = exmpp_jid:bare_to_list(From),
    ResourceToUpdate = exmpp_jid:resource_as_list(From),
    case dict:find(US, AutoStates) of
        {ok, Resources} ->
            dict:store(
                US,
                dict:map(
                    fun(Resource, AutoState) ->
		        if Resource =:= ResourceToUpdate ->
			    case dict:find(Thread, AutoState#auto_state.threads) of
			        {ok, ThreadInfo} ->
				    AutoState#auto_state{
				        threads = dict:store(
					    Thread,
					    ThreadInfo#thread_info{
					        last_access =
						    calendar:now_to_datetime(
						        mod_archive2_time:now())},
					    AutoState#auto_state.threads)};
				_ ->
				    AutoState
			    end;
			   true -> AutoState
			end
                    end,
                    Resources),
                AutoStates);
        _ ->
            AutoStates
    end.

% We're not expiring prefs cache here as
% session auto save calculation result is not stored in cache.
expire_threads(AutoStates, TimeOut) ->
    TS = calendar:now_to_datetime(mod_archive2_time:now()),
    dict:map(
        fun(_US, Resources) ->
	    dict:map(
	        fun(_Resource, AutoState) ->
		    AutoState#auto_state{threads =
		        dict:filter(
			    fun(_Thread, ThreadInfo) ->
			        not is_thread_expired(ThreadInfo#thread_info.last_access, TS, TimeOut)
			    end,
			    AutoState#auto_state.threads)}
	        end,
	        Resources)
	end,
	AutoStates).

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

%% Processes 'get' prefs requests
pref_get(From, IQ, DefaultGlobalPrefs, AutoStates) ->
    F = fun() ->
		    JidPrefsXML =
                [mod_archive2_xml:jid_prefs_to_xml(Prefs) ||
                    Prefs <- get_jid_prefs(From)],
            GlobalPrefs =
                get_global_prefs(From, #archive_global_prefs{}),
		PrefsUnSet =
		    if (GlobalPrefs#archive_global_prefs.save =/= undefined) orelse
		       (GlobalPrefs#archive_global_prefs.expire =/= undefined) orelse
		       (GlobalPrefs#archive_global_prefs.otr =/= undefined) ->
                false;
		       true ->
                true
		    end,
        AutoSave =
            case dict:find(exmpp_jid:bare_to_list(From), AutoStates) of
                {ok, Resources} ->
                    case dict:find(exmpp_jid:resource_as_list(From),
                        Resources) of
                        {ok, AutoState} ->
                            AutoState#auto_state.stream_auto_save;
                        _ ->
                            undefined
                    end;
                _ ->
                    undefined
            end,
		GlobalPrefsXML =
            mod_archive2_xml:global_prefs_to_xml(
                merge_global_prefs(GlobalPrefs, DefaultGlobalPrefs),
                    PrefsUnSet, AutoSave),
        exmpp_iq:result(IQ,
            exmpp_xml:element(?NS_ARCHIVING, pref, [],
                JidPrefsXML ++ GlobalPrefsXML))
        end,
    dbms_storage:transaction(exmpp_jid:prep_domain_as_list(From), F).

%% Processes 'set' prefs requests
pref_set(From, PrefsXML, EnforceExpire) ->
    F = fun() ->
            % Check that all children elements are as expected.
            lists:foreach(
	        fun(Element) ->
	            case not exmpp_xml:element_matches(Element, default) andalso
		         not exmpp_xml:element_matches(Element, method) andalso
		         not exmpp_xml:element_matches(Element, item) of
		        true ->
		           throw({error, 'bad-request'});
		        _ ->
		           ok
		    end
		end,
		exmpp_xml:get_child_elements(PrefsXML)),
            case exmpp_xml:has_element(PrefsXML, default) orelse
                exmpp_xml:has_element(PrefsXML, method) of
                true ->
                    GlobalPrefs =
                        mod_archive2_xml:global_prefs_from_xml(From, PrefsXML),
                    verify_expire_range(GlobalPrefs#archive_global_prefs.expire,
                        EnforceExpire),
                    case is_save_method_supported(
                        GlobalPrefs#archive_global_prefs.save) orelse
                        GlobalPrefs#archive_global_prefs.save =:= undefined of
                        true ->
                            store_global_prefs(GlobalPrefs);
                        false ->
                            throw({error, 'feature-not-implemented'})
                    end;
                _ ->
                    ok
            end,
            lists:foreach(
                fun(Item) ->
                    JidPrefs =
                        mod_archive2_xml:jid_prefs_from_xml(From, Item),
                    verify_expire_range(JidPrefs#archive_jid_prefs.expire,
                        EnforceExpire),
                    case is_save_method_supported(
                        JidPrefs#archive_jid_prefs.save) of
                        true ->
                            store_jid_prefs(JidPrefs);
                        false ->
                            throw({error, 'feature-not-implemented'})
                    end
                end,
                exmpp_xml:get_elements(PrefsXML, item))
        end,
    dbms_storage:transaction(exmpp_jid:prep_domain_as_list(From), F).

verify_expire_range(Expire, {MinExpire, MaxExpire}) ->
    if Expire >= MinExpire andalso
        (MaxExpire =:= infinity orelse Expire =< MaxExpire) ->
        ok;
       true ->
        throw({error, 'not-allowed'})
    end.

%% Returns true if given save method is supported.
is_save_method_supported(Save) ->
    case Save of
        body ->
            true;
        message ->
            true;
        false ->
            true;
        undefined ->
            true;
        _ ->
            false
    end.

%% Returns the archive_global_prefs record filled with default values
default_global_prefs(AutoSave, Expire) ->
    #archive_global_prefs{
        save = AutoSave,
        expire = Expire,
        method_auto =
            if AutoSave =/= false ->
                prefer;
               true ->
                concede
            end,
        method_local = concede,
        method_manual =
            if AutoSave =/= false ->
                concede;
               true ->
                prefer
            end,
        auto_save = AutoSave =/= false,
        otr = forbid}.

%% Returns global prefs for given client.
get_global_prefs(From, DefaultGlobalPrefs) ->
    InPrefs =
        #archive_global_prefs{us = exmpp_jid:prep_bare_to_list(From)},
    GlobalPrefs =
        case dbms_storage:read(InPrefs) of
            {selected, [Prefs]} ->
                Prefs;
            {selected, []} ->
                InPrefs;
            Result ->
                throw({error, Result})
        end,
    merge_global_prefs(GlobalPrefs, DefaultGlobalPrefs).

merge_global_prefs(GlobalPrefs, DefaultGlobalPrefs) ->
    list_to_tuple(
        lists:zipwith(
	        fun(Item1, Item2) ->
		        if Item1 =/= undefined ->
                    Item1;
		           true ->
                    Item2
		        end
	        end,
	        tuple_to_list(GlobalPrefs),
	        tuple_to_list(DefaultGlobalPrefs))).

%% Stores global prefs.
store_global_prefs(Prefs) ->
    write_prefs(Prefs).

%% Returns all JID prefs for given client.
get_jid_prefs(From) ->
    US = exmpp_jid:prep_bare_to_list(From),
    case dbms_storage:select(
        ets:fun2ms(
            fun(#archive_jid_prefs{us = US1} = Prefs)
                when US1 =:= US ->
                Prefs
            end)) of
        {selected, Items} when is_list(Items) ->
            Items;
        Result ->
            throw({error, Result})
    end.

%% Returns JID prefs for given client and given JID, ExactMatch: if no
%% prefs are available with exactly these JID and ExactMatch, returns
%% 'undefined'.
get_jid_prefs(From, JID, ExactMatch) ->
    case dbms_storage:read(#archive_jid_prefs{
        us = exmpp_jid:prep_bare_to_list(From),
        with_user = exmpp_jid:prep_node_as_list(JID),
        with_server = exmpp_jid:prep_domain_as_list(JID),
        with_resource = exmpp_jid:prep_resource_as_list(JID),
        exactmatch = ExactMatch}) of
        {selected, Items} when is_list(Items) ->
            case length(Items) of
                1 ->
                    [Item] = Items,
                    Item;
                0 ->
                    undefined;
                _ ->
                    throw({error, 'internal-server-error'})
            end;
        Result ->
            throw({error, Result})
    end.

%% Returns JID prefs for given client that apply to given JID: will perform
%% search for best matching prefs.
get_effective_jid_prefs(From, With) ->
    Candidates =
        [{With, false},
         {With, true},
         {exmpp_jid:bare(With), false},
         {exmpp_jid:make(exmpp_jid:domain(With)), false}],
    lists:foldl(
        fun({JID, ExactMatch}, Acc) ->
            case Acc of
                undefined ->
                    get_jid_prefs(From, JID, ExactMatch);
                _ ->
                    Acc
            end
        end,
        undefined,
        Candidates).

%% Store given JID prefs.
store_jid_prefs(Prefs) ->
    write_prefs(Prefs).

%% Remove given JID prefs.
remove_jid_prefs(Prefs) ->
    dbms_storage:delete(Prefs).

%% Code is the same for both global and JID prefs.
write_prefs(Prefs) ->
    case dbms_storage:read(Prefs) of
        {selected, []} ->
            dbms_storage:insert([Prefs]);
        {selected, [_]} ->
            dbms_storage:update(Prefs);
        Result ->
            throw({error, Result})
    end.

update_with_auto_states(From, With, Value, AutoStates) ->
    update_auto_states(
        From,
        fun(AutoState) ->
            AutoState#auto_state{with =
                dict:store(With, Value, AutoState#auto_state.with)}
        end,
        #auto_state{with = dict:store(With, Value, dict:new()), threads = dict:new()},
        AutoStates).

update_stream_auto_states(From, AutoSave, AutoStates) ->
    update_auto_states(
        From,
        fun(AutoState) ->
            AutoState#auto_state{stream_auto_save = AutoSave}
        end,
        #auto_state{stream_auto_save = AutoSave},
        AutoStates).

update_auto_states(From, Updater, Default, AutoStates) ->
    US = exmpp_jid:bare_to_list(From),
    Resource = exmpp_jid:resource_as_list(From),
    case dict:find(US, AutoStates) of
        {ok, Resources} ->
            case dict:find(Resource, Resources) of
                {ok, AutoState} ->
                    dict:store(US,
                        dict:store(Resource, Updater(AutoState), Resources),
                        AutoStates);
                _ ->
                    dict:store(US,
                        dict:store(Resource, Default, Resources),
                        AutoStates)
            end;
        _ ->
            dict:store(US,
                dict:store(Resource, Default, dict:new()),
                AutoStates)
    end.

clear_with_auto_states(From, AutoStates) ->
    US = exmpp_jid:bare_to_list(From),
    case dict:find(US, AutoStates) of
        {ok, Resources} ->
            dict:store(
                US,
                dict:map(
                    fun(_Resource, AutoState) ->
                        AutoState#auto_state{with = dict:new()}
                    end,
                    Resources),
                AutoStates);
        _ ->
            AutoStates
    end.

is_thread_expired(LastAccess, TS, TimeOut) ->
    TimeDiff =
        calendar:datetime_to_gregorian_seconds(TS) -
        calendar:datetime_to_gregorian_seconds(LastAccess),
    TimeDiff > TimeOut.
