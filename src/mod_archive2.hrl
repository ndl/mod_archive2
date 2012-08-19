%%%----------------------------------------------------------------------
%%% File    : mod_archive2.hrl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 common declarations
%%% Created : 30 Sep 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-include("dbms_storage.hrl").
-include("mod_archive2_utils.hrl").

-define(NS_ARCHIVING_AUTO, 'urn:xmpp:archive:auto').
-define(NS_ARCHIVING_AUTO_s, "urn:xmpp:archive:auto").
-define(NS_ARCHIVING_MANAGE, 'urn:xmpp:archive:manage').
-define(NS_ARCHIVING_MANAGE_s, "urn:xmpp:archive:manage").
-define(NS_ARCHIVING_MANUAL, 'urn:xmpp:archive:manual').
-define(NS_ARCHIVING_MANUAL_s, "urn:xmpp:archive:manual").
-define(NS_ARCHIVING_PREF, 'urn:xmpp:archive:pref').
-define(NS_ARCHIVING_PREF_s, "urn:xmpp:archive:pref").

% Common functionality missing from standard Erlang library: we do not want
% to mess with module name each time we use it.
-import(mod_archive2_utils, [filter_undef/1, list_to_bool/1]).

-record(archive_jid_prefs,
        {us,
         with_user,
         with_server,
         with_resource,
         exactmatch,
         save,
         expire,
         otr}).

-record(archive_global_prefs,
        {us,
         save,
         expire,
         otr,
         method_auto,
         method_local,
         method_manual,
         auto_save}).

-record(archive_collection,
        {id,
         prev_id,
         next_id,
         us,
         with_user,
         with_server,
         with_resource,
         utc,
         change_utc,
         version,
         deleted,
         subject,
         thread,
         crypt,
         extra}).

-record(archive_message,
        {id,
         coll_id,
         utc,
         direction,
         body,
         name,
         jid}).

-record(session, {utc, last_access, id, version, resource}).

-record(external_message, {type, thread, subject, nick, jid, body}).
