%%%----------------------------------------------------------------------
%%% File    : mod_archive2.hrl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 common declarations
%%% Created : 30 Sep 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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

-include("ejabberd.hrl").
-include("jlib.hrl").

-include("mod_archive2_storage.hrl").

-define(NS_ARCHIVE,
        "http://www.xmpp.org/extensions/xep-0136.html#ns").
-define(NS_ARCHIVE_AUTO,
        "http://www.xmpp.org/extensions/xep-0136.html#ns-auto").
-define(NS_ARCHIVE_MANAGE,
        "http://www.xmpp.org/extensions/xep-0136.html#ns-manage").
-define(NS_ARCHIVE_PREF,
        "http://www.xmpp.org/extensions/xep-0136.html#ns-pref").
-define(NS_ARCHIVE_MANUAL,
        "http://www.xmpp.org/extensions/xep-0136.html#ns-manual").

-record(archive_jid_prefs,
        {us,
         with_user,
         with_server,
         with_resource,
         save = undefined,
         expire = undefined,
         otr = undefined}).

-record(archive_global_prefs,
        {us,
         save = undefined,
         expire = undefined,
         otr = undefined,
         method_auto = undefined,
         method_local = undefined,
         method_manual = undefined,
         auto_save = undefined}).

-record(archive_collection,
        {id,
         prev_id,
         next_id,
         us,
         with_user,
         with_server,
         with_resource,
         utc,
         change_by,
         change_utc,
         deleted,
         subject = "",
         thread = "",
         crypt = false,
         extra = ""}).

-record(archive_message,
        {id,
         coll_id,
         utc,
         direction,
         body,
         name}).

-define(MOD_ARCHIVE2_SCHEMA,
        [#table{name = archive_jid_prefs,
                fields = record_info(fields, archive_jid_prefs),
                types = [string, string, string, string, bool, integer, enum],
                enums = [approve, concede, forbid, oppose, prefer, require],
                keys = 4},
         #table{name = archive_global_prefs,
                fields = record_info(fields, archive_global_prefs),
                types = [string, bool, integer, enum, enum, enum, enum, bool],
                enums = [approve, concede, forbid, oppose, prefer, require],
                keys = 1},
         #table{name = archive_collection,
                fields = record_info(fields, archive_collection),
                types = [integer, integer, integer, string, string, string,
                         string, time, string, time, bool, string, string,
                         bool, blob],
                enums = [],
                keys = 1},
         #table{name = archive_message,
                fields = record_info(fields, archive_message),
                types = [integer, integer, time, enum, string, string],
                enums = [from, to, note],
                keys = 1}]).