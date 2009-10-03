%%%----------------------------------------------------------------------
%%% File    : mod_archive2.hrl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Message Archiving (XEP-136) Common Declarations
%%% Created : 27 Sep 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 1.0.0
%%% Id      : $Id$
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
         us,
         with_user,
         with_server,
         with_resource,
         utc,
         change_by,
         change_utc,
         deleted,
         subject = "",
         prev = [],
         next = [],
         thread = "",
         crypt = false,
         extra = ""}).

-record(archive_message,
        {id,
         coll_id,
         utc,
         direction,
         body,
         name = ""}).

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
                types = [integer, string, string, string, string, time,
                         string, time, bool, string, integer, integer, string,
                         bool, blob],
                enums = [],
                keys = 1},
         #table{name = archive_message,
                fields = record_info(fields, archive_message),
                types = [integer, integer, time, enum, string, string],
                enums = [from, to, note],
                keys = 1}]).