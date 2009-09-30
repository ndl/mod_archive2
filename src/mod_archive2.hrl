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
-define(INFINITY, calendar:datetime_to_gregorian_seconds({{2038,1,19},{0,0,0}})).

%% Should be OK for most of modern DBs, I hope ...
-define(MAX_QUERY_LENGTH, 32768).

-record(archive_jid_prefs,
        {us,
         jid,
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
         jid,
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
