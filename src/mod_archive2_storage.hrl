%%%----------------------------------------------------------------------
%%% File    : mod_archive2_storage.hrl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 storage-related declarations
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

-define(MOD_ARCHIVE2_SCHEMA,
        [#table{name = archive_jid_prefs,
                fields = record_info(fields, archive_jid_prefs),
                types = [string, string, string, string, bool,
                         {enum, [body, false, message, stream]},
                         integer,
                         {enum, [approve, concede, forbid, oppose, prefer, require]}],
                keys = 5},
         #table{name = archive_global_prefs,
                fields = record_info(fields, archive_global_prefs),
                types = [string,
                         {enum, [body, false, message, stream]},
                         integer,
                         {enum, [approve, concede, forbid, oppose, prefer, require]},
                         {enum, [concede, forbid, prefer]},
                         {enum, [concede, forbid, prefer]},
                         {enum, [concede, forbid, prefer]},
                         bool],
                keys = 1},
         #table{name = archive_collection,
                fields = record_info(fields, archive_collection),
                types = [autoid, integer, integer, string, string, string,
                         string, time, time, integer, bool, string, string,
                         bool, xml],
                keys = 1},
         #table{name = archive_message,
                fields = record_info(fields, archive_message),
                types = [autoid, integer, time, {enum, [from, to, note]},
                         string, string, string],
                keys = 1}]).
