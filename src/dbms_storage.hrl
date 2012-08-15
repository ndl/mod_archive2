%%%----------------------------------------------------------------------
%%% File    : dbms_storage.hrl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : ejabberd common declarations for storage support
%%% Created : 03 Oct 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

%% Table information.
-record(table, {name, rdbms, fields, types, keys}).

%% Storage backend information.
-record(storage_backend, {name, host, rdbms, schema}).

-define(SELECT_NOBJECTS, 64).
