%%%----------------------------------------------------------------------
%%% File    : mod_archive2_auto.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 automatic archiving
%%% Created : 17 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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

-module(mod_archive2_auto).
-author('ejabberd@ndl.kiev.ua').

%% Our hooks
-export([filter_sessions/2]).

-include("mod_archive2.hrl").

%%--------------------------------------------------------------------
%% Filters given sessions according to specified filter function
%%--------------------------------------------------------------------

filter_sessions(Filter, Sessions) ->
    F =
        fun(BareJID, Value) ->
		    dict:filter(
                fun(_, Session) ->
				    Filter(BareJID, Session)
			    end,
                Value)
	    end,
    FilteredSessions = dict:map(F, Sessions),
    NewSessions =
        dict:filter(
            fun(_Key, Value) ->
			    dict:fetch_keys(Value) /= []
            end,
            FilteredSessions),
    NewSessions.