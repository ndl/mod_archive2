%%%----------------------------------------------------------------------
%%% File    : mod_archive2_time.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 helper functionality
%%% Created : 27 Oct 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-module(mod_archive2_time).
-author('xmpp@endl.ch').

-export([start/0, start/1, now/0, timestamp/0]).
-compile({no_auto_import, [now/0]}).

-define(MOD_ARCHIVE2_TIME_KEY, list_to_atom(atom_to_list(?MODULE) ++ "_key")).
-define(DEFAULT_TIME, {{2010, 1, 2}, {3, 4, 5, 0}}).

start() ->
    put(?MOD_ARCHIVE2_TIME_KEY, [?DEFAULT_TIME]).

start(Times) ->
    put(?MOD_ARCHIVE2_TIME_KEY, Times).

now() ->
    case get(?MOD_ARCHIVE2_TIME_KEY) of
        undefined ->
            put(?MOD_ARCHIVE2_TIME_KEY,
                [mod_archive2_utils:microseconds_to_datetime(
                    mod_archive2_utils:datetime_to_microseconds(?DEFAULT_TIME) + 1000000)]),
            datetime_to_now(?DEFAULT_TIME);
        [Time | Times] ->
            if Times =:= [] ->
                    put(?MOD_ARCHIVE2_TIME_KEY,
                        [mod_archive2_utils:microseconds_to_datetime(
                            mod_archive2_utils:datetime_to_microseconds(Time) + 1000000)]);
               true ->
                put(?MOD_ARCHIVE2_TIME_KEY, Times)
            end,
            datetime_to_now(Time)
    end.

datetime_to_now(DateTime) ->
    MicroSecs =
        mod_archive2_utils:datetime_to_microseconds(DateTime) -
        mod_archive2_utils:datetime_to_microseconds({{1970, 1, 1}, {0, 0, 0, 0}}),
    Secs = MicroSecs div 1000000,
    {Secs div 1000000, Secs rem 1000000, MicroSecs rem 1000000}.

timestamp() ->
    now().
