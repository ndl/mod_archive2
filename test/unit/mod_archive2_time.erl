%%%----------------------------------------------------------------------
%%% File    : mod_archive2_time.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 helper functionality
%%% Created : 27 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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
-author('ejabberd@ndl.kiev.ua').

-include_lib("eunit/include/eunit.hrl").
-include("testing.hrl").

-export([start/0, start/1, now/0, eunit_xml_report/1]).

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

-define(MOD_ARCHIVE2_TIME_KEY, list_to_atom(atom_to_list(?MODULE) ++ "_key")).
-define(DEFAULT_TIME, {{2010, 1, 2}, {3, 4, 5}}).

start() ->
    put(?MOD_ARCHIVE2_TIME_KEY, [?DEFAULT_TIME]).

start(Times) ->
    put(?MOD_ARCHIVE2_TIME_KEY, Times).

now() ->
    case get(?MOD_ARCHIVE2_TIME_KEY) of
        undefined ->
            put(?MOD_ARCHIVE2_TIME_KEY,
                [calendar:gregorian_seconds_to_datetime(
                    calendar:datetime_to_gregorian_seconds(?DEFAULT_TIME) + 1)]),
            datetime_to_now(?DEFAULT_TIME);
        [Time | Times] ->
            if Times =:= [] ->
                    put(?MOD_ARCHIVE2_TIME_KEY,
                        [calendar:gregorian_seconds_to_datetime(
                            calendar:datetime_to_gregorian_seconds(Time) + 1)]);
               true ->
                put(?MOD_ARCHIVE2_TIME_KEY, Times)
            end,
            datetime_to_now(Time)
    end.

datetime_to_now(DateTime) ->
    Secs =
        calendar:datetime_to_gregorian_seconds(DateTime) -
        calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    {Secs div 1000000, Secs rem 1000000, 0}.
