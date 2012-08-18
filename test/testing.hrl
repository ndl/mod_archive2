%%%----------------------------------------------------------------------
%%% File    : testing.hrl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 common definitions for testing
%%% Created : 27 Sep 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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

-include("config.hrl").

-define(CLIENTJID, ?CLIENTNAME "@" ?SERVERHOST).
-define(CLIENTJID2, ?CLIENTNAME2 "@" ?SERVERHOST).
-define(NS, "http://www.xmpp.org/extensions/xep-0136.html#ns").

-define(test_gen0(F),
        {timeout,
         ?TEST_TIMEOUT,
         {atom_to_list(F),
          {{?MODULE, F, 0},
          fun() ->
              fun F/0()
          end}}}).

-define(test_gen1(F),
        fun(Fixture) ->
            {timeout,
             ?TEST_TIMEOUT,
             {atom_to_list(F),
              {{?MODULE, F, 1},
               fun() ->
                   fun F/1(Fixture)
               end}}}
        end).

-define(test_foreach(Setup, Teardown, Tests),
        {timeout,
         ?TEST_TIMEOUT,
         {foreach,
          local,
          fun Setup/0,
          fun Teardown/1,
          Tests}}).

-define(EUNIT_XML_REPORT(Module, OutDir),
        eunit:test(Module, [{report, {eunit_surefire, [{dir, OutDir}]}}])).

-define(DEBUG_OUT(Value), ?DEBUG_FMT("~p~n", [Value])).
