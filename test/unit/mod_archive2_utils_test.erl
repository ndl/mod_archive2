%%%----------------------------------------------------------------------
%%% File    : mod_archive2_utils_test.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 utilities unit testing
%%% Created : 19 Aug 2012 by Alexander Tsvyashchenko <xmpp@endl.ch>
%%%
%%% mod_archive2, Copyright (C) 2009,2012 Alexander Tsvyashchenko
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

-module(mod_archive2_utils_test).
-author('xmpp@endl.ch').

-include("mod_archive2.hrl").
-include("testing.hrl").

-export([eunit_xml_report/1]).

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

mod_archive2_utils_test_() ->
{
    foreach,
    local,
    fun utils_tests_setup/0,
    fun utils_tests_teardown/1,
    [
        ?test_gen1(test_datetime_string_to_timestamp),
        ?test_gen1(test_datetime_string_to_timestamp2)
    ]
 }.

utils_tests_setup() -> exmpp:start().

utils_tests_teardown(_) -> ok.

test_datetime_string_to_timestamp(_) ->
    {-15792,-612203,0} = mod_archive2_utils:datetime_string_to_timestamp("1469-07-21T03:16:37Z").

test_datetime_string_to_timestamp2(_) ->
    {-15792,-612203,0} = mod_archive2_utils:datetime_string_to_timestamp("1469-07-21T03:16:37.000000Z").
