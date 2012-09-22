%%%----------------------------------------------------------------------
%%% File    : timing.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Performs simple timings for XML parsing
%%% Created : 20 Aug 2012 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-module(timing).
-author('xmpp@endl.ch').

-export([start/0, test_full_parser/1, test_parser/2, test_parser_with_reset/2, test_parser_fragment/1, test_empty/1]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-define(TEST_XML, "<body>Wow, I&apos;m green with envy!</body>"
    "<html xmlns='http://jabber.org/protocol/xhtml-im'>"
    "<body xmlns='http://www.w3.org/1999/xhtml'>"
    "<p style='font-size:large'>"
    "<em>Wow</em>, I&apos;m <span style='color:green'>green</span>"
    "with <strong>envy</strong>!"
    "</p>"
    "</body>"
    "</html>").

-define(ITERATIONS, 100000).

start() ->
    application:start(exmpp),
    {FullParserTime, _} =
        timer:tc(timing, test_full_parser, [?ITERATIONS]),
    io:format("Full parser took ~p msec.~n", [FullParserTime]),
    Parser = exmpp_xml:start_parser([{root_depth, 1}]),
    % Fake root element so that parsing doesn't fail.
    exmpp_xml:parse(Parser, "<root>"),
    {ParserTime, _} =
        timer:tc(timing, test_parser, [Parser, ?ITERATIONS]),
    io:format("Parser took ~p msec.~n", [ParserTime]),
    {ParserWithResetTime, _} =
        timer:tc(timing, test_parser_with_reset, [Parser, ?ITERATIONS]),
    io:format("Parser with reset took ~p msec.~n", [ParserWithResetTime]),
    {ParserFragmentTime, _} =
        timer:tc(timing, test_parser_fragment, [?ITERATIONS]),
    io:format("Parser fragment took ~p msec.~n", [ParserFragmentTime]),
    {EmptyTime, _} =
        timer:tc(timing, test_empty, [?ITERATIONS]),
    io:format("Empty call took ~p msec.~n", [EmptyTime]).

test_full_parser(0) -> ok;

test_full_parser(N) ->
    Parser = exmpp_xml:start_parser([{root_depth, 1}]),
    exmpp_xml:parse(Parser, "<root>"),
    exmpp_xml:parse(Parser, ?TEST_XML),
    exmpp_xml:stop_parser(Parser),
    test_full_parser(N - 1).

test_parser(_, 0) -> ok;

test_parser(Parser, N) ->
    exmpp_xml:parse(Parser, ?TEST_XML),
    test_parser(Parser, N - 1).

test_parser_with_reset(_, 0) -> ok;

test_parser_with_reset(Parser, N) ->
    NewParser = exmpp_xml:reset_parser(Parser, [{root_depth, 1}]),
    exmpp_xml:parse(NewParser, "<root>"),
    exmpp_xml:parse(NewParser, ?TEST_XML),
    test_parser(NewParser, N - 1).

test_parser_fragment(0) -> ok;

test_parser_fragment(N) ->
    exmpp_xml:parse_document_fragment("<root>" ++ ?TEST_XML),
    test_parser_fragment(N - 1).

test_empty(0) -> ok;

test_empty(N) ->
    _ = length(?TEST_XML),
    test_empty(N - 1).
