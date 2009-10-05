%%%----------------------------------------------------------------------
%%% File    : ejabberd_odbc.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Emulation of ejabberd_odbc module interface for unit testing.
%%% Created : 3 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_odbc).
-author('ejabberd@ndl.kiev.ua').

-include_lib("eunit/include/eunit.hrl").
-include("config.hrl").

-export([start/1, sql_transaction/2, sql_query_t/1, escape/1, eunit_xml_report/1]).

-define(SESSION_KEY, ejabberd_odbc_test).

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

start(Session) ->
    put(?SESSION_KEY, Session).

sql_transaction(_Host, F) ->
    Res = F(),
    % If there's non-empty value for transaction - use it, otherwise
    % return success status.
    case next_element() of
        {} -> {atomic, Res};
        Element -> Element
    end.

sql_query_t(Query) ->
    ?DEBUG_FMT("Got query: ~p~n", [Query]),
    {Query, Answer} = next_element(),
    Answer.

next_element() ->
    [Element | T] = get(?SESSION_KEY),
    ?DEBUG_FMT("Got element: ~p~n", [Element]),
    put(?SESSION_KEY, T),
    Element.

escape(S) when is_list(S) ->
    [escape_char(C) || C <- S].

%% Characters to escape
escape_char($\0) -> "\\0";
escape_char($\n) -> "\\n";
escape_char($\t) -> "\\t";
escape_char($\b) -> "\\b";
escape_char($\r) -> "\\r";
escape_char($')  -> "\\'";
escape_char($")  -> "\\\"";
escape_char($\\) -> "\\\\";
escape_char(C)   -> C.
