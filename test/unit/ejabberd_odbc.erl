%%%----------------------------------------------------------------------
%%% File    : ejabberd_odbc.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 emulation of ejabberd_odbc interface for testing
%%% Created : 03 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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

-module(ejabberd_odbc).
-author('ejabberd@ndl.kiev.ua').

-include_lib("eunit/include/eunit.hrl").
-include("testing.hrl").

-export([start/1, sql_transaction/2, sql_query_t/1, escape/1]).

-define(SESSION_KEY, ejabberd_odbc_test).

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
    {StoredQuery, Answer} = next_element(),
    case StoredQuery of
        ignore -> ok;
        _ ->
            if StoredQuery =/= Query ->
                ?debugFmt("Queries mismatch: stored query is '~p', received query is '~p'~n", [StoredQuery, Query]);
               true -> ok
            end,
            StoredQuery = Query
    end,
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
