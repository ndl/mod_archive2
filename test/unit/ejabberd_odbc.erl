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

-export([start/1, sql_transaction/2, sql_query_t/1]).

-define(SESSION_KEY, ejabberd_odbc_test).

start(Session) ->
    put(?SESSION_KEY, Session).

sql_transaction(_Host, F) ->
    F(),
    next_element().

sql_query_t(Query) ->
    {Query, Answer} = next_element(),
    Answer.

next_element() ->
    [Element | T] = get(?SESSION_KEY),
    put(?SESSION_KEY, T),
    Element.
