%%%----------------------------------------------------------------------
%%% File    : gen_mod.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Emulation of gen_mod module interface for unit testing.
%%% Created : 3 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(ejabberd_sup).
-author('ejabberd@ndl.kiev.ua').

-include_lib("eunit/include/eunit.hrl").
-include("config.hrl").

-behaviour(supervisor).

-export([start_link/0, init/1, eunit_xml_report/1]).

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) -> {ok, {{one_for_one, 10, 1}, []}}.