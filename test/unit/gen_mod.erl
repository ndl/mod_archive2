%%%----------------------------------------------------------------------
%%% File    : gen_mod.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Emulation of gen_mod module interface for unit testing.
%%% Created : 3 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(gen_mod).
-author('ejabberd@ndl.kiev.ua').

-include_lib("eunit/include/eunit.hrl").
-include("config.hrl").

-export([get_module_proc/2, eunit_xml_report/1]).

eunit_xml_report(OutDir) -> ?EUNIT_XML_REPORT(?MODULE, OutDir).

get_module_proc(Host, Base) ->
    list_to_atom(atom_to_list(Base) ++ "_" ++ Host).