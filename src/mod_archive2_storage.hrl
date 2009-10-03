%%%----------------------------------------------------------------------
%%% File    : mod_archive2_storage.hrl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Unified RDBMS and Mnesia Storage Support
%%% Created : 3 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

%% Table information.
-record(table, {name, rdbms, fields, types, enums, keys}).

%% Storage backend information.
-record(backend, {name, host, rdbms, schema}).