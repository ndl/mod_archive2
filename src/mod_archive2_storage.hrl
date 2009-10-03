%%%----------------------------------------------------------------------
%%% File    : mod_archive2_storage.hrl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Unified RDBMS and Mnesia Storage Support
%%% Created : 3 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-record(backend_info,
    {
     host,
     backend,
     rdbms,
     records,
     escape}).
