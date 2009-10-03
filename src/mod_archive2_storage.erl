%%%----------------------------------------------------------------------
%%% File    : mod_archive2_storage.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : Unified RDBMS and Mnesia Storage Support
%%% Created : 27 Sep 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Version : 2.0.0
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-module(mod_archive2_storage).
-author('ejabberd@ndl.kiev.ua').

-include("mod_archive2_storage.hrl").

%% gen_mod callbacks
-export([start/2, stop/1]).

%% API.
-export([start_link/2,
         delete/1, delete/2,
         read/1, select/3,
         insert/1, update/1, update/2,
         transaction/2]).

-define(SUPERVISOR, ejabberd_sup).
-define(BACKEND_KEY, mod_archive2_backend).
-define(MODULE_MNESIA, mod_archive2_mnesia).
-define(MODULE_ODBC, mod_archive2_odbc).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    Info = put_backend_info(Host, Opts),
    gen_server:start_link({local, Proc}, Info#backend_info.backend, Info, []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    Info = put_backend_info(Host, Opts),
    Spec = {Proc, {Info#backend_info.backend, start_link, Info},
            transient, 1000, worker, [?MODULE]},
    supervisor:start_child(?SUPERVISOR, Spec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:delete_child(?SUPERVISOR, Proc).

%% Deletes the specified record: key is mandatory, all other fields are unused.
delete(R) ->
    forward_query({delete, R}).

%% Deletes all records matching MS.
delete(Tab, MS) ->
    forward_query({delete, {Tab, MS}}).

%% Retrieves the specified record: key is mandatory, all other fields are unused.
read(R) ->
    forward_query({read, R}).

%% Retrieves all records matching MS and using Opts.
select(Tab, MS, Opts) ->
    forward_query({select, {Tab, MS, Opts}}).

%% Updates the specified record, which is assumed to exist: key is mandatory,
%% all other fields not set to "undefined" are used.
update(R) ->
    forward_query({update, R}).

%% Updates all records matching MS, all fields not set to "undefined" are used
%% (except key).
update(R, MS) ->
    forward_query({update, {R, MS}}).

%% Inserts all records in the list to their respective tables, records are
%% assumed to not exist, keys are auto-generated, all other fields not set to
%% "undefined" are used. Returns last inserted ID, but it is meaningful only
%% if Records contained single record.
insert(Records) ->
    forward_query({insert, Records}).

%%
%% TODO: DO WE NEED IT?
%%
%% Updates or inserts all records in their respective tables depending on
%% whether they exist or not, keys are auto-generated, all other fields not set
%% to "undefined" are used. Returns last inserted ID, but it is meaningful only
%% if Records contained single record.
%% write(Host, Records) ->
%%    ?FORWARD(Host, {write, Records}).

%% Runs transaction.
transaction(Host, F) ->
    gen_server:call(gen_mod:get_module_proc(Host, ?MODULE), {transaction, F}).

forward_query(Query) ->
    Info = get(?BACKEND_KEY),
    case Info#backend_info.backend of
        ?MODULE_MNESIA ->
            mod_archive2_mnesia:handle_query(Query);
        ?MODULE_ODBC ->
            mod_archive2_odbc:handle_query(Query, Info)
    end.

put_backend_info(Host, Opts) ->
    RDBMS = proplists:get_value(rdbms, Opts, mnesia),
    Backend =
        case RDBMS of
            mnesia -> ?MODULE_MNESIA;
            _ -> ?MODULE_ODBC
        end,
    RecordsInfo = dict:from_list(proplists:get_value(records, Opts, [])),
    EscapeInfo = proplists:get_value(escape, Opts, []),
    put(?BACKEND_KEY,
        #backend_info{
            host = Host,
            backend = Backend,
            rdbms = RDBMS,
            records = RecordsInfo,
            escape = EscapeInfo}),
    Backend.