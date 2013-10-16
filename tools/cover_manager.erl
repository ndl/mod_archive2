%%%----------------------------------------------------------------------
%%% File    : cover_manager.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 erlang coverage support and Cobertura XML export
%%% Created : 03 Oct 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-module(cover_manager).
-author('xmpp@endl.ch').

-behaviour(gen_server).

-include_lib("exmpp/include/exmpp.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API.
-export([start/1, stop/0]).

-define(PROC, list_to_atom(atom_to_list(?MODULE) ++ "_proc")).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Args) ->
    gen_server:start({local, ?PROC}, ?MODULE, Args, []).

stop() ->
    gen_server:call(?PROC, stop).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(Args) ->
    [PackageName, SrcPath, DataFile, XmlFile | FilesMasks] =
            lists:map(fun(Value) when is_atom(Value) ->
                      atom_to_list(Value);
                     (Value) ->
                         Value
                     end, Args),
    Files =
        lists:foldl(
            fun(FileMask, Acc) ->
                filelib:wildcard(filename:join(SrcPath, FileMask)) ++ Acc
            end,
        [],
        FilesMasks),
    lists:foreach(
        fun(File) -> cover:compile_beam(File) end,
        Files),
    cover:import(DataFile),
    {ok, {PackageName, SrcPath, DataFile, XmlFile,
          [list_to_atom(filename:basename(File, ".beam")) || File <- Files]}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, {PackageName, SrcPath, DataFile, XmlFile, Modules}) ->
    cover:export(DataFile),
    {SumCovered, SumUncovered} =
    lists:foldl(
        fun(Module, {Covered, Uncovered}) ->
            {ok, {_, {NewCovered, NewUncovered}}} = cover:analyse(Module, coverage, module),
            {Covered + NewCovered, Uncovered + NewUncovered}
        end,
        {0, 0},
        Modules),
    Total = SumCovered + SumUncovered,
    LineRate = if Total > 0 -> format_float(SumCovered / Total); true -> "1" end,
    CoverageXML =
    exmpp_xml:element(undefined, "coverage",
    [
        exmpp_xml:attribute("line-rate", LineRate),
        exmpp_xml:attribute("version", "mod_archive2 cover module v0.1"),
        exmpp_xml:attribute("timestamp", calendar:datetime_to_gregorian_seconds(calendar:universal_time()))
    ],
    [
        exmpp_xml:element(undefined, "sources", [],
        [
            exmpp_xml:element(undefined, "source", [],
            [
                exmpp_xml:cdata(SrcPath)
            ])
        ]),
        exmpp_xml:element(undefined, "packages", [],
        [
            exmpp_xml:element(undefined, "package",
            [
                exmpp_xml:attribute("name", PackageName),
                exmpp_xml:attribute("line-rate", LineRate)
            ],
            [analyse_module(M) || M <- Modules])
        ])
    ]),
    {ok, Dev} = file:open(XmlFile, write),
    file:write(Dev, "<?xml version='1.0' encoding='UTF-8'?>\n"
    "<!DOCTYPE coverage SYSTEM \"http://cobertura.sourceforge.net/xml/coverage-03.dtd\">\n"),
    file:write(Dev, exmpp_xml:document_to_iolist(CoverageXML)),
    file:close(Dev),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

analyse_module(Module) ->
    {ok, FunCovList} = cover:analyse(Module, coverage, function),
    {ok, LinesCovList} = cover:analyse(Module, calls, line),
    {ok, {_, {Covered, Uncovered}}} = cover:analyse(Module, coverage, module),
    Total = Covered + Uncovered,
    ModuleName = atom_to_list(Module),
    exmpp_xml:element(undefined, "class",
    [
        exmpp_xml:attribute("name", ModuleName),
        exmpp_xml:attribute("filename", ModuleName ++ ".erl"),
        exmpp_xml:attribute("line-rate", if Total > 0 -> format_float(Covered / Total); true -> "1" end)
    ],
    [
        exmpp_xml:element(undefined, "methods", [], [analyse_fun(FC) || FC <- FunCovList]),
        exmpp_xml:element(undefined, "lines", [], [analyse_line(Line) || Line <- LinesCovList])
    ]).

analyse_fun({{_Module, Fun, Arity}, {Covered, Uncovered}}) ->
    Total = Covered + Uncovered,
    exmpp_xml:element(undefined, "method",
    [
        exmpp_xml:attribute("name", atom_to_list(Fun) ++ "/" ++ integer_to_list(Arity)),
        exmpp_xml:attribute("signature", ""),
        exmpp_xml:attribute("line-rate", if Total > 0 -> format_float(Covered / Total); true -> "1" end),
        exmpp_xml:attribute("lines-covered", integer_to_list(Covered)),
        exmpp_xml:attribute("lines-uncovered", integer_to_list(Uncovered))
    ], []).

analyse_line({{_Module, Line}, Calls}) ->
    exmpp_xml:element(undefined, "line",
    [
        exmpp_xml:attribute("number", integer_to_list(Line)),
        exmpp_xml:attribute("hits", Calls)
    ], []).

format_float(Value) ->
    [Formatted] = io_lib:format("~.2f", [Value]),
    Formatted.
