-module(cover_manager).
-export([start/1, stop/0]).

-include_lib("exmpp/include/exmpp.hrl").

-define(COVER_KEY, cover_manager_key).

start(Args) ->
    [PackageName, SrcPath, DataFile, XmlFile, FileMask] =
    	lists:map(fun(Value) when is_atom(Value) ->
                      atom_to_list(Value);
                     (Value) ->
                         Value
                     end, Args),
    Files = filelib:wildcard(filename:join(SrcPath, FileMask)),
    lists:foreach(
        fun(File) -> cover:compile_beam(File) end,
	Files),
    put(?COVER_KEY, {PackageName, SrcPath, DataFile, XmlFile, [list_to_atom(filename:basename(File, ".beam")) || File <- Files]}),
    cover:import(DataFile).

stop() ->
    {PackageName, SrcPath, DataFile, XmlFile, Modules} = get(?COVER_KEY),
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
            lists:map(fun(M) -> analyse_module(M) end, Modules))
        ])
    ]),
    {ok, Dev} = file:open(XmlFile, write),
    file:write(Dev, "<?xml version='1.0' encoding='UTF-8'?>\n"
    "<!DOCTYPE coverage SYSTEM \"http://cobertura.sourceforge.net/xml/coverage-03.dtd\">\n"),
    file:write(Dev, exmpp_xml:document_to_iolist(CoverageXML)),
    file:close(Dev).

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
        exmpp_xml:element(undefined, "methods", [], lists:map(fun(FC) -> analyse_fun(FC) end, FunCovList)),
        exmpp_xml:element(undefined, "lines", [], lists:map(fun(Line) -> analyse_line(Line) end, LinesCovList))
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
