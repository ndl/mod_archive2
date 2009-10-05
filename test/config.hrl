-define(SERVERHOST, "ndl-server").
-define(SERVERPORT, 6222).
-define(CLIENTNAME, "client").
-define(CLIENTJID, ?CLIENTNAME "@" ?SERVERHOST).
-define(PASSWORD, "password").
-define(NS, "http://www.xmpp.org/extensions/xep-0136.html#ns").
-define(TIMEOUT, 5000).

-define(test_gen0(F),
        {atom_to_list(F),
         {{?MODULE, F, 0},
         fun() ->
             fun F/0()
         end}}).

-define(test_gen1(F),
        fun(Fixture) ->
            {atom_to_list(F),
             {{?MODULE, F, 1},
              fun() ->
                  fun F/1(Fixture)
              end}}
        end).

-define(EUNIT_XML_REPORT(Module, OutDir),
        eunit:test(Module, [{report, {eunit_surefire, [{dir, OutDir}]}}])).

-define(DEBUG_FMT(Format, Values), false).
%-define(DEBUG_FMT(Format, Values), ?debugFmt(Format, Values)).
