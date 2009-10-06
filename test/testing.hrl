-include("config.hrl").

-define(CLIENTJID, ?CLIENTNAME "@" ?SERVERHOST).
-define(CLIENTJID2, ?CLIENTNAME2 "@" ?SERVERHOST).
-define(NS, "http://www.xmpp.org/extensions/xep-0136.html#ns").

-define(test_gen0(F),
        {timeout,
         ?TEST_TIMEOUT,
         {atom_to_list(F),
          {{?MODULE, F, 0},
          fun() ->
              fun F/0()
          end}}}).

-define(test_gen1(F),
        fun(Fixture) ->
            {timeout,
             ?TEST_TIMEOUT,
             {atom_to_list(F),
              {{?MODULE, F, 1},
               fun() ->
                   fun F/1(Fixture)
               end}}}
        end).

-define(test_foreach(Setup, Teardown, Tests),
        {timeout,
         ?TEST_TIMEOUT,
         {foreach,
          local,
          fun Setup/0,
          fun Teardown/1,
          Tests}}).

-define(EUNIT_XML_REPORT(Module, OutDir),
        eunit:test(Module, [{report, {eunit_surefire, [{dir, OutDir}]}}])).

-define(DEBUG_OUT(Value), ?DEBUG_FMT("~p~n", [Value])).