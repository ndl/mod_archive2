%%%----------------------------------------------------------------------
%%% File    : mod_archive2_utils.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 helper functionality
%%% Created : 04 Oct 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-module(mod_archive2_utils).
-author('xmpp@endl.ch').

-include("mod_archive2_utils.hrl").

-export([list_to_bool/1,
         filter_undef/1,
         verify_iq_type/2,
         get_module_proc/2,
         rsm_encode/1,
	 rsm_encode/2,
	 rsm_decode/1,
         datetime_string_to_timestamp/1]).

list_to_bool("false") -> false;
list_to_bool("true") -> true;
list_to_bool("0") -> false;
list_to_bool("1") -> true;
list_to_bool(undefined) -> undefined.

filter_undef(List) ->
    [Element || Element <- List, Element =/= undefined].

verify_iq_type(IqType, ExpectedType) ->
    case IqType of
        ExpectedType -> ok;
        _ -> throw({error, 'bad-request'})
    end.

%% From gen_mod.
get_module_proc(Host, Base) ->
    list_to_atom(atom_to_list(Base) ++ "_" ++ Host).

%% RSM handling, from ejabberd 3.x code:
rsm_decode(#iq{payload=SubEl})->
	rsm_decode(SubEl);

rsm_decode(#xmlel{}=SubEl)->
	case exmpp_xml:get_element(SubEl, 'set') of
		undefined ->
			none;
		#xmlel{name = 'set', children = SubEls}->
			lists:foldl(fun rsm_parse_element/2, #rsm_in{}, SubEls)
	end.

rsm_parse_element(#xmlel{name = 'max'}=Elem, RsmIn)->
    CountStr = exmpp_xml:get_cdata_as_list(Elem),
    {Count, _} = string:to_integer(CountStr),
    RsmIn#rsm_in{max=Count};

rsm_parse_element(#xmlel{name = 'before'}=Elem, RsmIn)->
    UID = exmpp_xml:get_cdata_as_list(Elem),
    RsmIn#rsm_in{direction=before, id=UID};

rsm_parse_element(#xmlel{name = 'after'}=Elem, RsmIn)->
    UID = exmpp_xml:get_cdata_as_list(Elem),
    RsmIn#rsm_in{direction=aft, id=UID};

rsm_parse_element(#xmlel{name = 'index'}=Elem, RsmIn)->
    IndexStr = exmpp_xml:get_cdata_as_list(Elem),
    {Index, _} = string:to_integer(IndexStr),
    RsmIn#rsm_in{index=Index};

rsm_parse_element(_, RsmIn)->
    RsmIn.

rsm_encode(#iq{payload=SubEl}=IQ_Rec,RsmOut)->
    Set = #xmlel{ns = ?NS_RSM, name = 'set', children =
	   lists:reverse(rsm_encode_out(RsmOut))},
    New = exmpp_xml:prepend_child(SubEl, Set),
    IQ_Rec#iq{payload=New}.

rsm_encode(none)->
    [];
rsm_encode(RsmOut)->
    [#xmlel{ns = ?NS_RSM, name = 'set', children = lists:reverse(rsm_encode_out(RsmOut))}].
rsm_encode_out(#rsm_out{count=Count, index=Index, first=First, last=Last})->
    El = rsm_encode_first(First, Index, []),
    El2 = rsm_encode_last(Last,El),
    rsm_encode_count(Count, El2).

rsm_encode_first(undefined, undefined, Arr) ->
    Arr;
rsm_encode_first(First, undefined, Arr) ->
    [#xmlel{ns = ?NS_RSM, name = 'first', children = [#xmlcdata{cdata = list_to_binary(First)}]}|Arr];
rsm_encode_first(First, Index, Arr) ->
    [#xmlel{ns = ?NS_RSM, name = 'first', attrs = [?XMLATTR(<<"index">>, Index)], children = [#xmlcdata{cdata = list_to_binary(First)}]}|Arr].

rsm_encode_last(undefined, Arr) -> Arr;
rsm_encode_last(Last, Arr) ->
    [#xmlel{ns = ?NS_RSM, name = 'last', children = [#xmlcdata{cdata = list_to_binary(Last)}]}|Arr].

rsm_encode_count(undefined, Arr)-> Arr;
rsm_encode_count(Count, Arr)->
    [#xmlel{ns = ?NS_RSM, name = 'count', children = [#xmlcdata{cdata = i2b(Count)}]} | Arr].

i2b(I) when is_integer(I) -> list_to_binary(integer_to_list(I));
i2b(L) when is_list(L)    -> list_to_binary(L).

% yyyy-mm-ddThh:mm:ss[.sss]{Z|{+|-}hh:mm} -> {MegaSecs, Secs, MicroSecs}
datetime_string_to_timestamp(TimeStr) ->
    case catch parse_datetime(TimeStr) of
        {'EXIT', _Err} ->
            undefined;
        TimeStamp ->
            TimeStamp
    end.

parse_datetime(TimeStr) ->
    [Date, Time] = string:tokens(TimeStr, "T"),
    D = parse_date(Date),
    {T, MS, TZH, TZM} = parse_time(Time),
    S = calendar:datetime_to_gregorian_seconds({D, T}),
    S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds = (S - S1) - TZH * 60 * 60 - TZM * 60,
    {Seconds div 1000000, Seconds rem 1000000, MS}.

% yyyy-mm-dd
parse_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
    case calendar:valid_date(Date1) of
        true ->
            Date1;
        _ ->
            false
    end.

% hh:mm:ss[.sss]TZD
parse_time(Time) ->
    case string:str(Time, "Z") of
        0 ->
            parse_time_with_timezone(Time);
        _ ->
            [T | _] = string:tokens(Time, "Z"),
            {TT, MS} = parse_time1(T),
            {TT, MS, 0, 0}
    end.

parse_time_with_timezone(Time) ->
    case string:str(Time, "+") of
        0 ->
            case string:str(Time, "-") of
                0 ->
                    false;
                _ ->
                    parse_time_with_timezone(Time, "-")
            end;
        _ ->
            parse_time_with_timezone(Time, "+")
    end.

parse_time_with_timezone(Time, Delim) ->
    [T, TZ] = string:tokens(Time, Delim),
    {TZH, TZM} = parse_timezone(TZ),
    {TT, MS} = parse_time1(T),
    case Delim of
        "-" ->
            {TT, MS, -TZH, -TZM};
        "+" ->
            {TT, MS, TZH, TZM}
    end.
parse_timezone(TZ) ->
    [H, M] = string:tokens(TZ, ":"),
    {[H1, M1], true} = check_list([{H, 12}, {M, 60}]),
    {H1, M1}.

parse_time1(Time) ->
    [HMS | T] =  string:tokens(Time, "."),
    MS = case T of
             [] ->
                 0;
             [Val] ->
                 list_to_integer(string:left(Val, 6, $0))
         end,
    [H, M, S] = string:tokens(HMS, ":"),
    {[H1, M1, S1], true} = check_list([{H, 24}, {M, 60}, {S, 60}]),
    {{H1, M1, S1}, MS}.

check_list(List) ->
    lists:mapfoldl(
      fun({L, N}, B)->
          V = list_to_integer(L),
          if
              (V >= 0) and (V =< N) ->
                  {V, B};
              true ->
                  {false, false}
          end
      end, true, List).
