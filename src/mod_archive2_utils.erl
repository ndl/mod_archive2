%%%----------------------------------------------------------------------
%%% File    : mod_archive2_utils.erl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 helper functionality
%%% Created : 04 Oct 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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
-author('ejabberd@ndl.kiev.ua').

-export([list_to_bool/1, filter_undef/1, verify_iq_type/2]).

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
