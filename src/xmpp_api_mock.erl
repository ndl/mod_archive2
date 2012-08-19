%%%----------------------------------------------------------------------
%%% File    : xmpp_api_mock.erl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : Support XEP-136 for messages archiving
%%% Created : 18 Aug 2012 by Alexander Tsvyashchenko <xmpp@endl.ch>
%%%
%%% mod_archive2, Copyright (C) 2009,2012 Alexander Tsvyashchenko
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

-module(xmpp_api_mock).
-author('xmpp@endl.ch').

-include_lib("exmpp/include/exmpp.hrl").

-export([get_valid_hosts/0, info_msg/2, error_msg/2, broadcast_iq/2]).

get_valid_hosts() -> [].

info_msg(_Format, _Opts) -> ok.

error_msg(_Format, _Opts) -> ok.

%% Broadcasts IQ to all active sessions.
broadcast_iq(_From, _IQ) -> ok.
