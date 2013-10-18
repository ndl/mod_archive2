%%%----------------------------------------------------------------------
%%% File    : replication.hrl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 archiving replication regression testing expected
%%%           replies
%%% Created : 27 Sep 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-define(REPLICATION_TC1_UPLOAD_RESULT,
{received_packet,iq,"result",
    {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
    "stanza-" ++ _,?NS_ARCHIVING,
    {xmlel,'jabber:client',[],iq,
        [{xmlattr,undefined,<<"from">>,<<?CLIENTJID>>},
         {xmlattr,undefined,<<"to">>,<<?CLIENTJID "/", _/binary>>},
         {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>},
         {xmlattr,undefined,<<"type">>,<<"result">>}],
        [{xmlel,?NS_ARCHIVING,
             [{?NS_ARCHIVING,none}],
             save,[],
             [{xmlel,undefined,[],chat,
                  [{xmlattr,undefined,<<"with">>,<<"juliet@capulet.com/chamber">>},
                   {xmlattr,undefined,<<"start">>,<<"1469-07-21T02:56:15.000000Z">>},
                   {xmlattr,undefined,<<"subject">>,<<"She speaks!">>},
                   {xmlattr,undefined,<<"thread">>,<<"damduoeg09">>},
                   {xmlattr,undefined,<<"version">>,<<"3">>}],
                  []}]}]}}).

-define(REPLICATION_TC1_RETRIEVE_RESULT,
{received_packet,iq,"result",
    {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
    "stanza-" ++ _,?NS_ARCHIVING,
    {xmlel,'jabber:client',[],iq,
        [{xmlattr,undefined,<<"from">>,<<?CLIENTJID>>},
         {xmlattr,undefined,<<"to">>,<<?CLIENTJID "/", _/binary>>},
         {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>},
         {xmlattr,undefined,<<"type">>,<<"result">>}],
        [{xmlel,?NS_ARCHIVING,
             [{?NS_ARCHIVING,none}],
             modified,[],
             ReplicationElements}]}}).

-define(REPLICATION_TC1_ELEMENTS,
[{xmlel,'http://jabber.org/protocol/rsm',
        [{'http://jabber.org/protocol/rsm',none}],
        set,[],
        [{xmlel,'http://jabber.org/protocol/rsm',[],first,
                [{xmlattr,undefined,<<"index">>,<<"0">>}],
                [{xmlcdata,_}]},
         {xmlel,'http://jabber.org/protocol/rsm',[],last,[],
                [{xmlcdata,_}]},
         {xmlel,'http://jabber.org/protocol/rsm',[],count,[],
                [{xmlcdata,<<"7">>}]}]},
 {xmlel,undefined,[],changed,
        [{xmlattr,undefined,<<"with">>,
                  <<?CLIENTJID2 "/", _/binary>>},
         {xmlattr,undefined,<<"start">>,_},
         {xmlattr,undefined,<<"version">>,<<"1">>}],
        []},
 {xmlel,undefined,[],changed,
        [{xmlattr,undefined,<<"with">>,
                  <<?CLIENTJID2 "/", _/binary>>},
         {xmlattr,undefined,<<"start">>,_},
         {xmlattr,undefined,<<"version">>,<<"2">>}],
        []},
 {xmlel,undefined,[],changed,
        [{xmlattr,undefined,<<"with">>,<<"juliet@capulet.com/chamber">>},
         {xmlattr,undefined,<<"start">>,<<"1469-07-21T02:56:15.000000Z">>},
         {xmlattr,undefined,<<"version">>,<<"3">>}],
        []},
 {xmlel,undefined,[],removed,
        [{xmlattr,undefined,<<"with">>,<<"balcony@house.capulet.com">>},
         {xmlattr,undefined,<<"start">>,<<"1469-07-21T03:16:37.000000Z">>},
         {xmlattr,undefined,<<"version">>,<<"2">>}],
        []},
 {xmlel,undefined,[],removed,
        [{xmlattr,undefined,<<"with">>,<<"benvolio@montague.net">>},
         {xmlattr,undefined,<<"start">>,<<"1469-07-21T03:01:54.012340Z">>},
         {xmlattr,undefined,<<"version">>,<<"1">>}],
        []},
 {xmlel,undefined,[],removed,
        [{xmlattr,undefined,<<"with">>,<<"juliet@capulet.com/chamber">>},
         {xmlattr,undefined,<<"start">>,<<"1470-07-21T02:56:15.000000Z">>},
         {xmlattr,undefined,<<"version">>,<<"0">>}],
        []},
 {xmlel,undefined,[],removed,
        [{xmlattr,undefined,<<"with">>,<<?SERVERHOST>>},
         {xmlattr,undefined,<<"start">>,_},
         {xmlattr,undefined,<<"version">>,<<"1">>}],
        []}]).
