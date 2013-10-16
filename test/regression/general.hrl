%%%----------------------------------------------------------------------
%%% File    : general.hrl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 general regression testing expected replies
%%% Created : 30 Sep 2009 by Alexander Tsvyashchenko <xmpp@endl.ch>
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

-define(GENERAL_TC1_QUERY_RESULT,
    {received_packet,iq,"result",
     {undefined,<<?SERVERHOST>>,undefined},
     _,undefined,
     {xmlel,'jabber:client',[],iq,
      [{xmlattr,undefined,<<"from">>,<<?SERVERHOST>>},
       {xmlattr,undefined,<<"to">>,
        _},
       {xmlattr,undefined,<<"id">>,_},
       {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>}],
      [{xmlel,'http://jabber.org/protocol/disco#info',
        [{'http://jabber.org/protocol/disco#info',none}],
        'query',[],
        DiscoElements}]}}).

-define(GENERAL_TC1_DISCO_ELEMENTS,
        [
         {xmlel,'http://jabber.org/protocol/disco#info',[],
          feature,
          [{xmlattr,undefined,<<"var">>,
            <<?NS>>}],
          []},
         {xmlel,'http://jabber.org/protocol/disco#info',[],
          feature,
          [{xmlattr,undefined,<<"var">>,
            <<"http://www.xmpp.org/extensions/xep-0136.html#ns-auto">>}],
          []},
         {xmlel,'http://jabber.org/protocol/disco#info',[],
          feature,
          [{xmlattr,undefined,<<"var">>,
            <<"http://www.xmpp.org/extensions/xep-0136.html#ns-manage">>}],
          []},
         {xmlel,'http://jabber.org/protocol/disco#info',[],
          feature,
          [{xmlattr,undefined,<<"var">>,
            <<"http://www.xmpp.org/extensions/xep-0136.html#ns-manual">>}],
          []},
         {xmlel,'http://jabber.org/protocol/disco#info',[],
          feature,
          [{xmlattr,undefined,<<"var">>,
            <<"http://www.xmpp.org/extensions/xep-0136.html#ns-pref">>}],
          []}
        ]).

-define(GENERAL_TC2_RETRIEVE_RESULT,
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
             list,[],
             [{xmlel,undefined,[],chat,
                  [{xmlattr,undefined,<<"with">>,
                       <<?CLIENTJID2 "/", _/binary>>},
                   {xmlattr,undefined,<<"start">>,_},
                   {xmlattr,undefined,<<"version">>,<<"1">>}],
                  []},
              {xmlel,undefined,[],chat,
                  [{xmlattr,undefined,<<"with">>,
                       <<?CLIENTJID2 "/", _/binary>>},
                   {xmlattr,undefined,<<"start">>,_},
                   {xmlattr,undefined,<<"version">>,<<"2">>}],
                  []},
              {xmlel,'http://jabber.org/protocol/rsm',
                  [{'http://jabber.org/protocol/rsm',none}],
                  set,[],
                  [{xmlel,'http://jabber.org/protocol/rsm',[],first,
                       [{xmlattr,undefined,<<"index">>,<<"0">>}],
                       [{xmlcdata,_}]},
                   {xmlel,'http://jabber.org/protocol/rsm',[],last,[],
                       [{xmlcdata,_}]},
                   {xmlel,'http://jabber.org/protocol/rsm',[],count,[],
                       [{xmlcdata,<<"2">>}]}]}]}]}}).

-define(GENERAL_TC3_QUERY_RESULT,
    {received_packet,iq,"result",
       {<<?CLIENTNAME>>,<<?SERVERHOST>>,_},
       _,?NS_INBAND_REGISTER,
       {xmlel,'jabber:client',[],iq,
           [{xmlattr,undefined,<<"from">>,
                <<?CLIENTJID "/", _/binary>>},
            {xmlattr,undefined,<<"to">>,
                <<?CLIENTJID "/", _/binary>>},
            {xmlattr,undefined,<<"id">>,<<"reg-",_/binary>>},
            {xmlattr,undefined,<<"type">>,<<"result">>}],
           [{xmlel,'jabber:iq:register',
                [{'jabber:iq:register',none}],
                'query',[],
                [{xmlel,'jabber:iq:register',[],remove,[],[]}]}]}}).

-define(GENERAL_TC4_RETRIEVE_RESULT,
    {received_packet,iq,"result",
       {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
       "stanza-" ++ _,?NS_ARCHIVING,
       {xmlel,'jabber:client',[],iq,
           [{xmlattr,undefined,<<"from">>,<<?CLIENTJID>>},
            {xmlattr,undefined,<<"to">>,
                <<?CLIENTJID "/", _/binary>>},
            {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>},
            {xmlattr,undefined,<<"type">>,<<"result">>}],
           [{xmlel,?NS_ARCHIVING,
                [{?NS_ARCHIVING,none}],
                modified,[],
                [{xmlel,undefined,[],changed,
                     [{xmlattr,undefined,<<"with">>,<<?SERVERHOST>>},
                      {xmlattr,undefined,<<"start">>,
                          _},
                      {xmlattr,undefined,<<"version">>,<<"1">>}],
                     []},
                 {xmlel,'http://jabber.org/protocol/rsm',
                     [{'http://jabber.org/protocol/rsm',none}],
                     set,[],
                     [{xmlel,'http://jabber.org/protocol/rsm',[],first,
                          [{xmlattr,undefined,<<"index">>,<<"0">>}],
                          [{xmlcdata,_}]},
                      {xmlel,'http://jabber.org/protocol/rsm',[],last,[],
                          [{xmlcdata,_}]},
                      {xmlel,'http://jabber.org/protocol/rsm',[],count,[],
                          [{xmlcdata,<<"1">>}]}]}]}]}}).
