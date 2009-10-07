%%%----------------------------------------------------------------------
%%% File    : replication.hrl
%%% Author  : Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
%%% Purpose : mod_archive2 archiving replication regression testing expected
%%%           replies
%%% Created : 27 Sep 2009 by Alexander Tsvyashchenko <ejabberd@ndl.kiev.ua>
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
        _,undefined,
        {xmlel,'jabber:client',[],iq,
            [{xmlattr,undefined,from,<<?CLIENTJID>>},
             {xmlattr,undefined,to,
                 _},
             {xmlattr,undefined,id,_},
             {xmlattr,undefined,type,<<"result">>}],
            []}}).

-define(REPLICATION_TC1_RETRIEVE_RESULT,
    {received_packet,iq,"result",
     {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
     _,undefined,
     {xmlel,'jabber:client',[],iq,
      [{xmlattr,undefined,from,<<?CLIENTJID>>},
       {xmlattr,undefined,to,
        _},
       {xmlattr,undefined,id,_},
       {xmlattr,undefined,type,<<"result">>}],
      [{xmlel,
        ?NS,
        [{?NS,
          none}],
        modified,[],
	ReplicationElements}]}}).

-define(REPLICATION_TC1_RETRIEVE_ELEMENTS,
        [
         {xmlel,'http://jabber.org/protocol/rsm',
          [{'http://jabber.org/protocol/rsm',none}],
          set,[],
          [{xmlel,'http://jabber.org/protocol/rsm',[],first,
            [{xmlattr,undefined,index,<<"0">>}],
            [{xmlcdata,_}]},
           {xmlel,'http://jabber.org/protocol/rsm',[],last,[],
            [{xmlcdata,_}]},
           {xmlel,'http://jabber.org/protocol/rsm',[],changed,
            [],
            [{xmlcdata,_}]},
           {xmlel,'http://jabber.org/protocol/rsm',[],count,[],
            [{xmlcdata,<<"5">>}]}]},
         {xmlel,
          ?NS,[],
          changed,
          [{xmlattr,undefined,with,
            <<"juliet@capulet.com/chamber">>},
           {xmlattr,undefined,start,
            <<"1469-07-21T02:56:15.000000Z">>},
           {xmlattr,undefined,"by",
            _}],
          []},
         {xmlel,
          ?NS,[],
          removed,
          [{xmlattr,undefined,with,
            <<"balcony@house.capulet.com">>},
           {xmlattr,undefined,start,
            <<"1469-07-21T03:16:37.000000Z">>},
           {xmlattr,undefined,"by",
            _}],
          []},
         {xmlel,
          ?NS,[],
          removed,
          [{xmlattr,undefined,with,<<"benvolio@montague.net">>},
           {xmlattr,undefined,start,
            <<"1469-07-21T03:01:54.000000Z">>},
           {xmlattr,undefined,"by",
            _}],
          []},
         {xmlel,
          ?NS,[],
          removed,
          [{xmlattr,undefined,with,
            <<"juliet@capulet.com/chamber">>},
           {xmlattr,undefined,start,
            <<"1470-07-21T02:56:15.000000Z">>},
           {xmlattr,undefined,"by",
            _}],
          []},
	{xmlel,
          ?NS,[],
          removed,
          [{xmlattr,undefined,with,<<?SERVERHOST>>},
           {xmlattr,undefined,start,
            _},
           {xmlattr,undefined,"by",
            _}],
          []}
	]).
