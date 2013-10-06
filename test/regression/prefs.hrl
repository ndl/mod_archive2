%%%----------------------------------------------------------------------
%%% File    : prefs.hrl
%%% Author  : Alexander Tsvyashchenko <xmpp@endl.ch>
%%% Purpose : mod_archive2 archiving preferences regression testing expected
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

-define(PREFS_TC1_DEFAULT,
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
             pref,[],
             [{xmlel,undefined,[],default,
                  [{xmlattr,undefined,<<"save">>,<<"body">>},
                   {xmlattr,undefined,<<"otr">>,<<"forbid">>},
                   {xmlattr,undefined,<<"unset">>,<<"true">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"auto">>},
                   {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"local">>},
                   {xmlattr,undefined,<<"use">>,<<"concede">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"manual">>},
                   {xmlattr,undefined,<<"use">>,<<"concede">>}],
                  []},
              {xmlel,undefined,[],auto,
                  [{xmlattr,undefined,<<"save">>,<<"true">>},
                   {xmlattr,undefined,<<"scope">>,<<"global">>}],
                  []}]}]}}).

-define(PREFS_TC2_CHANGE_PUSH,
    {received_packet,iq,"set",
     {undefined,<<?SERVERHOST>>,undefined},
     "push",?NS_ARCHIVING,
     {xmlel,'jabber:client',[{'jabber:client',none}],iq,
         [
          {xmlattr,undefined,<<"from">>,<<?SERVERHOST>>},
          {xmlattr,undefined,<<"to">>,
              <<?CLIENTJID "/", _/binary>>},
          {xmlattr,undefined,<<"type">>,<<"set">>},
          {xmlattr,undefined,<<"id">>,<<"push">>}],
         [{xmlel,?NS_ARCHIVING,
              [{?NS_ARCHIVING,none}],
              pref,[],
              [{xmlel,?NS_ARCHIVING,[],default,
                   [{xmlattr,undefined,<<"otr">>,<<"prefer">>},
                    {xmlattr,undefined,<<"save">>,<<"false">>}],
                   []}]}]}}).

-define(PREFS_TC2_CHANGE_RESULT,
    {received_packet,iq,"result",
     {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
     "stanza-" ++ _,undefined,
     {xmlel,'jabber:client',[],iq,
         [{xmlattr,undefined,<<"from">>,<<?CLIENTJID>>},
          {xmlattr,undefined,<<"to">>,
              <<?CLIENTJID "/", _/binary>>},
          {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>},
          {xmlattr,undefined,<<"type">>,<<"result">>}],
         []}}).

-define(PREFS_TC2_CHANGED,
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
             pref,[],
             [{xmlel,undefined,[],default,
                  [{xmlattr,undefined,<<"save">>,<<"false">>},
                   {xmlattr,undefined,<<"otr">>,<<"prefer">>},
                   {xmlattr,undefined,<<"unset">>,<<"false">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"auto">>},
                   {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"local">>},
                   {xmlattr,undefined,<<"use">>,<<"concede">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"manual">>},
                   {xmlattr,undefined,<<"use">>,<<"concede">>}],
                  []},
              {xmlel,undefined,[],auto,
                  [{xmlattr,undefined,<<"save">>,<<"true">>},
                   {xmlattr,undefined,<<"scope">>,<<"global">>}],
                  []}]}]}}).

-define(PREFS_TC3_CHANGE_PUSH,
    {received_packet,iq,"set",
     {undefined,<<?SERVERHOST>>,undefined},
     "push",?NS_ARCHIVING,
     {xmlel,'jabber:client',[{'jabber:client',none}],iq,
         [{xmlattr,undefined,<<"from">>,<<?SERVERHOST>>},
          {xmlattr,undefined,<<"to">>,
              <<?CLIENTJID "/", _/binary>>},
          {xmlattr,undefined,<<"type">>,<<"set">>},
          {xmlattr,undefined,<<"id">>,<<"push">>}],
         [{xmlel,?NS_ARCHIVING,
              [{?NS_ARCHIVING,none}],
              pref,[],
              [{xmlel,?NS_ARCHIVING,[],method,
                   [{xmlattr,undefined,<<"type">>,<<"auto">>},
                    {xmlattr,undefined,<<"use">>,<<"concede">>}],
                   []},
               {xmlel,?NS_ARCHIVING,[],method,
                   [{xmlattr,undefined,<<"type">>,<<"local">>},
                    {xmlattr,undefined,<<"use">>,<<"forbid">>}],
                   []},
               {xmlel,?NS_ARCHIVING,[],method,
                   [{xmlattr,undefined,<<"type">>,<<"manual">>},
                    {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                   []}]}]}}).

-define(PREFS_TC3_CHANGE_RESULT,
    {received_packet,iq,"result",
     {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
     "stanza-" ++ _,undefined,
     {xmlel,'jabber:client',[],iq,
         [{xmlattr,undefined,<<"from">>,<<?CLIENTJID>>},
          {xmlattr,undefined,<<"to">>,
              <<?CLIENTJID "/", _/binary>>},
          {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>},
          {xmlattr,undefined,<<"type">>,<<"result">>}],
         []}}).

-define(PREFS_TC3_CHANGED,
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
             pref,[],
             [{xmlel,undefined,[],default,
                  [{xmlattr,undefined,<<"save">>,<<"false">>},
                   {xmlattr,undefined,<<"otr">>,<<"prefer">>},
                   {xmlattr,undefined,<<"unset">>,<<"false">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"auto">>},
                   {xmlattr,undefined,<<"use">>,<<"concede">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"local">>},
                   {xmlattr,undefined,<<"use">>,<<"forbid">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"manual">>},
                   {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                  []},
              {xmlel,undefined,[],auto,
                  [{xmlattr,undefined,<<"save">>,<<"true">>},
                   {xmlattr,undefined,<<"scope">>,<<"global">>}],
                  []}]}]}}).

-define(PREFS_TC4_CHANGE_PUSH,
    {received_packet,iq,"set",
     {undefined,<<?SERVERHOST>>,undefined},
     "push",?NS_ARCHIVING,
     {xmlel,'jabber:client',[{'jabber:client',none}],iq,
         [{xmlattr,undefined,<<"from">>,<<?SERVERHOST>>},
          {xmlattr,undefined,<<"to">>,
              <<?CLIENTJID "/", _/binary>>},
          {xmlattr,undefined,<<"type">>,<<"set">>},
          {xmlattr,undefined,<<"id">>,<<"push">>}],
         [{xmlel,?NS_ARCHIVING,
              [{?NS_ARCHIVING,none}],
              pref,[],
              [{xmlel,?NS_ARCHIVING,[],item,
                   [{xmlattr,undefined,<<"jid">>,<<"romeo@montague.net">>},
                    {xmlattr,undefined,<<"save">>,<<"body">>},
                    {xmlattr,undefined,<<"expire">>,<<"604800">>},
                    {xmlattr,undefined,<<"otr">>,<<"concede">>}],
                   []}]}]}}).

-define(PREFS_TC4_CHANGE_RESULT,
    {received_packet,iq,"result",
     {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
     "stanza-" ++ _,undefined,
     {xmlel,'jabber:client',[],iq,
         [{xmlattr,undefined,<<"from">>,<<?CLIENTJID>>},
          {xmlattr,undefined,<<"to">>,
              <<?CLIENTJID "/", _/binary>>},
          {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>},
          {xmlattr,undefined,<<"type">>,<<"result">>}],
         []}}).

-define(PREFS_TC4_CHANGED,
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
             pref,[],
             [{xmlel,undefined,[],item,
                  [{xmlattr,undefined,<<"jid">>,<<"romeo@montague.net">>},
                   {xmlattr,undefined,<<"exactmatch">>,<<"false">>},
                   {xmlattr,undefined,<<"save">>,<<"body">>},
                   {xmlattr,undefined,<<"expire">>,<<"604800">>},
                   {xmlattr,undefined,<<"otr">>,<<"concede">>}],
                  []},
              {xmlel,undefined,[],default,
                  [{xmlattr,undefined,<<"save">>,<<"false">>},
                   {xmlattr,undefined,<<"otr">>,<<"prefer">>},
                   {xmlattr,undefined,<<"unset">>,<<"false">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"auto">>},
                   {xmlattr,undefined,<<"use">>,<<"concede">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"local">>},
                   {xmlattr,undefined,<<"use">>,<<"forbid">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"manual">>},
                   {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                  []},
              {xmlel,undefined,[],auto,
                  [{xmlattr,undefined,<<"save">>,<<"true">>},
                   {xmlattr,undefined,<<"scope">>,<<"global">>}],
                  []}]}]}}).

-define(PREFS_TC5_CHANGE_PUSH,
    {received_packet,iq,"set",
     {undefined,<<?SERVERHOST>>,undefined},
     "push",?NS_ARCHIVING,
     {xmlel,'jabber:client',[{'jabber:client',none}],iq,
         [{xmlattr,undefined,<<"from">>,<<?SERVERHOST>>},
          {xmlattr,undefined,<<"to">>,
              <<?CLIENTJID "/", _/binary>>},
          {xmlattr,undefined,<<"type">>,<<"set">>},
          {xmlattr,undefined,<<"id">>,<<"push">>}],
         [{xmlel,?NS_ARCHIVING,
              [{?NS_ARCHIVING,none}],
              itemremove,[],
              [{xmlel,?NS_ARCHIVING,[],item,
                   [{xmlattr,undefined,<<"jid">>,<<"romeo@montague.net">>}],
                   []}]}]}}).
    
-define(PREFS_TC5_CHANGE_RESULT,
    {received_packet,iq,"result",
     {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
     "stanza-" ++ _,undefined,
     {xmlel,'jabber:client',[],iq,
         [{xmlattr,undefined,<<"from">>,<<?CLIENTJID>>},
          {xmlattr,undefined,<<"to">>,
              <<?CLIENTJID "/", _/binary>>},
          {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>},
          {xmlattr,undefined,<<"type">>,<<"result">>}],
         []}}).

-define(PREFS_TC5_CHANGED,
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
             pref,[],
             [{xmlel,undefined,[],default,
                  [{xmlattr,undefined,<<"save">>,<<"false">>},
                   {xmlattr,undefined,<<"otr">>,<<"prefer">>},
                   {xmlattr,undefined,<<"unset">>,<<"false">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"auto">>},
                   {xmlattr,undefined,<<"use">>,<<"concede">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"local">>},
                   {xmlattr,undefined,<<"use">>,<<"forbid">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"manual">>},
                   {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                  []},
              {xmlel,undefined,[],auto,
                  [{xmlattr,undefined,<<"save">>,<<"true">>},
                   {xmlattr,undefined,<<"scope">>,<<"global">>}],
                  []}]}]}}).

-define(PREFS_TC6_CHANGE_RESULT,
    {received_packet,iq,"result",
                 {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
                 "stanza-" ++ _,undefined,
                 {xmlel,'jabber:client',[],iq,
                        [{xmlattr,undefined,<<"from">>,<<?CLIENTJID>>},
                         {xmlattr,undefined,<<"to">>,
                                  <<?CLIENTJID "/", _/binary>>},
                         {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>},
                         {xmlattr,undefined,<<"type">>,<<"result">>}],
                        []}}).

-define(PREFS_TC6_CHANGED,
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
             pref,[],
             [{xmlel,undefined,[],default,
                  [{xmlattr,undefined,<<"save">>,<<"false">>},
                   {xmlattr,undefined,<<"otr">>,<<"prefer">>},
                   {xmlattr,undefined,<<"unset">>,<<"false">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"auto">>},
                   {xmlattr,undefined,<<"use">>,<<"concede">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"local">>},
                   {xmlattr,undefined,<<"use">>,<<"forbid">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"manual">>},
                   {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                  []},
              {xmlel,undefined,[],auto,
                  [{xmlattr,undefined,<<"save">>,<<"true">>},
                   {xmlattr,undefined,<<"scope">>,<<"global">>}],
                  []},
              {xmlel,undefined,[],auto,
                  [{xmlattr,undefined,<<"save">>,<<"false">>},
                   {xmlattr,undefined,<<"scope">>,<<"stream">>}],
                  []}]}]}}).

-define(PREFS_TC7_CHANGE_PUSH,
    {received_packet,iq,"set",
                  {undefined,<<?SERVERHOST>>,undefined},
                  "push",?NS_ARCHIVING,
                  {xmlel,'jabber:client',[{'jabber:client',none}],iq,
                         [{xmlattr,undefined,<<"from">>,<<?SERVERHOST>>},
                          {xmlattr,undefined,<<"to">>,
                                   <<?CLIENTJID "/", _/binary>>},
                          {xmlattr,undefined,<<"type">>,<<"set">>},
                          {xmlattr,undefined,<<"id">>,<<"push">>}],
                         [{xmlel,?NS_ARCHIVING,
                                 [{?NS_ARCHIVING,none}],
                                 auto,
                                 [{xmlattr,undefined,<<"save">>,<<"false">>},
                                  {xmlattr,undefined,<<"scope">>,<<"global">>}],
                                 []}]}}).

-define(PREFS_TC7_CHANGE_RESULT,
    {received_packet,iq,"result",
                  {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
                  "stanza-" ++ _,undefined,
                  {xmlel,'jabber:client',[],iq,
                         [{xmlattr,undefined,<<"from">>,<<?CLIENTJID>>},
                          {xmlattr,undefined,<<"to">>,
                                   <<?CLIENTJID "/", _/binary>>},
                          {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>},
                          {xmlattr,undefined,<<"type">>,<<"result">>}],
                         []}}).

-define(PREFS_TC7_CHANGED,
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
             pref,[],
             [{xmlel,undefined,[],default,
                  [{xmlattr,undefined,<<"save">>,<<"false">>},
                   {xmlattr,undefined,<<"otr">>,<<"prefer">>},
                   {xmlattr,undefined,<<"unset">>,<<"false">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"auto">>},
                   {xmlattr,undefined,<<"use">>,<<"concede">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"local">>},
                   {xmlattr,undefined,<<"use">>,<<"forbid">>}],
                  []},
              {xmlel,undefined,[],method,
                  [{xmlattr,undefined,<<"type">>,<<"manual">>},
                   {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                  []},
              {xmlel,undefined,[],auto,
                  [{xmlattr,undefined,<<"save">>,<<"false">>},
                   {xmlattr,undefined,<<"scope">>,<<"global">>}],
                  []}]}]}}).

-define(PREFS_TC8_RESULT,
{received_packet,iq,"result",
        {<<?CLIENTNAME2>>,<<?SERVERHOST>>,undefined},
        "stanza-" ++ _, ?NS_ARCHIVING,
        {xmlel,'jabber:client',[],iq,
            [{xmlattr,undefined,<<"from">>,<<?CLIENTJID2>>},
             {xmlattr,undefined,<<"to">>,
                 <<?CLIENTJID2 "/", _/binary>>},
             {xmlattr,undefined,<<"id">>,<<"stanza-", _/binary>>},
             {xmlattr,undefined,<<"type">>,<<"result">>}],
            [{xmlel,?NS_ARCHIVING,
                 [{?NS_ARCHIVING,none}],
                 pref,[],
                 [{xmlel,undefined,[],default,
                      [{xmlattr,undefined,<<"save">>,<<"body">>},
                       {xmlattr,undefined,<<"otr">>,<<"forbid">>},
                       {xmlattr,undefined,<<"unset">>,<<"true">>}],
                      []},
                  {xmlel,undefined,[],method,
                      [{xmlattr,undefined,<<"type">>,<<"auto">>},
                       {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                      []},
                  {xmlel,undefined,[],method,
                      [{xmlattr,undefined,<<"type">>,<<"local">>},
                       {xmlattr,undefined,<<"use">>,<<"concede">>}],
                      []},
                  {xmlel,undefined,[],method,
                      [{xmlattr,undefined,<<"type">>,<<"manual">>},
                       {xmlattr,undefined,<<"use">>,<<"concede">>}],
                      []},
                  {xmlel,undefined,[],auto,
                      [{xmlattr,undefined,<<"save">>,<<"true">>},
                       {xmlattr,undefined,<<"scope">>,<<"global">>}],
                      []}]}]}}).

-define(PREFS_TC9_CHANGE_PUSH,
{received_packet,iq,"set",
    {undefined,<<?SERVERHOST>>,undefined},
    "push",?NS_ARCHIVING,
    {xmlel,'jabber:client',
        [{'jabber:client',none}],
        iq,
        [{xmlattr,undefined,<<"from">>,<<?SERVERHOST>>},
         {xmlattr,undefined,<<"to">>,
             <<?CLIENTJID "/", _/binary>>},
         {xmlattr,undefined,<<"type">>,<<"set">>},
         {xmlattr,undefined,<<"id">>,<<"push">>}],
        [{xmlel,?NS_ARCHIVING,
             [{?NS_ARCHIVING,none}],
             pref,[],
             [{xmlel,?NS_ARCHIVING,[],session,
                  [{xmlattr,undefined,<<"thread">>,<<"123">>},
                   {xmlattr,undefined,<<"save">>,<<"false">>}],
                  []}]}]}}).

-define(PREFS_TC9_CHANGE_RESULT,
{received_packet,iq,"result",
    {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
    "stanza-" ++ _,undefined,
    {xmlel,'jabber:client',[],iq,
        [{xmlattr,undefined,<<"from">>,<<?CLIENTJID>>},
         {xmlattr,undefined,<<"to">>,
             <<?CLIENTJID "/", _/binary>>},
         {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>},
         {xmlattr,undefined,<<"type">>,<<"result">>}],
        []}}).

-define(PREFS_TC9_GET_RESULT,
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
                 pref,[],
                 [{xmlel,undefined,[],default,
                      [{xmlattr,undefined,<<"save">>,<<"body">>},
                       {xmlattr,undefined,<<"otr">>,<<"prefer">>},
                       {xmlattr,undefined,<<"unset">>,<<"false">>}],
                      []},
                  {xmlel,undefined,[],method,
                      [{xmlattr,undefined,<<"type">>,<<"auto">>},
                       {xmlattr,undefined,<<"use">>,<<"concede">>}],
                      []},
                  {xmlel,undefined,[],method,
                      [{xmlattr,undefined,<<"type">>,<<"local">>},
                       {xmlattr,undefined,<<"use">>,<<"forbid">>}],
                      []},
                  {xmlel,undefined,[],method,
                      [{xmlattr,undefined,<<"type">>,<<"manual">>},
                       {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                      []},
                  {xmlel,undefined,[],auto,
                      [{xmlattr,undefined,<<"save">>,<<"true">>},
                       {xmlattr,undefined,<<"scope">>,<<"global">>}],
                      []},
		 {xmlel,undefined,[],session,
                      [{xmlattr,undefined,<<"thread">>,<<"123">>},
                       {xmlattr,undefined,<<"save">>,<<"false">>},
                       {xmlattr,undefined,<<"timeout">>,<<"1800">>}],
                      []}]}]}}).

-define(PREFS_TC9_CHANGE_RESULT2,
    {received_packet,iq,"result",
        {<<?CLIENTNAME>>,<<?SERVERHOST>>,undefined},
        "stanza-" ++ _,undefined,
        {xmlel,'jabber:client',[],iq,
            [{xmlattr,undefined,<<"from">>,<<?CLIENTJID>>},
             {xmlattr,undefined,<<"to">>,
                 <<?CLIENTJID "/", _/binary>>},
             {xmlattr,undefined,<<"id">>,<<"stanza-",_/binary>>},
             {xmlattr,undefined,<<"type">>,<<"result">>}],
            []}}).

-define(PREFS_TC9_GET_RESULT2,
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
                 pref,[],
                 [{xmlel,undefined,[],default,
                      [{xmlattr,undefined,<<"save">>,<<"body">>},
                       {xmlattr,undefined,<<"otr">>,<<"prefer">>},
                       {xmlattr,undefined,<<"unset">>,<<"false">>}],
                      []},
                  {xmlel,undefined,[],method,
                      [{xmlattr,undefined,<<"type">>,<<"auto">>},
                       {xmlattr,undefined,<<"use">>,<<"concede">>}],
                      []},
                  {xmlel,undefined,[],method,
                      [{xmlattr,undefined,<<"type">>,<<"local">>},
                       {xmlattr,undefined,<<"use">>,<<"forbid">>}],
                      []},
                  {xmlel,undefined,[],method,
                      [{xmlattr,undefined,<<"type">>,<<"manual">>},
                       {xmlattr,undefined,<<"use">>,<<"prefer">>}],
                      []},
                  {xmlel,undefined,[],auto,
                      [{xmlattr,undefined,<<"save">>,<<"true">>},
                       {xmlattr,undefined,<<"scope">>,<<"global">>}],
                      []}]}]}}).
