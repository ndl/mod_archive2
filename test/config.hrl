-define(SERVERHOST, "ndl-server").
-define(SERVERPORT, 6222).
-define(CLIENTNAME, "client").
-define(CLIENTJID, ?CLIENTNAME "@" ?SERVERHOST).
-define(PASSWORD, "password").
-define(NS, "http://www.xmpp.org/extensions/xep-0136.html#ns").
-define(TIMEOUT, 5000).

-define(DEBUG_FMT(Format, Values), false).
%-define(DEBUG_FMT(Format, Values), ?debugFmt(Format, Values)).
