-module(build).
-export([all/0]).

all() ->
    case make:all() of
        up_to_date -> up_to_date;
        error -> halt(1)
    end.
