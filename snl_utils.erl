%% Utility module providing helper functions such as logging.
-module(snl_utils).
-export([log/2]).
%% @doc Logs a message with a specified log level.
-spec log(Level :: string(), Message :: string()) -> ok.
log(Level, Message) ->
    io:format("~s: ~s~n", [Level, Message]).
