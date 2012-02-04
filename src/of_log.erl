%% @Author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

-module(of_log).

-export([log/5]).

%%
%% Exported functions.
%%

log(Module, Level, Keys, Format, Args) ->
    Now = erlang:now(),
    {_, _, MicroSeconds} = Now,
    {{Year, Month, Day}, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
    io:format("~4.4.0w-~2.2.0w-~2.2.0w ", [Year, Month, Day]),
    io:format("~2.2.0w:~2.2.0w:~2.2.0w.~6.6.0w | ", [Hours, Minutes, Seconds, MicroSeconds]),
    io:format("~w | ", [Level]),
    io:format("~w ~w | ", [Module, self()]),
    log_keys(Keys),
    io:format(Format, Args),
    io:format("~n"),
    ok.

%%
%% Internal functions.
%%

log_keys([]) ->
    io:format("| "),
    ok;

log_keys([{Key, Value} | Rest]) ->
    log_key(Key, Value),
    log_keys(Rest).

log_key(Key, Value) ->
    if 
        is_list(Value) -> io:format("~p=~s ", [Key, Value]);
        true           -> io:format("~p=~p ", [Key, Value])
    end.



