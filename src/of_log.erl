%% @Author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

-module(of_log).

-export([log/4]).

%%
%% Exported functions.
%%

log(Module, Level, Format, Args) ->
    Now = erlang:now(),
    {_, _, MicroSeconds} = Now,
    {{Year, Month, Day}, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
    io:format("~4.4.0w-~2.2.0w-~2.2.0w ", [Year, Month, Day]),
    io:format("~2.2.0w:~2.2.0w:~2.2.0w.~6.6.0w | ", [Hours, Minutes, Seconds, MicroSeconds]),
    io:format("~w ~w | ~w | ", [Module, self(), Level]),
    io:format(Format, Args),
    io:format("~n"),
    ok.



