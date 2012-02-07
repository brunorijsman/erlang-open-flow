%% @Author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

-module(of_group).

-export([create/1,
         delete/1,
         send/2]).

-include_lib("include/of_log.hrl").

%%
%% Exported functions.
%%

create(Name) ->
    ?DEBUG_KEY([{name, Name}], "create"),
    pg2:create(Name).

delete(Name) ->
    ?DEBUG_KEY([{name, Name}], "delete"),
    pg2:delete(Name).

send(Name, Message) ->
    ?DEBUG_KEY([{name, Name}], "send Message=~w", [Message]),
    case pg2:get_members(Name) of
        {error, Reason} ->
            {error, Reason};
        Pids ->
            lists:foreach(fun(Pid) -> send(Pid, Message) end, Pids)
    end.
