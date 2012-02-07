%% @Author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

-module(of_group).

-export([create/1,
         delete/1,
         send/2,
         join/2,
         leave/2]).

-include_lib("include/of_log.hrl").

%%
%% Exported functions.
%%

create(Group) ->
    ?DEBUG_KEY([{group, Group}], "create"),
    pg2:create(Group).

delete(Group) ->
    ?DEBUG_KEY([{group, Group}], "delete"),
    pg2:delete(Group).

send(Group, Message) ->
    ?DEBUG_KEY([{group, Group}], "send Message=~w", [Message]),
    case pg2:get_members(Group) of
        {error, Reason} ->
            {error, Reason};
        Pids ->
            lists:foreach(fun(Pid) -> send(Pid, Message) end, Pids)
    end.

join(Group, Pid) ->
    ?DEBUG_KEY([{group, Group}], "join Pid=~w", [Pid]),
    pg2:join(Group, Pid).
    
leave(Group, Pid) ->
    ?DEBUG_KEY([{group, Group}], "leave Pid=~w", [Pid]),
    pg2:leave(Group, Pid).

