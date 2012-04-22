%% @Author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

-module(of_events).

-export([create_topic/1,
         delete_topic/1,
         subscribe_to_topic/2,
         unsubscribe_from_topic/2,
         unicast_event/4,
         multicast_event/3]).

-include_lib("include/of_log.hrl").

%%
%% Exported functions.
%%

create_topic(Topic) ->
    ?DEBUG_KEY([{topic, Topic}], "create"),
    pg2:create(topic_to_group_name(Topic)).

delete_topic(Topic) ->
    ?DEBUG_KEY([{topic, Topic}], "delete"),
    pg2:delete(topic_to_group_name(Topic)).

subscribe_to_topic(Topic, Pid) ->
    ?DEBUG_KEY_FMT([{topic, Topic}], "join Pid=~w", [Pid]),
    pg2:join(topic_to_group_name(Topic), Pid).
    
unsubscribe_from_topic(Topic, Pid) ->
    ?DEBUG_KEY_FMT([{topic, Topic}], "leave Pid=~w", [Pid]),
    pg2:leave(topic_to_group_name(Topic), Pid).

unicast_event(Pid, Topic, Event, Args) ->
    ?DEBUG_KEY_FMT([{topic, Topic}], "unicast event Pid=~w Event=~w Args=~w", [Pid, Event, Args]),
    Pid ! {of_event, Event, Args},
    ok.

multicast_event(Topic, Event, Args) ->
    ?DEBUG_KEY_FMT([{topic, Topic}], "multicast event Event=~w Args=~w", [Event, Args]),
    case pg2:get_members(Topic) of
        {error, Reason} ->
            {error, Reason};
        Pids ->
            Message = {of_event, Event, Args},
            lists:foreach(fun(Pid) -> Pid ! Message end, Pids),
            ok
    end.

%%
%% Internal functions.
%%

topic_to_group_name(Topic) ->
    {of_topic, Topic}.
