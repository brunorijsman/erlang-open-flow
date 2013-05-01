%%%=====================================================================================================================
%%% Copyright (c) 2012-2013, Bruno Rijsman
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted provided that the 
%%% following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this list of conditions and the following 
%%%   disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the 
%%%   following disclaimer in the documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
%%% USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%=====================================================================================================================

%%% @Author Bruno Rijsman <brunorijsman@hotmail.com>
%%% @copyright 2012-2013 Bruno Rijsman

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
