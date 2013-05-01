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

-module(of_logger).

-behavior(gen_server).

-export([start/0,
         stop/0,
         log/5]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%
%% Exported functions.
%%

%% TODO: Probably need to do something more sophisticated here to make sure there is only
%% a single instance of the of_logger process.

%% TODO: Should this be a global name?

%% TODO: Add configurable filtering, logging levels, etc.

start() ->
    gen_server:start_link({local, of_logger}, ?MODULE, [], []).

stop() ->
    gen_server:call({local, of_logger}, stop).

log(Level, LoggerModule, Keys, Format, Args) ->
    Time = erlang:now(),
    LoggerPid = self(),
    gen_server:cast(of_logger, {log, Time, Level, LoggerModule, LoggerPid, Keys, Format, Args}).

%%                 
%% gen_server callbacks.
%%

init([]) ->
    {ok, no_state}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast({log, Time, Level, LoggerModule, LoggerPid, Keys, Format, Args}, State) ->
    {_, _, MicroSeconds} = Time,
    {{Year, Month, Day}, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Time),
    io:format("~4.4.0w-~2.2.0w-~2.2.0w ", [Year, Month, Day]),
    io:format("~2.2.0w:~2.2.0w:~2.2.0w.~6.6.0w | ", [Hours, Minutes, Seconds, MicroSeconds]),
    io:format("~w | ", [Level]),
    io:format("~w ~w | ", [LoggerModule, LoggerPid]),
    log_keys(Keys),
    io:format(Format, Args),
    io:format("~n"),
    {noreply, State};

handle_cast(Cast, State) ->
    io:format("Cast=~w~n", [Cast]),
    {noreply, State}.

%% handle_cast(_Cast, State) ->
%%     {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

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
