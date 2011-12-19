%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman

-module(of_switch).

-behavior(gen_server).

-export([start_link/0,
         stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(of_switch_state, {
          connection_pid}).

%%
%% Exported functions.
%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%%                 
%% gen_server callbacks.
%%

init([]) ->
    ConnectionPid = connect(),
    State = #of_switch_state{connection_pid = ConnectionPid},
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%                 
%% Internal functions.
%%

%% TODO: This is just some temporary code to get started on testing.

connect() ->
    {ok, Pid} = of_connection:start_link(),
    ok = of_connection:connect(Pid, {127, 0, 0, 1}, 6633),
    Pid.
    
