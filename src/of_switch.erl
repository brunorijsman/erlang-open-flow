%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

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

-include_lib("../include/of.hrl").
-include_lib("../include/of_v10.hrl").

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
    State1 = #of_switch_state{connection_pid = undefined},
    State2 = connect(State1),
    {ok, State2}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(Cast, State) ->
    io:format("of_switch cast: ~w~n", [Cast]),
    {noreply, State}.

handle_info({of_receive_message, Xid, Message}, State) ->
    io:format("Received xid=0x~.16b message=~w~n", [Xid, Message]),
    NewState = switch_message_received(State, Xid, Message),
    {noreply, NewState};

handle_info(Info, State) ->
    io:format("of_switch info: ~w~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%                 
%% Internal functions.
%%

connect(State) ->
    {ok, Pid} = of_connection:start_link(),
    ok = of_connection:connect(Pid, {127, 0, 0, 1}, 6633),
    State1 = State#of_switch_state{connection_pid = Pid},
    switch_connected(State1).

switch_connected(State) ->
    HelloMessage = #of_v10_hello{},
    ConnectionPid = State#of_switch_state.connection_pid,
    Xid = 0, %% TODO: Xid allocation
    ok = of_connection:send(ConnectionPid, Xid, HelloMessage),
    State.

%% TODO: add and implement switch_disconnected

%% TODO: make hello version independent
switch_message_received(State, _Xid, Hello) 
  when is_record(Hello, of_v10_hello) ->
    %% TODO: state handling; version negotiation
    State;

switch_message_received(State, Xid, EchoRequest) 
  when is_record(EchoRequest, of_v10_echo_request) ->
    Data = EchoRequest#of_v10_echo_request.data,
    EchoReply = #of_v10_echo_reply{data = Data},
    ConnectionPid = State#of_switch_state.connection_pid,
    ok = of_connection:send(ConnectionPid, Xid, EchoReply),
    %% TODO: liveness checking (not here probably - more general)
    State;

switch_message_received(State, _Xid, _Message) ->
    %% TODO: add missing messages
    io:format("Message handling not implemented yet."),
    State.
