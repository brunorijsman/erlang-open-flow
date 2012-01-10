%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

-module(of_switch).

-behavior(gen_server).

-export([start_link/1,
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

%% TODO: Is it the right thing to do to start_link the connection in the context of the
%% caller, or should we postpone this to the init function? Certainly checking the
%% correctnes of the Args should be done in the caller's context. The current code looks
%% hacky -- change it to the latter option.

start_link(Args) ->
    try
        State = initial_state(Args),
        gen_server:start_link(?MODULE, [State], [])
    catch
        error:Reason ->
            {error, Reason}
    end.

stop(Pid) ->
    gen_server:call(Pid, stop).

%%                 
%% gen_server callbacks.
%%

init([State1]) ->
    ConnectionPid = State1#of_switch_state.connection_pid,
    State2 = case ConnectionPid of
                 undefined ->
                     State1;
                 _ ->
                     send_hello(State1)
             end,
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

initial_state(Args) ->
    State = #of_switch_state{connection_pid = undefined},
    parse_args(Args, State).

parse_args(Args, State) ->
    lists:foldl(fun parse_arg/2, State, Args).

parse_arg({socket, Socket}, State)
  when State#of_switch_state.connection_pid == undefined ->
    case of_connection:start_link([{socket, Socket}]) of
        {ok, Pid} ->
            State#of_switch_state{connection_pid = Pid};
        {error, Reason} ->
            erlang:error(Reason)
    end;

parse_arg({socket, _Socket}, _State) ->
    erlang:error(multiple_arg_socket);

parse_arg(Arg, _State) ->
    erlang:error({unrecognized_attribute, Arg}).

%% connect(State) ->
%%     {ok, Pid} = of_connection:start_link(),
%%     ok = of_connection:connect(Pid, {127, 0, 0, 1}, 6633),
%%     State1 = State#of_switch_state{connection_pid = Pid},
%%     switch_connected(State1).

send_hello(State) ->
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
