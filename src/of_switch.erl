%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

-module(of_switch).

-behavior(gen_server).

-export([start_link/0,
         stop/1,
         connect/3,
         accept/2]).

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

connect(Pid, IpAddress, TcpPort) ->
    gen_server:call(Pid, {connect, IpAddress, TcpPort}).

accept(Pid, Socket) ->
    ok = gen_tcp:controlling_process(Socket, Pid),
    gen_server:call(Pid, {accept, Socket}).

%%                 
%% gen_server callbacks.
%%

init([]) ->
    State = #of_switch_state{connection_pid = undefined},
    {ok, State}.

handle_call({connect, IpAddress, TcpPort}, _From, State) ->
    io:format("of_switch: connect IpAddress=~w TcpPort=~w~n", [IpAddress, TcpPort]),
    State1 = initiate_connection(IpAddress, TcpPort, State),
    State2 = process_connection_up(State1),
    {reply, ok, State2};

handle_call({accept, Socket}, _From, State) ->
    io:format("of_switch: accept Socket=~w~n", [Socket]),
    State1 = accept_connection(Socket, State),
    State2 = process_connection_up(State1),
    {reply, ok, State2};

handle_call(stop, _From, State) ->
    io:format("of_switch: stop~n"),
    {stop, normal, stopped, State}.

handle_cast(Cast, State) ->
    io:format("of_switch: cast Cast=~w~n", [Cast]),
    {noreply, State}.

handle_info({of_receive_message, Xid, Message}, State) ->
    io:format("of_switch: of_receive_message Xid=~w Message=~w~n", [Xid, Message]),
    State1 = process_received_message(Xid, Message, State),
    {noreply, State1};

handle_info(Info, State) ->
    io:format("of_switch: info Info=~w~n", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("of_switch: terminate Reason=~w~n", [Reason]),
    ok.

code_change(OldVersion, State, _Extra) ->
    io:format("of_switch: code_change OldVersion=~w~n", [OldVersion]),
    {ok, State}.

%%                 
%% Internal functions.
%%

initiate_connection(IpAddress, TcpPort, State) ->
    %% TODO make sure connection_pid is undefined
    {ok, ConnectionPid} = of_connection:start_link(),
    ok = of_connection:connect(ConnectionPid, IpAddress, TcpPort),
    State#of_switch_state{connection_pid = ConnectionPid}.

accept_connection(Socket, State) -> 
    %% TODO make sure connection_pid is undefined
    {ok, ConnectionPid} = of_connection:start_link(),
    ok = of_connection:accept(ConnectionPid, Socket),
    State#of_switch_state{connection_pid = ConnectionPid}.

send_hello(State) ->
    HelloMessage = #of_v10_hello{},
    ConnectionPid = State#of_switch_state.connection_pid,
    Xid = 0, %% TODO: Xid allocation
    ok = of_connection:send(ConnectionPid, Xid, HelloMessage),
    State.

process_connection_up(State) ->
    send_hello(State).

%% process_connection_down(State) ->
%%     %% TODO
%%     State.

process_received_message(Xid, Message, State) ->
    if
        is_record(Message, of_v10_hello)        -> process_received_hello(Xid, Message, State);
        is_record(Message, of_v10_echo_request) -> process_received_echo_request(Xid, Message, State);
        true                                    -> process_received_unknown_message(Xid, Message, State)
    end.

process_received_hello(_Xid, _Hello, State) ->
    %% TODO: Do version negotiation
    State.

process_received_echo_request(Xid, EchoRequest, State) ->
    Data = EchoRequest#of_v10_echo_request.data,
    EchoReply = #of_v10_echo_reply{data = Data},
    ConnectionPid = State#of_switch_state.connection_pid,
    ok = of_connection:send(ConnectionPid, Xid, EchoReply),
    %% TODO: liveness checking (not here probably - more general)
    State.

process_received_unknown_message(_Xid, Message, State) ->
    %% TODO: add missing messages
    io:format("of_switch: process_received_unknown_message Message=~w~n", [Message]),
    State.
