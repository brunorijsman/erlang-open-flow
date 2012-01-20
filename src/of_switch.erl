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

-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/of.hrl").
-include_lib("../include/of_v10.hrl").

-record(of_switch_state, {
          connection_pid,
          version,
          receive_hello_timer,
          send_echo_request_timer
         }).

%% TODO: Make this configurable. 
%% TODO: Have configurable option to only send echo requests in the absence of other received messages.
-define(SEND_ECHO_REQUEST_INTERVAL_MSECS, 30000).

%% TODO: Make this configurable. 
-define(RECEIVE_HELLO_INTERVAL_MSECS, 10000).

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
    State = #of_switch_state{connection_pid          = undefined, 
                             version                 = undefined,
                             receive_hello_timer     = undefined,
                             send_echo_request_timer = undefined},
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
    process_received_message(Xid, Message, State);

handle_info(timer_expired_receive_hello, State) ->
    process_timer_expired_receive_hello(State);

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
    ?assert(State#of_switch_state.connection_pid == undefined),
    {ok, ConnectionPid} = of_connection:start_link(),
    ok = of_connection:connect(ConnectionPid, IpAddress, TcpPort),
    State1 = State#of_switch_state{connection_pid = ConnectionPid},
    start_receive_hello_timer(State1).
    
accept_connection(Socket, State) -> 
    {ok, ConnectionPid} = of_connection:start_link(),
    ok = of_connection:accept(ConnectionPid, Socket),
    State1 = State#of_switch_state{connection_pid = ConnectionPid},
    start_receive_hello_timer(State1).

close_connection(State) ->
    ConnectionPid = State#of_switch_state.connection_pid,
    ?assert(is_pid(ConnectionPid)),
    ok = of_connection:close(ConnectionPid),
    of_connection:stop(ConnectionPid),
    State#of_switch_state{connection_pid = undefined, version = undefined}.

start_receive_hello_timer(State) ->
    State1 = case State#of_switch_state.receive_hello_timer of
                 undefined -> State;
                 _         -> stop_receive_hello_timer(State)
             end,
    {ok, Timer} = timer:send_after(?RECEIVE_HELLO_INTERVAL_MSECS, timer_expired_receive_hello),
    State1#of_switch_state{receive_hello_timer = Timer}.
    
stop_receive_hello_timer(State) ->
    case State#of_switch_state.receive_hello_timer of
        undefined -> 
            State;
        _         -> 
            Timer = State#of_switch_state.receive_hello_timer,
            {ok, cancel} = timer:cancel(Timer),
            State#of_switch_state{receive_hello_timer = undefined}
    end.
    
send_hello(State) ->
    Message = #of_vxx_hello{version = ?OF_VERSION_MAX},
    ConnectionPid = State#of_switch_state.connection_pid,
    Xid = 0,
    ok = of_connection:send(ConnectionPid, Xid, Message),
    State.

send_error_incompatible(State) ->
    Message = #of_vxx_error{version = ?OF_VERSION_MIN,
                            type    = ?OF_VXX_ERROR_TYPE_HELLO_FAILED,
                            code    = ?OF_VXX_ERROR_CODE_HELLO_FAILED_INCOMPATIBLE,
                            data    = << ?OF_IMPLEMENTATION_NAME >>},
    ConnectionPid = State#of_switch_state.connection_pid,
    Xid = 0,
    ok = of_connection:send(ConnectionPid, Xid, Message),
    State.

process_connection_up(State) ->
    send_hello(State).

%% process_connection_down(State) ->
%%     %% TODO
%%     State.

process_timer_expired_receive_hello(State) ->
    io:format("of_switch: no hello received from peer, closing connection~n"),
    State1 = close_connection(State),
    {stop, no_hello_received, State1}.

%% TODO: process_send_echo_request_timer_expired

process_received_message(Xid, Message, State) ->
    case State#of_switch_state.version of
        undefined -> process_received_initial_message(Xid, Message, State);
        _         -> process_received_subsequent_message(Xid, Message, State)
    end.

process_received_initial_message(Xid, Message, State) ->
    if
        is_record(Message, of_vxx_hello)        -> process_received_initial_hello(Xid, Message, State);
        true                                    -> process_received_initial_unexpected_message(Xid, Message, State)
    end.

process_received_initial_hello(_Xid, Hello, State) ->
    State1 = stop_receive_hello_timer(State),
    Version = Hello#of_vxx_hello.version,
    if
        (Version >= ?OF_VERSION_MIN) andalso (Version =< ?OF_VERSION_MAX) ->
            io:format("of_switch: version negotiation succeeded Version=~w~n", [Version]),
            State2 = State#of_switch_state{version = Version},
            {noreply, State2};
        true ->
            io:format("of_switch: version negotiation failed Version=~w~n", [Version]),
            State2 = send_error_incompatible(State1),
            State3 = close_connection(State2),
            %% TODO: This causes a "=ERROR REPORT===="; can that be avoided?
            {stop, version_negotiation_failed, State3}
    end.

process_received_initial_unexpected_message(_Xid, _Message, State) ->
    %% TODO
    {noreply, State}.

process_received_subsequent_message(Xid, Message, State) ->
    %% TODO: make sure version is negotiated version; pass Version along with Xid after all.
    if
        is_record(Message, of_vxx_hello)        -> process_received_hello(Xid, Message, State);
        is_record(Message, of_v10_echo_request) -> process_received_v10_echo_request(Xid, Message, State);
        true                                    -> process_received_unknown_message(Xid, Message, State)
    end.

process_received_hello(_Xid, Hello, State) ->
    %% Be tolerant: allow peer to send hello message as non-initial message (ignore it)
    io:format("of_switch: peer unexpectedly sent non-initial hello message Hello=~w~n", [Hello]),
    {noreply, State}.

process_received_v10_echo_request(Xid, EchoRequest, State) ->
    Data = EchoRequest#of_v10_echo_request.data,
    EchoReply = #of_v10_echo_reply{data = Data},
    ConnectionPid = State#of_switch_state.connection_pid,
    ok = of_connection:send(ConnectionPid, Xid, EchoReply),
    %% TODO: liveness checking (not here probably - more general)
    {noreply, State}.

process_received_unknown_message(_Xid, Message, State) ->
    %% TODO: add missing messages
    io:format("of_switch: process_received_unknown_message Message=~w~n", [Message]),
    {noreply, State}.
