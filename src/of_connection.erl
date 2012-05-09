%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

-module(of_connection).

-behavior(gen_server).

-export([start_link/0,
         stop/1,
         connect/3,
         accept/2,
         close/1,
         send/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("include/of.hrl").
-include_lib("include/of_v10.hrl").
-include_lib("include/of_log.hrl").

-record(of_connection_state, {
          socket,
          direction,
          receive_state,
          receive_need_len,
          received_data,
          received_header,
          receiver_pid,
          log_keys}).

%% TODO: State (receive data) needs to be updated when connect or close

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

close(Pid) ->
    gen_server:call(Pid, close).

send(Pid, Xid, Message) ->
    gen_server:call(Pid, {send, Xid, Message}).

%%                 
%% gen_server callbacks.
%%

init([]) ->
    State = #of_connection_state{
      socket           = undefined,
      direction        = undefined,
      receive_state    = undefined,
      receive_need_len = undefined,
      received_data    = undefined,
      received_header  = undefined,
      receiver_pid     = undefined,
      log_keys         = []},
    {ok, State}.

-define(DEBUG_STATE(State, Message), ?DEBUG_KEY(State#of_connection_state.log_keys, Message)).
-define(INFO_STATE(State, Message), ?INFO_KEY(State#of_connection_state.log_keys, Message)).
-define(NOTICE_STATE(State, Message), ?NOTICE_KEY(State#of_connection_state.log_keys, Message)).
-define(NOTICE_STATE_FMT(State, Format, Args), ?NOTICE_KEY_FMT(State#of_connection_state.log_keys, Format, Args)).
-define(ERROR_STATE_FMT(State, Format, Args), ?ERROR_KEY_FMT(State#of_connection_state.log_keys, Format, Args)).

handle_call(stop, _From, State) ->
    ?DEBUG_STATE(State, "stop"),
    {stop, normal, stopped, State};

%% TODO: Add address:port->address:port as key to all debug messages

handle_call({connect, _Address, _Port}, _From, State) 
  when State#of_connection_state.socket /= undefined ->
    ?DEBUG_STATE(State, "initiate outgoing connection when already connected"),
    {reply, {error, already_connected}, State};

handle_call({connect, Address, Port}, From, State) ->
    ?DEBUG_STATE(State, "initiate outgoing connection"),
    Options = [binary, {active, once}],
    case gen_tcp:connect(Address, Port, Options) of
        {ok, Socket} ->
            {FromPid, _FromTag} = From,
            State1 = State#of_connection_state{
                       socket           = Socket,
                       direction        = outgoing,
                       receive_state    = header,
                       receive_need_len = ?OF_HEADER_LEN,
                       received_data    = <<>>,
                       receiver_pid     = FromPid},
            State2 = set_log_keys(State1),
            {reply, ok, State2};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({accept, _Socket}, _From, State) 
  when State#of_connection_state.socket /= undefined ->
    ?DEBUG_STATE(State, "accept incoming connection when already connected"),
    {reply, {error, already_connected}, State};

handle_call({accept, Socket}, From, State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {FromPid, _FromTag} = From,
    State1 = State#of_connection_state{
               socket           = Socket,
               direction        = incoming,
               receive_state    = header,
               receive_need_len = ?OF_HEADER_LEN,
               received_data    = <<>>,
               receiver_pid     = FromPid},
    State2 = set_log_keys(State1),
    ?DEBUG_STATE(State2, "accept incoming connection"),
    {reply, ok, State2};

handle_call(close, _From, State) 
  when State#of_connection_state.socket == undefined ->
    ?DEBUG_STATE(State, "close connection when not connected"),
    {reply, ok, State};

handle_call(close, _From, State) ->
    ?DEBUG_STATE(State, "close connection"),
    #of_connection_state{socket = Socket} = State,
    ok = gen_tcp:close(Socket),
    State1 = disconnected_state(State),
    {reply, ok, State1};

handle_call({send, Xid, MessageRec}, _From, State) ->
    %% TODO: Cannot assume version 1.0; we need some way to establish the version of the protocol
    MessageBin = of_v10_encoder:encode(MessageRec, Xid),
    Socket = State#of_connection_state.socket,
    case gen_tcp:send(Socket, MessageBin) of
        ok -> 
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.
    
handle_cast(Cast, State) ->
    ?ERROR_STATE_FMT(State, "unknown cast Cast=~w", [Cast]),
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    case State#of_connection_state.socket of
        Socket -> 
            {noreply, receive_data(Data, State)};
        undefined ->
            ?NOTICE_STATE(State, "receive data on connection when not connected"),
            {noreply, State};
        _OtherSocket ->
            ?NOTICE_STATE(State, "receive data on connection on unexpected socket"),
            {noreply, State}
    end;

handle_info({tcp_closed, Socket}, State) ->
    case State#of_connection_state.socket of
        Socket -> 
            ?INFO_STATE(State, "connection closed by remote"),
            State#of_connection_state.receiver_pid ! of_closed,
            State1 = disconnected_state(State),
            {noreply, State1};
        undefined ->
            ?NOTICE_STATE(State, "connection closed by remote when not connected"),
            {noreply, State};
        _OtherSocket ->
            ?NOTICE_STATE(State, "connection closed by remote on unexpected socket"),
            {noreply, State}
    end;

handle_info({tcp_error, Socket, Reason}, State) ->
    case State#of_connection_state.socket of
        Socket -> 
            ?NOTICE_STATE_FMT(State, "connection error Reason=~p", [Reason]),
            State#of_connection_state.receiver_pid ! {of_error, Reason},
            State1 = disconnected_state(State),
            {noreply, State1};
        undefined ->
            ?NOTICE_STATE_FMT(State, "connection error by remote when not connected Reason=~p", [Reason]),
            {noreply, State};
        _OtherSocket ->
            ?NOTICE_STATE_FMT(State, "connection error by remote on unexpected socket Reason=~p", [Reason]),
            {noreply, State}
    end;

handle_info(Info, State) ->
    ?ERROR_STATE_FMT(State, "unknown info Info=~w", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    ?NOTICE_STATE_FMT(State, "terminate Reason=~w", [Reason]),
    ok.

code_change(OldVersion, State, _Extra) ->
    ?NOTICE_STATE_FMT(State, "code_change OldVersion=~w", [OldVersion]),
    {ok, State}.

%%
%% Internal functions.
%%

receive_data(Data, State) ->
    Socket = State#of_connection_state.socket,
    ok = inet:setopts(Socket, [{active, once}]),
    NewState = append_data(Data, State),
    consume_data(NewState).

append_data(Data, State) ->
    OldReceivedData = State#of_connection_state.received_data,
    NewReceivedData = <<OldReceivedData/binary, Data/binary>>,
    State#of_connection_state{received_data = NewReceivedData}.

consume_data(#of_connection_state{receive_state    = ReceiveState,
                                  received_data    = ReceivedData, 
                                  receive_need_len = ReceiveNeedLen} = State)
  when byte_size(ReceivedData) >= ReceiveNeedLen ->
    <<ConsumeData: ReceiveNeedLen/binary, RestData/binary>> = ReceivedData,
    NewState1 = State#of_connection_state{received_data = RestData, receive_need_len = 0},
    NewState2 = case ReceiveState of
                    header ->
                        consume_header(ConsumeData, NewState1);
                    body ->
                        consume_body(ConsumeData, NewState1)
                end,
    consume_data(NewState2);

consume_data(State) ->
    State.

%% TODO: Catch decode exceptions
%% TODO: Be consistent in naming (Data vs Bin)
%% TODO: Update receive state and needed_len

consume_header(HeaderBin, State) ->
    HeaderRec = of_v10_decoder:decode_header(HeaderBin),
    #of_v10_header{length = Length} = HeaderRec,
    BodyLength = Length - ?OF_V10_HEADER_LEN,    %% TODO: validate >=
    State#of_connection_state{receive_state    = body,
                              receive_need_len = BodyLength,
                              received_header  = HeaderRec}.

consume_body(BodyBin, State) ->
    #of_connection_state{received_header = HeaderRec,
                         receiver_pid    = ReceiverPid} = State,
    #of_v10_header{type = MessageType, xid = Xid} = HeaderRec,
    MessageRec = of_v10_decoder:decode_body(MessageType, BodyBin),
    ReceiverPid ! {of_receive_message, Xid, MessageRec},
    State#of_connection_state{receive_state    = header,
                              receive_need_len = ?OF_HEADER_LEN,
                              received_header  = undefined}.

set_log_keys(State) ->
    {ok, {Address, Port}} = inet:peername(State#of_connection_state.socket),
    RemoteStr = inet_parse:ntoa(Address) ++ ":" ++ integer_to_list(Port),
    LogKeys = [{remote, RemoteStr}],
    State#of_connection_state{log_keys = LogKeys}.

disconnected_state(State) ->
    State#of_connection_state{
      socket           = undefined,
      direction        = undefined,
      receive_state    = undefined,
      receive_need_len = undefined,
      received_data    = undefined,
      log_keys         = []}.

%%
%% Unit tests. 
%%

-ifdef(TEST).

tcp_echo_server_start() ->
    Options = [binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    {ok, ListenSocket} = gen_tcp:listen(0, Options),
    {ok, Port} = inet:port(ListenSocket),
    spawn(fun() -> tcp_echo_server_accept(ListenSocket) end),
    {ok, Port}.

tcp_echo_server_accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    tcp_echo_server_loop(Socket).
    
tcp_echo_server_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            ok = gen_tcp:send(Socket, Data),
            tcp_echo_server_loop(Socket);
        {error, closed} ->
            ok
    end.

of_echo_server_start() ->
    Options = [binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    {ok, ListenSocket} = gen_tcp:listen(0, Options),
    {ok, Port} = inet:port(ListenSocket),
    spawn(fun() -> of_echo_server_accept(ListenSocket) end),
    {ok, Port}.

of_echo_server_accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    {ok, ConnectionPid} = start_link(),
    accept(ConnectionPid, Socket),
    of_echo_server_loop(ConnectionPid).

of_echo_server_loop(ConnectionPid) ->
    receive
        {of_receive_message, Xid, Message} ->
            send(ConnectionPid, Xid, Message),
            of_echo_server_loop(ConnectionPid);
        of_closed ->
            stopped = stop(ConnectionPid),
            ok
    end.
                
start_and_stop_test() ->
    {ok, Pid} = start_link(),
    ?assert(is_process_alive(Pid)),
    stopped = stop(Pid),
    ?assert(not is_process_alive(Pid)).
 
connect_and_close_test() ->
    {ok, Port} = tcp_echo_server_start(),
    {ok, Pid} = start_link(),
    ok = connect(Pid, "localhost", Port),
    ok = close(Pid),
    stopped = stop(Pid).

connect_already_connected_test() ->
    {ok, Port} = tcp_echo_server_start(),
    {ok, Pid} = start_link(),
    ok = connect(Pid, "localhost", Port),
    {error, already_connected} = connect(Pid, "localhost", Port),
    ok = close(Pid),
    stopped = stop(Pid).

connect_no_server_test() ->
    {ok, Pid} = start_link(),
    UnusedPort = 106,
    {error, econnrefused} = connect(Pid, "localhost", UnusedPort),
    stopped = stop(Pid).
    
close_not_connected_test() ->
    {ok, Pid} = start_link(),
    ok = close(Pid),
    stopped = stop(Pid).

tcp_echo_test() ->
    {ok, Port} = tcp_echo_server_start(),
    {ok, Pid} = start_link(),
    ok = connect(Pid, "localhost", Port),
    SendXid = 1,
    SendMessage = #of_vxx_hello{version = 1},
    ok = send(Pid, SendXid, SendMessage),
    receive
        {of_receive_message, ReceiveXid, ReceiveMessage} ->
            ?assertEqual(SendXid, ReceiveXid),
            ?assertEqual(SendMessage, ReceiveMessage),
            nop
    end,
    ok = close(Pid),
    stopped = stop(Pid).

of_echo_test() ->
    {ok, Port} = of_echo_server_start(),
    {ok, Pid} = start_link(),
    ok = connect(Pid, "localhost", Port),
    SendXid = 1,
    SendMessage = #of_vxx_hello{version = 1},
    ok = send(Pid, SendXid, SendMessage),
    receive
        {of_receive_message, ReceiveXid, ReceiveMessage} ->
            ?assertEqual(SendXid, ReceiveXid),
            ?assertEqual(SendMessage, ReceiveMessage),
            nop
    end,
    ok = close(Pid),
    stopped = stop(Pid).

accept_already_connected_test() ->
    {ok, Port} = of_echo_server_start(),
    {ok, Pid} = start_link(),
    ok = connect(Pid, "localhost", Port),
    {ok, Socket} = gen_tcp:connect("localhost", Port, []),
    Result = accept(Pid, Socket),
    ?assertEqual(Result, {error, already_connected}),
    ok = gen_tcp:close(Socket),
    ok = close(Pid),
    stopped = stop(Pid).

stray_info_test() ->
    {ok, Port} = of_echo_server_start(),
    {ok, Pid} = start_link(),
    ok = connect(Pid, "localhost", Port),
    Pid ! {tcp, fake_stale_socket, <<>>}, 
    Pid ! {tcp_closed, fake_stale_socket},
    Pid ! {tcp_error, fake_stale_socket, fake_reason}, 
    ok = close(Pid),
    Pid ! {tcp, fake_stale_socket, <<>>},
    Pid ! {tcp_closed, fake_stale_socket},
    Pid ! {tcp_error, fake_stale_socket, fake_reason}, 
    stopped = stop(Pid).
    
unknown_cast_test() ->
    {ok, Pid} = start_link(),
    gen_server:cast(Pid, unknown_cast),
    ?assert(is_process_alive(Pid)),
    stopped = stop(Pid).
    
unknown_info_test() ->
    {ok, Pid} = start_link(),
    Pid ! unknown_info,
    ?assert(is_process_alive(Pid)),
    stopped = stop(Pid).

-endif.
