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

-include_lib("../include/of.hrl").
-include_lib("../include/of_v10.hrl").

-record(of_connection_state, {
          socket,
          direction,
          receive_state,
          receive_need_len,
          received_data,
          received_header,
          receiver_pid}).

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
      receiver_pid     = undefined},
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({connect, _Address, _Port}, _From, State) 
  when State#of_connection_state.socket /= undefined ->
    {reply, {error, already_connected}, State};

handle_call({connect, Address, Port}, From, State) ->
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
            {reply, ok, State1};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({accept, _Socket}, _From, State) 
  when State#of_connection_state.socket /= undefined ->
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
    {reply, ok, State1};

handle_call(close, _From, State) 
  when State#of_connection_state.socket == undefined ->
    {reply, ok, State};

handle_call(close, _From, State) ->
    #of_connection_state{socket = Socket} = State,
    ok = gen_tcp:close(Socket),
    State1 = State#of_connection_state{
               socket           = undefined,
               direction        = undefined,
               receive_state    = undefined,
               receive_need_len = undefined,
               received_data    = undefined},
    {reply, ok, State1};

handle_call({send, Xid, MessageRec}, _From, State) ->
%% TODO    io:format("of_connection: send xid=~w message=~w~n", [Xid, MessageRec]),
    MessageBin = of_v10_encoder:encode(MessageRec, Xid),
    Socket = State#of_connection_state.socket,
    ok = gen_tcp:send(Socket, MessageBin),    %% TODO: handle send failure. Async send?
    {reply, ok, State}.
    
handle_cast(_Cast, State) ->
    %% TODO: Is ignoring unexpected casts (and infos) the right thing to do?
    {noreply, State}.

handle_info({tcp, Socket, Data}, State)
  when State#of_connection_state.socket == Socket ->
    {noreply, receive_data(Data, State)};

handle_info({tcp, _Socket, _Data}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
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
%% TODO    io:format("of_connection: receive Xid=~w Message=~w ReceiverPid=~w~n", [Xid, MessageRec, ReceiverPid]),
    ReceiverPid ! {of_receive_message, Xid, MessageRec},
    State#of_connection_state{receive_state    = header,
                              receive_need_len = ?OF_HEADER_LEN,
                              received_header  = undefined}.

    
%%
%% Unit tests. 
%%

%% TODO: ifdef TEST for echo server code

echo_server_start() ->
    Options = [binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    {ok, ListenSocket} = gen_tcp:listen(0, Options),
    {ok, Port} = inet:port(ListenSocket),
    spawn(fun() -> echo_server_accept(ListenSocket) end),
    {ok, Port}.

echo_server_accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    echo_server_loop(Socket).
    
echo_server_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            ok = gen_tcp:send(Socket, Data),
            echo_server_loop(Socket);
        {error, closed} ->
            ok
    end.
                
start_and_stop_test() ->
    {ok, Pid} = start_link(),
    ?assert(is_process_alive(Pid)),
    stopped = stop(Pid),
    ?assert(not is_process_alive(Pid)).
 
connect_and_close_test() ->
    {ok, Port} = echo_server_start(),
    {ok, Pid} = start_link(),
    ok = connect(Pid, "localhost", Port),
    ok = close(Pid),
    stopped = stop(Pid).

connect_already_connected_test() ->
    {ok, Port} = echo_server_start(),
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
