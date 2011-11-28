%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2011 Bruno Rijsman

-module(of_connection).

-behavior(gen_server).

-export([start_link/0,
         stop/1,
         connect/3,
         close/1,
         send/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("../include/of.hrl").

-record(of_connection_state, {
          state,
          receive_state,
          receive_need_len,
          received_data,
          receiver_pid,
          socket}).

%% TODO: State (receive data) needs to be updated when connect or close

%%
%% Exported functions.
%%

%% TODO: add specs everywhere; avoid generic term()

-spec start_link() -> term().   
start_link() ->
    gen_server:start_link(?MODULE, [self()], []).

-spec stop(pid()) -> term().
stop(Pid) ->
    gen_server:call(Pid, stop).

connect(Pid, Address, Port) ->
    gen_server:call(Pid, {connect, Address, Port}).

close(Pid) ->
    gen_server:call(Pid, close).

send(Pid, Message) ->
    gen_server:call(Pid, {send, Message}).

%% TODO: add bind(Pid, ...)
%% TODO: add send_message(Pid, Message)

%%                 
%% gen_server callbacks.
%%

%% TODO: Add -spec for all of the following

init([ReceiverPid]) ->
    State = #of_connection_state{
      state            = closed,
      receive_state    = undefined,
      receive_need_len = undefined,
      received_data    = undefined,
      receiver_pid     = ReceiverPid,
      socket           = undefined},
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({connect, _Address, _Port}, _From, State) 
  when State#of_connection_state.state == connected ->
    {reply, {error, already_connected}, State};

handle_call({connect, Address, Port}, _From, State) ->
    Options = [binary, {active, once}],
    case gen_tcp:connect(Address, Port, Options) of
        {ok, Socket} ->
            NewState = State#of_connection_state{
                         state            = connected,
                         receive_state    = header,
                         receive_need_len = ?OF_HEADER_LEN,
                         received_data    = <<>>,
                         socket           = Socket},
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(close, _From, State) 
  when State#of_connection_state.state == closed ->
    {reply, ok, State};

handle_call(close, _From, State) ->
    #of_connection_state{socket = Socket} = State,
    ok = gen_tcp:close(Socket),
    NewState = State#of_connection_state{
                 state            = closed,
                 receive_state    = undefined,
                 receive_need_len = undefined,
                 received_data    = undefined,
                 socket           = undefined},
    {reply, ok, NewState};

handle_call({send, _Message}, _From, State) ->
    %% TODO: encode message
    %% TODO: call gen_tcp to send message
    %% TODO: make sure TCP connection is connected
    {reply, ok, State}.
    
handle_cast(_Cast, State) ->
    %% TODO: Is ignoring unexpected casts (and infos) the right thing to do?
    {noreply, State}.

handle_info({tcp, Socket, Data}, State)
  when State#of_connection_state.state == connected andalso 
       State#of_connection_state.socket == Socket ->
    {noreply, receive_data(State, Data)};

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

receive_data(State, Data) ->
    NewState = append_data(State, Data),
    io:format("NewState = ~w~n", [NewState]),
    io:format("~w~n", [byte_size(NewState#of_connection_state.received_data)]),
    consume_data(NewState).

append_data(State, Data) ->
    OldReceivedData = State#of_connection_state.received_data,
    NewReceivedData = <<OldReceivedData/binary, Data/binary>>,
    State#of_connection_state{received_data = NewReceivedData}.

consume_data(#of_connection_state{receive_state    = ReceiveState,
                                  received_data    = ReceivedData, 
                                  receive_need_len = ReceiveNeedLen} = State)
  when byte_size(ReceivedData) >= ReceiveNeedLen ->
    io:format("consume_data 1 State = ~w~n", [State]),
    case ReceiveState of
        header ->
            consume_header(State);
        body ->
            consume_body(State)
    end;
%% TODO: loop => consume more data

consume_data(State) ->
    io:format("consume_data 2 State = ~w~n", [State]),
    State.

%% TODO: Catch decode exceptions
%% TODO: Be consistent in naming (Data vs Bin)
%% TODO: Update receive state and needed_len

consume_header(#of_connection_state{received_data = ReceivedData} = State) ->
    io:format("consume_header State = ~w~n", [State]),
    <<HeaderData: ?OF_HEADER_LEN/binary, RestData/binary>> = ReceivedData,
    HeaderRec = of_decoder:decode_header(HeaderData),
    io:format("HeaderRec = ~w~n", [HeaderRec]),
    State#of_connection_state{received_data = RestData}.


%% TODO: Implement this
%% TODO: Send decoded message to receiver pid

consume_body(State) ->
    State.
    
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
