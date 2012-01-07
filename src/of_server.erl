%% @Author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

-module(of_server).

-behavior(gen_server).

-export([start_link/1,
         stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(of_server_state, {
          listen_port,
          listen_socket,
          acceptor_pid,
          handle_connection
         }).

-define(DEFAULT_LISTEN_PORT, 6636).
-define(TEST_LISTEN_PORT, 7000).

%%
%% Exported functions.
%%

start_link(Args) ->
    try
        State = initial_state(Args),
        gen_server:start_link(?MODULE, State, [])
    catch
        error:Reason ->
            {error, Reason}
    end.

stop(Pid) ->
    gen_server:call(Pid, stop).

%%                 
%% gen_server callbacks.
%%

init(State) ->
    ServerPid = self(),
    ListenSocket = State#of_server_state.listen_socket,
    AcceptorPid = spawn_link(fun() -> accept_loop(ServerPid, ListenSocket) end),
    State1 = State#of_server_state{acceptor_pid = AcceptorPid},
    {ok, State1}.

handle_call(stop, _From, State) ->
    case State#of_server_state.listen_socket of
        undefined ->
            nop;
        Socket ->
            gen_tcp:close(Socket)
    end,
    {stop, normal, stopped, State}.

handle_cast({accepted, Socket}, State) ->
    io:format("New connection!~n"),
    HandleConnection = State#of_server_state.handle_connection,
    HandleConnection(Socket),
    {noreply, State};

handle_cast(_Cast, State) ->
    %% TODO: Is ignoring unexpected casts the right thing to do?
    {noreply, State}.

handle_info(_Info, State) ->
    %% TODO: Is ignoring unexpected infos the right thing to do?
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions.
%%

initial_state(Args) ->
    State1 = #of_server_state{listen_port       = ?DEFAULT_LISTEN_PORT,
                              listen_socket     = undefined,
                              acceptor_pid      = undefined,
                              handle_connection = undefind},
    State2 = parse_args(Args, State1),
    TcpOptions = [binary, 
                  {packet, raw}, 
                  {active, false}, 
                  {reuseaddr, true},
                  {keepalive, true},
                  {backlog, 30}],
    ListenPort = State2#of_server_state.listen_port,
    case gen_tcp:listen(ListenPort, TcpOptions) of
        {ok, ListenSocket} ->
            State2#of_server_state{listen_socket = ListenSocket};
        {error, Reason} ->
            erlang:error(Reason)
    end.
    
parse_args(Args, State) ->
    lists:foldl(fun parse_arg/2, State, Args).

parse_arg({handle_connection, HandleConnection}, State) ->
    State#of_server_state{handle_connection = HandleConnection};

parse_arg({listen_port, ListenPort}, State) ->
    State#of_server_state{listen_port = ListenPort};

parse_arg(Arg, _State) ->
    erlang:error({unrecognized_attribute, Arg}).

accept_loop(ServerPid, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("cast ServerPid=~w~n", [ServerPid]),   %% @@@
            gen_server:cast(ServerPid, {accepted, Socket}),
            accept_loop(ServerPid, ListenSocket);
        {error, Reason} ->
            exit({accept_error, Reason})
    end.

%%
%% Unit tests.
%%

test_handle_connection(Socket, TestPid) ->
    TestPid ! {connection, Socket}.

start_link_and_stop_test() ->
    StartLinkResult = start_link([]),
    ?assertMatch({ok, _}, StartLinkResult),
    {ok, Pid} = StartLinkResult,
    ?assertEqual(stopped, stop(Pid)).

start_link_arg_handle_connection_test() ->
    TestPid = self(),
    HandleConnection = fun(Socket) -> test_handle_connection(Socket, TestPid) end,
    StartLinkResult = start_link([{handle_connection, HandleConnection}]),
    ?assertMatch({ok, _}, StartLinkResult),
    {ok, Pid} = StartLinkResult,
    ?assertMatch(stopped, stop(Pid)).

start_link_arg_listen_port_test() ->
    StartLinkResult = start_link([{listen_port, ?TEST_LISTEN_PORT}]),
    ?assertMatch({ok, _}, StartLinkResult),
    {ok, Pid} = StartLinkResult,
    ?assertMatch(stopped, stop(Pid)).

start_link_arg_all_test() ->
    TestPid = self(),
    HandleConnection = fun(Socket) -> test_handle_connection(Socket, TestPid) end,
    StartLinkResult = start_link([{handle_connection, HandleConnection}, {listen_port, ?TEST_LISTEN_PORT}]),
    ?assertMatch({ok, _}, StartLinkResult),
    {ok, Pid} = StartLinkResult,
    ?assertMatch(stopped, stop(Pid)).

start_link_arg_bad_test() ->
    ?assertEqual({error,{unrecognized_attribute,bad}}, start_link([bad])).

connect_default_port_test() ->
    TestPid = self(),
    HandleConnection = fun(Socket) -> test_handle_connection(Socket, TestPid) end,
    StartLinkResult = start_link([{handle_connection, HandleConnection}]),
    ?assertMatch({ok, _}, StartLinkResult),
    {ok, Pid} = StartLinkResult,
    LocalHost = {127, 0, 0, 1},
    ConnectResult = gen_tcp:connect(LocalHost, ?DEFAULT_LISTEN_PORT, []),
    ?assertMatch({ok, _}, ConnectResult),
    {ok, ClientSocket} = ConnectResult,
    receive
        {connection, _ServerSocket} -> ok
    end,
    ?assertMatch(ok, gen_tcp:close(ClientSocket)),
    ?assertMatch(stopped, stop(Pid)).

connect_other_port_test() ->
    TestPid = self(),
    HandleConnection = fun(Socket) -> test_handle_connection(Socket, TestPid) end,
    StartLinkResult = start_link([{handle_connection, HandleConnection}, {listen_port, ?TEST_LISTEN_PORT}]),
    ?assertMatch({ok, _}, StartLinkResult),
    {ok, Pid} = StartLinkResult,
    LocalHost = {127, 0, 0, 1},
    ConnectResult = gen_tcp:connect(LocalHost, ?TEST_LISTEN_PORT, []),
    ?assertMatch({ok, _}, ConnectResult),
    {ok, ClientSocket} = ConnectResult,
    receive
        {connection, _ServerSocket} -> ok
    end,
    ?assertMatch(ok, gen_tcp:close(ClientSocket)),
    ?assertMatch(stopped, stop(Pid)).
