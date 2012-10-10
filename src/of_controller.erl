%%%=====================================================================================================================
%%% Copyright (c) 2012, Bruno Rijsman
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

%%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%%% @copyright 2012 Bruno Rijsman

%% TODO: Add support for IPv6
%% TODO: Add support for SSL

-module(of_controller).

-behavior(gen_server).

-export([start_link/1,
         stop/1,
         subscribe/2,
         unsubscribe/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/of_log.hrl").

-record(of_controller_state, {
          listen_port,
          listen_socket,
          acceptor_pid,
          handle_connection,    %% TODO: Do this with parse transform stubbing instead
          switches
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

stop(ControllerPid) ->
    gen_server:call(ControllerPid, stop).

subscribe(ControllerPid, Topic) ->
    gen_server:call(ControllerPid, {subscribe, Topic}).

unsubscribe(ControllerPid, Topic) ->
    gen_server:call(ControllerPid, {unsubscribe, Topic}).

%%                 
%% gen_server callbacks.
%%

init(State) ->
    process_flag(trap_exit, true),
    #of_controller_state{listen_socket = ListenSocket} = State,
    ServerPid = self(),
    AcceptorPid = spawn_link(fun() -> accept_loop(ServerPid, ListenSocket) end),
    State1 = State#of_controller_state{acceptor_pid = AcceptorPid},
    ok = of_events:create_topic(switch),
    {ok, State1}.

handle_call(stop, _From, State) ->
    ?DEBUG("stop"),
    of_events:delete_topic(switch),
    #of_controller_state{listen_socket = ListenSocket, switches = Switches} = State,
    case ListenSocket of
        undefined -> 
            nop;
        Socket ->
            gen_tcp:close(Socket)
    end,
    lists:foreach(fun(Pid) -> of_switch:stop(Pid) end, Switches),
    {stop, normal, stopped, State};

handle_call({subscribe, Topic = switch}, From, State) ->
    {SubscriberPid, _} = From,
    ?DEBUG_FMT("subscribe Topic=~w Subscriber=~w", [Topic, SubscriberPid]),
    of_events:subscribe_to_topic(switch, SubscriberPid),
    #of_controller_state{switches = Switches} = State,
    lists:foreach(fun(SwitchPid) -> of_events:unicast_event(SubscriberPid, switch, add, SwitchPid) end, Switches),
    {reply, ok, State};

handle_call({subscribe, Topic}, From, State) ->
    ?DEBUG_FMT("subscribe unknown topic Topic=~w Subscriber=~w", [Topic, From]),
    {reply, {error, unknown_topic}, State};

handle_call({unsubscribe, Topic = switch}, From, State) ->
    {SubscriberPid, _} = From,
    ?DEBUG_FMT("unsubscribe Topic=~w Subscriber=~w", [Topic, SubscriberPid]),
    of_events:unsubscribe_from_topic(owitch, SubscriberPid),
    {reply, ok, State};

handle_call({unsubscribe, Topic}, From, State) ->
    ?DEBUG_FMT("unsubscribe unknown topic Topic=~w Subscriber=~w", [Topic, From]),
    {reply, {error, unknown_topic}, State}.

%% TODO: Also handle removing switches and reporting a corresponding event
%% TODO: Also do some of this processing for outgoing connections
handle_cast({accepted, Socket}, State) ->
    #of_controller_state{handle_connection = HandleConnection, switches = Switches} = State,
    {ok, SwitchPid} = HandleConnection(Socket),
    %% TODO: Handle this differently and get rid of the undefined case
    State1 = case SwitchPid of
                 undefined ->
                     State;
                 _ ->
                     of_events:multicast_event(switch, add, SwitchPid),
                     Switches1 = [SwitchPid | Switches],
                     State#of_controller_state{switches = Switches1}
                 end,
    {noreply, State1};

handle_cast({'EXIT', From, Reason}, State) ->
    ?DEBUG_FMT("received EXIT from ~w for reason ~w", [From, Reason]),
    {noreply, State};

handle_cast(Cast, State) ->
    ?DEBUG_FMT("received cast ~w", [Cast]),
    {noreply, State}.

handle_info(Info, State) ->
    ?DEBUG_FMT("received info ~w", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?DEBUG_FMT("terminate Reason=~w", [Reason]),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions.
%%

initial_state(Args) ->
    State1 = #of_controller_state{listen_port       = ?DEFAULT_LISTEN_PORT,
                              listen_socket     = undefined,
                              acceptor_pid      = undefined,
                              handle_connection = fun handle_connection/1,
                              switches          = []},
    State2 = parse_args(Args, State1),
    TcpOptions = [binary, 
                  {packet, raw}, 
                  {active, false}, 
                  {reuseaddr, true},
                  {keepalive, true},
                  {backlog, 30}],
    ListenPort = State2#of_controller_state.listen_port,
    case gen_tcp:listen(ListenPort, TcpOptions) of
        {ok, ListenSocket} ->
            ?DEBUG_FMT("listening on port ~w", [ListenPort]),
            State2#of_controller_state{listen_socket = ListenSocket};
        {error, Reason} ->
            erlang:error(Reason)
    end.

parse_args(Args, State) ->
    lists:foldl(fun parse_arg/2, State, Args).

parse_arg({handle_connection, HandleConnection}, State) ->
    State#of_controller_state{handle_connection = HandleConnection};

parse_arg({listen_port, ListenPort}, State) ->
    State#of_controller_state{listen_port = ListenPort};

parse_arg(Arg, _State) ->
    erlang:error({unrecognized_attribute, Arg}).

handle_connection(Socket) ->
    %% TODO: Don't crash if switch doesn't start -- log something instead
    %% TODO: switch might already exist!
    %% TODO: handle of_close from switch
    {ok, SwitchPid} = of_switch:start_link(),
    ok = of_switch:accept(SwitchPid, Socket),
    {ok, SwitchPid}.
    
accept_loop(ServerPid, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            ok = gen_tcp:controlling_process(Socket, ServerPid),
            gen_server:cast(ServerPid, {accepted, Socket}),
            accept_loop(ServerPid, ListenSocket);
        {error, Reason} ->
            exit({accept_error, Reason})
    end.

%%
%% Unit tests.
%%

test_handle_connection(Socket, TestPid) ->
    TestPid ! {connection, Socket},
    {ok, undefined}.

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
