%% @author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

-module(of_server).

-behavior(gen_server).

-export([start_link/0,
         stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(of_server_state, {
          listen_socket,
          acceptor_pid
         }).

-define(DEFAULT_TCP_LISTEN_PORT, 6636).

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
    io:format("init~n", []),
    Port = ?DEFAULT_TCP_LISTEN_PORT,      %% TODO: make this configurable using an option
    TcpOptions = [binary, 
                  {packet, raw}, 
                  {active, false}, 
                  {reuseaddr, true},
                  {keepalive, true},
                  {backlog, 30}],
    case gen_tcp:listen(Port, TcpOptions) of
        {ok, ListenSocket} ->
            AcceptorPid = spawn_link(fun() -> accept_loop(self(), ListenSocket) end),
            State = #of_server_state{listen_socket = ListenSocket,
                                     acceptor_pid  = AcceptorPid},
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast({accepted, _Socket}, State) ->
    %% TODO: Do something with the newly accepted connection => start an of_connection
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

accept_loop(ServerPid, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            gen_server:cast(ServerPid, {accepted, Socket}),
            accept_loop(ServerPid, ListenSocket);
        {error, Reason} ->
            exit({accept_error, Reason})
    end.

%%
%% Unit tests.
%%

start_link_and_stop_test() ->
    StartLinkResult = start_link(),
    ?assertMatch({ok, _}, StartLinkResult),
    {ok, Pid} = StartLinkResult,
    ?assertMatch(stopped, stop(Pid)).

