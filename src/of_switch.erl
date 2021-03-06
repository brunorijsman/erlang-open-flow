%%%=====================================================================================================================
%%% Copyright (c) 2012-2013, Bruno Rijsman
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
%%% @copyright 2012-2013 Bruno Rijsman

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

-define(STATE_RECORD, of_switch_state).
-include_lib("../include/of_log.hrl").

%% @@@ TODO: Send of_closed (of_down?) when connection goes down. Owner can decide whether to stop.

-record(of_switch_state, {
          log_keys,
          owner_pid,
          connection_pid,
          version,
          next_local_xid,
          receive_hello_timer,
          send_echo_request_timer,
          pending_requests
         }).

-record(pending_request, {
          xid,
          timer,
          process_reply_fun
         }).

-define(MAX_UINT32, 4294967295).

%% TODO: Make all of these configurable. 
-define(SEND_ECHO_REQUEST_INTERVAL_MSECS, 30000).
-define(RECEIVE_HELLO_TIMEOUT_MSECS, 1000).
-define(RECEIVE_REPLY_TIMEOUT_MSECS, 1000).

%%
%% Exported functions.
%%

start_link() ->
    gen_server:start_link(?MODULE, [self()], []).

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

init([OwnerPid]) ->
    State = #of_switch_state{
      log_keys                = [],
      owner_pid               = OwnerPid,
      connection_pid          = undefined, 
      version                 = undefined,
      next_local_xid          = 1,
      receive_hello_timer     = undefined,
      send_echo_request_timer = undefined,
      pending_requests        = dict:new()},
    {ok, State}.

handle_call({connect, IpAddress, TcpPort}, _From, State) ->
    ?DEBUG_STATE_FMT(State, "connect IpAddress=~w TcpPort=~w", [IpAddress, TcpPort]),   %% TODO
    State1 = initiate_connection(IpAddress, TcpPort, State),
    {reply, ok, State1};

handle_call({accept, Socket}, _From, State) ->
    State1 = accept_connection(Socket, State),
    {reply, ok, State1};

handle_call(stop, _From, State) ->
    ?DEBUG_STATE(State, "stop"),
    {stop, normal, stopped, State}.

handle_cast(Cast, State) ->
    ?ERROR_STATE_FMT(State, "unknown cast Cast=~w", [Cast]),
    {noreply, State}.

handle_info({of_receive_message, Xid, Message}, State) ->
    process_received_message(Xid, Message, State);

handle_info(of_closed, State) ->
    process_closed(State);

handle_info(timer_expired_receive_hello, State) ->
    process_timer_expired_receive_hello(State);

handle_info(timer_expired_send_echo_request, State) ->
    process_timer_expired_send_echo_request(State);

handle_info({timer_expired_receive_reply, Xid}, State) ->
    process_timer_expired_receive_reply(Xid, State);

handle_info(Info, State) ->
    ?ERROR_STATE_FMT(State, "unknown info Info=~w", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    ?NOTICE_STATE_FMT(State, "terminate Reason=~w", [Reason]),
    of_events:multicast_event(of_switch, remove, self()),
    ok.

code_change(OldVersion, State, _Extra) ->
    ?NOTICE_STATE_FMT(State, "code change OldVersion=~w", [OldVersion]),
    {ok, State}.

%%                 
%% Internal functions.
%%

address_and_port_to_string(IpAddress, TcpPort) ->
    inet_parse:ntoa(IpAddress) ++ ":" ++ integer_to_list(TcpPort).

initiate_connection(IpAddress, TcpPort, State) ->
    ?assert(State#of_switch_state.connection_pid == undefined),
    Remote = address_and_port_to_string(IpAddress, TcpPort),
    {ok, ConnectionPid} = of_connection:start_link(),                %% TODO: handle errors?
    ok = of_connection:connect(ConnectionPid, IpAddress, TcpPort),   %% TODO: handle errors
    State1 = State#of_switch_state{log_keys = [{remote, Remote}], connection_pid = ConnectionPid},
    process_connection_up(State1).

accept_connection(Socket, State) -> 
    {ok, {IpAddress, TcpPort}} = inet:peername(Socket),
    Remote = address_and_port_to_string(IpAddress, TcpPort),
    {ok, ConnectionPid} = of_connection:start_link(),                %% TODO: handle errors
    ok = of_connection:accept(ConnectionPid, Socket),                %% TODO: handle errors
    State1 = State#of_switch_state{log_keys = [{remote, Remote}], connection_pid = ConnectionPid},
    ?DEBUG_STATE(State1, "accept incoming connection"),
    process_connection_up(State1).

close_connection(State) ->
    ConnectionPid = State#of_switch_state.connection_pid,
    ?assert(is_pid(ConnectionPid)),
    of_connection:close(ConnectionPid),
    of_connection:stop(ConnectionPid),
    State#of_switch_state{connection_pid = undefined, version = undefined}.

start_timer(Timer, Time, Message) ->
    case Timer of
        undefined -> nop;
        _         -> {ok, cancel} = timer:cancel(Timer)
    end,
    {ok, NewTimer} = timer:send_after(Time, Message),
    NewTimer.

stop_timer(Timer) ->
    case Timer of
        undefined -> nop;
        _         -> {ok, cancel} = timer:cancel(Timer)
    end,
    ok.
    
start_receive_hello_timer(State) ->
    Timer = start_timer(State#of_switch_state.receive_hello_timer,
                        ?RECEIVE_HELLO_TIMEOUT_MSECS, 
                        timer_expired_receive_hello),
    State#of_switch_state{receive_hello_timer = Timer}.
    
stop_receive_hello_timer(State) ->
    stop_timer(State#of_switch_state.receive_hello_timer),
    State#of_switch_state{receive_hello_timer = undefined}.

start_send_echo_request_timer(State) ->
    Timer = start_timer(State#of_switch_state.send_echo_request_timer,
                        ?SEND_ECHO_REQUEST_INTERVAL_MSECS, 
                        timer_expired_send_echo_request),
    State#of_switch_state{send_echo_request_timer = Timer}.

stop_send_echo_request_timer(State) ->
    stop_timer(State#of_switch_state.send_echo_request_timer),
    State#of_switch_state{send_echo_request_timer = undefined}.

%% TODO: need this?    
%% stop_send_echo_request_timer(State) ->
%%     stop_timer(State#of_switch_state.send_echo_request_timer),
%%     State#of_switch_state{send_echo_request_timer = undefined}.

send_message(Xid, Message, State) ->
    ?DEBUG_STATE_FMT(State, "send message Xid=~w Message=~w", [Xid, Message]),
    #of_switch_state{connection_pid = ConnectionPid} = State,
    case of_connection:send(ConnectionPid, Xid, Message) of
        ok ->
            State;
        {error, _Reason} ->
            %% TODO: handle the error (we are disconnected)
            State
    end.

allocate_xid(State) ->
    Xid = State#of_switch_state.next_local_xid,
    NextXid = case Xid of
                  ?MAX_UINT32 -> 1;
                  _           -> Xid + 1
              end,
    State1 = State#of_switch_state{next_local_xid = NextXid},
    {Xid, State1}.

add_pending_request(Xid, State, ProcessReplyFun) ->
    Message = {timer_expired_receive_reply, Xid},
    {ok, Timer} = timer:send_after(?RECEIVE_REPLY_TIMEOUT_MSECS, Message),
    PendingRequest = #pending_request{xid               = Xid, 
                                      timer             = Timer, 
                                      process_reply_fun = ProcessReplyFun},
    PendingRequests = State#of_switch_state.pending_requests,
    PendingRequests1 = dict:store(Xid, PendingRequest, PendingRequests),
    State#of_switch_state{pending_requests = PendingRequests1}.

extract_pending_request(Xid, State) ->
    PendingRequests = State#of_switch_state.pending_requests,
    case dict:is_key(Xid, PendingRequests) of
        true ->
            PendingRequest = dict:fetch(Xid, PendingRequests),
            PendingRequests1 = dict:erase(Xid, PendingRequests),
            State1 = State#of_switch_state{pending_requests = PendingRequests1},
            {PendingRequest, State1};
        false ->
            {undefined, State}
    end.

send_request(Message, ProcessReplyFun, State) ->
    {Xid, State1} = allocate_xid(State),
    State2 = add_pending_request(Xid, State1, ProcessReplyFun),
    send_message(Xid, Message, State2).

send_hello(State) ->
    Hello = #of_vxx_hello{version = ?OF_VERSION_MAX},
    send_message(_Xid = 0, Hello, State).

send_echo_request(State) ->
    %% TODO: v10 => vxx or immutable or remove
    EchoRequest = #of_v10_echo_request{data = << >>},
    send_request(EchoRequest, fun process_received_v10_echo_reply/3, State).

send_features_request(State) ->
    %% TODO: use right version of message depending on negotiated version
    FeaturesRequest = #of_v10_features_request{},
    send_request(FeaturesRequest, fun process_received_v10_features_reply/3, State).

send_error_incompatible(State) ->
    Error = #of_vxx_error{version = ?OF_VERSION_MIN,
                          type    = ?OF_VXX_ERROR_TYPE_HELLO_FAILED,
                          code    = ?OF_VXX_ERROR_CODE_HELLO_FAILED_INCOMPATIBLE,
                          data    = << ?OF_IMPLEMENTATION_NAME >>},
    send_message(_Xid = 0, Error, State).

process_connection_up(State) ->
    State1 = send_hello(State),
    State2 = send_features_request(State1),
    State3 = start_receive_hello_timer(State2),
    start_send_echo_request_timer(State3).

%% process_connection_down(State) ->
%%     %% TODO
%%     State.

process_timer_expired_receive_hello(State) ->
    ?DEBUG_STATE(State, "no hello received from peer, closing connection"),
    State1 = State#of_switch_state{receive_hello_timer = undefined},
    State2 = close_connection(State1),
    {stop, no_hello_received, State2}.

process_timer_expired_send_echo_request(State) ->
    State1 = State#of_switch_state{send_echo_request_timer = undefined},
    State2 = send_echo_request(State1),
    {noreply, State2}.

process_timer_expired_receive_reply(Xid, State) ->
    %% TODO: look up the Xid and determine what to do
    ?DEBUG_STATE_FMT(State, "no reply received from peer for Xid=~w, closing connection", [Xid]),
    State1 = close_connection(State),
    {stop, no_reply_received, State1}.

process_received_message(Xid, Message, State) ->
    ?DEBUG_STATE_FMT(State, "receive message Xid=~w Message=~w", [Xid, Message]),
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
            ?DEBUG_STATE_FMT(State1, "version negotiation succeeded Version=~w", [Version]),
            State2 = State#of_switch_state{version = Version},
            {noreply, State2};
        true ->
            ?DEBUG_STATE_FMT(State1, "version negotiation failed Version=~w", [Version]),   %% TODO: report both version
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
        is_record(Message, of_vxx_hello)                    -> process_received_hello(Xid, Message, State);
        is_record(Message, of_vxx_error)                    -> process_received_unimplemented_message(Xid, Message, State);   %% TODO
        is_record(Message, of_v10_echo_request)             -> process_received_v10_echo_request(Xid, Message, State);
        is_record(Message, of_v10_echo_reply)               -> process_received_reply(Xid, Message, State);
        is_record(Message, of_v10_vendor)                   -> process_received_unimplemented_message(Xid, Message, State);   %% TODO
        is_record(Message, of_v10_features_request)         -> process_received_unexpected_from_switch_message(Xid, Message, State);
        is_record(Message, of_v10_features_reply)           -> process_received_reply(Xid, Message, State);
        is_record(Message, of_v10_get_config_request)       -> process_received_unexpected_from_switch_message(Xid, Message, State);
        is_record(Message, of_v10_get_config_reply)         -> process_received_reply(Xid, Message, State);
        is_record(Message, of_v10_set_config)               -> process_received_unexpected_from_switch_message(Xid, Message, State);
        is_record(Message, of_v10_packet_in)                -> process_received_unimplemented_message(Xid, Message, State);   %% TODO
        is_record(Message, of_v10_flow_removed)             -> process_received_unimplemented_message(Xid, Message, State);   %% TODO
        is_record(Message, of_v10_port_status)              -> process_received_unimplemented_message(Xid, Message, State);   %% TODO
        is_record(Message, of_v10_packet_out)               -> process_received_unexpected_from_switch_message(Xid, Message, State);
        is_record(Message, of_v10_flow_mod)                 -> process_received_unexpected_from_switch_message(Xid, Message, State);
        is_record(Message, of_v10_port_mod)                 -> process_received_unexpected_from_switch_message(Xid, Message, State);
        is_record(Message, of_v10_stats_request)            -> process_received_unexpected_from_switch_message(Xid, Message, State);
        is_record(Message, of_v10_stats_reply)              -> process_received_reply(Xid, Message, State);
        is_record(Message, of_v10_barrier_request)          -> process_received_unexpected_from_switch_message(Xid, Message, State);
        is_record(Message, of_v10_barrier_reply)            -> process_received_reply(Xid, Message, State);
        is_record(Message, of_v10_queue_get_config_request) -> process_received_unexpected_from_switch_message(Xid, Message, State);
        is_record(Message, of_v10_queue_get_config_reply)   -> process_received_reply(Xid, Message, State);
        true                                                -> process_received_unknown_message(Xid, Message, State)
    end.

process_received_hello(_Xid, Hello, State) ->
    %% Be tolerant: allow peer to send hello message as non-initial message (ignore it)
    ?DEBUG_STATE_FMT(State, "peer unexpectedly sent non-initial hello message Hello=~w", [Hello]),
    {noreply, State}.

process_received_v10_echo_request(Xid, EchoRequest, State) ->
    Data = EchoRequest#of_v10_echo_request.data,
    EchoReply = #of_v10_echo_reply{data = Data},
    State1 = send_message(Xid, EchoReply, State),
    {noreply, State1}.

process_received_reply(Xid, Reply, State) ->
    {PendingRequest, State1} = extract_pending_request(Xid, State),
    case PendingRequest of
        undefined ->
            ?DEBUG_STATE_FMT(State, "received unsolicited or late reply Xid=~w Reply=~w", [Xid, Reply]),
            {noreply, State1};
        _ ->
            #pending_request{timer = Timer, process_reply_fun = ProcessReplyFun} = PendingRequest,
            timer:cancel(Timer),
            ProcessReplyFun(Xid, Reply, State1)
    end.

process_received_v10_echo_reply(_Xid, EchoReply, State) ->
    %% Be tolerant; accept echo reply with data which does not match echo request
    case EchoReply#of_v10_echo_reply.data of
        << >> -> nop;
        Data  -> ?DEBUG_STATE_FMT(State, "echo reply contains unexpected data ~w", [Data])
    end,
    State1 = start_send_echo_request_timer(State),
    {noreply, State1}.

process_received_v10_features_reply(_Xid, _FeaturesReply, State) ->
    %% TODO: implement this 
    {noreply, State}.

process_received_unknown_message(_Xid, Message, State) ->
    %% TODO: add missing messages
    ?DEBUG_STATE_FMT(State, "received unknown message Message=~w", [Message]),
    {noreply, State}.

process_received_unexpected_from_switch_message(_Xid, Message, State) ->
    ?DEBUG_STATE_FMT(State, "received unexpected message from switch Message=~w", [Message]),
    {noreply, State}.

process_received_unimplemented_message(_Xid, Message, State) ->
    %% TODO: this goes away once all messages are implemented
    ?DEBUG_STATE_FMT(State, "received unimplemented message Message=~w", [Message]),
    {noreply, State}.

process_closed(State) ->
    ?INFO_STATE(State, "switch closed connection"),
    State#of_switch_state.owner_pid ! of_closed,   %% TODO: of_connection_closed / of_switch_disconnected ?
    State1 = disconnected_state(State),
    {noreply, State1}.

disconnected_state(State) ->
    State1 = stop_receive_hello_timer(State),
    State2 = stop_send_echo_request_timer(State1),
    State3 = State2#of_switch_state{
               log_keys         = [],     %% TODO: set remote in start_link instead of connect (create connection there as well)
               version          = undefined,
               pending_requests = dict:new()},
    State3.
