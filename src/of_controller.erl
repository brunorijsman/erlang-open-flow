%% @Author Bruno Rijsman <brunorijsman@hotmail.com>
%% @copyright 2012 Bruno Rijsman

-module(of_controller).

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
-include_lib("include/of_log.hrl").

-record(of_controller_state, {
          server_pid
         }).

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
    process_flag(trap_exit, true),
    ok = of_group:create(of_switch),
    {ok, ServerPid} = of_server:start_link([]),
    State = #of_controller_state{server_pid = ServerPid},
    {ok, State}.

handle_call(stop, _From, State) ->
    ?DEBUG("stop"),
    #of_controller_state{server_pid = ServerPid} = State,
    of_server:stop(ServerPid),
    of_group:delete(of_switch),
    State1 = State#of_controller_state{server_pid = undefined},
    {stop, normal, stopped, State1}.

handle_cast({'EXIT', From, Reason}, State) ->
    ?DEBUG("received EXIT from ~w for reason ~w", [From, Reason]),
    {noreply, State};

handle_cast(Cast, State) ->
    ?DEBUG("received cast Cast=~w", [Cast]),
    {noreply, State}.

handle_info(Info, State) ->
    ?DEBUG("received info Info=~w", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?DEBUG("terminate Reason=~w", [Reason]),
    ok.

code_change(OldVersion, State, Extra) ->
    ?DEBUG("code change OldVersion=~w Extra=~w", [OldVersion, Extra]),
    {ok, State}.

%%
%% Internal functions.
%%
