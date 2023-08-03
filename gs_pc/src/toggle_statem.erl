-module(toggle_statem).
-behaviour(gen_statem).

% API functions
-export([start_link/0, stop/0]).

% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0, code_change/4]).
-export([state_a/3, state_b/3]).

-define(TIMEOUT, 1000).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?MODULE).

% Initialization of the state machine
init([]) ->
    io:format("Init~n"),
    {ok, state_a, [], [{state_timeout, ?TIMEOUT, time_tick}]}.

terminate(_Reason, _State, _Data) ->
    ok.

callback_mode() ->
    state_functions.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


state_a(state_timeout, _From, _Data) ->
    io:format("timeout in state_a~n"),
    {next_state, state_b, _Data,[{state_timeout, ?TIMEOUT, time_tick}]};

state_a(_Event, _From, _Data) ->
    io:format("unknown event ~p in state_a~n", [_Event]),
    {keep_state, _Data,[{state_timeout, ?TIMEOUT, time_tick}]}.


state_b(state_timeout, _From, _Data) ->
    io:format("timeout in state_b~n"),
    {next_state, state_a, _Data,[{state_timeout, ?TIMEOUT, time_tick}]};

state_b(_Event, _From, _Data) ->
    io:format("unknown event ~p in state_b~n", [_Event]),
    {keep_state, _Data, [{state_timeout, ?TIMEOUT, time_tick}]}.
