-module(time_server).
-include("../../project_def.hrl").
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).




start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).




init([]) ->
    {ok, []}.



handle_call(get_time, _From, State) ->
    Current_time_stamp = erlang:monotonic_time(millisecond),
    {reply, {ok,Current_time_stamp}, State};

handle_call(_Request, _From, State) ->
    io:format("Unknown message: ~p~n", [_Request]),
    {reply, ignored, State}.


handle_cast(_Msg, State) ->
    io:format("Unknown message: ~p~n", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
