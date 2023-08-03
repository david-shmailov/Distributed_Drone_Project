-module(gs_server).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).
-define(RETRY_DELAY, 1000).

% record for drone location and speed update:
-record(drone, {id, location, theta, speed}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    {ok, #state{}}.


handle_call({drone_update, Drone}, _From, State) when is_record(Drone,drone) ->
    io:format("Drone update: ~p~n", [Drone]),
    gen_server:cast({gui_server, 'gui@localhost'} , {drone_update, Drone}),

    {reply, ok, State};

handle_call({establish_comm, _}, _From, State) ->
    Reply = io_lib:format("This is node ~p", [node()]),
    io:format("Establishing communication with GUI~n"),
    {reply, Reply , State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions