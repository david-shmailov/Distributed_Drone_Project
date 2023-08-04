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

-define(RETRY_DELAY, 1000).
-define(GUI_NODE, 'gui@localhost').
-define(GUI_SERVER, 'gui_server').
-define(STACK_SIZE, 0). % we might not need aggregation at all 
-define(WORLD_SIZE,650).

%% State record
-record(state, {ets, data_stack}).

% record for drone location and speed update:
-record(drone, {id, location, theta=0, speed=0}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    GS_ETS = ets:new(gs_ets, [named_table,set, private, {write_concurrency, true}]), % think if we need write_concurrency
    gen_server:call({?GUI_SERVER, ?GUI_NODE}, {establish_comm, self()}),
    {ok, #state{ets=GS_ETS, data_stack=[]}}.


handle_call({drone_update, Drone}, _From, State) when is_record(Drone,drone) ->
    io:format("Drone update: ~p~n", [Drone]),
    ets:insert(State#state.ets, Drone),
    New_State = send_to_gui(Drone, State),
    case crossing_border(Drone) of
        true ->
            % todo send message to neighbor GS to create replacement drone
            io:format("Drone ~p is crossing border~n", [Drone]),
            {reply, crossing_border, New_State}; % will make the drone kill itself
        false ->
            {reply, ok, New_State} % return normal ok
    end;
    

handle_call({establish_comm, _}, _From, State) ->
    Reply = io_lib:format("This is node ~p", [node()]),
    io:format("Establishing communication with GUI~n"),
    {reply, Reply , State};


handle_call({launch_drones, Num}, _From, State) ->
    io:format("Launching ~p drones~n", [Num]),
    Drones_ID = [{Id, drone_statem:start_link(#drone{id=Id,location={?WORLD_SIZE/2,?WORLD_SIZE/2}})} || Id <- lists:seq(0,Num-1)],
    io:format("launched ~p drones~n", [Num]),
    % insert drone ID / PID into ETS table
    [ets:insert(gs_ets, {ID, PID}) || {ID,{ok,PID}} <- Drones_ID],
    {reply, ok, State};



handle_call(_Request, _From, State) ->
    io:format("Unknown message: ~p~n", [_Request]),
    {reply, ignored, State}.




handle_cast({drone_update, Drone}, State) when is_record(Drone,drone) ->
    io:format("Drone update: ~p~n", [Drone]),
    ets:insert(State#state.ets, Drone),
    New_State = send_to_gui(Drone, State),
    {noreply, New_State};


handle_cast(_Msg, State) ->
    io:format("Unknown message: ~p~n", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

% todo
crossing_border(#drone{location= {X,Y}}) ->
    case X of
        X when X < 0 ->
            true;
        X when X > 100 ->
            true;
        _ ->
            false
    end.


send_to_gui(Drone, #state{data_stack = Stack} = State ) ->
    if  
        length(Stack) >= ?STACK_SIZE ->
            io:format("Stack is full~n"), % debug
            gen_server:cast({?GUI_SERVER, ?GUI_NODE} , {drone_update, [Drone|Stack]}),
            State#state{data_stack = []};
        true ->
            io:format("Stack is not full~n"), % debug
            State#state{data_stack = [Drone|Stack]}
    end.


    