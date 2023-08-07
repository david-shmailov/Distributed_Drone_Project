-module(gs_server).
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



%% State record
-record(state, {gs_id, num_of_drones, data_stack, borders, neighbors}).

% record for drone location and speed update:


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    ets:new(gs_ets, [named_table,set, private, {write_concurrency, true}]), % think if we need write_concurrency
    {ok,GS_ID} = extract_number(node()),
    {Borders, Neighbors} = calculate_borders_and_neighbors(GS_ID),
    gen_server:call({?GUI_SERVER, ?GUI_NODE}, {establish_comm, self()}),
    {ok, #state{gs_id = GS_ID, data_stack=[], borders = Borders, neighbors= Neighbors}}.


    

handle_call({establish_comm, _}, _From, State) ->
    Reply = io_lib:format("This is node ~p", [node()]),
    io:format("Establishing communication with GUI~n"),
    {reply, Reply , State};


handle_call({launch_drones, Num}, _From, #state{borders = Borders} =State) ->
    io:format("Launching ~p drones~n", [Num]),
    Drones_ID = [{Id, drone_statem:start_link(#drone{id=Id,location={?WORLD_SIZE/2-100,?WORLD_SIZE/2+100}}, Borders)} || Id <- lists:seq(0,Num-1)],
    % insert drone ID / PID into ETS table
    [ets:insert(gs_ets, {ID, PID}) || {ID,{ok,PID}} <- Drones_ID],
    {reply, ok, State#state{num_of_drones = Num}};


handle_call({set_waypoints, Waypoints}, _From, State) ->
    io:format("Setting waypoints: ~p~n", [Waypoints]),
    send_to_drone(0, {waypoints_stack, Waypoints}),
    {reply, ok, State};

handle_call(set_followers,_From, #state{num_of_drones = Num}=State) ->
    io:format("Setting followers~n"),
    Drone_neighbors = [calculate_neighbor(ID, Num) || ID <- lists:seq(0,Num-1)],
    Drone_neighbors_PIDs = [get_followers_PIDs(Neighbor_IDs) || Neighbor_IDs <- Drone_neighbors],
    [send_to_drone(ID, {followers, PIDs}) || {ID,PIDs} <- lists:zip(lists:seq(0,Num-1), Drone_neighbors_PIDs)],
    {reply, ok, State};




handle_call({create_drone, ID, Drone_state}, _From, #state{borders=Borders}=State) ->
    {ok, PID} = drone_statem:rebirth(Drone_state, Borders),
    % io:format("Create Drone :Drone ~p is reborn at ~p with PID~p and neighbours~p~n ", [ID,node(),PID,get_value(followers,Drone_state)]),
    ets:insert(gs_ets, {ID, PID}),
    {reply, {ok, PID}, State};


handle_call({crossing_border,ID, Location, Drone_state}, _From, State) ->
    Transfer_to_GS_ID = check_borders(Location, State),
    case Transfer_to_GS_ID of
        no_crossing ->
            {reply, ok, State};
        Next_GS ->
            io:format("Drone ~p is crossing border~n", [ID]),
            case gen_server:call({gs_server, Next_GS}, {create_drone, ID, Drone_state}) of 
                {ok, New_PID} -> 
                    % io:format("Reply from GS ~p: ~p~n", [Next_GS, New_PID]),
                    ets:insert(gs_ets, {ID, New_PID}),
                    reupdate_neighbour(ID,New_PID),
                    {reply, terminate, State}; % terminate the old drone
                _ -> 
                    io:format("Error creating drone on neighbor GS ~p~n", [Next_GS]),
                    {reply, ok, State} % drone creation failed, dont terminate the old drone
            end
    end;




handle_call({dead_neighbour,ID}, _From, State) ->%%function that get called by the drone when he detects that his neighbour is dead
    [New_Pid]=ets:lookup(gs_ets, ID),
    % io:format("Drone ID: ~p, new PID~p~n", [ID,New_Pid]),
    {reply, {ok,New_Pid}, State};

handle_call({ask_for_pid,ID}, _From, State) ->%%function that get called by the previous gs when he transfered a drone to this gs
    %the gs that killed his drone will ask for the pid of the drone that was killed
    case ets:lookup(gs_ets, ID) of
        [{ID, PID}] ->
            {reply, {ok, PID}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    io:format("Unknown message: ~p~n", [_Request]),
    {reply, ignored, State}.






handle_cast({drone_update, Drone}, State) when is_record(Drone,drone) ->
    % io:format("Drone update: ~p~n", [Drone]),
    ets:insert(gs_ets, Drone),
    New_State = send_to_gui(Drone, State),
    {noreply, New_State};


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


extract_number(NodeName) ->
    case re:run(atom_to_list(NodeName), "gs([0-9]+)@.*", [{capture, all_but_first, list}]) of
        {match, [Number]} -> 
            {ok, list_to_integer(Number)};
        _ -> 
            {error, "Invalid NodeName format"}
    end.





calculate_neighbor(0,Num_of_drones) ->
    % ID = 0 is leader
    % Leader has 2 neighbors 
    case Num_of_drones of % num of drones including leader
        1 ->
            [];
        2 ->
            [1];
        _ ->
            [1,2]
    end;

calculate_neighbor(ID,Num_of_drones) ->
    % non leader drones follow the drone with ID +2 
    if
        Num_of_drones - ID < 3 ->
            []; % no more followers
        true ->
            [ID+2]
    end.


check_borders({X,Y}, #state{borders = Borders, neighbors = Neighbors}) ->
    #borders{left=Left,right=Right,top = Top,bottom = Bottom}=Borders,
    if 
        X=<Left -> Neighbors#borders.left;
        X>=Right -> Neighbors#borders.right;
        Y=<Bottom -> Neighbors#borders.bottom;
        Y>=Top -> Neighbors#borders.top;
        true -> no_crossing
    end.





send_to_drone(ID, {Key,Value}) ->
    io:format("updating drone ~p with {~p, ~p}~n", [ID, Key, Value]),
    case ets:lookup(gs_ets, ID) of
        [{ID, PID}] ->
            gen_statem:cast(PID, {update_value, {Key,Value}});
        [] ->
            io:format("Drone ~p not found~n", [ID])
    end.


get_followers_PIDs(Followers_IDs) -> get_followers_PIDs(Followers_IDs, []).


get_followers_PIDs([ID|T], Followers_PIDs) ->
    [{ID, PID}] = ets:lookup(gs_ets, ID),
    get_followers_PIDs(T, Followers_PIDs ++ [{ID,PID}]);


get_followers_PIDs([], Followers_PIDs) ->
    Followers_PIDs.



calculate_borders_and_neighbors(GS_ID) ->
    case GS_ID of
        1 -> % TODO change hard coded node names into a mechanism to get the node names
            Borders =   #borders{left = -?INFINITY, right = ?WORLD_SIZE/2, top = ?INFINITY, bottom = ?WORLD_SIZE/2},
            Neighbors = #borders{left = undefined, right = 'gs2@localhost', top = undefined, bottom = 'gs4@localhost'};
        2 ->
            Borders =   #borders{left = ?WORLD_SIZE/2, right = ?INFINITY, top = ?INFINITY, bottom = ?WORLD_SIZE/2},
            Neighbors = #borders{left = 'gs1@localhost', right = undefined, top = undefined, bottom = 'gs3@localhost'};
        3 ->
            Borders =   #borders{left = ?WORLD_SIZE/2, right = ?INFINITY, top = ?WORLD_SIZE/2, bottom = -?INFINITY},
            Neighbors = #borders{left = 'gs4@localhost', right = undefined, top = 'gs2@localhost', bottom = undefined};
        4 ->
            Borders =   #borders{left = -?INFINITY, right = ?WORLD_SIZE/2, top = ?WORLD_SIZE/2, bottom = -?INFINITY},
            Neighbors = #borders{left = undefined, right = 'gs3@localhost', top = 'gs1@localhost', bottom = undefined}
    end,
    {Borders, Neighbors}.



send_to_gui(Drone, #state{data_stack = Stack} = State ) ->
    if  
        length(Stack) >= ?STACK_SIZE ->
            % io:format("Stack is full~n"), % debug
            gen_server:cast({?GUI_SERVER, ?GUI_NODE} , {drone_update, [Drone|Stack]}),
            State#state{data_stack = []};
        true ->
            io:format("Stack is not full~n"), % debug
            State#state{data_stack = [Drone|Stack]}
    end.


get_value(Key, [{Key, Value} | _]) ->
    Value;
get_value(Key, [_ | Rest]) ->
    get_value(Key, Rest);
get_value(_, []) ->
    not_found.

reupdate_neighbour(Reborn_ID,New_PID) ->
    io:format("Reupdate neighbour ~p with ~p~n",[Reborn_ID,New_PID]),
    case Reborn_ID >= 2 of
        false -> %means that the drone leader is the pack leader
            case ets:lookup(gs_ets,0) of
                [{0, PID}] ->
                    gen_statem:cast(PID,{replace_neighbour,{Reborn_ID,New_PID}});
                [] ->
                    {error, not_found}
            end;
        true ->
            Leader = Reborn_ID -2,
            case ets:lookup(gs_ets,Leader) of
                [{Leader, PID}] ->
                    gen_statem:cast(PID,{replace_neighbour,{Reborn_ID,New_PID}});
                [] ->
                    {error, not_found}
            end
    end.




