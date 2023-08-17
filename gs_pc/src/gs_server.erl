-module(gs_server).
-include("../../project_def.hrl").
-behaviour(gen_server).

-export([start_link/0,start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).





%% State record
-record(state, {gs_id, num_of_drones=0, data_stack=[], gs_location, all_areas, node_to_monitor}).


start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ask_for_restoration], []).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).




init([]) ->
    {ok, GS_ID} = extract_number(node()),
    ets:new(gs_ets, [named_table,set, private, {write_concurrency, true}]), % think if we need write_concurrency
    GS_location = get_gs_location(),
    All_Areas = init_global_areas(),
    wait_for_gui(GS_location),
    logger(to_server,"1"),
    Node_to_monitor = start_monitor(),
    {ok, #state{gs_id = GS_ID, all_areas=All_Areas, gs_location = GS_location, node_to_monitor = Node_to_monitor}};

init([Atom])->
    case Atom of
        ask_for_restoration ->
            {ok, GS_ID} = extract_number(node()),
            [Node|_]=nodes(),
            case Node of
                [] ->
                    io:format("ERROR: no nodes to back up found~n"),
                    init([]);
                _ ->
                    {ok,ETS,#state{num_of_drones=NOF,all_areas=All_Areas}=_} = gen_server:call({gs_server,Node},{ask_for_restoration}),
                    ets:new(gs_ets, [named_table,set, private, {write_concurrency, true}]), % think if we need write_concurrency
                    [ets:insert(gs_ets, {ID, Drone})|| {ID,Drone} <- ETS],
                    GS_location = get_gs_location(),
                    wait_for_gui(GS_location),
                    logger(to_server,"1"),
                    Node_to_monitor = start_monitor(),
                    {ok, #state{gs_id = GS_ID,num_of_drones=NOF, all_areas=All_Areas, gs_location = GS_location, node_to_monitor = Node_to_monitor}}
            end;   
        _ ->
            init([])
    end.




start_monitor()->
    Node_to_monitor = get_node_to_monitor(),
    case net_adm:ping(Node_to_monitor) of
        pong ->
            io:format("Node ~p is up~n",[Node_to_monitor]),
            monitor_node(Node_to_monitor,true),
            Node_to_monitor;
        pang ->
            {ok, Number} = extract_number(node()),
            io:format("Node gs~p is down, trying again~n",[Number+1]),
            timer:sleep(1000),
            start_monitor()
    end.

start_monitor(Node)-> % bug we should be using this after we replace someone else
    Node_to_monitor = get_node_to_monitor(Node),
    case net_adm:ping(Node_to_monitor) of
        pong ->
            io:format("Node ~p is up~n",[Node_to_monitor]),
            monitor_node(Node_to_monitor,true);
        pang ->
            timer:sleep(500),
            start_monitor()
    end.


init_global_areas() ->
    [
        {1, [#area{left_border = -?INFINITY, right_border = ?WORLD_SIZE/4}]},
        {2, [#area{left_border = ?WORLD_SIZE/4, right_border = ?WORLD_SIZE/2}]},
        {3, [#area{left_border = ?WORLD_SIZE/2, right_border = 3*?WORLD_SIZE/4}]},
        {4, [#area{left_border = 3*?WORLD_SIZE/4, right_border = ?INFINITY}]}
    ].


handle_call({establish_comm, _}, _From, State) ->
    Reply = io_lib:format("This is node ~p", [node()]),
    io:format("Establishing communication with GUI~n"),
    {reply, Reply , State};


handle_call({launch_drones, Num}, _From, #state{gs_location = GS_location} =State) -> % todo debug- replace borders with proper area
    io:format("Launching ~p drones~n", [Num]),
    Borders = get_home_area(State),
    Drones_States = [#drone{id=Id,location=GS_location, gs_server=node(),time_stamp=get_time(), borders = Borders} || Id <- lists:seq(0,Num-1)],
    Drones_ID = [{Id, drone_statem:start_link(Drone)} || #drone{id=Id}=Drone <- Drones_States],
    Drones_ID_updated = [{ID, Drone_State#drone{pid=PID}} || {#drone{id=ID}=Drone_State,  {_,{ok,PID}}} <- lists:zip(Drones_States,Drones_ID)],
    [gen_server:cast({gs_server,Server},{all_drone_update,Drones_ID_updated})|| Server <- nodes()],
    logger(to_server,"4"),
    [ets:insert(gs_ets, {ID,Drone})|| {ID,Drone} <- Drones_ID_updated],
    set_followers(Num),
    {reply, ok, State#state{num_of_drones = Num}};


handle_call({set_waypoints, Waypoints}, _From, State) ->
    % io:format("Setting waypoints: ~p~n", [Waypoints]),
    send_to_drone(0, {waypoints_stack, Waypoints}),
    {reply, ok, State};





handle_call({create_drone, ID, Drone_state, Next_area, Time_stamp, Node}, _From, State) -> % todo update to work with areas
    {Res, Current_time_stamp} = get_current_time_from_node(Node),
    if
        (Current_time_stamp - Time_stamp)>= ?RETRY_DELAY ->
            {reply,{timeout, ok},State}; % throw the old irrelevant message
        (Res == noproc) orelse  (Current_time_stamp - Time_stamp < ?RETRY_DELAY) -> % assume if time_server dead, the node is dead
            % io:format("Create Drone :Drone ~p is reborn at ~p~n ", [ID,node()]),
            New_Drone_state = Drone_state#drone{borders = Next_area, gs_server=node()},
            {ok, PID} = drone_statem:rebirth(New_Drone_state),
            [gen_server:cast({gs_server,Server},{id_drone_update,[{ID,New_Drone_state#drone{pid=PID}}]}) || Server <- nodes()],
            logger(to_server,"4"),
            % io:format("Create Drone :Drone ~p is reborn at ~p with PID~p and neighbours~p~n ", [ID,node(),PID,get_value(followers,Drone_state)]),
            set_pid(ID,PID),
            {reply, {ok, PID}, State}
    end;


handle_call({crossing_border,ID, Location, #drone{time_stamp = Time_stamp} = Drone_state}, _From, State) ->
    Current_time_stamp = get_time(), % time here is within the same node, which is synchronized
    case Current_time_stamp - Time_stamp > ?RETRY_DELAY of
        true ->
            {reply,ok,State}; % throw the old irrelevant message
        false ->
            {Next_GS,Next_area} = check_borders(Location, State),
            logger(to_drone,"2"),
            logger(to_server,"2"),
            if 
                Next_GS == no_crossing ->
                    {reply, {change_area, Next_area},State};    
                Next_GS == undefined ->
                    io:format("ERROR: Next GS is Dead~n"),
                    {reply, ok, State}; 
                true ->
                    % io:format("Drone ~p is crossing border~n", [ID]),
                    try 
                        {Res, New_PID} = gen_server:call({gs_server, Next_GS}, {create_drone, ID, Drone_state, Next_area, get_time(), node()}, ?RETRY_DELAY),
                        case Res of
                            timeout -> % in a very rare case where call doesn't timeout but timestamp does.
                                {reply, ok, State}; % drone creation failed, dont terminate the old drone, will try again
                            ok ->
                                % io:format("Reply from GS ~p: ~p~n", [Next_GS, New_PID]),
                                set_pid(ID, New_PID),
                                reupdate_neighbour(ID,New_PID),
                                {reply, terminate, State} % terminate the old drone
                        end
                    catch
                        exit:{timeout, _} ->  % to prevent a deadlock between two GSs
                            {reply, ok, State} % drone creation failed, dont terminate the old drone, will try again
                    end
            end
    end;
    





handle_call({dead_neighbour,ID}, _From, State) ->%%function that get called by the drone when he detects that his neighbour is dead
    New_Pid=get_pid(ID),
    logger(to_drone,"2"),
    % [New_Pid]=ets:lookup(gs_ets, ID),
    % io:format("Drone ID: ~p, new PID~p~n", [ID,New_Pid]),
    {reply, {ok,New_Pid}, State};

handle_call({ask_for_pid,ID}, _From, State) ->%%function that get called by the previous gs when he transfered a drone to this gs
    %the gs that killed his drone will ask for the pid of the drone that was killed
    case  get_pid(ID) of %ets:lookup(gs_ets, ID)
        PID when is_pid(PID)->
            {reply, {ok, PID}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call(ask_for_restoration, _From, State) ->
    ETS = ets:tab2list(gs_ets),
    {reply, {ok,ETS, State}, State};

handle_call(_Request, _From, State) ->
    io:format("Unknown message: ~p~n", [_Request]),
    {reply, ignored, State}.


handle_cast({all_drone_update, ID_Drone_List}, State) ->
    [ets:insert(gs_ets, {ID, Drone})|| {ID,Drone} <- ID_Drone_List],
    Num_of_drones = length(ID_Drone_List),
    {noreply, State#state{num_of_drones = Num_of_drones}};




handle_cast({drone_update, #drone{id = ID, gs_server=GS_Server}=Drone}, State) when is_record(Drone,drone) ->
    % io:format("Drone update: ~p~n", [Drone]),
    ets:insert(gs_ets, {ID, Drone}),
    Self_Node= node(),
    if
        GS_Server == Self_Node ->
            [gen_server:cast({gs_server,Server},{drone_update,Drone})|| Server <- nodes()], % todo remove hard coded!
            logger(to_server,"4"),
            logger(to_drone,"1"),
            {noreply,send_to_gui(Drone, State)};
        true->
            {noreply,State}
    end;
    

handle_cast({aquire_target,Target}, #state{num_of_drones = Nof_Drones} = State) ->
    [send_target_to_drone(ID, Target) || ID <- lists:seq(0,Nof_Drones-1)],%% sends target one at a time only the drone has a list of targets
    {noreply, State};


handle_cast({id_pid_update,ID_PID_LIST}, State) ->
    io:format("ID PID update: ~p~n", [ID_PID_LIST]),
    id_pid_insertion(ID_PID_LIST),
    Num_of_drones = length(ID_PID_LIST),
    {noreply, State#state{num_of_drones = Num_of_drones}};

handle_cast({id_drone_update,ID_Drone_List}, State) ->
    [ets:insert(gs_ets, {ID, Drone})|| {ID,Drone} <- ID_Drone_List],
    {noreply, State};

handle_cast({target_found,Target}, #state{num_of_drones = Num_of_drones} = State) ->
    List_of_PIDs=[get_pid(ID)||ID <- lists:seq(0,Num_of_drones-1)],
    [gen_statem:cast(PID,{target_found,Target}) || PID <- List_of_PIDs],
    logger(to_drone, integer_to_list(length(List_of_PIDs)+ 1)),
    gen_server:cast(get_gui_node(), {target_found,Target}),
    logger('to_server',"1"),
    {noreply, State};

handle_cast({update_areas,New_Areas}, State) ->
    io:format("update areas: ~p~n", [New_Areas]),
    {noreply, State#state{all_areas = New_Areas}};


handle_cast(Message, State) when is_record(Message, log_message) ->
    gen_server:cast(get_gui_node(), Message), % do not count this message! its part of statistics
    {noreply, State};

handle_cast(_Msg, State) ->
    io:format("Unknown message: ~p~n", [_Msg]),
    {noreply, State}.

handle_info({nodedown, _}, #state{num_of_drones=NOF, node_to_monitor= Node}=State) ->
    io:format("Node ~p went down!~n", [Node]),
    %% Handle the node down event as needed
    {ok, DeadGS} = extract_number(Node),
    New_State = expand_areas(DeadGS, State),
    New_Backup_Node = New_State#state.node_to_monitor,
    case New_Backup_Node of
        undefined ->
            ok;
        _ ->
            monitor_node(New_State#state.node_to_monitor, true)
    end,
    Lost_Drones = retrieve_all_drones(Node, New_State), 
    ETS = ets:tab2list(gs_ets),
    io:format("ETS: ~p", [ETS]),
    if 
        Lost_Drones ==[] ->
            io:format("No drones lost~n");
        true->
            io:format("Lost drones: ~p~n", [Lost_Drones]),
            New_PIDS = [drone_statem:rebirth(Drone_state#drone{gs_server=node()}) || Drone_state <- Lost_Drones],% rebirth the drones
            io:format("New PIDS: ~p~n", [New_PIDS]),
            Drones_ID_updated = [{ID, Drone_state#drone{pid=PID}} || { #drone{id=ID}=Drone_state  , {ok,PID}} <- my_zip(Lost_Drones, New_PIDS)],
            % Drones_ID_updated =[{ID,Drone#drone{pid=PID}}   || {#drone{id=ID}=Drone, {_,PID}} <-lists:zip(Lost_Drones, New_PIDS)],
            [set_pid(ID,PID) || #drone{id=ID, pid=PID} <- Drones_ID_updated],% update the ets with the new pids
            [gen_server:cast({gs_server,Server},{id_drone_update,Drones_ID_updated}) || Server <- nodes()],
            logger(to_server,"4"),
            set_followers(NOF)     
        end,
    {noreply, New_State};


handle_info(_Info, State) ->
    {noreply, State}.

my_zip(List1,List2) ->
    my_zip(List1,List2,[]).
my_zip([],[],Acc)->
    Acc;
my_zip([H1|T1],[H2|T2],Acc)->
    my_zip(T1,T2, Acc ++ [{H1,H2}]).


terminate(_Reason, _State) ->
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions


wait_for_gui(GS_location) ->
    try
        gen_server:call(get_gui_node(), {establish_comm, node(), GS_location}),
        logger(to_server,"2"),
        io:format("GUI is up~n")
    catch
        exit:{{nodedown, _},_} ->
            timer:sleep(1000),
            io:format("GUI is down, retrying...~n"),
            wait_for_gui(GS_location);
        exit:{noproc, _} ->
            timer:sleep(1000),
            io:format("GUI is down, retrying...~n"),
            wait_for_gui(GS_location)
    end.



extract_number(NodeName) ->
    case re:run(atom_to_list(NodeName), "gs([0-9]+)@.*", [{capture, all_but_first, list}]) of
        {match, [Number]} -> 
            {ok, list_to_integer(Number)};
        _ -> 
            {error, "Invalid Node Name format in extract_number!!!!!! "}
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

% gets position and returns the next GS PID or no_crossing if not within borders
check_borders({X,_}, #state{gs_id=MyGS ,all_areas = All_Areas}) -> % todo debug modify this function to work with {GS, [Area1, Area2, ...]}
    % {GS_Node, MyAreas}  = lists:keyfind(GS_Node, 1, All_Areas),
    {Next_GS,Next_area} = assign_area(X, All_Areas),
    if
        MyGS =:= Next_GS ->
            {no_crossing,Next_area};
        true ->
            Next_GS_Node = number_to_gs(Next_GS) ,
            {Next_GS_Node,Next_area} % todo debug update drone borders if it crossed to a different area that is also in this node
    end.

assign_area(X,[])->
    io:format("Error: no area found~n");

assign_area(X, [{_,[]} | Other_GS])->
    assign_area(X,Other_GS);

assign_area(X, [{GS,[H|T]}  |Other_GS])->
    case X=<H#area.right_border andalso X>H#area.left_border of
        true ->
            {GS,H};
        false ->
            assign_area(X,[{GS,T}|Other_GS])
    end.


set_followers(Num) ->
    % io:format("Setting followers~n"),
    Drone_neighbors = [calculate_neighbor(ID, Num) || ID <- lists:seq(0,Num-1)],
    Drone_neighbors_PIDs = [get_followers_PIDs(Neighbor_IDs) || Neighbor_IDs <- Drone_neighbors],
    [send_to_drone(ID, {update_followers,  ID_PID_Pairs}) || {ID, ID_PID_Pairs} <- lists:zip(lists:seq(0,Num-1), Drone_neighbors_PIDs)].

send_target_to_drone(ID, Target) ->
    send_to_drone(ID, {add_target, Target}).


send_to_drone(ID, Message) ->
    % io:format("updating drone ~p with {~p, ~p}~n", [ID, Key, Value]),
    % io:format("sending message ~p to drone ~p~n", [Message, ID]),
    case get_pid(ID) of
        PID when is_pid(PID) ->
            gen_statem:cast(PID, Message),
            logger(to_drone,"1");
        [] ->
            io:format("ERROR Drone ~p not found~n", [ID]);
        ERR ->
            io:format("unexpected error ~p", [ERR])
    end.

% gets a list of drone IDs and returns the list of matching PIDs
get_followers_PIDs(Followers_IDs) -> get_followers_PIDs(Followers_IDs, []).
get_followers_PIDs([ID|T], Followers_PIDs) ->
    PID = get_pid(ID), %ets:lookup(gs_ets, ID),
    get_followers_PIDs(T, Followers_PIDs ++ [{ID,PID}]);


get_followers_PIDs([], Followers_PIDs) ->
    Followers_PIDs.


% calculates location of a GS given its ID
get_gs_location() ->
    {ok,GS_ID} = extract_number(node()),
    {(?WORLD_SIZE/4)*GS_ID- ?WORLD_SIZE/8, ?WORLD_SIZE/2}. % places X coordinate in the middle of the area, Y coordinate in the middle
    


% sends list of drone updates to GUI
send_to_gui(Drone, #state{data_stack = Stack} = State ) ->
    if  
        length(Stack) >= ?STACK_SIZE ->
            % io:format("Stack is full~n"), % debug
            logger(to_server,"1"),
            gen_server:cast(get_gui_node() , {drone_update, [Drone|Stack]}),
            State#state{data_stack = []};
        true ->
            io:format("Stack is not full~n"), % debug
            State#state{data_stack = [Drone|Stack]}
    end.


% get_value(Key, [{Key, Value} | _]) ->
%     Value;
% get_value(Key, [_ | Rest]) ->
%     get_value(Key, Rest);
% get_value(_, []) ->
%     not_found.

% actively updates the drone of their follower's new PID
reupdate_neighbour(Reborn_ID,New_PID) ->
    % io:format("Reupdate neighbour ~p with ~p~n",[Reborn_ID,New_PID]),
    case Reborn_ID >= 2 of
        false -> %means that the drone leader is the pack leader
            case get_pid(0) of %ets:lookup(gs_ets,0)
                PID when is_pid(PID) ->
                    logger(to_drone,"1"),
                    gen_statem:cast(PID,{replace_neighbour,{Reborn_ID,New_PID}});
                [] ->
                    {error, not_found}
            end;
        true ->
            Leader = Reborn_ID -2,
            case get_pid(Leader) of %ets:lookup(gs_ets,Leader)
                PID when is_pid(PID) ->
                    logger(to_drone, "1"),
                    gen_statem:cast(PID,{replace_neighbour,{Reborn_ID,New_PID}});
                [] ->
                    {error, not_found}
            end
    end.

% gets a list of new ID,PID pairs and updates them in the ets
id_pid_insertion([])->
    ok;
id_pid_insertion([{ID,PID}|T]) ->
    % io:format("inserting {~p,~p}~n",[ID,PID]),
    set_pid(ID,PID),
    % ets:insert(gs_ets,{ID,Drone}),
    id_pid_insertion(T).

% mnesia api
% get_pid_from_db(ID) ->
%     case mnesia:transaction(fun() -> mnesia:read({database, ID}) end) of
%         {atomic, [#mnesia_record{id = ID, pid = PID}]} ->
%             {ok,PID};
%         _ ->
%             {error, not_found},
%             io:format("Drone ~p not found~n", [ID])
%     end.

% set_pid_in_db(ID, PID) ->
%     mnesia:transaction(fun() -> mnesia:write(database, #mnesia_record{id = ID, pid = PID}, write) end).

logger(to_server,Message) ->
    {_,Time} = calendar:local_time(),
    % make a string of "drone" and Id
    {ok, Node_ID} = extract_number(node()),
    Name = lists:flatten(io_lib:format("server_server gs~p",[Node_ID])),
    Log = #log_message{time=Time, source = Name, message = Message},
    gen_server:cast(get_gui_node() , Log);

logger(to_drone,Message) ->
    {_,Time} = calendar:local_time(),
    % make a string of "drone" and Id
    {ok, Node_ID} = extract_number(node()),
    Name = lists:flatten(io_lib:format("server_drone gs~p",[Node_ID])),
    Log = #log_message{time=Time, source = Name, message = Message},
    gen_server:cast(get_gui_node() , Log).

get_time()->%%in milliseconds-needs to be verified
    erlang:monotonic_time(millisecond).

% Gets the PID of the drone with ID
get_pid(ID)->
    case ets:lookup(gs_ets,ID) of
        [{ID,#drone{pid = PID}=_}]->
            PID;
        [] ->
            io:format("ERROR get_pid:Drone ~p not found~n", [ID]),
            []
    end.
% sets new PID for drone with ID
set_pid(ID,New_PID)->
    case ets:lookup(gs_ets,ID) of
        [{ID,Drone}]->
            ets:insert(gs_ets,{ID,Drone#drone{pid = New_PID}});
            
        [] ->
            {error, not_found},
            io:format("ERROR set_pid:Drone ~p not found~n", [ID])
    end.

drone_restore_state(ID)-> %returns the approximation of drone state 
    case ets:lookup(gs_ets,ID) of
        [{ID, #drone{time_stamp = Old_time_stamp, speed= Speed, theta= Theta, location = {Old_X,Old_Y}} = Drone}]->
            Current_time_stamp = get_time(),
            Number_of_steps= (Current_time_stamp-Old_time_stamp)/?TIMEOUT,
            io:format("number of steps for drone ~p is ~p~n",[ID,Number_of_steps]),
            New_location = {Old_X+Number_of_steps*?STEP_SIZE*Speed*math:cos(Theta), Old_Y+Number_of_steps*?STEP_SIZE*Speed*math:sin(Theta)},
            % New_location = {Old_X, Old_Y}, % for debug to disable location approximation
            Drone#drone{location=New_location,time_stamp=Current_time_stamp};
        [] ->
            io:format("restore_drone:Drone ~p not found~n", [ID])
    end.



% Given the number of the dead GS and current all areas,
% returns if the calling GS should backup the dead GS, updated neighbors, and updated borders.
expand_areas(DeadGS, #state{gs_id = MyGS, all_areas=All_Areas}=State) ->
    % All_Areas is a list of tuples [{GS, [Area1, Area2,...]}, ...]
    {MyGS, MyAreas}  = lists:keyfind(MyGS, 1, All_Areas),
    {DeadGS, BackupAreas}  = lists:keyfind(DeadGS, 1, All_Areas),
    Curr_R_Border = get_rightmost_border_modulu(MyAreas),
    io:format("Current right border: ~p~n", [Curr_R_Border]),
    io:format("all areas: ~p~n", [All_Areas]),
    
    % If the dead GS is not the GS we backup, return false.
    % remove old area
    Areas_Without_Dead_GS = lists:keydelete(DeadGS, 1, All_Areas),
    % add backup areas to my areas
    io:format("Backup areas: ~p~n", [BackupAreas]),
    io:format("Areas_without_dead_gs: ~p~n", [Areas_Without_Dead_GS]),
    My_New_Areas = MyAreas ++ BackupAreas,
    io:format("My new areas: ~p~n", [My_New_Areas]),
    New_Areas = lists:keyreplace(MyGS, 1, Areas_Without_Dead_GS, {MyGS, My_New_Areas}),
    io:format("New areas: ~p~n", [New_Areas]),
    logger(to_server,"4"),
    [gen_server:cast({'gs_server',GS_NODE}, {update_areas,New_Areas}) || GS_NODE <- nodes()],
    New_Backup_Node = get_next_node(filter_and_sort_nodes(), MyGS),
    % io:format("New backup GS: ~p~n", [New_Backup_Node]),
    State#state{node_to_monitor=New_Backup_Node, all_areas=New_Areas}.




% Filter and sort the nodes
filter_and_sort_nodes() ->
    AllNodes = nodes(),
    GsNodes = [Node || Node <- AllNodes, lists:prefix("gs", atom_to_list(Node))],
    lists:sort(fun(Node1, Node2) -> 
                   extract_number(Node1) < extract_number(Node2)
               end, GsNodes).

get_next_node([], _) ->
    undefined;
% Get the next available node in a cyclic manner
get_next_node(SortedNodes, MyID) ->
    % Find the nodes with ID greater than MyID
    HigherNodes = [Node || Node <- SortedNodes, extract_number(Node) > MyID],
    
    % Return the node based on availability
    case HigherNodes of
        [] -> % If no higher node, return the first one (lowest ID)
            hd(SortedNodes);
        [FirstHigherNode|_] -> % If there are higher nodes, return the first one
            FirstHigherNode
    end.

find_adjacent_GS([], _) ->
    io:format("ERROR find_adjacent_GS: No areas found~n", []);
find_adjacent_GS([{GS, Areas} | T], My_R_Border) ->
    case find_adjacent_area(Areas, My_R_Border) of
        true ->
            {GS, Areas};
        false ->
            find_adjacent_GS(T, My_R_Border)
    end.

% returns true if atleast one Area is adjacent to my right border
find_adjacent_area([], _) ->
    false;
find_adjacent_area([#area{left_border=His_L_Border} = _ | T], My_R_Border) ->
    if His_L_Border =:= My_R_Border ->
        true;
    true -> % else continue searching
        find_adjacent_area(T, My_R_Border)
    end.

get_rightmost_border_modulu(Areas)->
    get_rightmost_border_modulu(Areas, -?INFINITY).  % [ 1, 2 ,4 ] 
get_rightmost_border_modulu([], Max)->
    Max;
get_rightmost_border_modulu([#area{right_border=Border} | T], Max)-> % todo check if this is the right border
    if 
        Border == ?INFINITY ->
            get_rightmost_border_modulu(T, Max);
        Border > Max ->
            get_rightmost_border_modulu(T, Border);
        true ->
            get_rightmost_border_modulu(T, Max)
    end.

get_drone(ID) ->
    get_key(ID).

get_key(Key)->
    case ets:lookup(gs_ets,Key) of
        [{Key,Value}]->
            Value;
        [] ->
            io:format("ERROR get_key: Key ~p not found~n", [Key]),
            []
    end.

retrieve_all_drones(Node, #state{num_of_drones = Nof_Drones}) ->
    All_Drones = [get_drone(ID) || ID <- lists:seq(0,Nof_Drones-1)],
    Old_states=lists:filter(fun(Drone) -> Drone#drone.gs_server == Node end, All_Drones),
    [drone_restore_state(Drone#drone.id)|| Drone <- Old_states].





get_node_to_monitor() ->
    get_node_to_monitor(node()).

get_node_to_monitor(Node) ->
    {ok,Number} = extract_number(Node),
    Next_Node = (Number rem 4) + 1,
    number_to_gs(Next_Node).


get_home_area(#state{gs_id = MyGS, all_areas=All_Areas, gs_location= {X,_}}) ->
    {MyGS, MyAreas}  = lists:keyfind(MyGS, 1, All_Areas),
    [Home_area] = [Area || #area{left_border = Left_border, right_border= Right_border} = Area <- MyAreas, Left_border =< X, Right_border >= X],
    Home_area.


% Function that finds the node by index
number_to_gs(Index) ->
    Pattern = io_lib:format("gs~p@", [Index]), % Creating the pattern like gs1@, gs2@, etc.
    MatchingNodes = lists:filter(fun(Node) -> 
        case re:run(atom_to_list(Node), Pattern) of
            {match, _} -> true; 
            nomatch -> false
        end
    end, nodes()),
    case MatchingNodes of
        [Node] -> Node; % If there's exactly one matching node
        [] -> undefined; % If no nodes match
        _ -> {error, multiple_matches} % If there are multiple matches, which shouldn't happen
    end.


get_gui_node() ->
    Pattern = io_lib:format("gui@", []),
    MatchingNodes = lists:filter(fun(Node) -> 
        case re:run(atom_to_list(Node), Pattern) of
            {match, _} -> true; 
            nomatch -> false
        end
    end, nodes()),
    case MatchingNodes of
        [Node] -> {?GUI_SERVER, Node}; % If there's exactly one matching node
        [] -> undefined; % If no nodes match
        _ -> {error, multiple_matches} % If there are multiple matches, which shouldn't happen
    end.

get_current_time_from_node(Node) ->
    try
        gen_server:call({time_server,Node}, get_time) % we must get time fron the same node that the timestamp is created in
    catch
        exit:{noproc, _} ->
            io:format("ERROR: time_server is down~n"),
            {noproc, 0}; % if the time_server is down, we assume the gs_server is also down
        Error:Reason ->
            io:format("ERROR: ~p, Reason:~p~n", [Error, Reason]),
            {Error, Reason}
    end.