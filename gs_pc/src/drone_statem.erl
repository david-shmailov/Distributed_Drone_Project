-module(drone_statem).
-include("../../project_def.hrl").

-behaviour(gen_statem).

% API functions
-export([start/1, start_link/1, stop/0, rebirth/1]).

% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0, code_change/4]).
-export([slave/3, leader/3]).
%circle/3


%%%===================================================================
%%% Intialzation Arguments - id, location,state
%%% id - id of the drone - intedger
%%% location - location of the drone - {x,y} ,x,y are integers
%%% state - state of the drone - atom - slave/leader
%%% 

%%% @todo: add the following event: become leader
%%%       : add the following event: target found
%%% 

start_link(Drone) when is_record(Drone,drone)  ->
    gen_statem:start_link(?MODULE, [Drone], []).

start(Drone) when is_record(Drone,drone)  ->
    gen_statem:start(?MODULE, [Drone], []).

rebirth(State)->
    % gen_statem:start(?MODULE, [rebirth, State, Borders], []).
    % io:format("rebirth Drone state:~p~n",[State]),
    gen_statem:start_link(?MODULE, [rebirth, State], []).

stop() ->
    gen_statem:stop(?MODULE).

% Initialization of the state machine
%%%-------------------------------------------------------------------
%%% @doc Initialize the state machine.
%%% minimial init([Location,GS_PID,State]) 
%%% full init([Location,GS_PID,Follower_PID,Self_id,State,Waypoint,Waypoints_stack])
%%% @end
%%% '


init([rebirth | [#drone{id = ID} = Internal_state]]) when is_record(Internal_state, drone) -> % needed for pattern match on rebirth
    % io:format("Drone ~p is reborn in node ~p, PID: ~p~n", [ID, node(), self()]),
    case ID of
        0 ->
            State = leader;
        _ ->
            State = slave
    end,
    {ok, State, Internal_state#drone{pid = self(),state=State},[{state_timeout, ?TIMEOUT, time_tick}]};


init([#drone{id = ID, location = Location, theta = Theta }=Internal_state]) ->
    io:format("Drone ~p born~n", [ID]),
    case ID of
        0 ->
            State = leader,
            Indentation = {0,0}; % leader doesn't need this
        _ ->
            State = slave,
            Indentation = indentation_update(ID)
    end,
    update_gs(Internal_state),
    New_internal_state = Internal_state#drone{pid = self(),next_waypoint= {Location, Theta}, state= State, indentation = Indentation},
    % put(internal_state, New_internal_state),
    {ok, State, New_internal_state,[{state_timeout, ?TIMEOUT, time_tick}]}.




terminate(_Reason, _State, _Data) ->
    ok.

callback_mode() ->
    state_functions.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% State functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% slave state


slave(state_timeout, _From, Internal_state) ->
    % io:format("Drone ~p :slave state_timeout~n",[get(id)]),
    Temp_State = step(slave, Internal_state),
    {_Res, New_Targets} = look_for_target(Temp_State),
    New_Internal_state = Temp_State#drone{targets = New_Targets},
    case check_borders(New_Internal_state) of
        ok -> 
            {keep_state, New_Internal_state, [{state_timeout, ?TIMEOUT, time_tick}]};
        {change_area, New_Area}->
            io:format("got new area ~p~n",[New_Area]),
            {keep_state, New_Internal_state#drone{borders = New_Area}, [{state_timeout, ?TIMEOUT, time_tick}]};
        terminate ->
            {stop, normal, New_Internal_state}
    end;



slave({call,_From},{vector_update, {Leader_Location,Leader_Theta}}, #drone{followers = Followers} = Internal_state) -> % test
    gen_statem:reply(_From, ok),    
    % io:format("Drone ~p :call vector_update in slave~n",[get(id)]),
    {{WP_Position, WP_Theta}, Theta_to_wp} = waypoint_update(Leader_Location, Leader_Theta, Internal_state),
    case Followers of
        []->
            New_followers = Followers;
        [_|_] ->
            New_followers = update_neighbors(WP_Position, WP_Theta, Internal_state) 
    end,
    {keep_state, Internal_state#drone{followers = New_followers, next_waypoint = {WP_Position,WP_Theta}, theta = Theta_to_wp}};




slave(cast,{add_target, Target},#drone{targets = Targets} = Internal_state) ->
    {keep_state, Internal_state#drone{targets = [Target | Targets]}};

slave(cast,{replace_neighbour,{Reborn_ID,New_PID}},Internal_state) ->
    New_Neighbors = replace_dead_neighbour(Reborn_ID, New_PID, Internal_state),
    {keep_state,Internal_state#drone{followers = New_Neighbors}};

slave(cast,{target_found,Target}, #drone{targets=Targets} = Internal_state)->
    New_targets = lists:delete(Target,Targets),
    {keep_state, Internal_state#drone{targets = New_targets}};

slave(cast,{update_followers, Followers}, Internal_state) ->
    {keep_state, Internal_state#drone{followers = Followers}};

slave(_Event, _From, _Data ) -> % test
    io:format("ERROR unknown event ~p in slave~n", [_Event]),
    io:format("the data is ~p~n, from:~p~n", [_Data, _From]),
    {keep_state, _Data,[{state_timeout, ?TIMEOUT, time_tick}]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% leader state

leader(state_timeout,_From , #drone{followers = Followers, location = Location, theta = Theta, waypoints_stack=Waypoints_stack, next_waypoint=Next_WP} =Internal_state) ->
    % io:format("~p location is ~p~n", [Internal_state#drone.id,Location]),
    Distance_to_waypoint = get_distance(Next_WP,Location),
    if
        (Waypoints_stack== []) ->
            New_Internal_state = Internal_state,
            New_followers = Followers;
        Distance_to_waypoint>=?STEP_SIZE ->
            New_Internal_state = step(leader,Internal_state),
            New_followers = Followers,
            debug_update(Internal_state);
        true ->
            Temp_Internal = next_waypoint(Internal_state),
            New_Internal_state = step(leader,Temp_Internal),
            update_gs(New_Internal_state),
            case Followers of
                []->
                    New_followers = Followers;
                [_|_] = Followers ->
                    New_Location = New_Internal_state#drone.location,
                    New_Theta = New_Internal_state#drone.theta,
                    New_followers = update_neighbors(New_Location,New_Theta, New_Internal_state)
            end
    end,
    {_Res, New_Targets} = look_for_target(New_Internal_state),
    case check_borders(New_Internal_state) of
        ok -> 
            {keep_state, New_Internal_state#drone{targets = New_Targets, followers = New_followers},[{state_timeout, ?TIMEOUT, time_tick}]};
        {change_area, New_Area}->
            io:format("got new area ~p~n",[New_Area]),
            {keep_state, New_Internal_state#drone{borders = New_Area, targets = New_Targets, followers = New_followers},[{state_timeout, ?TIMEOUT, time_tick}]};
        terminate ->
            {stop, normal, New_Internal_state#drone{targets = New_Targets, followers = New_followers}}
    end;
    

leader(cast,{target_found,Target},#drone{targets = Targets}= Internal_state)->
    New_targets = lists:delete(Target,Targets),
    {keep_state, Internal_state#drone{targets = New_targets}};


leader(cast,{add_target, Target},#drone{targets = Targets} = Internal_state) ->
    {keep_state, Internal_state#drone{targets = [Target | Targets]}};


leader(cast,{replace_neighbour,{Reborn_ID,New_PID}},Internal_state) ->
    New_Neighbors = replace_dead_neighbour(Reborn_ID, New_PID, Internal_state),
    {keep_state,Internal_state#drone{followers = New_Neighbors}};

leader(cast,{update_followers, Followers}, Internal_state) ->
    {keep_state, Internal_state#drone{followers = Followers}};

leader(cast, {waypoints_stack, Waypoints_stack}, Internal_state) ->
    {keep_state, Internal_state#drone{waypoints_stack = Waypoints_stack}};

leader(_Event, Message, _Data) ->
    io:format("unknown ~p event ~p in leader~n", [_Event, Message]),
    {keep_state, _Data, [{state_timeout, ?TIMEOUT, time_tick}]}.


%%%===================================================================
%%% Internal functions

get_theta_to_wp(#drone{location = {Pos_X,Pos_Y}, next_waypoint={{WP_X,WP_Y},_}}) ->
    X = WP_X - Pos_X,
    Y = WP_Y - Pos_Y,
    math:atan2(Y , X).

next_waypoint(#drone{waypoints_stack = WP_Stack}=Internal_state) ->%%that function is only for leader state
    case WP_Stack of
        [] -> io:format("FORBIDDEN CASE - waypoints_stack is empty~n");
        [{_,K} = WP|T] when is_atom(K)->
            Internal_state#drone{waypoints_stack = T, next_waypoint= WP, theta = get_theta_to_wp(Internal_state#drone{next_waypoint = WP})};
        [{_,K} = WP|T] when is_number(K) ->
            Internal_state#drone{waypoints_stack = T ++ [WP], next_waypoint= WP, theta = get_theta_to_wp(Internal_state#drone{next_waypoint = WP})}
    end.

get_distance({{X1,Y1},_},{X2,Y2}) ->
    get_distance({X1,Y1},{X2,Y2});
get_distance({X1,Y1},{X2,Y2}) ->
    math:sqrt((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2)).

rotation_matrix({X,Y},Theta)->
    {X_new,Y_new} = {X*math:cos(Theta)-Y*math:sin(Theta), X*math:sin(Theta)+Y*math:cos(Theta)},
    {X_new,Y_new}.

waypoint_update({X,Y},Leader_Theta, #drone{location = Location, indentation = Indentation} = Internal_state) ->
    {X_new,Y_new} = rotation_matrix(Indentation, Leader_Theta),
    Theta_to_wp = get_theta_to_wp(Internal_state),
    New_Waypoint ={{X-X_new,Y-Y_new},Leader_Theta},
    % Interception_WP = calculate_interception_point(New_Waypoint, Location), % returns new waypoint
    % {Interception_WP, Theta_to_wp}. %update waypoint
    {New_Waypoint, Theta_to_wp}. %update waypoint

indentation_update(ID) ->
    {INDENTATION_X, INDENTATION_Y} = ?INDENTATION,
    {INDENTATION_X,INDENTATION_Y*math:pow(-1,ID)}.


calculate_interception_point({{WP_X,WP_Y}, WP_Theta}=Waypoint, Position) ->
    Distance = get_distance(Waypoint,Position),
    ToT = 3/Distance,
    X_future = WP_X + ToT*math:cos(WP_Theta),
    Y_future = WP_Y + ToT*math:sin(WP_Theta),
    % Theta_to_future_WP = math:atan2(Y_future - Y , X_future - X),
    {{X_future,Y_future},WP_Theta}.


%%%-------------------------------------------------------------------
%%% leader time tick:
%%% <time-tick> -> next_waypoint -> step -> waypoint_update if needed

%%% slave time tick:
%%% <time-tick> -> calculate_speed -> step
%%% step = calculates theta, makes a step towards the waypoint in the dictionary, saves theta in dictionary
%%% calculate speed = if distance is large, makes no change to waypoint, increases speed to 2
%%% calculate speed = if distance is small, updates waypoint with previous theta and speed 1
%%% Neriya's addition - if distance is 0, speed is 0 in order to avoid bugs


calculate_speed(#drone{next_waypoint= Waypoint, location = Location, speed = Current_speed}) ->
    Distance_to_waypoint = get_distance(Waypoint,Location),
    if
        % Distance_to_waypoint > ?STEP_SIZE*10 ->
        %     5;
        Distance_to_waypoint > ?STEP_SIZE*2 ->
            2;
        Distance_to_waypoint < ?STEP_SIZE andalso Current_speed == 0 -> % might be problematic we need a range
            0;
        true ->
            1
    end.




increment_waypoint(#drone{next_waypoint={{_,_}, Theta} , location= {X,Y}})->
    New_X = X+?STEP_SIZE*math:cos(Theta),
    New_Y = Y+?STEP_SIZE*math:sin(Theta),
    {{New_X,New_Y},Theta}. %return new waypoint

max_theta(New_Theta, Old_Theta) ->
    New_Theta.
    % Pi = math:pi(),
    % if 
    %     New_Theta > Pi ->
    %         Normalized = New_Theta - 2*Pi;
    %     New_Theta < -Pi ->
    %         Normalized = New_Theta + 2*Pi;
    %     true ->
    %         Normalized = New_Theta
    % end,
    % Max_Counter_clockwise = Old_Theta + ?MAX_THETA,
    % Max_clockwise = Old_Theta - ?MAX_THETA,
    % if 
    %     Normalized > Max_Counter_clockwise ->
    %         Max_Counter_clockwise;
    %     Normalized < Max_clockwise ->
    %         Max_clockwise;
    %     true ->
    %         Normalized
    % end.


step(leader, #drone{location = {X,Y}, theta=Theta} = Internal_state)->
    New_Theta = get_theta_to_wp(Internal_state),
    Cappted_Theta = max_theta(New_Theta, Theta),
    New_Location = {X+?STEP_SIZE*math:cos(Cappted_Theta),Y+?STEP_SIZE*math:sin(Cappted_Theta)}, %update location
    Internal_state#drone{location=New_Location, speed=1, theta = Cappted_Theta};


step(slave, #drone{speed= Old_speed, theta = Old_theta, location={X,Y}, next_waypoint= Waypoint} = Internal_state)->
    % io:format("Drone ~p is stepping~n",[get(id)]),
    New_Speed = calculate_speed(Internal_state),
    case New_Speed of
        1 -> % stable cruising speed
            New_waypoint = increment_waypoint(Internal_state);
        _ ->
            New_waypoint = Waypoint
    end,
    New_Theta = get_theta_to_wp(Internal_state#drone{next_waypoint = New_waypoint}),
    Cappted_Theta = max_theta(New_Theta, Old_theta),
    New_Location = {X+New_Speed*?STEP_SIZE*math:cos(Cappted_Theta), Y+New_Speed*?STEP_SIZE*math:sin(Cappted_Theta)},
    New_internal_state = Internal_state#drone{location=New_Location, theta = Cappted_Theta, speed= New_Speed, next_waypoint=New_waypoint},
    if
        Old_theta /= Cappted_Theta orelse Old_speed/=New_Speed ->
            update_gs(New_internal_state);
        true ->
            debug_update(New_internal_state)
    end,
    New_internal_state.






update_neighbors(Location, Theta, #drone{followers= Followers} = Internal_state)->
    update_neighbors(Followers, Location, Theta, Internal_state).

update_neighbors([], _, _,#drone{followers = Followers}) ->
    Followers; % return followers list unchanged
update_neighbors([{Neighbor_ID,PID}|T], Location, Theta, #drone{id=ID} = Internal_state)-> % returns new followers list
    try
        % io:format("calling ~p~n",[PID]),
        gen_statem:call(PID, {vector_update, {Location,Theta}}), % returns call dirty after returning to GS1 from GS4 and trying to update
        logger(ID,"2"),
        update_neighbors(T, Location, Theta,Internal_state)
    catch
        exit:{noproc, _} ->
            % io:format("Requesting new PID~n"),
            try
                {ok,New_PID} = gen_server:call(gs_server, {dead_neighbour, Neighbor_ID}),
                Updated_Neighbors = replace_dead_neighbour(Neighbor_ID,New_PID,Internal_state),
                update_neighbors([{Neighbor_ID,New_PID} | T], Location, Theta,Internal_state#drone{followers = Updated_Neighbors})
            catch
                exit:{timeout, _} ->
                    io:format("ERROR: timeout in line 365\n")
            end;
        Error:Kind->
            io:format("Error in update_neighbors~n~p:~p~n",[Error,Kind]),
            update_neighbors(T ++ [{Neighbor_ID,PID}], Location, Theta,Internal_state)
    end.


replace_dead_neighbour(ID, New_PID,  #drone{followers= Neighbors}) ->
    lists:keyreplace(ID, 1, Neighbors, {ID, New_PID}).


update_gs(#drone{id=ID} = Internal_state) ->
    gen_server:cast(gs_server, {drone_update, Internal_state#drone{pid=self(),gs_server=node(),time_stamp=get_time()}}).

check_borders(#drone{borders=Border_record, location = {X,_}} = Internal_state) ->
    #area{left_border=Left,right_border=Right}=Border_record,
    case X=<Left orelse X>=Right of
        false ->
            ok;
        true ->
            cross_border(Internal_state)
    end.



cross_border(#drone{id=ID, location=Location}=Internal_state) ->
    try
        gen_server:call(gs_server, {crossing_border, ID, Location,Internal_state#drone{time_stamp=get_time()}}, ?RETRY_DELAY)
    catch
        exit:{timeout, _} ->
            ok
    end.






look_for_target(#drone{targets = Targets} = Internal_state) ->
    look_for_target(Targets, Internal_state).

look_for_target([], #drone{targets = Targets} )->
    {not_found,Targets};
look_for_target([Target|Rest], #drone{location=Location} = Internal_state)-> 
    Distance = get_distance({Location,0},Target),
    % io:format("Distance to target is ~p~n",[Distance]),
    case  Distance =< ?SERACH_RADIUS of
        true ->
            found_target(Target, Internal_state);
        false ->
            look_for_target(Rest,Internal_state)
    end.

logger(ID,Message) ->
    {_,Time} = calendar:local_time(),
    % make a string of "drone" and Id
    Name = lists:flatten(io_lib:format("drone ~p",[ID])),
    Log = #log_message{time=Time, source = Name, message = Message},
    gen_server:cast(gs_server, Log).

get_time()->%%in milliseconds-needs to be verified
    erlang:monotonic_time(millisecond).




found_target(Target, #drone{id = ID, targets= Targets}) ->
    io:format("Drone ~p , found target at ~p~n",[ID,Target]), % todo
    gen_server:cast(gs_server, {target_found, Target}),
    {found, lists:delete(Target, Targets)}.


debug_update(Internal_state) ->
    case ?DEBUG_MODE of
        true ->
            update_gs(Internal_state);
        false ->
            ok
    end.