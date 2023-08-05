-module(drone_statem).
-behaviour(gen_statem).

% API functions
-export([start_link/1, stop/0]).

% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0, code_change/4]).
-export([slave/3, leader/3]).

-define(TIMEOUT, 100).
-define(INDENTATION,{5,0}).
-define(STEP_SIZE,1).
-define(WORLD_SIZE,650).
-record(drone, {id, location, theta, speed}).

%%%===================================================================
%%% Intialzation Arguments - id, location,state
%%% id - id of the drone - intedger
%%% location - location of the drone - {x,y} ,x,y are integers
%%% state - state of the drone - atom - slave/leader
%%% 

start_link(Drone) when is_record(Drone,drone) ->
    gen_statem:start_link(?MODULE, [Drone], []).


stop() ->
    gen_statem:stop(?MODULE).

% Initialization of the state machine
%%%-------------------------------------------------------------------
%%% @doc Initialize the state machine.
%%% minimial init([Location,GS_PID,State]) 
%%% full init([Location,GS_PID,Follower_PID,Self_id,State,Waypoint,Waypoints_stack])
%%% @end
%%% '


init([#drone{id = ID, location = Location, theta = Theta, speed= Speed}=Drone]) ->
    io:format("Init~n"),
    put(location, Location),
    put(id, ID),
    put(theta, degree_to_radian(Theta)),
    put(speed, Speed),
    put(waypoint, Location),
    case ID of
        0 ->
            State = leader;
        _ ->
            State = slave,
            indentation_update()
    end,
    gen_server:cast(gs_server, {drone_update, Drone}),
    {ok, State, [],[{state_timeout, ?TIMEOUT, time_tick}]}.



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

slave(state_timeout, _From, _Data) ->
    step(slave),
    io:format("~p location is ~p~n", [get(id), get(location)]),
    {keep_state, _Data,[{state_timeout, ?TIMEOUT, time_tick}]};


slave(cast,{vector_update, {Leader_Location,Leader_Theta}}, _From) -> % test
    io:format("cast vector_update in slave~n"),
    New_Theta = waypoint_update(Leader_Location, Leader_Theta),
    gen_server:call(gs_server, {drone_update, #drone{id = get(id), location = get(location), theta = radian_to_degree(get_theta_to_wp()), speed = ?STEP_SIZE}}),
    Current_Location = get(location),
    case get(followers_pid) of
        undefined ->
            io:format("followers_pid is undefined~n");
        []->
            io:format("followers_pid is empty~n");
        [_|_] = Followers ->
            update_neighbors(Followers, Current_Location, New_Theta)
    end,
    {keep_state,[]};

slave(cast,{update_value,{Key,Value}},_From) -> % test
    io:format("cast update_value in slave~n"),
    put(Key,Value),
    {keep_state,[]};

slave(_Event, _Data, _From) -> % test
    io:format("unknown event ~p in slave~n", [_Event]),
    io:format("the data is ~p~n, from:~p~n", [_Data, _From]),
    {keep_state, _Data,[{state_timeout, ?TIMEOUT, time_tick}]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% leader state

leader(state_timeout,_From , _Data) ->
    % io:format("~p location is ~p~n", [get(id), get(location)]),
    Distance_to_waypoint = get_distance(get(waypoint),get(location)),
    Waypoints_stack = get(waypoints_stack),
    if
        (Waypoints_stack== undefined) ->
            ok;
        Distance_to_waypoint>=?STEP_SIZE ->
            step(leader);
        true ->
            next_waypoint(),
            step(leader),
            gen_server:call(gs_server, {drone_update, #drone{id = get(id), location = get(location), theta = radian_to_degree(get_theta_to_wp()), speed = ?STEP_SIZE}}),
            Theta = get(theta),
            Location = get(location),
            case get(followers_pid) of
                undefined ->
                    io:format("followers_pid is undefined~n");
                []->
                    io:format("followers_pid is empty~n");
                [_|_] = Followers ->
                    update_neighbors(Followers, Location, Theta)
            end
    end,
    {keep_state,_Data,[{state_timeout, ?TIMEOUT, time_tick}]};

leader(cast,{update_value,{Key,Value}},_From) ->
    io:format("cast update_value in leader Key: ~p, Value: ~p ~n", [Key,Value]),
    put(Key,Value),
    {keep_state,[]};

leader(_Event, _From, _Data) ->
    io:format("unknown event ~p in leader~n", [_Event]),
    {keep_state, _Data, [{state_timeout, ?TIMEOUT, time_tick}]}.


%%%===================================================================
%%% Internal functions

get_theta_to_wp() ->
    {X_old,Y_old} =get(waypoint),
    {X_new,Y_new} = get(location),
    X = X_old - X_new,
    Y = Y_old - Y_new,
    math:atan2(Y , X).

next_waypoint() ->%%that function is only for leader state
    case get(waypoints_stack) of
        undefined ->
            ok;
        [] -> get(waypoint);
        [H|T] ->
            put(waypoints_stack, T ++ [H]),
            put(waypoint, H)
    end.
get_distance({X1,Y1},{X2,Y2}) ->
    math:sqrt((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2)).

rotation_matrix({X,Y},Theta)->
    {X_new,Y_new} = {X*math:cos(Theta)+Y*math:sin(Theta),Y*math:cos(Theta)-X*math:sin(Theta)},
    {X_new,Y_new}.

waypoint_update({X,Y},Theta) ->
    {X_new,Y_new} = rotation_matrix(get(indentation), Theta),
    New_Theta = get_theta_to_wp(),
    New_Waypoint ={X-X_new,Y-Y_new},
    put(waypoint,New_Waypoint),
    put(theta,New_Theta),
    New_Theta.

indentation_update() ->
    {INDENTATION_X, INDENTATION_Y} = ?INDENTATION,
    put(indentation,{INDENTATION_X*math:pow(-1,get(id)),INDENTATION_Y}).



%%%-------------------------------------------------------------------
%%% leader time tick:
%%% <time-tick> -> next_waypoint -> step -> update_waypoint if needed

%%% slave time tick:
%%% <time-tick> -> calculate_speed -> step
%%% step = calculates theta, makes a step towards the waypoint in the dictionary, saves theta in dictionary
%%% calculate speed = if distance is large, makes no change to waypoint, increases speed to 2
%%% calculate speed = if distance is small, updates waypoint with previous theta and speed 1
%%% Neriya's addition - if distance is 0, speed is 0 in order to avoid bugs


calculate_speed() ->
    Distance_to_waypoint = get_distance(get(waypoint),get(location)),
    if
        Distance_to_waypoint > ?STEP_SIZE ->% speed must not be larger than 2!!! otherwise unstable system
            put(speed,2),
            2;
        Distance_to_waypoint == 0 -> % might be problematic we need a range
            put(speed,0),
            0;
        true ->
            put(speed,1),
            increment_waypoint(get(theta)),
            1
    end.



increment_waypoint(undefined)->
    {X,Y} = get(location),
    case get(waypoint) == {X,Y} of
        true ->
            put(theta,0);%assign random theta
        false ->
            put(theta,get_theta_to_wp())
    end,
    increment_waypoint(get(theta));    
     %update waypoint

increment_waypoint(Angle)->
    {X,Y} = get(location),
    put(waypoint,{X+?STEP_SIZE*math:cos(Angle),Y+?STEP_SIZE*math:sin(Angle)}). %update waypoint


step(leader)->
    Angle = get_theta_to_wp(),
    {X,Y} = get(location),
    put(location,{X+?STEP_SIZE*math:cos(Angle),Y+?STEP_SIZE*math:sin(Angle)}); %update location

step(slave)->
    {X,Y} = get(location),
    Speed = calculate_speed(),
    Angle = get(theta),
    put(location,{X+Speed*?STEP_SIZE*math:cos(Angle), Y+Speed*?STEP_SIZE*math:sin(Angle)}).


radian_to_degree(Radian) ->
    DegreesFloat = Radian * (180/math:pi()),
    round(DegreesFloat).

degree_to_radian(Degree) ->
    Degree * (math:pi()/180).

update_neighbors([], _, _) ->
    ok;
update_neighbors([H|T], Location, Theta)->
    gen_statem:cast(H, {vector_update, {Location,Theta}}),
    update_neighbors(T, Location, Theta).