-module(toggle_statem).
-behaviour(gen_statem).

% API functions
-export([start_link/1, stop/0]).

% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0, code_change/4]).
-export([slave/3, leader/3]).

-define(TIMEOUT, 4000).
-define(INDENTATION,{5,0}).
-define(STEP_SIZE,1).
-record(drone, {id, location, theta, speed}).

%%%===================================================================
%%% Intialzation Arguments - id, location,state
%%% id - id of the drone - intedger
%%% location - location of the drone - {x,y} ,x,y are integers
%%% state - state of the drone - atom - slave/leader
%%% 

%%Location,GS_PID,Follower_PID,Self_id,State,Waypoint,Points_to_follow
start_link(ID) ->
    % List = [{0,0},'gs_server',0,0,'slave',{1,1},[{1,1},{3,2},{5,1},{1,5},{10,1},{1,10},{20,2},{5,30},{50,50}]],
    List1 = [{0,0},'gs_server',slave,{0,0},ID],
    gen_statem:start_link(?MODULE, List1, []).
    % List2 = [{0,0},'gs_server',PID1,0,'leader',{1,1},[{1,1},{3,2},{5,1},{1,5},{10,1},{1,10},{20,2},{5,30},{50,50}]],
    % gen_statem:start_link(?MODULE, List2, []).


stop() ->
    gen_statem:stop(?MODULE).

% Initialization of the state machine
%%%-------------------------------------------------------------------
%%% @doc Initialize the state machine.
%%% minimial init([Location,GS_PID,State]) 
%%% full init([Location,GS_PID,Follower_PID,Self_id,State,Waypoint,Points_to_follow])
%%% @end
init([Location,GS_PID,State,Waypoint,ID])->
    io:format("Init~n"),
    put(location, Location),
    put(gs_pid, GS_PID),
    put(state, State),
    put(waypoint, Waypoint),
    put(self_id, ID),
    {ok, State, [], [{state_timeout, ?TIMEOUT, time_tick}]};

init([Location,GS_PID,Follower_PID,Self_id,State,Waypoint,Points_to_follow]) ->
    io:format("Init~n"),
    put(location, Location),
    put(gs_pid, GS_PID),
    put(follower_pid, Follower_PID),
    put(self_id, Self_id),
    put(state, State),
    put(waypoint, Waypoint),
    put(points_to_follow, Points_to_follow),
    {ok, State, [], [{state_timeout, ?TIMEOUT, time_tick}]}.

terminate(_Reason, _State, _Data) ->
    ok.

callback_mode() ->
    state_functions.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


slave(state_timeout, _From, _Data) ->
    io:format("timeout in slave~n"),
    step(get(state)),
    io:format("location is ~p~n", [get(location)]),
    {keep_state, _Data,[{state_timeout, ?TIMEOUT, time_tick}]};
slave(vector_update, _From, {Location,Theta}) ->
    io:format("vector_update in slave~n"),
    waypoint_update(Location, Theta),
    {keep_state};
slave(cast,{vector_update, {Location,Theta}}, _From) ->
    io:format("cast vector_update in slave~n"),
    waypoint_update(Location, Theta),
    gen_server:call(gs_server, {drone_update, #drone{id = get(id), location = get(location), theta = radian_to_degree(get_theta()), speed = ?STEP_SIZE}}),
    try gen_statem:cast(get(follower_pid), {vector_update, {get(location),get_theta()}}) of
        __ ->
            io:format("Follower is alive~n")
    catch
        _:_ ->
            io:format("Follower is not alive~n")
    end,
    {keep_state,[]};

slave(cast,{update_value,{Key,Value}},_From) ->
    io:format("cast update_value in slave~n"),
    put(Key,Value),
    {keep_state,[]};

slave(_Event, _Data, _From) ->
    io:format("unknown event ~p in slave~n", [_Event]),
    io:format("the data is ~p~n, from:~p~n", [_Data, _From]),
    {keep_state, _Data,[{state_timeout, ?TIMEOUT, time_tick}]}.


leader(state_timeout, _Data, _From) ->
    io:format("timeout in leader~n"),
    Distance_to_waypoint = get_distance(get(waypoint),get(location)),
    case Distance_to_waypoint>=?STEP_SIZE of
        true->
            step(leader);
        false ->
            next_waypoint(),
            step(leader),
            gen_server:call(gs_server, {drone_update, #drone{id = get(id), location = get(location), theta = radian_to_degree(get_theta()), speed = ?STEP_SIZE}}),
            case get(follower_pid) of
                undefined ->
                    io:format("follower_pid is undefined~n");
                _ ->
                    gen_statem:cast(get(follower_pid), {vector_update, {get(location),get_theta()}}) %%update neighbor only if it is defined
            end
    end,
    io:format("location is ~p~n", [get(location)]),
    {keep_state,_Data,[{state_timeout, ?TIMEOUT, time_tick}]};

leader(cast,{update_value,{Key,Value}},_From) ->
    io:format("cast update_value in leader~n"),
    put(Key,Value),
    {keep_state,[]};

leader(_Event, _Data, _From) ->
    io:format("unknown event ~p in leader~n", [_Event]),
    {keep_state, _Data, [{state_timeout, ?TIMEOUT, time_tick}]}.


%%%===================================================================
%%% Internal functions

get_theta() ->
    {X_old,Y_old} =get(waypoint),
    {X_new,Y_new} = get(location),
    X = X_old - X_new,
    Y = Y_old - Y_new,
    Angle = math:atan(Y / X),
    case X >= 0 of
        true -> Angle;
        false -> Angle + math:pi()
    end.
next_waypoint() ->%%that function is only for leader state
    case get(points_to_follow) of
        [] -> get(waypoint);
        [H|T] ->
            put(points_to_follow, T),
            put(waypoint, H)
    end.
get_distance({X1,Y1},{X2,Y2}) ->
    math:sqrt((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2)).

rotation_matrix({X,Y},Theta)->
    {X_new,Y_new} = {X*math:cos(Theta)+Y*math:sin(Theta),Y*math:cos(Theta)-X*math:sin(Theta)},
    {X_new,Y_new}.
waypoint_update({X,Y},Theta) ->
    {X_new,Y_new} = rotation_matrix(?INDENTATION, Theta),
    put(waypoint,{X-X_new,Y-Y_new}),
    put(theta,get_theta()).



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
        Distance_to_waypoint == 0 ->
            put(speed,0),
            0;
        true ->
            put(speed,1),
            update_waypoint(get(theta)),
            1
        end.



update_waypoint(undefined)->
    {X,Y} = get(location),
    case get(waypoint) == {X,Y} of
        true ->
            put(theta,0);%assign random theta
        false ->
            put(theta,get_theta())
    end,
    update_waypoint(get(theta));    
     %update waypoint

update_waypoint(Angle)->
    {X,Y} = get(location),
    put(waypoint,{X+?STEP_SIZE*math:cos(Angle),Y+?STEP_SIZE*math:sin(Angle)}). %update waypoint


step(leader)->
    Angle = get_theta(),
    {X,Y} = get(location),
    put(location,{X+?STEP_SIZE*math:cos(Angle),Y+?STEP_SIZE*math:sin(Angle)}); %update location

step(slave)->
    {X,Y} = get(location),
    Speed = calculate_speed(),
    Angle = get(theta),
    put(location,{X+Speed*?STEP_SIZE*math:cos(Angle),Y+Speed*?STEP_SIZE*math:sin(Angle)}).


radian_to_degree(Radian) ->
    DegreesFloat = Radian * (180/math:pi()),
    round(DegreesFloat).