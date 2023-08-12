-module(drone_statem).
-include("../../project_def.hrl").

-behaviour(gen_statem).

% API functions
-export([start/2, start_link/2, stop/0, rebirth/2]).

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

start_link(Drone, Borders) when is_record(Drone,drone) andalso is_record(Borders,borders) ->
    gen_statem:start_link(?MODULE, [Drone, Borders], []).

start(Drone, Borders) when is_record(Drone,drone) andalso is_record(Borders,borders) ->
    gen_statem:start(?MODULE, [Drone, Borders], []).

rebirth(State, Borders) when is_record(Borders,borders) ->
    % gen_statem:start(?MODULE, [rebirth, State, Borders], []).
    io:format("rebirth Drone state:~p~n",[State]),
    gen_statem:start_link(?MODULE, [rebirth, State, Borders], []).

stop() ->
    gen_statem:stop(?MODULE).

% Initialization of the state machine
%%%-------------------------------------------------------------------
%%% @doc Initialize the state machine.
%%% minimial init([Location,GS_PID,State]) 
%%% full init([Location,GS_PID,Follower_PID,Self_id,State,Waypoint,Waypoints_stack])
%%% @end
%%% '


init([rebirth | [Internal_state, Borders]]) -> % needed for pattern match on rebirth
    lists:foreach(fun({Key, Value}) -> put(Key, Value) end, Internal_state),
    put(borders, Borders),
    ID = get(id),
    io:format("Drone ~p is reborn in node ~p, PID: ~p~n", [ID, node(), self()]),
    case ID of
        0 ->
            State = leader;
        _ ->
            State = slave
    end,
    {ok, State, [],[{state_timeout, ?TIMEOUT, time_tick}]};


init([#drone{id = ID, location = Location, theta = Theta, speed= Speed}=Drone, Borders]) ->
    io:format("Init~n"),
    put(location, Location),
    put(id, ID),
    put(theta, degree_to_radian(Theta)),
    put(speed, Speed),
    put(waypoint, {Location, Theta}),
    put(borders, Borders),
    case ID of
        0 ->
            State = leader,
            put(state,leader);
        _ ->
            State = slave,
            put(state,slave),
            indentation_update()
    end,
    gen_server:cast(gs_server, {drone_update, Drone}),
    logger("1"),
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
    % io:format("Drone ~p :slave state_timeout~n",[get(id)]),
    step(slave),
    {Found,Result} = look_for_target(),
    % io:format("Drone ~p :slave state_timeout target:~p~n",[get(id),Found]),
    Check_borders = check_borders(),
    if
        Found == found_target -> 
            io:format("Drone ~p :found target ~p~n",[get(id),Result]),
            gen_server:cast(gs_server, {target_found, Result}),
            logger("1"),
            {keep_state,_Data,[{state_timeout, ?TIMEOUT, time_tick}]};
        Check_borders == ok->
            {keep_state, _Data,[{state_timeout, ?TIMEOUT, time_tick}]};
        Check_borders == terminate ->
            {stop, normal, _Data};
        true ->
            {keep_state, _Data,[{state_timeout, ?TIMEOUT, time_tick}]}
    end;



slave({call,_From},{vector_update, {Leader_Location,Leader_Theta}},_Data) -> % test
    gen_statem:reply(_From, ok),    
    io:format("Drone ~p :call vector_update in slave~n",[get(id)]),
    waypoint_update(Leader_Location, Leader_Theta),
    {WP_Position, WP_Theta}= get(waypoint),
    case get(followers) of
        undefined ->
            io:format("followers is undefined~n");
        []->
            io:format("followers is empty~n");
        [_|_] = Followers ->
            update_neighbors(Followers, WP_Position, WP_Theta) 
    end,

    {keep_state,[]};



slave(cast,{update_value,{Key,Value}},_From) -> % test
    io:format("cast update_value in slave Key:~p ,Value:~p~n",[Key,Value]),
    if
        Key == targets ->
            io:format("Drone ~p :update targets to ~p~n",[get(id),Value]),
            case get(targets) of
                undefined ->
                    put(targets,[Value]);
                Exsiting_targets ->
                    put(targets,Exsiting_targets ++ [Value])
            end;
        true ->
            put(Key,Value)
    end,
    {keep_state,[]};

slave(cast,{replace_neighbour,{Reborn_ID,New_PID}},_From) ->
    replace_dead_neighbour(Reborn_ID, New_PID),
    {keep_state,[]};
% slave(cast,{aquire_targets, Target_Locations},_From)->%%target location is a list of {x,y}
%     io:format("Drone ~p :aquire_target at ~p~n",[get(id),Target_Locations]),
%     case get(targets) of
%         undefined ->
%             put(targets,Target_Locations);
%         Exsiting_targets ->
%             put(targets,Exsiting_targets ++ Target_Locations)
%         end,
%     {keep_state,[]};
slave(cast,{target_found,Target},_From)->
    Current_targets = get(targets),
    Next_targets = lists:delete(Target,Current_targets),
    put(targets,Next_targets),
    {keep_state,[]};
% slave(cast,{back_to_normal,_Target},_From)->
%     put(targets,undefined),
%     {keep_state,[],[{state_timeout, ?TIMEOUT, time_tick}]};
% slave(info,back_to_normal,_Data)->
%     put(targets,undefined),
%     {next_state,get(state),[],[{state_timeout, ?TIMEOUT, time_tick}]};

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
            update_gs(),
            case get(followers) of
                undefined ->
                    io:format("followers is undefined~n");
                []->
                    io:format("followers is empty~n");
                [_|_] = Followers ->
                    update_neighbors(Followers, get(location), get(theta))
            end
    end,
    {Found,Result} = look_for_target(),
    Check_borders = check_borders(),
    if
        Found == found_target -> 
            io:format("Drone ~p :found target ~p~n",[get(id),Result]),
            % gen_server:cast(gs_server, {target_found, Target}),
            {keep_state, _Data,[{state_timeout, ?TIMEOUT, time_tick}]};
        Check_borders == ok->
            {keep_state, _Data,[{state_timeout, ?TIMEOUT, time_tick}]};
        Check_borders == terminate ->
            {stop, normal, _Data};
        true ->
            {keep_state, _Data,[{state_timeout, ?TIMEOUT, time_tick}]}
    end;

leader(cast,{update_value,{Key,Value}},_From) ->
    io:format("cast update_value in leader Key: ~p, Value: ~p ~n", [Key,Value]),
    if
        Key == targets ->
            io:format("Drone ~p :update targets to ~p~n",[get(id),Value]),
            case get(targets) of
                undefined ->
                    put(targets,[Value]);
                Exsiting_targets ->
                    put(targets,Exsiting_targets ++ [Value])
            end;
        true ->
            put(Key,Value)
    end,
    {keep_state,[]};

leader(cast,{append_circle, [H|_]=List},_From) when is_list(List)->
    Way_points = get(waypoints_stack),
    case get(waypoints_stack) of
        undefined ->
            put(waypoints_stack,List);
        Exsiting_waypoints ->
            {Location,_} = H,
            Current_WP = get(waypoint),
            put(waypoint,{Location,get_theta_to_wp()}),
            put(waypoints_stack,List ++ [Current_WP]++Exsiting_waypoints)
    end,
    {keep_state,[]};

leader(cast,{replace_neighbour,{Reborn_ID,New_PID}},_From) ->
    replace_dead_neighbour(Reborn_ID, New_PID),
    {keep_state,[]};
% leader(cast,{target_found,Target},_From)->
%     {next_state,circle,[{state_timeout, ?TIMEOUT, Target}]};
% leader(cast,{back_to_normal,_Target},_From)->
%     put(targets,[]),
%     next_waypoint(),
%     {keep_state,[],[{state_timeout, ?TIMEOUT, time_tick}]};

leader(_Event, _From, _Data) ->
    io:format("unknown event ~p in leader~n", [_Event]),
    {keep_state, _Data, [{state_timeout, ?TIMEOUT, time_tick}]}.

%%%===================================================================
%%%target functions
%%%
% circle(back_to_normal,_,_)->
%     step(get(state)),
%     {next_state, get(state),[{state_timeout, ?TIMEOUT, time_tick}]};
% circle(state_timeout,Target, _From) ->
%     step(circle,Target),
%     {keep_state, Target,[{state_timeout, ?TIMEOUT, Target}]};
% circle(info,back_to_normal,_From) ->
%     put(targets,undefined),
%     {next_state, get(state),[{state_timeout, ?TIMEOUT, time_tick}]};
% circle(cast,{target_found,Target},_From) ->
%     {keep_state, Target,[{state_timeout, ?TIMEOUT, Target}]};


% circle(Event, Target, From) ->
%     io:format("unknown event ~p in circle~n", [Event]),
%     {keep_state, Target,[{state_timeout, ?TIMEOUT, Target}]}.

%%%===================================================================
%%% Internal functions

get_theta_to_wp() ->
    {{X_old,Y_old},_}=get(waypoint),
    {X_new,Y_new} = get(location),
    X = X_old - X_new,
    Y = Y_old - Y_new,
    math:atan2(Y , X).

next_waypoint() ->%%that function is only for leader state
    case get(waypoints_stack) of
        undefined ->
            ok;
        [] -> io:format("FORBIDDEN CASE - waypoints_stack is empty~n");
        [{Location,K}|T] when is_atom(K)->
            put(waypoints_stack,T),
            put(waypoint,{Location,K}),
            put(theta, get_theta_to_wp());
        [{_,K}=H|T] when is_number(K) ->
            put(waypoints_stack, T ++ [H]),
            put(waypoint, H),
            put(theta, get_theta_to_wp())
    end.
get_distance({{X1,Y1},_},{X2,Y2}) ->
    math:sqrt((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2)).

rotation_matrix({X,Y},Theta)->
    {X_new,Y_new} = {X*math:cos(Theta)-Y*math:sin(Theta), X*math:sin(Theta)+Y*math:cos(Theta)},
    {X_new,Y_new}.

waypoint_update({X,Y},Leader_Theta) ->
    {X_new,Y_new} = rotation_matrix(get(indentation), Leader_Theta),
    Theta_to_wp = get_theta_to_wp(),
    New_Waypoint ={{X-X_new,Y-Y_new},Leader_Theta},
    Interception_point = calculate_interception_point(New_Waypoint),
    put(waypoint,Interception_point),
    Theta_to_wp.

indentation_update() ->
    {INDENTATION_X, INDENTATION_Y} = ?INDENTATION,
    put(indentation,{INDENTATION_X,INDENTATION_Y*math:pow(-1,get(id))}).


calculate_interception_point({{WP_X,WP_Y}, WP_Theta}=Waypoint) ->
    Position = get(location),
    Distance = get_distance(Waypoint,Position),
    ToT = 3/Distance,
    X_future = WP_X + ToT*math:cos(WP_Theta),
    Y_future = WP_Y + ToT*math:sin(WP_Theta),
    % Theta_to_future_WP = math:atan2(Y_future - Y , X_future - X),
    {{X_future,Y_future},WP_Theta}.


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
    Current_speed = get(speed),
    if
        Distance_to_waypoint > ?STEP_SIZE ->% speed must not be larger than 2!!! otherwise unstable system
            put(speed,2),
            2;
        Distance_to_waypoint < ?STEP_SIZE andalso Current_speed == 0 -> % might be problematic we need a range
            put(speed,0),
            0;
        true ->
            put(speed,1),
            increment_waypoint(),
            1
    end.



increment_waypoint()->
    {{_,_}, Theta} = get(waypoint),
    {X,Y} = get(location),
    New_X = X+?STEP_SIZE*math:cos(Theta),
    New_Y = Y+?STEP_SIZE*math:sin(Theta),
    put(waypoint,{{New_X,New_Y},Theta}). %update waypoint


step(leader)->
    Theta = get_theta_to_wp(),
    put(speed,1),
    {X,Y} = get(location),
    put(location,{X+?STEP_SIZE*math:cos(Theta),Y+?STEP_SIZE*math:sin(Theta)}); %update location


step(slave)->
    % io:format("Drone ~p is stepping~n",[get(id)]),
    Old_speed = get(speed),
    Old_theta_deg = radian_to_degree(get(theta)),
    {X,Y} = get(location),
    Speed = calculate_speed(),
    Theta = get_theta_to_wp(),
    Theta_deg = radian_to_degree(Theta),
    put(theta,Theta),
    put(location,{X+Speed*?STEP_SIZE*math:cos(Theta), Y+Speed*?STEP_SIZE*math:sin(Theta)}),
    if
        Old_theta_deg /= Theta_deg orelse Old_speed/=Speed ->
            update_gs();
        true ->
            ok
    end.

step(circle,{X,Y})->
    Distance_to_target = get_distance({get(location),0},{X,Y}),
    case Distance_to_target > ?SERACH_RADIUS of
        true ->
            put(waypoint,{{X,Y},0}),
            {X,Y} = get(location),
            Theta = get_theta_to_wp(),
            put(waypoint,{{X,Y},Theta}),
            put(location,{X+?STEP_SIZE*2*math:cos(Theta),Y+?STEP_SIZE*2*math:sin(Theta)});
        false ->
            {X1,Y1}=get(location),
            Current_angle = math:atan2(X1-X,Y1-Y),
            New_angle = Current_angle + math:pi()/20,
            Next_WP = {X+?SERACH_RADIUS*math:cos(New_angle),Y+?SERACH_RADIUS*math:sin(New_angle)},
            put(waypoint,{Next_WP,0}),
            Theta = get_theta_to_wp(),
            put(waypoint,{Next_WP,Theta}),
            put(location,{X+?STEP_SIZE*math:cos(Theta),Y+?STEP_SIZE*math:sin(Theta)}),
            update_gs()
    end.


radian_to_degree(Radian) when is_atom(Radian)->
    radian_to_degree(get(theta));
radian_to_degree(Radian) ->
    DegreesFloat = Radian * (180/math:pi()),
    round(DegreesFloat).

degree_to_radian(Degree) ->
    Degree * (math:pi()/180).

update_neighbors([], _, _) ->
    ok;
update_neighbors([{Neighbor_ID,PID}|T], Location, Theta)->
    try
        % io:format("calling ~p~n",[PID]),
        logger("1"),
        gen_statem:call(PID, {vector_update, {Location,Theta}}), % returns call dirty after returning to GS1 from GS4 and trying to update
        update_neighbors(T, Location, Theta)
    catch
        exit:{noproc, _} ->
            io:format("Requesting new PID~n"),
            logger("1"),
            {ok,New_PID} = gen_server:call(gs_server, {dead_neighbour, Neighbor_ID}),
            replace_dead_neighbour(Neighbor_ID,New_PID),
            update_neighbors([New_PID | T], Location, Theta);
        Error:Kind->
            io:format("other error ~p:~p~n",[Error,Kind]),
            logger("1"),
            {ok,New_PID} = gen_server:call(gs_server, {dead_neighbour, Neighbor_ID}),
            replace_dead_neighbour(Neighbor_ID,New_PID),
            update_neighbors([New_PID | T], Location, Theta)

    end.


replace_dead_neighbour(ID, New_PID) ->
    Neighbors = get(followers),
    New_Neighbors = lists:map(fun({Neighbor_ID,PID}) -> if Neighbor_ID == ID -> {Neighbor_ID, New_PID}; true -> {Neighbor_ID,PID} end end, Neighbors),
    put(followers, New_Neighbors).

update_gs() ->
    Theta = radian_to_degree(get(theta)),
    {{Wp_X, Wp_Y},Wp_rad} = get(waypoint),
    Wp_deg = radian_to_degree(Wp_rad),
    Waypoint = {{Wp_X, Wp_Y}, Wp_deg},
    gen_server:cast(gs_server, {drone_update, #drone{id = get(id), location = get(location), theta = Theta, speed = get(speed), next_waypoint=Waypoint,gs_server=node(),time_stamp=get_time()}}),
    logger("1").

check_borders() ->
    Border_record = get(borders),
    #borders{left=Left,right=Right,top = Top,bottom = Bottom}=Border_record,
    {X,Y} = Location = get(location),
    case X=<Left orelse X>=Right orelse Y=<Bottom orelse Y>=Top of
        false ->
            ok;
        true ->
            cross_border(Location)
    end.



cross_border(Location) ->
    % get all key value pairs from process dictionary in a list
    case process_info(self(), dictionary) of
        {dictionary, Dict} -> terminate;
        Dict ->  terminate
    end,
    % gen_server:call and grab the reply
    % returns 'ok' or 'terminate' atom
    % io:format("Crossing border and my neighbours are~p~n",[Dict]),
    logger("1"),
    gen_server:call(gs_server, {crossing_border, get(id), Location,Dict}, infinity).

    % {stop,normal,Location}.





get_value(Key, [{Key, Value} | _]) ->
    Value;
get_value(Key, [_ | Rest]) ->
    get_value(Key, Rest);
get_value(_, []) ->
    not_found.



look_for_target() ->
    case get(targets) of
        undefined ->
            {not_found,ok};
        Target ->
            look_for_target(Target,get(location))
        end.

look_for_target([],_)->
    {not_found,ok};
look_for_target([Target|Rest],Location)-> 
    Distance = get_distance({Location,0},Target),
    io:format("Distance to target is ~p~n",[Distance]),
    case  Distance =< ?SERACH_RADIUS of
        true ->
            io:format("Drone ~p has reached target at location:~p ~n",[get(id),Target]),
            {found_target,Target};
        false ->
            look_for_target(Rest,Location)
    end.

logger(Message) ->
    {_,Time} = calendar:local_time(),
    % make a string of "drone" and Id
    Name = lists:flatten(io_lib:format("drone ~p",[get(id)])),
    Log = #log_message{time=Time, source = Name, message = Message},
    gen_server:cast(gs_server, Log).

get_time()->%%in milliseconds-needs to be verified
    Time_in_nano = erlang:monotonic_time(),
    Time_in_nano/1000000.