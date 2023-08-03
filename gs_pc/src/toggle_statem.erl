-module(toggle_statem).
-behaviour(gen_statem).

% API functions
-export([start_link/0, stop/0]).

% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0, code_change/4]).
-export([slave/3, leader/3]).

-define(TIMEOUT, 4000).
-define(INDENTATION,{0,5}).
-define(STEP_SIZE,1).

%%Location,GS_PID,Follower_PID,Self_id,State,Waypoint,Points_to_follow
start_link() ->
    % List = [{0,0},'gs_server',0,0,'slave',{1,1},[{1,1},{3,2},{5,1},{1,5},{10,1},{1,10},{20,2},{5,30},{50,50}]],
    List = [{0,0},'gs_server',slave,{0,0}],
    gen_statem:start_link(?MODULE, List, []).

stop() ->
    gen_statem:stop(?MODULE).

% Initialization of the state machine
%%%-------------------------------------------------------------------
%%% @doc Initialize the state machine.
%%% minimial init([Location,GS_PID,State]) 
%%% full init([Location,GS_PID,Follower_PID,Self_id,State,Waypoint,Points_to_follow])
%%% @end
init([Location,GS_PID,State,Waypoint])->
    io:format("Init~n"),
    put(location, Location),
    put(gs_pid, GS_PID),
    put(state, State),
    put(waypoint, Waypoint),
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
    {keep_state,[]};
slave(cast,{follower_update,PID},_From)->
    io:format("cast follower_update in slave~n"),
    put(follower_pid, PID),
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
    step(get(state)),
    io:format("location is ~p~n", [get(location)]),
    {keep_state,_Data,[{state_timeout, ?TIMEOUT, time_tick}]};
leader(cast,{update_value,{Key,Value}},_From) ->
    io:format("cast update_value in leader~n"),
    put(Key,Value),
    {keep_state,[]};

leader(_Event, _Data, _From) ->
    io:format("unknown event ~p in leader~n", [_Event]),
    {keep_state, _Data, [{state_timeout, ?TIMEOUT, time_tick}]}.

% handle_common(cast, _Event, _From, _Data) ->
%     io:format("cast event ~p~n", [_Event]),
%     io:format("the data is ~p~n", [_Data]),
%     {next_state,_Event, _Data};

% handle_common(_Type, _Event, _From, _Data) ->
%     io:format("unknown event ~p~n", [_Event]),
%     {keep_state, _Data}.

%gen_statem:cast('toggle_statem',{vector_update,{{30,40},2}})

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
    put(waypoint,{X-X_new,Y-Y_new}).

step(State)->
    case State of
        leader ->
            Angle = get_theta(),
            {X,Y} = get(location),
            Distance_to_waypoint = get_distance(get(waypoint),get(location)),
            case Distance_to_waypoint>=?STEP_SIZE of
                true->
                    put(location,{X+?STEP_SIZE*math:cos(Angle),Y+?STEP_SIZE*math:sin(Angle)}); %update location
                false->
                    next_waypoint(),
                    step(State)
            end;

        slave ->
            Distance_to_waypoint = get_distance(get(waypoint),get(location)),
            %the slave will get to his waypoint in a single step and update the waypoint to be a single step size away from his new location
            case Distance_to_waypoint >= ?STEP_SIZE of
                true ->
                    {X,Y} = get(waypoint),
                    Angle = get_theta(),
                    put(location,{X,Y}),
                    io:format("the waypoint is~p~n",[{X+?STEP_SIZE*math:cos(Angle),Y+?STEP_SIZE*math:sin(Angle)}]),
                    put(waypoint,{X+?STEP_SIZE*math:cos(Angle),Y+?STEP_SIZE*math:sin(Angle)});
                false ->
                    put(location,get(waypoint))
                end
    end.