-module(drone_module).
-behaviour(gen_statem).
-define(INDENTATION,{0,5}).
-define(STEP_SIZE,1).

-export([start_link/1,test/0]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([leader/3, slave/3]).



-record(data, {
    timeout_ref,
    location,
    velocity,
    waypoint,
    gs_pid,
    follower_pid,
    self_id,
    points_to_follow
}).

test() ->
    io:format("test~n"),
    start_link([{0,0},0,0,0,'leader',{1,1},[{1,1},{3,2},{5,1},{1,5},{10,1},{1,10},{20,2},{5,30},{50,50}]]).
start_link(Data) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Data, []).

init([Location,GS_PID,Follower_PID,Self_id,State,Waypoint,Points_to_follow]) ->
    {ok, State, #data{  timeout_ref=reset_timeout(),
                        location=Location,
                        velocity=0,
                        waypoint=Waypoint,
                        gs_pid=GS_PID,
                        follower_pid=Follower_PID,
                        self_id=Self_id,
                        points_to_follow=Points_to_follow}
                    }. % Initially a slave with a 5-second time_tick.

callback_mode() ->
    state_functions.

reset_timeout() ->
    io:format("reset_timeout~n"),
    {keep_state, 5000, time_tick}.
    % erlang:send_after(100, self(), time_tick).
    

%% API functions

become_leader() ->
    gen_statem:call(?MODULE, become_leader).

become_slave() ->
    gen_statem:call(?MODULE, become_slave).

%%% State callbacks
%%% 
%%% Leader


leader(enter, _From, Data) ->
    {keep_state, Data};

leader(time_tick, _From, Data) -> % time_tick in leader state
    io:format("time_tick in leader state~n"),
    New = step(leader,Data),
    New_Data = New#data{timeout_ref=reset_timeout()},
    {keep_state, New_Data};

leader(become_slave, _From, Data) ->
    {next_state, slave, [{state_timeout, 5000, time_tick}]}; % Reset time_tick

leader(become_leader, _From, Data) ->
    {keep_state, Data};

leader(Event, _From, Data) -> % Catch-all for other events
    io:format("Unhandled event in leader~n~p~n",[Event]),
    {keep_state, Data}.



%%% Slave


slave(enter, _From, Data) ->
    {keep_state, Data};

slave(state_timeout, State, Data) -> % time_tick in slave state
    io:format("time_tick in slave state~n"),
    {next_state, leader, Data};

slave(become_leader, _From, Data) ->
    {next_state, leader, Data};

slave(become_slave, _From, Data) ->
    {keep_state, Data};

slave(time_tick, _From, Data) ->
    io:format("time_tick in slave state~n"),
    % reset_timeout(),
    New = step(slave,Data),
    New_Data = New#data{timeout_ref=reset_timeout()},
    io:format("The location:~p",[New_Data#data.location]),
    {keep_state, New_Data};

slave({vector_update,{Location,Theta}}, _From,Data) ->
    New_Data = waypoint_update(Location, Theta, Data),
    %%% send the new theta and location to the follower
    gen_statem:cast(Data#data.follower_pid, {vector_update,{New_Data#data.location,get_theta(New_Data)}}),
    {keep_state,New_Data};


slave(_Event, _From, Data) -> % Catch-all for other events
    io:format("Unhandled event in slave~n"),
    {keep_state_and_data, Data}.
%%[{state_timeout, 5000, time_tick}]}

%%% Boilerplate callbacks

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.



%%% internal functions
%%% 
%%% step generic for both slave and leader, but the velocity(or step size) is different
%%%               : step should be called in time_tick

step(State,Data)->
    case State of
        leader ->
            Angle = get_theta(Data),
            {X,Y} = Data#data.location,
            Distance_to_waypoint = get_distance(Data#data.waypoint,Data#data.location),
            case Distance_to_waypoint>=?STEP_SIZE of
                true->
                    Data#data{location={X+?STEP_SIZE*math:cos(Angle),Y+?STEP_SIZE*math:sin(Angle)}}; %update location
                false->
                    step(State,next_waypoint(Data))
            end;

        slave ->
            %the slave will get to his waypoint in a single step and update the waypoint to be a single step size away from his new location
            {X,Y} = Data#data.waypoint,
            Angle = get_theta(Data),
            Data#data{location={X,Y},waypoint={X+?STEP_SIZE*math:cos(Angle),Y+?STEP_SIZE*math:sin(Angle)}}
    end.




next_waypoint(Data) ->%%that function is only for leader state
    case Data#data.points_to_follow of
        [] -> Data#data.waypoint;
        [H|T] ->
             Data#data{waypoint=H,points_to_follow=T}
    end.

get_distance({X1,Y1},{X2,Y2}) ->
    math:sqrt((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2)).

get_theta(Data) ->
    {X_old,Y_old} =Data#data.waypoint,
    {X_new,Y_new} = Data#data.location,
    X = X_old - X_new,
    Y = Y_old - Y_new,
    Angle = math:atan(Y / X),
    case X >= 0 of
        true -> Angle;
        false -> Angle + math:pi()
    end.

rotation_matrix({X,Y},Theta)->
    {X_new,Y_new} = {X*math:cos(Theta)+Y*math:sin(Theta),Y*math:cos(Theta)-X*math:sin(Theta)},
    {X_new,Y_new}.
waypoint_update({X,Y},Theta,Data) ->
    {X_new,Y_new} = rotation_matrix(?INDENTATION, Theta),
    % {X_old,Y_old}= Data#data.waypoint,
    Data#data{waypoint={X-X_new,Y-Y_new}}.

