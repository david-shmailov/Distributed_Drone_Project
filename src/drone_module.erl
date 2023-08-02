-module(drone_statem).
-behaviour(gen_statem).

-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([leader/3, slave/3]).



-record(data, {
    timeout_ref,
    location,
    velocity,
    waypoint,
    gs_pid,
    follower_pid,
    self_id
}).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([Location,GS_PID,Follower_PID,Self_id]) ->
    {ok, slave, #data{  timeout_ref=reset_timeout(),
                        location=Location,
                        velocity={0,0},
                        waypoint=Location,
                        gs_pid=GS_PID,
                        follower_pid=Follower_PID,
                        self_id=Self_id}
                    }. % Initially a slave with a 5-second time_tick.

callback_mode() ->
    state_functions.

reset_timeout() ->
    {state_timeout, 5000, time_tick}.

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

leader(time_tick, State, Data) -> % time_tick in leader state
    io:format("time_tick in leader state~n"),
    {next_state, slave, Data};

leader(become_slave, _From, Data) ->
    {next_state, slave, [{state_timeout, 5000, time_tick}]}; % Reset time_tick

leader(become_leader, _From, Data) ->
    {keep_state, Data};

leader(_Event, _From, Data) -> % Catch-all for other events
    io:format("Unhandled event in leader~n"),
    {keep_state_and_data, [{state_timeout, 5000, time_tick}]}.



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
    Location = Data#data.location,
    waypoint_update(Data),
    step(),
    {keep_state, Data};

slave(vector_update, _From, Data) ->
    waypoint_update(Location, Velocity),
    {keep_state, Data};


slave(_Event, _From, Data) -> % Catch-all for other events
    io:format("Unhandled event in slave~n"),
    {keep_state_and_data, [{state_timeout, 5000, time_tick}]}.

%%% Boilerplate callbacks

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.



%%% internal functions
step() ->
    {Velocity_x,Velocity_y} = get(velocity),
    {Location_x,Location_y} = get(location),
    put(location,{Location_x+Velocity_x,Location_y+Velocity_y}).

fast_speed({New_velocity_x,New_velocity_y}) ->
    put(velocity,{New_velocity_x,New_velocity_y}).

normal_speed({New_velocity_x,New_velocity_y}) ->
    put(velocity,{New_velocity_x,New_velocity_y}).

slow_speed({New_velocity_x,New_velocity_y}) ->
    put(velocity,{New_velocity_x,New_velocity_y}).


waypoint_update(Data) -> fuckyou.
