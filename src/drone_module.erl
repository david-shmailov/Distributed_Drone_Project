-module(drone_module).
-author("Neriya").


-behaviour(gen_statem).

%% External API
-export([start_link/0, trigger_event/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State record - define your FSM states here
-record(state, {leader
    % Add any other state-specific data here
}).

%%  Drone data record structure
%% -ground_station_id - PID
%% -drone_to_follow_id -PID
%% -velocity -{velocity_x,velocity_y} - {int,int}
%% -location - {location_x,location_y} - {int,int}
%% - order_number -int

%% External API

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

trigger_event(Pid, Event) ->
    gen_statem:cast(Pid, {trigger_event, Event}).

%% gen_statem callbacks

init([]) ->
    InitialState = #state{
        current_state = initial_state,
        % Initialize any other state-specific data here
    },
    {ok, InitialState}.

handle_call(_Request, _From, State) ->
    %% No synchronous calls expected in this FSM, just ignore the call.
    {noreply, State}.

handle_cast({trigger_event, Event}, State) ->
    %% Handle the event and transition to the new state
    NewState = handle_event(Event, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    %% Unknown message, ignore it
    {noreply, State}.

handle_info(_Info, State) ->
    %% No special handling of Erlang system messages
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Perform any cleanup actions if needed
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% Code change handling, usually not needed for FSM
    {ok, State}.

%% FSM Logic

handle_event(Event, State) ->
    %% Define your FSM logic here, including state transitions based on events
    case {State#state.current_state, Event} of
        %% Example transition from state1 to state2 on receiving 'event1'
        {state1, event1} -> handle_event1(State);
        %% Add more transitions as needed
        _ -> State %% No state change for unknown events, return the same state
    end.

%% Define state transition functions here

handle_event1(State) ->
    %% Perform actions for transitioning from state1 to state2
    NewState = State#state{current_state = state2},
    NewState.

%% Add more state transition functions as needed for other states and events.

%% FSM States

initial_state() -> state1.
state1() -> state2.
state2() -> state1.
%% Add more states as needed.

update_location() ->
    {Velocity_x,Velocity_y} = get(velocity),
    {Location_x,Location_y} = get(location),
    put(location,{Location_x+Velocity_x,Location_y+Velocity_y}).

update_velocity({New_velocity_x,New_velocity_y}) ->
    put(velocity,{New_velocity_x,New_velocity_y}).

    %% Update the location of the drone
    