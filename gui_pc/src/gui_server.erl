
-module(gui_server).

-behaviour(gen_server).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket = undefined}).
-define(RETRY_DELAY, 5000).
-record(drone, {id, location, theta, speed}).


start_link(Host, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).



init([Host, Port]) ->
    % send_message_to_gs({establish_comm, <<"hello world">>}),
    connect_with_retry(Host, Port, ?RETRY_DELAY).

connect_with_retry(Host, Port, Delay) ->
    case gen_tcp:connect(Host, Port, [binary, {active, false}]) of
        {ok, Socket} -> 
            {ok, #state{socket = Socket}};
        {error, econnrefused} ->
            io:format("Connection refused. Retrying in ~p milliseconds.~n", [Delay]),
            timer:sleep(Delay),
            connect_with_retry(Host, Port, Delay);
        {error, OtherReason} ->
            io:format("Error: ~p~n", [OtherReason]),
            {stop, OtherReason}
    end.




send_message_to_gs(Message) ->
    % Iterate over all GS nodes and send them the message.
    Nodes = ['gs1@localhost', 'gs2@localhost', 'gs3@localhost', 'gs4@localhost'], 
    Answers = [lists:flatten(gen_server:call({gs_server, Node}, Message)) || Node <- Nodes],
    io:format("Answers: ~p~n", [Answers]),
    ok.


handle_call({send_data, Data}, _From, #state{socket = Socket} = State) ->
    case gen_tcp:send(Socket, Data) of
        ok -> {reply, ok, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast({drone_update, Drone}, State) when is_record(Drone,drone) ->
    io:format("Drone update: ~p~n", [Drone]),
    Binary = term_to_binary(drone_to_list(Drone)),
    send_to_gui(Binary, State),
    {noreply, State};

handle_cast(_Msg, State) ->
    io:format("Unknown message: ~p~n", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions


drone_to_list(Drone) ->
    [Drone#drone.id, Drone#drone.location, Drone#drone.theta, Drone#drone.speed].
    


send_to_gui(Data, #state{socket = Socket} = State) ->
    case gen_tcp:send(Socket, Data) of
        ok -> {reply, ok, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.