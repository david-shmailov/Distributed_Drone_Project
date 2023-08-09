
-module(gui_server).
-include("../project_def.hrl").
-behaviour(gen_server).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {out_port, in_port, input_socket = undefined}).


start_link(Port_erl2py,Port_py2erl) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port_erl2py,Port_py2erl], []).



init([Port_erl2py,Port_py2erl]) ->
    {ok, Socket} = open_socket_for_listener(Port_py2erl),
    {ok, #state{out_port = Port_erl2py, in_port = Port_py2erl, input_socket = Socket}}.




% send_message_to_gs(Message) ->
%     % Iterate over all GS nodes and send them the message.
%     Nodes = ['gs1@localhost', 'gs2@localhost', 'gs3@localhost', 'gs4@localhost'], 
%     Answers = [lists:flatten(gen_server:call({gs_server, Node}, Message)) || Node <- Nodes],
%     io:format("Answers: ~p~n", [Answers]),
%     ok.


%% gen_server callbacks

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast({drone_update, Stack}, State) ->
    send_stack_to_gui(Stack, State),
    {noreply, State};

handle_cast(_Msg, State) ->
    io:format("Unknown message: ~p~n", [_Msg]),
    {noreply, State}.


handle_info({udp, Socket, _Host, _Port, Data}, #state{input_socket = Socket} = State) ->
    io:format("Received: ~p~n", [Data]),
    {noreply, State};


handle_info(_Info, State) ->
    io:format("Unknown info: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{input_socket = Socket}) ->
    if Socket =/= undefined ->
        gen_udp:close(Socket)
    end,
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions


send_stack_to_gui([Drone| Rest], State) ->
    io:format("Drone: ~p~n", [Drone]),
    Binary = drone_to_binary(Drone),
    send_to_gui(Binary, State),
    % todo stress check to see if we need to add a delay
    send_stack_to_gui(Rest, State);

send_stack_to_gui([], _State) ->
    ok.



% drone_to_binary(Drone) ->
%     List = [Drone#drone.id] ++ tuple_to_list(Drone#drone.location) ++ [Drone#drone.theta, Drone#drone.speed],
%     String = string:join([integer_to_list(X) || X <- List], ","),
%     list_to_binary(String).
    
drone_to_binary(Drone) ->
    Waypoint_flat = flatten_waypoint(Drone#drone.next_waypoint),
    List = [Drone#drone.id] ++ tuple_to_list_float(Drone#drone.location) ++ [Drone#drone.theta, Drone#drone.speed] ++ Waypoint_flat,
    String = string:join([number_to_string(X) || X <- List], ","),
    list_to_binary(String).

tuple_to_list_float({X, Y}) ->
    [X, Y].

flatten_waypoint({{Wp_X, Wp_Y}, Wp_deg}) ->
    [Wp_X, Wp_Y, Wp_deg].

number_to_string(Num) when is_integer(Num) ->
    integer_to_list(Num);
number_to_string(Num) when is_float(Num) ->
    float_to_list(Num).


send_to_gui(Data, #state{out_port = Port}) ->
    % Create a socket (this doesn't bind to the out_port, it's just for sending)
    {ok, Socket} = gen_udp:open(0),
    gen_udp:send(Socket, "localhost", Port, Data),
    gen_udp:close(Socket).




open_socket_for_listener(In_Port) ->
    % Open a port to the python listener
    {ok, _Socket} = gen_udp:open(In_Port, [binary, {active,true}]).



init_tables() ->
    GS1_ETS = ets:new(gs1_ets, [named_table, public, {write_concurrency, true}]),
    GS2_ETS = ets:new(gs2_ets, [named_table, public, {write_concurrency, true}]),
    GS3_ETS = ets:new(gs3_ets, [named_table, public, {write_concurrency, true}]),
    GS4_ETS = ets:new(gs4_ets, [named_table, public, {write_concurrency, true}]), % Todo figure out if you really need write_concurrency
    {GS1_ETS, GS2_ETS, GS3_ETS, GS4_ETS}.