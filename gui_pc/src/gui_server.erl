
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

-record(state, {out_port, in_port, input_socket = undefined, log_fd = undefined, gs_nodes = [], waypoints = []}).


start_link(Port_erl2py,Port_py2erl) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port_erl2py,Port_py2erl], []).



init([Port_erl2py,Port_py2erl]) ->
    {ok, Socket} = open_socket_for_listener(Port_py2erl),
    {ok, File}=file:open(?LOG_NAME, [write]),
    {ok, #state{out_port = Port_erl2py, in_port = Port_py2erl, input_socket = Socket, log_fd = File}}.




% send_message_to_gs(Message) ->
%     % Iterate over all GS nodes and send them the message.
%     Nodes = ['gs1@localhost', 'gs2@localhost', 'gs3@localhost', 'gs4@localhost'], 
%     Answers = [lists:flatten(gen_server:call({gs_server, Node}, Message)) || Node <- Nodes],
%     io:format("Answers: ~p~n", [Answers]),
%     ok.


%% gen_server callbacks

handle_call({establish_comm, Node, GS_location}, _From, #state{gs_nodes = Nodes} = State) ->
    io:format("Establishing comm with ~p~n", [Node]),
    Command = io_lib:format("establish_comm , ~p,~p", [Node, GS_location]),
    send_to_gui(Command ,State),
    {reply, ok, State#state{gs_nodes = [Node|Nodes]}}; % add the node to the list of nodes

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast({drone_update, Stack}, State) ->
    send_stack_to_gui(Stack, State),
    {noreply, State};

handle_cast(Message, #state{log_fd=File}=State)  when is_record(Message, log_message)->
    io:format(File, "~p~n", [Message]),
    {noreply, State};


handle_cast(_Msg, State) ->
    io:format("Unknown message: ~p~n", [_Msg]),
    {noreply, State}.


handle_info({udp, Socket, _Host, _Port, Data}, #state{input_socket = Socket} = State) ->
    io:format("Received: ~p~n", [Data]),
    % assumes Data is only one dictionary pair of python! the key should be converted to atom
    Gui_MSG = parse_pair(binary_to_list(Data)),
    New_state = handle_gui_msg(Gui_MSG, State),
    {noreply, New_state};


handle_info(_Info, State) ->
    io:format("Unknown info: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{input_socket = Socket, log_fd = File}) ->
    if Socket =/= undefined ->
        gen_udp:close(Socket)
    end,
    if File =/= undefined ->
        file:close(File)
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

parse_pair(String) ->
    % Remove leading and trailing curly braces
    Stripped = string:strip(string:strip(String, both, $}), both, ${),

    % Convert each key-value pair to {key, value}
    [KeyStr, ValueStr] = string:tokens(Stripped, ":"),
    % extract the string between ' '
    CleanKey = string:strip(string:strip(KeyStr, both, $'), both, $'),
    Key = list_to_atom(CleanKey),
    Value = string:strip(string:strip(ValueStr,both), both, $'),

    {Key, Value}.

handle_gui_msg({add_waypoint, Msg} , #state{waypoints = Waypoints} = State) ->
    Pattern = "\\((\\d+\\.?\\d*)\\s*,\\s*(\\d+\\.?\\d*)\\)",
    case re:run(Msg, Pattern, [{capture, all_but_first, list}]) of
        {match, [Wp_X, Wp_Y]} ->
            Waypoint = {{list_to_float(Wp_X), list_to_float(Wp_Y)}, 0},
            State#state{waypoints = Waypoints ++ [Waypoint]}; % update state
        nomatch ->
            io:format("Invalid waypoint: ~p~n", [Msg]),
            State % return state unchanged
    end;

handle_gui_msg({add_target, Msg} , #state{gs_nodes = [GS | _]} = State) ->
    Pattern = "\\((\\d+\\.?\\d*)\\s*,\\s*(\\d+\\.?\\d*)\\)",
    case re:run(Msg, Pattern, [{capture, all_but_first, list}]) of
        {match, [Target_X, Target_Y]} ->
            % send to first gs in list, which will spread to others
            gen_server:cast({gs_server, GS}, {aquire_target,{list_to_float(Target_X),list_to_float(Target_Y)}}),
            State; % update state
        nomatch ->
            io:format("Invalid waypoint: ~p~n", [Msg]),
            State % return state unchanged
    end;


handle_gui_msg({set_waypoints, _} , #state{waypoints = Waypoints} = State) ->
    % todo change this to the gs of the leader
    gen_server:call({gs_server, 'gs1@localhost'}, {set_waypoints, Waypoints}),
    State#state{waypoints = []}; % empty waypoints stack


handle_gui_msg({launch_drones, Body}, State) ->
    [GSstr, NumStr] = string:tokens(Body, " "),
    Num_of_drones = list_to_integer(NumStr),
    GS = list_to_atom(GSstr),
    gen_server:call({gs_server, GS}, {launch_drones, Num_of_drones}),
    State;

handle_gui_msg(Unknown, State) ->
    io:format("Unknown message: ~p~n", [Unknown]),
    State.


    
drone_to_binary(#drone{id = ID, location= Location, theta = Theta, speed = Speed, next_waypoint = Waypoint}) ->
    Waypoint_flat = flatten_waypoint(Waypoint),
    List = [ID] ++ tuple_to_list_float(Location) ++ [Theta, Speed] ++ Waypoint_flat,
    String = "drone," ++ string:join([number_to_string(X) || X <- List], ","),
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


