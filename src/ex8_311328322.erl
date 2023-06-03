-module(ex8_311328322).

-export([startChat/1, call/1,steadyLink/1, steadyMon/1]).


startChat({remote, Host, Remote_receiver_pid}) ->
    Table_owner_Pid = spawn(fun () -> table_owner_init() end), % initiates ETS table
    wait_for_table_ready(),
    set_global(remote_node, Host),
    set_global(remote_receiver, Remote_receiver_pid),
    case whereis(local_receiver) of 
        undefined ->
            set_global(received, 0),
            Local_receiver_pid = spawn(fun () -> remote_receiver_init(Remote_receiver_pid) end),
            register(local_receiver, Local_receiver_pid);
        Local_receiver_pid -> ok
    end,

    case whereis(local_sender) of
        undefined ->
            set_global(sent, 0),
            Pid_local_sender = spawn(fun () -> sender_init(Table_owner_Pid, Remote_receiver_pid) end),
            register(local_sender, Pid_local_sender);
        _ -> ok
    end,
    Local_receiver_pid;

startChat(Host) -> 
    Table_owner_Pid = spawn(fun () -> table_owner_init() end), % initiates ETS table
    wait_for_table_ready(),
    set_global(remote_node, Host),
    case whereis(local_receiver) of 
        undefined ->
            Local_receiver_pid = spawn(fun () -> local_receiver_init(Table_owner_Pid) end),
            set_global(received, 0),
            register(local_receiver, Local_receiver_pid);
        Local_receiver_pid -> ok
    end,
    case get_remote_receiver_pid() of 
        undefined ->
            Remote_receiver_pid = rpc:call(Host, ?MODULE, startChat, [{remote, node(), Local_receiver_pid}]),
            set_global(remote_receiver, Remote_receiver_pid),
            set_global(received, 0);
        Remote_receiver_pid -> ok
    end,

    case whereis(local_sender) of
        undefined ->
            set_global(sent, 0),
            Pid_local_sender = spawn(fun () -> sender_init(Table_owner_Pid, Remote_receiver_pid) end),
            register(local_sender, Pid_local_sender);
        Pid_local_sender -> ok
    end,

    Pid_local_sender.


call({remote, Message}) ->
    local_sender ! Message; % this runs on the remote node, it also has its own local_sender process

call(Message) -> 
    Callee_node = get_global(remote_node),
    rpc:call(Callee_node, ?MODULE, call, [{remote,Message}]).


get_remote_receiver_pid() ->
    case ets:member(node_table, remote_receiver) of
        false ->
            undefined;
        true ->
            Remote_receiver_pid = get_global(remote_receiver),
            try
                erlang:is_process_alive(Remote_receiver_pid),
                Remote_receiver_pid
            catch
                _:badarg ->
                    ets:delete(node_table, remote_receiver),
                    undefined
            end
    end.

local_receiver_init(Table_owner_Pid) ->
    erlang:process_flag(trap_exit, true), % needed to trap normal exit signal (trap means its converted into a message)
    link(Table_owner_Pid),
    local_receiver_loop().

local_receiver_loop() ->
    receive
        {'EXIT', _, _} ->
            % io:format("Local receiver exiting via link~n"),
            exit(normal);
        quit ->
            % io:format("Local receiver exiting via quit~n"),
            exit(normal);
        stats ->
            Received = get_global(received),
            Sent = get_global(sent),
            io:format("Local Stats: sent: ~p, received: ~p~n", [Sent, Received]),
            local_receiver_loop();
        Message ->
            Received = get_global(received),
            set_global(received, Received + 1),
            io:format("~p~n", [Message]),
            local_receiver_loop()
    end.

remote_receiver_init(Caller_receiver_pid) ->
    erlang:process_flag(trap_exit, true), % needed to trap normal exit signal (trap means its converted into a message)
    link(Caller_receiver_pid),
    remote_receiver_loop().

remote_receiver_loop() ->
    receive
        {'EXIT', _, _} ->
            % io:format("Remote receiver exiting via link~n"),
            exit(normal);
        quit ->
            % io:format("Remote receiver exiting via quit~n"),
            exit(normal);
        stats ->
            Received = get_global(received),
            Sent = get_global(sent),
            io:format("Remote Stats: sent: ~p, received: ~p~n", [Sent, Received]),
            remote_receiver_loop();
        Message ->
            Received = get_global(received),
            set_global(received, Received + 1),
            io:format("~p~n", [Message]),
            remote_receiver_loop()
    end.

sender_init(Table_owner_Pid, Remote_receiver_pid) ->
    erlang:process_flag(trap_exit, true), % needed to trap normal exit signal (trap means its converted into a message)
    link(Remote_receiver_pid),
    link(Table_owner_Pid),
    sender_loop().

sender_loop() ->
    receive
        {'EXIT', _, _} ->
            % io:format("Sender of ~p exiting via link~n",[node()]),
            exit(normal);
        Message ->
            Remote_receiver = get_global(remote_receiver),
            Remote_receiver ! Message,
            Sent = get_global(sent),
            set_global(sent, Sent + 1),
            sender_loop()
    end.


table_owner_init() ->
    case ets:info(node_table) of 
        undefined ->
            ets:new(node_table, [named_table, set, public]),
            erlang:process_flag(trap_exit, true), % needed to trap normal exit signal (trap means its converted into a message)
            table_owner_loop();
        _ ->
            ok
    end.

table_owner_loop() ->
    % this loop ensures the table owner process lives so the table will not get deleted.
    receive
        {'EXIT', _, _} ->
            % io:format("Table owner of ~p exiting via link~n",[node()]),
            exit(normal);
        _ ->
            table_owner_loop()
    end.

wait_for_table_ready() ->
    case ets:info(node_table) of 
        undefined ->
            timer:sleep(20),
            wait_for_table_ready();
        _ ->
            ok
    end.


get_global(Key) ->
    try
        case ets:lookup(node_table, Key) of 
            [] -> 
                io:format("Key ~p not found in node ~p~n", [Key, node()]),
                undefined;
            [{Key, Value}] -> 
                Value
        end
    catch
        _:Error ->
            io:format("Error: ~p when getting key ~p in node ~p ~n", [Error, Key, node()]),
            undefined
    end.

set_global(Key, Value) ->
    try
        ets:insert(node_table, {Key, Value})
    catch
        _:Error ->
            io:format("Error: ~p when setting key ~p in node ~p ~n", [Error, Key, node()])
    end.




steadyLink(F) ->
    Pid = spawn_link(fun () -> F() end),
    timer:sleep(5000),
    Pid.


steadyMon(F) -> 
    {Pid, Ref} = spawn_monitor(fun () -> F() end),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            io:format("Normal termination of process ~p was detected~n",[Pid]);
        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("An exception in process ~p was detected: ~p~n",[Pid,Reason])
        after 5000 ->
            exit(Pid, kill)
    end,
            
    Pid.








