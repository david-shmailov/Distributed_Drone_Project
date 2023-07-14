-module(ex5_311328322).

-export([ring_parallel/2,ring_serial/2,mesh_parallel/3,mesh_serial/3]).
-record(neighbors, {upper, lower, left, right}). % record for neighbors of a process
-define(Ring_P_delay, 300). % delay per process in ring, in ms


empty_mailbox() ->
    receive
        _ -> empty_mailbox()
    after
        0 -> ok
    end.

% ***************************************** mesh_serial *****************************************

mesh_serial(N,M,C)  when N > 1 andalso C >= 1 andalso C =< N*N andalso M >= 0 ->
    io:format("Starting mesh_serial with N=~p, M=~p, C=~p~n",[N,M,C]),
    Start_time = erlang:monotonic_time(),
    {Sent, Received} = master_serial_main(N,M,C),
    TotalTime = (erlang:monotonic_time() - Start_time)/1000,
    io:format("First Process PID ~p Received all ~p messages and finished\n",[self(),Received]),
    io:format("mesh_serial total time: ~p ms~n", [TotalTime]),
    empty_mailbox(),
    {TotalTime, Sent, Received};
mesh_serial(_,_,_) ->
    io:format("Invalid input, make sure: 0 < N, 1 <= C <= N*N, 0 <= M ~n").

master_serial_main(N,M,C) ->
    % init all verticies and receive an array of vertix states
    Vertex_states = [mesh_vertix_init(ID,N,C,M) || ID <- lists:seq(1,N*N)],
    % enter event loop
    mesh_event_loop(Vertex_states,C,N,M).
    


mesh_event_loop(Vertex_states,C,N,M) ->
    % Vertex_states is a list of all remaining messages per each state
    States = mesh_scheduler(Vertex_states,C),
    % get the master vertix state
    {C, Master_state} = lists:keyfind(C, 1,States), 
    % get a list of all verticies counters (how many messages each vertix received)
    Messages_received = maps:get(messages_received, Master_state),
    % count the number of total received messages from all verticies:
    Total_received = length(Messages_received),
    if 
        % all messages received
        Total_received >= (N*N-1)*M -> 
            Total_sent = maps:get(messages_sent, Master_state),
            {Total_sent,Total_received};
        % not all messages received
        true -> mesh_event_loop(States,C,N,M)
    end.
    
mesh_scheduler(Vertex_states,C) ->
    % round-robin scheduler
    % assign each vertix CPU time, return a list of vertix states
    [proccess_all_messages(ID, Vertex_state,C) || {ID, Vertex_state} <- Vertex_states]. 

mesh_vertix_init(C,N,C,M) ->
    % initialize master vertix state
    Neighbors = calculate_neighbors(C,N),
    Messages = [{C,M_num} || M_num <- lists:seq(1,M)], % create M messages to send

    % send messages to neighbors, and count how many messages were sent:
    Msg_sent_per_neighbor = [send_to_neighbors(serial,{ID,M_num},Neighbors) || {ID,M_num} <- Messages],
    Total_sent = lists:foldl(fun(X,Acc) -> X + Acc end, 0, Msg_sent_per_neighbor),
    Vertex_state = #{neighbors=>Neighbors, messages_received=>[], messages_sent=>Total_sent, m => M, n => N},
    {C, Vertex_state}; % return {ID, Vertex_state}

mesh_vertix_init(ID, N, _, _) ->
    % initialize vertix state
    Neighbors = calculate_neighbors(ID,N),
    Vertex_state = #{neighbors=>Neighbors, messages_received=>[]},
    {ID,Vertex_state}. % return Vertex_state

mesh_vertix_loop(ID, Vertex_state, _, []) -> % no more messages to process
    {ID, Vertex_state};
mesh_vertix_loop(ID, Vertex_state, C, [Message|Rest]) ->
    {ID, State} = handle_serial_message(Message, ID, Vertex_state, C),
    mesh_vertix_loop(ID, State, C, Rest).

proccess_all_messages(ID, Vertex_state, C) -> proccess_all_messages(ID, Vertex_state, C, []).
proccess_all_messages(ID, Vertex_state, C, Messages) -> % get all messages from the mailbox
    receive
        {ID, Payload} -> % if message is directed towards this vertix
            proccess_all_messages(ID, Vertex_state, C, [Payload|Messages])
        after 0 ->
            mesh_vertix_loop(ID, Vertex_state, C, Messages)
    end.

handle_serial_message({Src_ID, Msg}, C, Vertex_state, C) -> % master message handler
    Messages_received = maps:get(messages_received, Vertex_state),
    case lists:member({Src_ID, Msg}, Messages_received) of
        true -> % already received this message discard it
            {C,Vertex_state};
        false -> % first time receiving this message
            Updated_MR = [{Src_ID, Msg}|Messages_received],
            {C, Vertex_state#{messages_received := Updated_MR}}
    end;

handle_serial_message(Payload, ID, Vertex_state, C) -> % slave message handler
    Messages_received = maps:get(messages_received, Vertex_state),
    case lists:member(Payload,Messages_received) of
        true -> % already received this message discard it
            {ID,Vertex_state};
        false -> % first time receiving this message
            Neighbors = maps:get(neighbors, Vertex_state),
            send_to_neighbors(serial,Payload,Neighbors), % resend message to neighbors
            case Payload of
                {C,M_num} -> % master's message
                    send_to_neighbors(serial,{ID,M_num},Neighbors), % respond to master
                    {ID, Vertex_state#{messages_received := [Payload,{ID,M_num}] ++ Messages_received}};
                _ -> % not master's message
                    {ID, Vertex_state#{messages_received := [Payload|Messages_received]}}
            end
    end.













% ***************************************** mesh_parallel *****************************************

mesh_parallel(N,M,C) when N >= 1 andalso C >= 1 andalso C =< N*N andalso M >= 0 -> 
    io:format("Starting mesh_parallel with N=~p, M=~p, C=~p~n",[N,M,C]),
    Start_time = erlang:monotonic_time(),
    {Sent, Received} = master_process(N,M,C),
    TotalTime = (erlang:monotonic_time() - Start_time)/1000,
    io:format("Master Process ~p, PID ~p Received all ~p messages and finished\n",[C,self(),Received]),
    io:format("mesh_parallel total time: ~p ms~n", [TotalTime]),
    timer:sleep(5), % let all processes die
    empty_mailbox(),
    {TotalTime, Sent, Received};
mesh_parallel(_,_,_) ->
    io:format("Invalid input, make sure: 0 < N, 1 <= C <= N*N, 0 <= M ~n").

master_process(N,M,C) ->
    % create all other process except C (C will remain this process):
    Proc_dict = [{C,self()}] ++ [{ID, spawn(fun() -> slave_init(ID,N,C,M) end)} || ID <- lists:seq(1,N*N), ID /= C], 
    % send proc_dict to all processes
    [Pid ! {proc_dict, Proc_dict} || {ID,Pid} <- Proc_dict, ID /= C],
    % calculate master's neighbors:
    Neighbors = get_neighbors_procs(C,N,Proc_dict),
    % wait for all messages to return:
    {Total_sent,Total_received} = master_loop(N,C,M,Neighbors),
    % kill all processes:
    [Pid ! kill || {ID,Pid} <- Proc_dict, Pid /= self(), ID /= C],
    {Total_sent, Total_received}.


master_loop(N,C,M,Neighbors) -> 
    Messages_to_send = [{C,M_num} || M_num <- lists:seq(1,M)], % create M messages to send
    master_loop(N,C,M,Neighbors,Messages_to_send,0,[]).
master_loop(N, C, M, Neighbors, [{ID,M_num}|Rest], Total_sent, Messages_received)->
    Msg_sent_now = send_to_neighbors(parallel,{ID,M_num},Neighbors),
    receive
        {C,_} -> % discard self echo
            master_loop(N, C, M, Neighbors, Rest, Total_sent+Msg_sent_now, Messages_received);
        {Sender_ID,Msg} ->
            handle_parallel_message({Sender_ID,Msg},C,N,C,M,Neighbors,Rest, Total_sent+Msg_sent_now, Messages_received)
    end;
% if master finished sending his messages:
master_loop(N, C, M, Neighbors, [], Total_sent, Messages_received)->
    master_loop_wait(N, C, M, Neighbors, Total_sent, Messages_received).

master_loop_wait(N, C, M, Neighbors, Total_sent, Messages_received) ->
    Total_messages = length(Messages_received),
    if 
        % all messages received
        Total_messages >= (N*N-1)*M -> {Total_sent,Total_messages};
        % not all messages received
        true -> 
            receive
                {C,_} -> % discard self echo
                    master_loop_wait(N,C,M,Neighbors, Total_sent, Messages_received);
                {Sender_ID,Msg} ->
                    handle_parallel_message({Sender_ID,Msg},C,N,C,M,Neighbors,[],Total_sent, Messages_received);
                Msg ->
                    io:format("unexpected message received by master: ~p ~n", [Msg])
            after 0 ->
                master_loop_wait(N,C,M,Neighbors, Total_sent, Messages_received)
            end
    end.





slave_init(ID,N,C,M) ->
    % wait to receive proc_dict from master
    % io:format("process ~p created with PID ~p~n", [ID,self()]),
    receive
        {proc_dict, Proc_dict} ->
            Neighbors = get_neighbors_procs(ID,N,Proc_dict)
            % io:format("process ~p with PID ~p connected to grid~n", [ID,self()])
            % report_connections(ID,Neighbors) % debug
    end,
    slave_loop(ID,N,C,M,Neighbors).
    
slave_loop(ID,N,C,M,Neighbors) -> slave_loop(ID,N,C,M,Neighbors,[]).
slave_loop(C,_,C,_,_,_) -> exit("slave_loop: C == ID");
slave_loop(ID,N,C,M,Neighbors,Messages_received) ->
    receive
        kill ->
            exit(normal);
        {Sender_ID, Msg} ->
            handle_parallel_message({Sender_ID, Msg}, ID,N,C,M,Neighbors,Messages_received); % optimization
        Msg ->
            io:format("unexpected message received by process ~p with PID ~p: ~p ~n", [ID,self(),Msg])
        after 0 ->
            slave_loop(ID,N,C,M,Neighbors,Messages_received)
    end.

handle_parallel_message({Sender_ID,Msg}, C, N,C,M,Neighbors,Messages_to_send,Total_sent , Messages_received) -> % master handler
    case lists:member({Sender_ID,Msg},Messages_received) of
        true -> % already received this message discard it
            master_loop(N,C,M,Neighbors,Messages_to_send,Total_sent ,Messages_received);
        false -> % first time receiving this message
            master_loop(N,C,M,Neighbors,Messages_to_send,Total_sent ,[{Sender_ID,Msg}|Messages_received])
    end.

handle_parallel_message(Message, ID,N,C,M,Neighbors,Messages_received) -> % slave handler
    case lists:member(Message,Messages_received) of
        true -> % already received this message discard it
            slave_loop(ID,N,C,M,Neighbors,Messages_received);
        false -> % first time receiving this message
            send_to_neighbors(parallel,Message,Neighbors), % resend the message to neighbors
            case Message of
                {C,Msg} ->
                    send_to_neighbors(parallel,{ID,Msg},Neighbors), % respond to master
                    MR = [Message|Messages_received],
                    Updated_MR = [{ID,Msg}|MR];
                _ ->
                    Updated_MR = [Message|Messages_received]
            end,
            slave_loop(ID,N,C,M,Neighbors,Updated_MR) % only add the received message
    end.
% Arch = parallel | serial
% Sender_ID = current sender ID
% Message = message to send
% Neighbors = #neighbors{} record of sender
send_to_neighbors(Arch,Message,Neighbors) ->

    Messages_sent = [
        send_message(Arch,Neighbors#neighbors.upper,Message),
        send_message(Arch,Neighbors#neighbors.lower,Message),
        send_message(Arch,Neighbors#neighbors.left,Message),
        send_message(Arch,Neighbors#neighbors.right,Message)
    ],
    Total_sent = lists:foldl(fun(X,Acc) -> X + Acc end, 0, Messages_sent),
    Total_sent.

% wrapper function to verify for non-void neighbors
% returns 1 if sent, 0 if not sent
send_message(parallel,{ID,PID},Message) ->
    case ID of
        false -> 0;
        undefined -> 0;
        _ -> 
            PID ! Message, % Message is {Src,Msg}
            1
    end;
send_message(serial,DST,Message) ->
    case DST of
        undefined -> 0;
        false -> 0;
        _ -> 
            self() ! {DST,Message}, % Message is {Src,Msg}
            1
    end.

% was used for debugging
% report_connections(ID,#neighbors{upper = {Upper_ID,Upper_PID}, lower = {Lower_ID,Lower_PID}, left = {Left_ID,Left_PID}, right = {Right_ID,Right_PID}}) ->
%     io:format("process ~p with PID ~p has neighbors:~n
%         upper: ~p with PID ~p~n
%         lower: ~p with PID ~p~n
%         left: ~p with PID ~p~n
%         right: ~p with PID ~p~n",[ID,self(),Upper_ID,Upper_PID,Lower_ID,Lower_PID,Left_ID,Left_PID,Right_ID,Right_PID]).


get_neighbors_procs(ID,N,Proc_dict) ->
    #neighbors{upper = Upper_ID, lower = Lower_ID, left = Left_ID, right = Right_ID} = calculate_neighbors(ID,N),
    Upper = case lists:keyfind(Upper_ID,1,Proc_dict) of 
        false -> {undefined,undefined};
        {Upper_ID,Upper_PID} -> {Upper_ID,Upper_PID}
    end,
    Lower = case lists:keyfind(Lower_ID,1,Proc_dict) of 
        false -> {undefined,undefined};
        {Lower_ID,Lower_PID} -> {Lower_ID,Lower_PID}
    end,
    Left = case lists:keyfind(Left_ID,1,Proc_dict) of 
        false -> {undefined,undefined};
        {Left_ID,Left_PID} -> {Left_ID,Left_PID}
    end,
    Right = case lists:keyfind(Right_ID,1,Proc_dict) of 
        false -> {undefined,undefined};
        {Right_ID,Right_PID} -> {Right_ID,Right_PID}
    end,
    #neighbors{upper = Upper, lower = Lower, left = Left, right = Right}. % return neighbors as #neighbors record

calculate_neighbors(ID,N) ->
    case {ID div N, ID rem N} of
        {0,1} -> % upper left corner
            #neighbors{upper = undefined, lower = ID+N, left = undefined, right = ID+1};
        {1,0} -> % upper right corner
            #neighbors{upper = undefined, lower = ID+N, left = ID-1, right = undefined};
        {Row,1} when Row == N-1 -> % lower left corner
            #neighbors{upper = ID-N, lower = undefined, left = undefined, right = ID+1};
        {N,0} -> % lower right corner
            #neighbors{upper = ID-N, lower = undefined, left = ID-1, right = undefined};
        {_,1} -> % left edge
            #neighbors{upper = ID-N, lower = ID+N, left = undefined, right = ID+1};
        {_,0} -> % right edge
            #neighbors{upper = ID-N, lower = ID+N, left = ID-1, right = undefined};
        {0,_} -> % upper edge
            #neighbors{upper = undefined, lower = ID+N, left = ID-1, right = ID+1};
        {Row,_} when Row == N-1 -> % lower edge
            #neighbors{upper = ID-N, lower = undefined, left = ID-1, right = ID+1};
        {_,_} -> % middle
            #neighbors{upper = ID-N, lower = ID+N, left = ID-1, right = ID+1}
    end.

















% ***************************************** ring_parallel *****************************************


ring_parallel(N,M) when N >= 1 andalso M >= 0 -> 
    io:format("Starting ring parallel with ~p processes and ~p messages~n", [N,M]),
    Start_time = erlang:monotonic_time(),
    {Sent,Received} = ring_parallel(first, N,M),
    TotalTime = (erlang:monotonic_time() - Start_time)/1000,
    io:format("First Process PID ~p Received all ~p messages and finished\n",[self(),M]),
    io:format("Total time: ~p ms~n", [TotalTime]),
    {TotalTime, Sent, Received};
ring_parallel(_,_) ->
    io:format("Invalid input, make sure: 0 < N, 0 <= M ~n").

ring_parallel(first,1,M) -> % edge case for N = 1
    io:format("process ~p created with PID ~p~n", [1,self()]),
    [self() ! message || _ <- lists:seq(1,M)],
    case receive_messages(M,1) of % wait to receive M messages
        ok ->
            {M,M};
        {timeout,Remaining} ->
            {M,M-Remaining}
    end;

ring_parallel(first, N, M) ->
    io:format("process ~p created with PID ~p~n", [N,self()]),
    Self_PID = self(),
    Next_Pid = spawn(fun() -> ring_parallel_rest(N-1, M, Self_PID,N) end),
    receive
        Pid ->
            io:format("process ~p connected to ~p!~n", [Pid,self()])
    end,
    [Next_Pid ! message || _ <- lists:seq(1,M)],
    case receive_messages(M,N) of % wait to receive M messages
        ok ->
            io:format("PID ~p Finished\n",[self()]),
            Next_Pid ! kill,
            {M,M};
        {timeout,Remaining} ->
            Next_Pid ! kill,
            {M,M-Remaining}
    end.
    

receive_messages(0,_) ->
    ok;
receive_messages(M,N)->
    receive
        message ->
            io:format("PID ~p Received message, ~p messages remaining.~n",[self(),M-1]),
            receive_messages(M-1,N)
    after ?Ring_P_delay*N -> % scalable timeout
        io:format("PID ~p Timed out, ~p messages remaining.~n",[self(),M]),
        {timeout,M}
    end.

receive_messages_and_send(0,_,_) ->
    io:format("PID ~p Finished\n",[self()]);
receive_messages_and_send(M,Next_PID,Total_N)->
    receive
        message ->
            io:format("PID ~p Received message, Sending to ~p, ~p messages remaining.~n",[self(),Next_PID,M-1]),
            Next_PID ! message,
            receive_messages_and_send(M-1,Next_PID,Total_N);
        kill ->
            io:format("process PID ~p received kill message~n", [self()]),
            Next_PID ! kill, % propegade kill message to the next process
            exit(killed)
    after ?Ring_P_delay*Total_N -> % scalable timeout
        io:format("PID ~p Timed out, ~p messages remaining.~n",[self(),M])
    end.

ring_parallel_rest(1, M, First_PID,Total_N) ->
    io:format("process ~p created with PID ~p~n", [1,self()]),
    First_PID ! self(),
    receive_messages_and_send(M,First_PID,Total_N); % wait to receive M messages

ring_parallel_rest(N, M, First_PID,Total_N) ->
    io:format("process ~p created with PID ~p~n", [N,self()]),
    Next_PID = spawn(fun() -> ring_parallel_rest(N-1,M, First_PID,Total_N) end),
    receive_messages_and_send(M,Next_PID,Total_N). % wait to receive M messages




% ***************************************** ring_serial *****************************************


ring_serial(V,M) when V >= 1 andalso M >= 0 -> 
    io:format("starting ring serial with ~p vertices and ~p messages~n", [V,M]),
    Start_time = erlang:monotonic_time(),
    ring_serial(first, V,M),
    End_time = erlang:monotonic_time(),
    TotalTime = (End_time - Start_time)/1000,
    io:format("Total time: ~p ms~n", [TotalTime]),
    {TotalTime, M, M};

ring_serial(_,_) ->
    io:format("Invalid input, make sure: 0 < V, 0 <= M ~n").

ring_serial(first,1,M) ->
    io:format("Vertex ~p started ~n", [1]),
    [self() ! {1,1,message} || _ <- lists:seq(1,M)],
    io:format("Vertex 1 Sent ~p messages to itself ~n", [M]),
    loop(1,M); % wait to receive M messages

ring_serial(first,V,M) ->
    io:format("Vertex ~p started ~n", [V]),
    [self() ! {V-1,V,message} || _ <- lists:seq(1,M)],
    io:format("Vertex ~p Sent ~p messages to vertix ~p ~n", [V,M,V-1]),
    loop(V,M). % wait to receive M messages
    


vertix_proc(_,_, 0) ->
    0;
vertix_proc(V,1, M) -> % vertix last before the first
    receive
        {1,Vertex_SRC, message} ->
            io:format("Vertex 1 received message from ~p, Returning to ~p, ~p messages remaining.~n", [Vertex_SRC,V,M-1]), % debug
            self() ! {V,1, message}, % send message to the next vertix
            M-1 % return remaining messages left for this vertix
    after 0 ->
        M % return remaining messages left for this vertex
    end;
vertix_proc(V,V, M) -> % the first vertix
    receive
        {V,Vertex_SRC, message} ->
            io:format("Vertex ~p received message from ~p, ~p messages remaining.~n", [V,Vertex_SRC,M-1]), % debug
            M-1 % return remaining messages left for this vertix
    after 0 ->
        M % return remaining messages left for this vertex
    end;
vertix_proc(_,Vertex_ID, M) ->
    receive
        {Vertex_ID,Vertex_SRC, message} ->
            io:format("Vertex ~p received message from ~p, Sending to ~p, ~p messages remaining.~n", [Vertex_ID,Vertex_SRC,Vertex_ID-1,M-1]), % debug
            self() ! {Vertex_ID-1,Vertex_ID, message}, % send message to the next vertix
            M-1 % return remaining messages left for this vertix
    after 0 ->
        M % return remaining messages left for this vertex
    end.

loop(V,M) ->
    % initialize all vertix states
    Vertex_states = [M || _ <- lists:seq(1,V)],
    event_loop(V, Vertex_states).

event_loop(V, Vertex_states) ->
    % Vertex_states is a list of all remaining messages per each state
    
    States = scheduler(V,Vertex_states),
    case hd(States) of % check if the first vertix received all messages
        0 ->
            io:format("All messages circled and vertix 1 finished!\n");
        _ -> 
            event_loop(V, States)
    end.

scheduler(V,Vertex_states) ->
    % assign each vertix CPU time until timeout, return a list of vertix states
    [vertix_proc(V, Vertex_ID, M) || {Vertex_ID, M} <- lists:zip(lists:seq(1,V),Vertex_states)].



