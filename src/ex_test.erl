-module(ex_test).
-author("David").

-export([test/0]).
-import(ex5_311328322,[ring_parallel/2,ring_serial/2,mesh_parallel/3,mesh_serial/3]).

print_time(Start_time)->
    TotalTime = floor((erlang:monotonic_time()/1000000000 - Start_time)),
    io:format("Elapsed time: ~p\n", [TotalTime]).


test() ->
    io:format("**********************************Starting test**********************************~n"),
    % ring_parallel(2,1),
    % ring_serial(1000,1),
    % mesh_parallel(3,100,1),
    % mesh_serial(3,2,1),
    Start_time = erlang:monotonic_time()/1000000000,
    %% Open the file in write mode
    {ok, File} = file:open("output.txt", [write]),


    
    % Ring_results = [{ring_parallel(N,M), ring_serial(N,M), {N,M}} || M <- [1000], N <- lists:seq(2,100) ],
    % Parallel_avg = lists:sum([Time || {{Time,_,_},_,_} <- Ring_results]) / length(Ring_results),
    % Serial_avg = lists:sum([Time || {_,{Time,_,_},_} <- Ring_results]) / length(Ring_results),
    % [io:format(File,"N: ~p\tM: ~p\tParallel: ~p\t\tSerial: ~p~n", [N,M,Time_P,Time_S]) || {{Time_P,_,_},{Time_S,_,_},{N,M}} <- Ring_results],
    % io:format(File,"Parallel avg: ~p~nSerial avg: ~p~n", [Parallel_avg, Serial_avg]),
    List_of_N_Centers = [{2,1},{3,1},{4,1},{5,1},{2,2},{3,5},{4,6},{5,13}],
    Mesh_results = [{print_time(Start_time),mesh_parallel(N,M,C), mesh_serial(N,M,C), {N,M,C}} || {N,C} <-List_of_N_Centers, M <- [10,100,1000] ],
    % Mesh_results = [{print_time(Start_time),mesh_parallel(N,M,C), void , {N,M,C}} || {N,C} <-List_of_N_Centers, M <- [10,100,1000] ],

    Speed_up = [{N,M,C,Time_P/Time_S} || {_,{Time_P,_,_},{Time_S,_,_},{N,M,C}} <- Mesh_results],
    [io:format(File,"N: ~p\tM: ~p\tC: ~p\tSpeed up: ~p~n", [N,M,C,Speed]) || {N,M,C,Speed} <- Speed_up],
    %% Close the file
    file:close(File),
    io:format("**********************************Finished test**********************************~n").

