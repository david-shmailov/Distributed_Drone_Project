-module(ex_test).
-author("David").

-export([test/0]).
% -import(ex5_311328322,[ring_parallel/2,ring_serial/2,mesh_parallel/3,mesh_serial/3]).

print_time(Start_time)->
    TotalTime = floor((erlang:monotonic_time()/1000000000 - Start_time)),
    io:format("Elapsed time: ~p\n", [TotalTime]).


test() ->
    io:format("**********************************Starting test**********************************~n"),
    %% Open the file in write mode
    {ok, File} = file:open("output.txt", [write]),

    %% Close the file
    file:close(File),
    io:format("**********************************Finished test**********************************~n").

