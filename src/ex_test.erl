-module(ex_test).
-author("David").

-export([test/0]).
-import(ex6_311328322,[songList/1,songGen/3]).

print_time(Start_time)->
    TotalTime = floor((erlang:monotonic_time()/1000000000 - Start_time)),
    io:format("Elapsed time: ~p\n", [TotalTime]).


test() ->
    io:format("**********************************Starting test**********************************~n"),
    G = songList(["ABC","CBA","BAC","ACB","BBA"]),
    Path = songGen(G,"ABC","BAC"),

    Res = songGen(songList(["ABC","CBA","BAC","ACB","BBA"]),"ABC","BAC"),
    % %% Open the file in write mode
    % {ok, File} = file:open("output.txt", [write]),
    % %% Close the file
    % file:close(File),
    io:format("**********************************Finished test**********************************~n").

