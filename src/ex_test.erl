-module(ex_test).
-author("David").

-export([test/0]).
-import(ex7_311328322,[steady/1,calc/3]).



test() ->
    io:format("**********************************Starting test**********************************~n"),
    ex7_311328322:steady(fun() -> ex7_311328322:calc(division, 1, 0) end),
    ex7_311328322:steady(fun() -> ex7_311328322:calc(division, 1, 1) end),
    ex7_311328322:steady(fun() -> ex7_311328322:calc(division, 1, 2) end),
    ex7_311328322:steady(fun() -> exit(0) end),
    ex7_311328322:steady(fun() -> exit(1) end),
    ex7_311328322:steady(fun() -> throw({mine, "because"}) end),
    ex7_311328322:steady(fun() -> 1/0 end),
    
    % %% Open the file in write mode
    % {ok, File} = file:open("output.txt", [write]),
    % %% Close the file
    % file:close(File),
    io:format("**********************************Finished test**********************************~n").

