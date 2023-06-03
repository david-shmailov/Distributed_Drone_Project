-module(ex_test).
-author("David").

-export([test/0]).
-import(ex8_311328322,[startChat/1, call/1,steadyLink/1, steadyMon/1]).


print_per_sec() ->
    io:format("I am alive~n"),
    timer:sleep(1000),
    print_per_sec().

main() ->
    Pid = spawn(fun () -> print_per_sec() end),
    link(Pid),
    io:format("printer Pid: ~p~n", [Pid]),
    timer:sleep(5000),
    exit(because).

test() ->
    io:format("**********************************Starting test**********************************~n"),
    % Pid = spawn(fun () -> main() end),
    % io:format("main Pid: ~p~n", [Pid]),
    Pid_Local_sender = ex8_311328322:startChat('callee@Legion'),
    timer:sleep(1000),
    Pid_Local_sender ! "Hello",
    % timer:sleep(1000),
    % ex8_311328322:call("Hey"),
    % timer:sleep(1000),
    % Pid_Local_sender ! stats,
    % timer:sleep(1000),
    % ex8_311328322:call(stats),
    
    % %% Open the file in write mode
    % {ok, File} = file:open("output.txt", [write]),
    % %% Close the file
    % file:close(File),
    io:format("**********************************Finished test**********************************~n").

