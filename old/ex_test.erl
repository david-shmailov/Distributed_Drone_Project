-module(ex_test).
-author("David").

-export([test/0]).
-import(ex9_311328322,[etsBot/0]).


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
    ex9_311328322:etsBot(),
    io:format("**********************************Finished test**********************************~n").

