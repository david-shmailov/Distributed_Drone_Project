-module(ex7_311328322).

-export([steady/1,calc/3]).




steady(F) ->
    case file:open("myLog_311328322.elog", [append]) of
        {ok, File} ->
            try
                Value = apply(F, []), % we assume F has no arguments
                io:format(File,"~p~n",[{os:system_time(), success, Value}]), % on success, print to file
                Value % return value
            catch % catch any issue within F and log it
                error:Error ->
                    io:format(File,"~p~n",[{os:system_time(), error, Error}]);
                exit:Exit ->
                    io:format(File,"~p~n",[{os:system_time(), exit, Exit}]);
                throw:Throw ->
                    io:format(File,"~p~n",[{os:system_time(), throw, Throw}])
            end,
            file:close(File); % close file
        {error, Reason} ->
            io:format("Error opening file: ~p~n", [Reason])
    end.



calc(division,A,B) ->
    try 
        A / B
    catch
        error:badarith ->
            infinity % return infinity atom on division by zero
    end.