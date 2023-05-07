-module(ex4_311328322).

-export([flatten/1, smaller/2, replace/3, mapSub/2]).

flatten(List) -> flatten(List, []).

flatten ([], Acc) -> Acc; % stopping condition
flatten ([H|T], Acc) when is_list(H) -> 
    flatten(T, Acc ++ flatten(H)); % if H is a list, flatten it and append to the end of Acc
flatten ([H|T], Acc) -> 
    flatten(T,Acc++[H]). % if H is not a list, append it to the end of Acc


smaller(List, Thr) when is_list(List) ->
    lists:map(fun(X) -> X =< Thr end, List). % for each element, X =< Thr returns true/false atom



replace(List, Old, New) -> replace(List, Old, New, []).
replace([], _, _, Acc) -> Acc; % stopping condition
replace([H|T], Old, New, Acc) -> 
    case H of
        Old -> replace(T, Old, New, Acc ++ [New]); % if H is 'Old', append 'New' to the end of Acc
        _ -> replace(T, Old, New, Acc ++ [H])   % if H is not 'Old', append H to the end of Acc
    end.



mapSub(List1, []) -> List1; % edge case defined by example in the exercise
mapSub(List1, List2) when is_list(List2) ->
    if length(List1) == length(List2) -> % if the lists are of equal length, subtract element by element
        Tuples = lists:zip(List1, List2), % zip allows us to iterate on both lists at the same time
        lists:map(fun({X,Y}) -> X-Y end, Tuples);
    true -> lenError % if the lists are not of equal length, return lenError atom as defined in the exercise
    end;
    
mapSub(List1, Arg2) -> % if Arg2 is not a list, subtract Arg2 from each element in List1
    lists:map(fun(X) -> X-Arg2 end, List1).

