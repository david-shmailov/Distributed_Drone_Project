-module(ex2_311328322).
-author("David").

-export([main/0, findKelem/2, reverse/1, deleteKelem/2, addKelem/3, union/2]).



% I assumed that K>0

findKelem([H|_],1)  -> H; % stopping condition
findKelem([],_)     -> notFound; % stopping condition
findKelem([_|T],K)  -> findKelem(T,K-1). % recursivly reduce Kth index and continue with the tail of the list

%arity 1 as required by the task
reverse(List) -> 
    reverse(List,[]).

%arity 2 with accumulator
reverse([H|T],[]) -> reverse(T, [H]); % needed so we wont get a reverse result like this [[],5,4,3,2,1]
reverse([H|T],Reversed) -> reverse(T, [H|Reversed]); % the actual construction of the reversed list, we can safely assume Reversed will be a non empty list here.
reverse([],Reversed) -> Reversed. % stopping condition

% arity 2
deleteKelem(List,K) -> 
    deleteKelem(List,K,[]).

% arity 3 with accumulator
deleteKelem([K|T],K,Acc)    -> deleteKelem(T,K,Acc); % proceeds recursivly without the first element, pattern matching ensures first element == K
deleteKelem([H|T],K,Acc)    -> deleteKelem(T,K,[H|Acc]); % adds H to the start of the accumulator, the accumulator is in reversed order to the original list
deleteKelem([],_,Acc)       -> reverse(Acc). % stopping condition. Acc needs to be reversed duo to the way it was constructed

% Adds Elem to list in k'th place. 
% I assumed that K>0
addKelem(List,K,Elem) -> 
    addKelem(List,K,Elem,[]).

addKelem(List,1,Elem,[])    -> [Elem|List]; % to avoid having an empty list as first element, we need this. this also deals with addKelem([],1,new)
addKelem(List,1,Elem,Acc)   -> concatLists(reverse(Acc),[Elem|List]);
addKelem([], _, Elem, Acc)  -> reverse([Elem|Acc]); % stopping condition, i decided that for any K that is larger than list size +1, i will just add the element to the end
addKelem([H|T] ,K,Elem, Acc) -> addKelem(T,K-1,Elem, [H|Acc]). % recursivly iterate over the list, accumulate in reverse order in acc, until K==1


union(List1,List2) -> remove_dup(concatLists(List1,List2)).

%removes duplicate elements from the list
remove_dup(List) -> remove_dup(List,[]).

remove_dup([H|T],Acc) -> remove_dup(deleteKelem(T,H) ,[H|Acc]); % removes all H's from T, accumulates H.
remove_dup([],Acc) -> reverse(Acc). % acc is the list without duplicates in reverse


% joins two lists together
concatLists(List1,List2) -> concatLists(List1, List2, []).

concatLists([H|T], List2, Acc) -> concatLists(T, List2, [H|Acc]); % as long as list1 is not empty, iterate and accumulate in reverse.
concatLists([], [H|T], Acc) -> concatLists([],T,[H|Acc]); % List1 is empty, as long as list2 is not empty  iterate and accumulate in reverse.
concatLists([],[],Acc) -> reverse(Acc). % Acc was built in reverse so it needs to be reversed

% main() ->
%     io:format("started\n"),
%     % A = findKelem([1,2,3,4,5],5),
%     % B = findKelem([1,2,3,4,5],6),
%     % C = findKelem([],3),
%     % A = reverse([1,2,3,4,5]),
%     % B = reverse([]),
%     % A = deleteKelem([1,2,1,2,1],1),
%     % B = deleteKelem([1,2,1,2,1],3),
%     % C = deleteKelem([],3),
%     % A = addKelem([1,2,3,4,5],3,new),
%     % B = addKelem([1,2,3,4,5],6,new),
%     % C = addKelem([],1,new),
%     % A = union([],[]),
%     % B = union([1,2,3],[2,3,4]),
%     % C = union([1,2,2,5],[1,2,3]),

%     % A = concatLists([1],[2,3]),
%     io:format("finished\n").


