-module(ex3_311328322).
-author("David").

% -export([main/0]).
-export([mSort/1,qSort/1,sortRes/2,sortResLC/1,sortResPM/1,sortResLM/1,matElemMult/2,filter_g/2,filter_p/2,even/1,fiboR/1,fiboT/1]).


sortRes([],_) -> [];
sortRes(List,lc) -> sortResLC(List);
sortRes(List,pm) -> sortResPM(List);
sortRes(List,lm) -> sortResLM(List).


sortResLC(List) -> 
    % filter using list comprehension and sort then join them all
    Mod0 = qSort([X || X <- List, X rem 3 == 0]),
    Mod1 = qSort([X || X <- List, X rem 3 == 1]),
    Mod2 = qSort([X || X <- List, X rem 3 == 2]),
    Mod0 ++ Mod1 ++ Mod2.

% Arity 1
sortResPM([H|T]) -> sortResPM([H|T],[],[],[]).

% Arity 4
sortResPM([],Mod0,Mod1,Mod2) ->
    qSort(Mod0) ++ qSort(Mod1) ++ qSort(Mod2);
sortResPM([H|T],Mod0,Mod1,Mod2) -> sortResPM([H|T],Mod0,Mod1,Mod2, H rem 3). % pass a remainder of H

% decide using pattern matching on the remainder to which accumulator add H and return to arity 4 
sortResPM([H|T],Mod0,Mod1,Mod2, 0) -> sortResPM(T,[H|Mod0],Mod1,Mod2); 
sortResPM([H|T],Mod0,Mod1,Mod2, 1) -> sortResPM(T,Mod0,[H|Mod1],Mod2);
sortResPM([H|T],Mod0,Mod1,Mod2, 2) -> sortResPM(T,Mod0,Mod1,[H|Mod2]).


% pass a custom sorting key defined below to the sort function from lists.
sortResLM(List) -> lists:sort(fun mod3Key/2,List).

% this key will return true or false according to the assignment
mod3Key(A,B) ->
    ModA = A rem 3,
    ModB = B rem 3,
    if % predicat to sort by mod3
        ModA < ModB -> true; 
        ModA > ModB -> false;
        A < B -> true;
        true -> false % if they are equal, sort normally.
    end.

% quick sort arity 1
qSort(List) -> qSortByKey(List, fun(A,B) -> A < B end).

% quick sort that can receive a custom key to sort elements by (ended up not using this feature)
qSortByKey([], _) -> [];
qSortByKey([Pivot|T], Key) ->
    LessThanPivot = [X || X <- T, Key(X,Pivot)],
    GreaterThanPivot =  [X || X <- T, Key(Pivot,X)],
    SortedRight = qSort(LessThanPivot),
    SortedLeft = qSort(GreaterThanPivot),
    SortedRight ++ [Pivot] ++ SortedLeft.

% merge sort 
mSort([]) -> [];
mSort([H]) -> [H];
mSort(List) ->
    {Right, Left} = splitMiddle(List),
    SortedRight = mSort(Right),
    SortedLeft = mSort(Left),
    merge(SortedRight,SortedLeft).

% splits a List right in the middle into two lists
splitMiddle(List) -> % we assume List is of length > 1 because we checked it in mSort.
    Length = length(List),
    Middle = Length div 2,
    splitList(List,Middle).


% splits the List at Index , the right list will include the index. returned lists are in reverse order
splitList(List,Index) when Index >= 0 ->
    splitList(List,Index,[],[]).

splitList([], _, Right, Left) -> % stopping condition we dont care about index, once the list is empty we return
    {Right,Left};

splitList([H|T], 0, Right, Left) -> % when the index is 0 we start adding elements to the left list
    splitList(T, 0, Right, [H|Left]);

splitList([H|T], Index, Right, Left) -> % when the index is bigger than 0 we add elements to the right list
    splitList(T, Index-1, [H|Right], Left).

% merges two sorted lists into one sorted list.
merge(List1,List2) -> merge(List1,List2,[]).

merge([],[], Acc) -> reverse(Acc);
merge([],[H|T], Acc)-> merge([],T,[H|Acc]);
merge([H|T],[], Acc)-> merge(T,[],[H|Acc]);
merge([H1|T1],[H2|T2], Acc) ->
    case H1 < H2 of
        true    -> merge(T1,        [H2|T2], [H1|Acc]);
        false   -> merge([H1|T1],   T2,      [H2|Acc])
    end.

%reverse List
reverse(List) -> 
    reverse(List,[]).

%arity 2 with accumulator
reverse([H|T],[]) -> reverse(T, [H]); % needed so we wont get a reverse result like this [[],5,4,3,2,1]
reverse([H|T],Reversed) -> reverse(T, [H|Reversed]); % the actual construction of the reversed list, we can safely assume Reversed will be a non empty list here.
reverse([],Reversed) -> Reversed. % stopping condition


matElemMult(A,B) when length(A) == length(B) -> [ [float(X*Y) || {X,Y} <- pack(R1,R2) ] || {R1,R2} <- pack(A,B)].


% pack receives two lists and returns a list where each element is a tuple of two elements from the lists
pack([],[]) -> [];
pack([],List) -> List;
pack(List,[]) -> List;
pack([H1|T1],[H2|T2]) -> [{H1,H2}| pack(T1,T2)].


% Filter := [numbers|atoms] 
% will return a list with elements that are not of type Filter.
filter_g(List,Filter) -> filter_g(List,Filter,[]).

filter_g([],_,Acc) -> reverse(Acc);

% if Filter == numbers, and H is number, skip it
filter_g([H|T],numbers,Acc) when is_number(H) ->
    filter_g(T,numbers, Acc);
% if Filter == atoms, and H is atom, skip it
filter_g([H|T],atoms,Acc) when is_atom(H) ->
    filter_g(T,atoms, Acc);
% keep the element if it doesn't need to be filtered:
filter_g([H|T],Filter,Acc) ->
    filter_g(T,Filter,[H|Acc]).




% same as filter_g but without using guards:
filter_p(List,Filter) -> filter_p(List,Filter,[]).

filter_p([],_,Acc) -> reverse(Acc);
filter_p([H|T],numbers,Acc) ->
    case is_number(H) of 
        true -> filter_p(T,numbers,Acc);
        false -> filter_p(T,numbers,[H|Acc])
    end;
filter_p([H|T],atoms,Acc) ->
    case is_atom(H) of 
        true -> filter_p(T,atoms,Acc);
        false -> filter_p(T,atoms,[H|Acc])
    end;
filter_p(List,Filter,_) ->
    io:format("Filter of type \"~p\" is not supported!\n",[Filter]),
    List.


% even function receives a list of numbers and returns a list with only the even numbers in respective order
even([]) -> [];
% we cant use reverse, and i assume we cant use ++ operator or similar things from lists, so i went with regular non tail recursion.
even([H|T]) when is_number(H), H rem 2 == 0 ->
    [H | even(T)]; % H is even, add him to the head of the list
even([H|T]) when is_number(H), H rem 2 == 1 ->
    even(T). % H is odd, skip it and continue

% fibonacci using regular recursion, runtime for N=30 is 1,451,797 microseconds
fiboR(1) -> 1;
fiboR(2) -> 1;
fiboR(N) when is_number(N), N > 2 ->
    fiboR(N-1) + fiboR(N-2).

% fibonacci using regular recursion, runtime for N=30 is 137 microseconds
fiboT(N) when is_number(N), N > 0 -> fiboT(N,0,1,1).
% we start from the bottom up, carry the last two elements
fiboT(N,Elem1,Elem2,N) -> Elem1 + Elem2; % stopping condition
fiboT(N,Elem1,Elem2,Index) ->
    fiboT(N,Elem1 + Elem2, Elem1, Index+1).

% tail recursion can run indefinitly with any N, only limited by the overflow of integers. much faster.

% measure_runtime() ->
%     {Time, Result} = timer:tc(fun() -> fiboT(30) end),
%     io:format("Runtime: ~p microseconds~n", [Time]),
%     io:format("Result: ~p~n", [Result]).

% main() ->
%     io:format("started\n"),
%     % List = [],
%     % List = [2,3,9,5,6,7,8,1,4,10],
%     % MatA = [
%     %     [1,2,3],
%     %     [4,5,6],
%     %     [5,6,7]
%     %     ],
%     % MatB = [
%     %     [8,9,10],
%     %     [11,12,13],
%     %     [14,15,16]
%     %     ],

%     % MatB = [[6],[5],[4],4],

%     % List1 = filter_g([1,a,2,b,c,3,4,5],numbers),
%     % List2 = filter_g([1,elem,elem,c,b,a,2,3,4,5],atoms),
%     % List3 =  filter_g([1.0,elem,1.5,1.789,5], numbers) ,
    
%     % List1 = filter_p([1,a,2,b,c,3,4,5],numbers),
%     % List2 = filter_p([1,elem,elem,c,b,a,2,3,4,5],atoms),
%     % List3 =  filter_p([1.0,elem,1.5,1.789,5], number) ,

%     % List4 = even([1,2,3,4,5,6,7,8,9]),
%     % List5 = even([1,3,4,2,6,5,1]),
%     % List6 = even([1]),
%     % N = 3,
%     % NumT = fiboT(N),
%     % NumR = fiboR(N),
%     % measure_runtime(),
%     % SortedListLC = sortRes(List,lc),
%     % SortedListpm = sortRes(List,pm),
%     % SortedListlm = sortRes(List,lm),
%     % Output = matElemMult(MatA,MatB),
%     io:format("finished\n").



