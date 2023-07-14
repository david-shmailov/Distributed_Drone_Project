-module(exm_311328322).

-export([exp_to_bdd/3, solve_bdd/2,listOfLeaves/1,reverseIteration/1]).
-record(tree, {node, left, right, path}). % record for the tree


exp_to_bdd(BoolFunc_RAW, Ordering, DataStructure) ->
    BoolFunc = parse_boolean_expression(BoolFunc_RAW), % this was added to support input from boolPrime.txt
    Start_time = erlang:monotonic_time(),
    % get all possible permutations of the variables
    Perms = get_permutations(BoolFunc),
    % build a tree for each permutation 
    Trees = [build_tree(BoolFunc,Perm, DataStructure) || Perm <- Perms],
    % reduce the trees by Rule 1 reduction
    Reduced_Trees = [rule_one_reduction(Tree) || Tree <- Trees],
    % get the best tree by the given ordering
    Best_Tree = get_best_tree(Reduced_Trees, Ordering),
    End_time = erlang:monotonic_time(),
    io:format("exp_to_bdd with ~ps Runtime: ~p ms~n", [DataStructure, (End_time-Start_time)/1000]), 
    Best_Tree.

% time measurement wrapper
solve_bdd(BddTree, VarAssigns) -> 
    Start_time = erlang:monotonic_time(),
    % call the actual function
    Result = solve_bdd(BddTree, VarAssigns, timed),
    End_time = erlang:monotonic_time(),
    case is_map(BddTree) of
        true -> DataStructure = map;
        false -> DataStructure = record
    end,
    io:format("solve_bdd with ~ps Runtime: ~p ms~n", [DataStructure, (End_time-Start_time)/1000]), 
    Result.
% the real function, needed to add timed to seperate recursion from runtime analysis
solve_bdd(#{node := Node, left := undefined, right := undefined}, _, timed) -> Node; % stopping condition for map types
solve_bdd(#tree{node = Node, left = undefined, right = undefined} ,_, timed) -> Node; % stopping condition for record types
solve_bdd(BddTree, VarAssigns, timed) when is_map(BddTree) -> % recursive call for map types
    Node = maps:get(node,BddTree),
    case lists:keyfind(Node,1,VarAssigns) of
        {Node, Value} ->case Value of % to support both true/false and 1/0
                            true -> solve_bdd(maps:get(right,BddTree), VarAssigns, timed);
                            false -> solve_bdd(maps:get(left,BddTree), VarAssigns, timed);
                            1 -> solve_bdd(maps:get(right,BddTree), VarAssigns, timed);
                            0 -> solve_bdd(maps:get(left,BddTree), VarAssigns, timed);
                            _ -> io:format("Variable \"~p\" has an invalid value: \"~p\"", [Node,Value])
                        end;
        false -> io:format("ERROR: Variable \"~p\" not found in VarAssigns", [Node])
    end; 
solve_bdd(BddTree, VarAssigns, timed) when is_record(BddTree,tree) ->  % recursive call for record types
    Node = BddTree#tree.node,
    case lists:keyfind(Node,1,VarAssigns) of
        {Node, Value} ->case Value of % to support both true/false and 1/0
                            true -> solve_bdd(BddTree#tree.right, VarAssigns, timed);
                            false -> solve_bdd(BddTree#tree.left, VarAssigns, timed);
                            1 -> solve_bdd(BddTree#tree.right, VarAssigns, timed);
                            0 -> solve_bdd(BddTree#tree.left, VarAssigns, timed);
                            _ -> io:format("Variable \"~p\" has an invalid value: \"~p\"", [Node, Value])
                        end;
        false -> io:format("ERROR: Variable \"~p\" not found in VarAssigns", [Node])
    end.

% runtime measurement wrapper function
listOfLeaves(BddTree) -> 
    Start_time = erlang:monotonic_time(),
    Leaves = listOfLeaves(BddTree, timed),
    End_time = erlang:monotonic_time(),
    case is_map(BddTree) of
        true -> DataStructure = map;
        false -> DataStructure = record
    end,
    io:format("listOfLeaves with ~ps Runtime: ~p ms~n", [DataStructure, (End_time-Start_time)/1000]), 
    Leaves.
% the real function, needed to add timed to seperate recursion from runtime analysis
listOfLeaves(#{node := Node,left := undefined, right := undefined, path := Path}, timed) -> 
    [#{node => Node, left => undefined, right => undefined, path => Path}]; % stopping condition for map types
listOfLeaves(#tree{node = Node,left = undefined, right = undefined, path=Path}, timed) -> 
    [#tree{node = Node,left = undefined, right = undefined, path = Path}]; % stopping condition for record types

% recursive call for map types
listOfLeaves(BddTree, timed) when is_map(BddTree) ->
    Right_leaves = listOfLeaves(maps:get(right,BddTree), timed),
    Left_leaves = listOfLeaves(maps:get(left,BddTree), timed),
    Right_leaves ++ Left_leaves;
% recursive call for record types
listOfLeaves(BddTree, timed) when is_record(BddTree,tree) ->
    Right_leaves = listOfLeaves(BddTree#tree.right, timed),
    Left_leaves = listOfLeaves(BddTree#tree.left, timed),
    Right_leaves ++ Left_leaves.

% runtime measurement wrapper function
reverseIteration(Leaf) ->
    Start_time = erlang:monotonic_time(),
    Path = reverseIteration(Leaf, timed),
    End_time = erlang:monotonic_time(),
    case is_map(Leaf) of
        true -> DataStructure = map;
        false -> DataStructure = record
    end,
    io:format("reverseIteration with ~ps Runtime: ~p ms~n", [DataStructure, (End_time-Start_time)/1000]),
    Path.
% the real function, needed to add timed to seperate recursion from runtime analysis
% reverseIteration runs in O(1) complexity with my implementation,
% because we keep the shortest path to root during the building of the tree
reverseIteration(Leaf, timed) when is_record(Leaf,tree) ->
    Leaf#tree.path;
reverseIteration(Leaf, timed) when is_map(Leaf) ->
    maps:get(path,Leaf).

% this function calculates the score of each tree and returns the tree with the lowest score
get_best_tree(Trees, Ordering) -> 
    Scores = [{get_tree_score(Tree, Ordering),Tree} || Tree <- Trees],
    get_min_tree_by_score(Scores).


% these functions call the appropriate function to calculate the score of the tree
get_tree_score(Tree, tree_height)   -> get_tree_height(Tree);
get_tree_score(Tree, num_of_nodes)  -> get_num_of_nodes(Tree,false);
get_tree_score(Tree, num_of_leafs)  -> get_num_of_nodes(Tree,true).

% calculates the height of the tree using depth accumulator
get_tree_height(Tree) -> get_tree_height(Tree, 0).
get_tree_height(#{left := undefined, right := undefined}, Depth) -> Depth; % stopping condition for map types
get_tree_height(#tree{left = undefined, right = undefined}, Depth) -> Depth; % stopping condition for record types
% recursive call for map types
get_tree_height(Tree, Depth) when is_map(Tree) ->
    Right_height = get_tree_height(maps:get(right,Tree), Depth+1),
    Left_height = get_tree_height(maps:get(left,Tree), Depth+1),
    case Left_height > Right_height of
        true -> Left_height;
        false -> Right_height
    end;
% recursive call for record types
get_tree_height(Tree, Depth) when is_record(Tree,tree) ->
    Right_height = get_tree_height(Tree#tree.right, Depth+1),
    Left_height = get_tree_height(Tree#tree.left, Depth+1),
    case Left_height > Right_height of
        true -> Left_height;
        false -> Right_height
    end.

% calculates the number of nodes/leaves in the tree using node accumulator
% to support both true/false and 1/0
get_num_of_nodes(#{left := undefined, right := undefined}, _) -> 1; % stopping condition for map types
get_num_of_nodes(#tree{left = undefined, right = undefined}, _) -> 1; % stopping condition for record types
get_num_of_nodes(Tree, Only_Leaves) when is_map(Tree) ->
    Right_nodes = get_num_of_nodes(maps:get(right,Tree), Only_Leaves),
    Left_nodes = get_num_of_nodes(maps:get(left,Tree), Only_Leaves),
    case Only_Leaves of
        true -> Right_nodes + Left_nodes; % if we only want the leaves, we don't count middle nodes
        false -> Right_nodes + Left_nodes +1 % if we want all nodes, we count the middle nodes as well
    end;
get_num_of_nodes(Tree, Only_Leaves) when is_record(Tree,tree) ->
    Right_nodes = get_num_of_nodes(Tree#tree.right, Only_Leaves),
    Left_nodes = get_num_of_nodes(Tree#tree.left, Only_Leaves),
    case Only_Leaves of
        true -> Right_nodes + Left_nodes; % if we only want the leaves, we don't count middle nodes
        false -> Right_nodes + Left_nodes +1 % if we want all nodes, we count the middle nodes as well
    end.
    

% simply iterate through the list of scores and return the *tree* with the lowest score
get_min_tree_by_score(Scores) -> get_min_tree_by_score(Scores, undefined).
get_min_tree_by_score([], {_,Min_tree}) -> Min_tree;
get_min_tree_by_score([{Score,Tree}|T], undefined) -> get_min_tree_by_score(T, {Score,Tree});
get_min_tree_by_score([{Score,Tree}|T], {Min_score,Min_tree}) ->
    case Score < Min_score of
        true -> get_min_tree_by_score(T, {Score,Tree});
        false -> get_min_tree_by_score(T, {Min_score,Min_tree})
    end.

% calls the appropriate function to build the tree
% BoolFunc: the boolean function that the tree will represent - required to evaluate leaf values.
% Perm:  ordered list (Permutation) of variables to build the tree from, e.g. [x1,x4,x2,x3]
% Returns: a Tree object (either a record or a map) representing the boolean function,
%         with the shortest path to root stored in the path field. Leaves are nodes with left and right branches undefined.
build_tree(BoolFunc, Perm, record) -> build_record_tree(BoolFunc,Perm);
build_tree(BoolFunc, Perm, map) -> build_map_tree(BoolFunc,Perm).

% builds a tree using records
build_record_tree(BoolFunc,Perm) -> build_record_tree(BoolFunc,Perm,[]).
build_record_tree(BoolFunc,[], VarAssigns) -> % stopping condition, make a leaf
    Node = eval_node(BoolFunc,VarAssigns),
    Path = lists:map(fun({Var, _}) -> Var end, VarAssigns ), % extract the variables from the VarAssigns
    #tree{node = Node, path = Path ,left = undefined, right = undefined}; % these are the leaves
build_record_tree(BoolFunc,[H|T],VarAssigns) ->
    % if we go left, the variable is false, if we go right, the variable is true
    Left_branch = build_record_tree(BoolFunc,T, [{H, false}|VarAssigns]),
    Right_branch = build_record_tree(BoolFunc,T, [{H, true}|VarAssigns]),
    Path = lists:map(fun({Var, _}) -> Var end, VarAssigns),
    % construct the tree:
    #tree{node = H, left = Left_branch, right = Right_branch, path=Path}.

% builds a tree using maps
build_map_tree(BoolFunc,Perm) -> build_map_tree(BoolFunc,Perm,[]).
build_map_tree(BoolFunc,[], VarAssigns) -> 
    Node = eval_node(BoolFunc,VarAssigns),
    Path = lists:map(fun({Var, _}) -> Var end, VarAssigns ), % extract the variables from the VarAssigns
    #{node => Node, path => Path, left => undefined, right => undefined}; % these are the leaves

build_map_tree(BoolFunc,[H|T],VarAssigns) ->
    % if we go left, the variable is false, if we go right, the variable is true
    Left_branch = build_map_tree(BoolFunc,T, [{H, false}|VarAssigns]),
    Right_branch = build_map_tree(BoolFunc,T, [{H, true}|VarAssigns]),
    Path = lists:map(fun({Var, _}) -> Var end, VarAssigns ), % extract the variables from the VarAssigns
    % construct the tree:
    #{node => H, left => Left_branch, right => Right_branch, path => Path}.

% evaluates the node via the boolean function. pattern matching matches the definition of the BoolFunc,
% BoolFunc: e.g. {'and', {x1, x2}} or {'not', x1}
% VarAssigns: list of tuples of the form {Var, Val} where Var is the variable name and Val is the value of the variable
eval_node({'not', Arg}, VarAssigns) -> not eval_node(Arg, VarAssigns);
eval_node({'or', {Arg1, Arg2}}, VarAssigns) ->  eval_node(Arg1, VarAssigns) or eval_node(Arg2, VarAssigns);
eval_node({'and', {Arg1, Arg2}}, VarAssigns) -> eval_node(Arg1, VarAssigns) and eval_node(Arg2, VarAssigns);
eval_node(Atom, VarAssigns)  when is_atom(Atom) ->
    case lists:keyfind(Atom,1,VarAssigns) of
        {Atom,Val} -> Val;
        false -> io:format("variable \"~p\" not found", [Atom])
    end.

% to support both true/false and 1/0
% if we have a map type leaf:
rule_one_reduction(#{node := Value, left := undefined, right := undefined, path := Path}) -> #{node => Value, left => undefined, right => undefined, path => Path};
% else map not a leaf:
rule_one_reduction(Tree) when is_map(Tree) ->
    Left_branch = rule_one_reduction(maps:get(left,Tree)),
    Right_branch = rule_one_reduction(maps:get(right,Tree)),
    case Right_branch == Left_branch of  % could be equal leaves or isomerphic subtrees! (Bonus points)
        true ->  % replace the current Tree with the right branch, except for the path, which is the same
            #{node => maps:get(node,Right_branch), left => maps:get(left,Right_branch), right => maps:get(right,Right_branch), path => maps:get(path,Tree)};
        false -> % keep the current tree, with the updated right and left branches recursivly
            Tree#{right => Right_branch, left => Left_branch}
    end;
% if we have a record type leaf:
rule_one_reduction(#tree{node = Value, left = undefined, right = undefined, path=Path}) -> 
    #tree{node = Value, left = undefined, right = undefined, path=Path};
% else record not a leaf:
rule_one_reduction(Tree) when is_record(Tree, tree) ->
    Right_branch = rule_one_reduction(Tree#tree.right),
    Left_branch = rule_one_reduction(Tree#tree.left),
    case Right_branch == Left_branch of % could be equal leaves or equal subtrees!
        % % replace the current Tree with the right branch, except for the path, which is the same
        true    -> 
            #tree{node = Right_branch#tree.node, left = Right_branch#tree.left , right = Right_branch#tree.right , path=Tree#tree.path};
        % keep the current tree, with the updated right and left branches recursivly:
        false   -> 
            #tree{node = Tree#tree.node, left = Left_branch, right = Right_branch, path=Tree#tree.path}
    end.

% Receives a boolean function and returns a list of all possible permutations of the variables
% required for building all possible trees from the variables
get_permutations(BoolFunc) ->
    List_of_vars = extract_variables(BoolFunc),
    permute_list(List_of_vars).

% get all possible permutations of List O(n!)
permute_list([]) -> [[]];
% Takes variable X and joins with all permutations of all other variables
permute_list(List) ->
    [ [X | Perm] || X <- List, Perm <- permute_list(List -- [X])].

% extracts all variables from a boolean function
extract_variables(BoolFunc) -> extract_variables(BoolFunc,[]).
extract_variables({'not', Arg} , Acc) -> extract_variables(Arg, Acc);
extract_variables({'or'  , {Arg1, Arg2}},Acc)  -> extract_variables(Arg1, extract_variables(Arg2, Acc));
extract_variables({'and' , {Arg1, Arg2}},Acc) -> extract_variables(Arg1, extract_variables(Arg2, Acc));
extract_variables(Var, Acc)  when is_atom(Var) ->
    case lists:member(Var,Acc) of
        true  -> Acc; % if Var is already in the list, don't add it again
        false -> [Var|Acc] % else add it to the list
    end.



% for debugging the boolPrime.txt
parse_boolean_expression(Expr) ->
    case Expr of
        {'not', Arg} ->
            {'not', parse_boolean_expression(Arg)};
        {'or', Arg1, Arg2} ->
            {'or', {parse_boolean_expression(Arg1), parse_boolean_expression(Arg2)}};
        {'and', Arg1, Arg2} ->
            {'and', {parse_boolean_expression(Arg1), parse_boolean_expression(Arg2)}};
        {'or', {Arg1, Arg2}} ->
            {'or', {parse_boolean_expression(Arg1), parse_boolean_expression(Arg2)}};
        {'and',{ Arg1, Arg2}} ->
            {'and', {parse_boolean_expression(Arg1), parse_boolean_expression(Arg2)}};
        Atom when is_atom(Atom) ->
            Atom
    end.








