-module(project_test).
-author("David").

-export([test/0,test/1,report/0]).
-import(exm_311328322,[exp_to_bdd/3, solve_bdd/2,listOfLeaves/1,reverseIteration/1]).


% for debugging the boolPrime.txt
parse_boolean_expression(Expr) ->  % debug
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


parse_line(Line)->  % debug
    case re:run(Line, "^([0-9]+)\t\"(.*)\"\\n$", [{capture, all, list}]) of
        {match, Captured} ->
            {Number_str, String} = {lists:nth(2,Captured), lists:nth(3, Captured)};
        nomatch ->
            {Number_str, String} = {0, "fail"}
    end,
    String_dot = String ++ ".",
    {Number,_} = string:to_integer(Number_str),
    {ok,Tokens,_EndLine} = erl_scan:string(String_dot),
    {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
    {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    {Number, Value}.

% run_test(File) -> run_test(File, [], undefined).  % debug
run_test(File,Test_number) -> run_test(File, [], Test_number,0,0,0,0).
run_test(File, Failed_tests, Test_number, MbddT,RbddT,MST,RST) ->  % debug
    case file:read_line(File) of
        {ok, Line} ->
            {Number, Tuple} = parse_line(Line),
            case Test_number of
                undefined ->
                    try
                        BoolFunc = Tuple,%parse_boolean_expression(Tuple),
                        {MbddT1,RbddT1,MST1,RST1} = test_func(BoolFunc, tree_height),
                        {MbddT2,RbddT2,MST2,RST2} = test_func(BoolFunc, num_of_nodes),
                        {MbddT3,RbddT3,MST3,RST3} = test_func(BoolFunc, num_of_leafs),
                        Avg_MbddT = (MbddT1 + MbddT2 + MbddT3) / 3,
                        Avg_RbddT = (RbddT1 + RbddT2 + RbddT3) / 3,
                        Avg_MST = (MST1 + MST2 + MST3) / 3,
                        Avg_RST = (RST1 + RST2 + RST3) / 3,  
                        run_test(File, Failed_tests, Test_number, MbddT + Avg_MbddT, RbddT + Avg_RbddT, MST + Avg_MST, RST + Avg_RST)
                    catch
                        error:{assertion_error, ErrorMessage} ->
                            io:format("Test number ~p failed with error: ~p ~n", [Number, ErrorMessage]),
                            run_test(File, [Number |Failed_tests], Test_number, MbddT,RbddT,MST,RST)
                    end;
                _ when Number < Test_number ->
                    run_test(File, Failed_tests, Test_number, MbddT,RbddT,MST,RST);
                _ when Number == Test_number ->
                    try
                        io:format("Running Test number ~p: ~n", [Number]),
                        BoolFunc = Tuple, %parse_boolean_expression(Tuple),
                        test_func(BoolFunc, tree_height),
                        test_func(BoolFunc, num_of_nodes),
                        test_func(BoolFunc, num_of_leafs),
                        Failed_tests
                    catch
                        error:{assertion_error, ErrorMessage} ->
                            io:format("Test number ~p failed with error: ~p ~n", [Number, ErrorMessage]),
                            [Number |Failed_tests]
                    end
            end;
        eof ->
            Num_of_tests = 34246,
            io:format("Average map tree building time: ~p ~n", [MbddT/Num_of_tests]),
            io:format("Average record tree building time: ~p ~n", [RbddT/Num_of_tests]),
            io:format("Average map tree solving time: ~p ~n", [MST/Num_of_tests]),
            io:format("Average record tree solving time: ~p ~n", [RST/Num_of_tests]),
            Failed_tests
    end.



test_func(BoolFunc, Order) ->  % debug
    Assignments = generate_binary_combinations(get_vars(parse_boolean_expression(BoolFunc))),
    Start_map_bdd_build = erlang:monotonic_time(),
    Best_map_tree = exp_to_bdd(BoolFunc,Order, map),
    End_map_bdd_build = erlang:monotonic_time(),
    Best_record_tree = exp_to_bdd(BoolFunc,Order, record),
    End_record_bdd_build = erlang:monotonic_time(),
    {SolveBDD_Map_time, SolveBDD_Rec_time} = solve_all_assignments(Assignments, Best_map_tree, Best_record_tree,parse_boolean_expression(BoolFunc),0,0),
    Map_leaves = listOfLeaves(Best_map_tree),
    Record_leaves = listOfLeaves(Best_record_tree),
    assert_Equal(length(Record_leaves), length(Map_leaves)),
    test_all_paths(Map_leaves, Record_leaves),
    Build_map_bdd_time = (End_map_bdd_build - Start_map_bdd_build)/1000,
    Build_record_bdd_time = (End_record_bdd_build - End_map_bdd_build)/1000,
    {Build_map_bdd_time, Build_record_bdd_time, SolveBDD_Map_time, SolveBDD_Rec_time}.

test_all_paths([], []) -> ok;
test_all_paths([Map_leaf | Map_rest], [Record_leaf | Record_rest]) ->  % debug
    assert_Equal(reverseIteration(Map_leaf), reverseIteration(Record_leaf)),
    test_all_paths(Map_rest, Record_rest).

solve_all_assignments([], _, _,_,MapTime,RecTime) -> {MapTime,RecTime};
solve_all_assignments([Assignment | Rest], Best_map_tree, Best_record_tree,BoolFunc,MapTime,RecTime) ->  % debug
    Start_map_solveBDD = erlang:monotonic_time(),
    MapBddRes = solve_bdd(Best_map_tree,Assignment),
    End_map_solveBDD = erlang:monotonic_time(),
    RecBddRes = solve_bdd(Best_record_tree,Assignment),
    End_rec_solveBDD = erlang:monotonic_time(),
    FuncRes = solve_func(BoolFunc, Assignment),
    assert_Equal(MapBddRes, RecBddRes),
    assert_Equal(MapBddRes, FuncRes),
    assert_Equal(RecBddRes, FuncRes),
    UpdatedMapTime = MapTime + (End_map_solveBDD - Start_map_solveBDD)/1000,
    UpdatedRecTime = RecTime + (End_rec_solveBDD - End_map_solveBDD)/1000,
    solve_all_assignments(Rest, Best_map_tree, Best_record_tree,BoolFunc,UpdatedMapTime,UpdatedRecTime).
        
    
    
generate_binary_combinations(Atoms) -> generate_binary_combinations(Atoms, []). % debug
generate_binary_combinations([], Acc) -> [Acc];
generate_binary_combinations([Atom | Rest], Acc) ->
    generate_binary_combinations(Rest, [{Atom, false} | Acc]) ++ generate_binary_combinations(Rest, [{Atom, true} | Acc]).

assert_Equal(Term1,Term2) -> % debug
    case
        Term1 == Term2 of
            true -> ok;
            false -> 
                Message = binary_to_list(list_to_binary(io_lib:format("Test failed: ~p != ~p", [Term1, Term2]))),
                erlang:error({assertion_error, Message})
    end.

get_vars(BoolFunc) -> get_vars(BoolFunc,[]).
get_vars({'not', Arg} , Acc) -> get_vars(Arg, Acc);
get_vars({'or'  , {Arg1, Arg2}},Acc)  -> get_vars(Arg1, get_vars(Arg2, Acc));
get_vars({'and' , {Arg1, Arg2}},Acc) -> get_vars(Arg1, get_vars(Arg2, Acc));
get_vars(Atom, Acc)  when is_atom(Atom) ->
    case lists:member(Atom,Acc) of
        true  -> Acc;
        false -> [Atom|Acc]
    end.
solve_func({'not', Arg}, VarAssigns) -> not solve_func(Arg, VarAssigns);
solve_func({'or', {Arg1, Arg2}}, VarAssigns) ->  solve_func(Arg1, VarAssigns) or solve_func(Arg2, VarAssigns);
solve_func({'and', {Arg1, Arg2}}, VarAssigns) -> solve_func(Arg1, VarAssigns) and solve_func(Arg2, VarAssigns);
solve_func(Atom, VarAssigns)  when is_atom(Atom) ->
    case lists:keyfind(Atom,1,VarAssigns) of
        {Atom,Val} -> Val;
        false -> io:format("variable \"~p\" not found", [Atom])
    end.
test() -> test(undefined).
test(Test_number) -> 
    case file:open("boolPrime.txt", [read]) of
        {ok, File} ->
            Failed_tests = run_test(File,Test_number),
            case Failed_tests of
                [] ->
                    io:format("All tests passed!~n");
                _ ->
                    io:format("Failed tests: ~p~n", [Failed_tests])
            end,
            file:close(File);
        {error, Reason} ->
            io:format("Error opening file: ~p~n", [Reason])
    end.

report()->
    Nx2 = {'not', 'x2'},
    Nx3 = {'not', 'x3'},
    Nx4 = {'not', 'x4'},
    Nx1 = {'not', 'x1'},
    ANDx1x3 = {'and', {'x1', 'x3'}},
    FirstTerm = {'and', {Nx2, ANDx1x3}},
    ORNx4x2 = {'or', {Nx4, 'x2'}},
    ANDx1Nx3 = {'and', {'x1', Nx3}},
    ANDx1Nx3ORx4x2 = {'and', {ANDx1Nx3, ORNx4x2}},
    SecondTerm = {'not', ANDx1Nx3ORx4x2},
    ThirdTerm = {'and',{Nx4,Nx1}},
    BoolFunc = {'or', {FirstTerm, {'or', {SecondTerm, ThirdTerm}}}},
    io:format("tree_height BDD creation:~n"),
    BDD_TH_M = exm_311328322:exp_to_bdd(BoolFunc, tree_height, map),
    BDD_TH_R = exm_311328322:exp_to_bdd(BoolFunc, tree_height, record),
    io:format("num_of_nodes BDD creation:~n"),
    BDD_NN_M = exm_311328322:exp_to_bdd(BoolFunc, num_of_nodes, map),
    BDD_NN_R = exm_311328322:exp_to_bdd(BoolFunc, num_of_nodes, record),
    io:format("num_of_leafs BDD creation:~n"),
    BDD_NL_M = exm_311328322:exp_to_bdd(BoolFunc, num_of_leafs, map),
    BDD_NL_R = exm_311328322:exp_to_bdd(BoolFunc, num_of_leafs, record),
    Trees = [{BDD_TH_M, BDD_TH_R, tree_height}, {BDD_NN_M, BDD_NN_R, num_of_nodes}, {BDD_NL_M, BDD_NL_R, num_of_leafs}],
    Assignments = generate_binary_combinations(get_vars(BoolFunc)),
    [{io:format("Solving Trees by: ~p~n", [Order]),solve_all_assignments(Assignments,MAP_TREE,REC_TREE,BoolFunc,0,0)} || {MAP_TREE, REC_TREE, Order} <- Trees],
    io:format("Experiment finished. ~n").
