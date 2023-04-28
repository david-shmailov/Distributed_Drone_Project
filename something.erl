-module(project_test).
-author("David").

-export([test/0]).
-import(exm_315284729,[exp_to_bdd/3, solve_bdd/2,listOfLeaves/1,reverseIteration/1]).

%% Parsing string into expression, for use in text input
string_to_expr(S) ->
    try
        {ok, Tokens, _} = erl_scan:string(S),
        {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
        {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
        {ok, Value}
    catch
        error:_Error -> {error, S}
    end.


run_test(File) -> run_test(File, [], 1).  % debug
run_test(File, Failed_tests, Number) ->  % debug
    case file:read_line(File) of
        {ok, Line} ->
                    {ok, BoolFunc} = string_to_expr(Line),
            try
                test_func(BoolFunc, tree_height),
                test_func(BoolFunc, num_of_nodes),
                test_func(BoolFunc, num_of_leafs),
                run_test(File, Failed_tests, Number+1)
            catch
                error:ErrorMessage ->
                    io:format("Test number ~p failed with error: ~p ~n", [Number, ErrorMessage]),
                    run_test(File, [Number | Failed_tests], Number+1)
            end;
        eof ->
            Failed_tests
    end.

test_func(BoolFunc, Order) ->  % debug
    Assignments = all_val_perms(extract_args(BoolFunc)),
    Best_map_tree = exp_to_bdd(BoolFunc,Order, map),
    Best_record_tree = exp_to_bdd(BoolFunc,Order, record),
    solve_all_assignments(Assignments, Best_map_tree, Best_record_tree),
    Map_leaves = listOfLeaves(Best_map_tree),
    Record_leaves = listOfLeaves(Best_record_tree),
    assert_Equal(length(Record_leaves), length(Map_leaves)),
    test_all_paths(Map_leaves, Record_leaves).

test_all_paths([], []) -> ok;
test_all_paths([Map_leaf | Map_rest], [Record_leaf | Record_rest]) ->  % debug
    assert_Equal(reverseIteration(Map_leaf), reverseIteration(Record_leaf)),
    test_all_paths(Map_rest, Record_rest).

solve_all_assignments([], _, _) -> ok;
solve_all_assignments([Assignment | Rest], Best_map_tree, Best_record_tree) ->  % debug
    assert_Equal(solve_bdd(Best_map_tree,Assignment), solve_bdd(Best_record_tree,Assignment)),
    solve_all_assignments(Rest, Best_map_tree, Best_record_tree).
        
all_val_perms([]) -> [[]];
all_val_perms([H|L]) -> [[{H, Val}|T] || Val <- [false, true], T <- all_val_perms(L)].

assert_Equal(Term1,Term2) -> % debug
    case
        Term1 == Term2 of
            true -> ok;
            false -> 
                Message = binary_to_list(list_to_binary(io_lib:format("Test failed: ~p != ~p~n", [Term1, Term2]))),
                erlang:error(Message)
    end.

extract_args(Expr) -> maps:keys(extract_args(Expr, #{})).
extract_args(Expr, Map) when is_atom(Expr) ->
    case Expr of
        true -> Map;
        false -> Map;
        undefined -> throw(reserved_name);
        _ -> Map#{Expr => found}
    end;
extract_args(Expr, Map) when is_integer(Expr) -> 
    case Expr of
        0 -> Map;
        1 -> Map;
        _ -> throw(invalid_boolean_value)
    end;
%% Recursive call inside the not argument, and for both arguments in 'and' and 'or' cases.
%% note that we have 1 call which is not in tail position, we might be able to optimize it
%% using 2 accumulators and calling different arity functions, but thats a job for another time.
extract_args(Expr, Map) ->
    case Expr of
        {'not', E} -> extract_args(E, Map);
        {'and', E1, E2} -> extract_args(E1, extract_args(E2, Map));
        {'or', E1, E2} -> extract_args(E1, extract_args(E2, Map));
        _ -> throw(invalid_expr)
    end.


test() -> 
    case file:open("boolPrime.txt", [read]) of
        {ok, File} ->
            Failed_tests = run_test(File),
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
