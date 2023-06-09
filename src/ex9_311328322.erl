-module(ex9_311328322).

-export([etsBot/0]).

-define(INPUT_FILE, "etsCommands.txt").
-define(OUTPUT_FILE, "etsRes_311328322.ets").


etsBot() ->
    case file:read_file(?INPUT_FILE) of
        {ok, Data} ->
            Lines = string:tokens(binary_to_list(Data), "\n"),
            etsBot(Lines);
        {error, Reason} ->
            io:format("Error reading file: ~p~n", [Reason])
    end.

etsBot([]) -> write_ets_to_file();
etsBot([Line | Rest]) ->
    case string:tokens(Line, " ") of
        ["set"] ->
            delete_table_if_exists(),
            ets:new(ets_table, [named_table, set]);
        ["ordered_set"] ->
            delete_table_if_exists(),
            ets:new(ets_table, [named_table, ordered_set]);
        ["bag"] ->
            delete_table_if_exists(),
            ets:new(ets_table, [named_table, bag]);
        ["insert"|T] -> 
            insert_keys(T);
        ["update"|T] ->
            update_keys(T);
        ["delete"|T] ->
            delete_keys(T);
        ["lookup"|T] ->
            lookup_keys(T)
    end,
    etsBot(Rest).

delete_table_if_exists() ->
    case ets:info(ets_table) of
        undefined -> ok;
        _ -> ets:delete(ets_table)
    end.

write_ets_to_file() ->
    case file:open(?OUTPUT_FILE, [write]) of
        {ok, File} ->
            ets:foldl(fun({Key, Value}, _) ->
                            io:format(File, "~s ~s~n", [Key, Value])
                      end, ok, ets_table),
            file:close(File);
        {error, Reason} ->
            io:format("Error opening file: ~p~n", [Reason])
    end.

insert_keys([]) -> ok;
insert_keys([Key, Value | Rest]) ->
    % check that the key exists first:
    case ets:lookup(ets_table, Key) of
        [] ->
            ets:insert(ets_table, {Key, Value}),
            insert_keys(Rest);
        [{Key, _}] ->
            insert_keys(Rest)
    end.

update_keys([]) -> ok;
update_keys([Key, Value | Rest]) ->
    % check that the key exists first:
    case ets:lookup(ets_table, Key) of
        [] ->
            update_keys(Rest);
        [{Key, _}] ->
            ets:insert(ets_table, {Key, Value}),
            update_keys(Rest)
    end.

delete_keys([]) -> ok;
delete_keys([Key|T]) ->
    ets:delete(ets_table, Key),
    delete_keys(T).

lookup_keys([]) -> ok;
lookup_keys([Key|T]) ->
    case ets:lookup(ets_table, Key) of
        [] ->
            lookup_keys(T);
        [{Key, Value}] ->
            io:format("Key: ~p val: ~p~n", [Key, Value]),
            lookup_keys(T)
    end.