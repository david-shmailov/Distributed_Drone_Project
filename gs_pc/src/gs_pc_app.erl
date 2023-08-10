%%%-------------------------------------------------------------------
%% @doc gs_pc public API
%% @end
%%%-------------------------------------------------------------------

-module(gs_pc_app).
-include("../project_def.hrl").

-behaviour(application).

-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
    {ok, SupPid} = gs_pc_sup:start_link(),
    gs_server:start_link(),
    % only gs1 creates!
    case node() of
        'gs1@localhost' ->
            % All_system_nodes = [node(), 'gui@localhost', 'gs2@localhost', 'gs3@localhost', 'gs4@localhost'], % todo make this not hardcoded
            % rpc:multicall(All_system_nodes, mnesia, stop, []),
            % io:format("deleted: ~p~n" ,[mnesia:delete_schema(All_system_nodes)]),
            % % application:start(mnesia),
            % io:format("~p~n" ,[mnesia:create_schema(All_system_nodes)]),
            % io:format("schema created~n"),
            % [io:format("~p : ~p~n",[Node,rpc:call(Node, application, start, [mnesia])]) || Node <- All_system_nodes],
            % io:format("mnesia started on all nodes~n"),
            % io:format("~p~n" ,[mnesia:create_table(database, [{disc_copies,All_system_nodes}, {attributes, record_info(fields, mnesia_record)}])]),
            % io:format("table created~n"),
            gen_server:call(gs_server, {launch_drones, 30}),
            gen_server:call(gs_server, set_followers),
            gen_server:call(gs_server, {set_waypoints, [ {{?WORLD_SIZE/4,?WORLD_SIZE/4}, 0}, {{?WORLD_SIZE/4,3*?WORLD_SIZE/4}, 0}, {{3*?WORLD_SIZE/4,3*?WORLD_SIZE/4},0},
            {{3*?WORLD_SIZE/4,?WORLD_SIZE/4},0}]}),
            {ok,File}=file:open(?FILE_NAME,[write]),
            io:format(File,"~s~n",["number of drones:30"]),
            file:close(File),
            gen_server:cast(gs_server,{aquire_target,{?WORLD_SIZE/2-100,?WORLD_SIZE/2+100}}),
            gen_server:cast(gs_server, {aquire_target,{3*?WORLD_SIZE/4,3*?WORLD_SIZE/4}});
        _ ->
            ok
    end,
    {ok, SupPid}.
    
radian_to_degree(Radian) ->
    DegreesFloat = Radian * (180/math:pi()),
    round(DegreesFloat).

stop(_State) ->
    ok.

%% internal functions
