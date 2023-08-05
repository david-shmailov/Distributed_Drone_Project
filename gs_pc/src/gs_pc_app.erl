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
            gen_server:call(gs_server, {launch_drones, 1}),
            gen_server:call(gs_server, set_followers),
            gen_server:call(gs_server, {set_waypoints, [ {{?WORLD_SIZE/4,?WORLD_SIZE/4}, 0}, {{?WORLD_SIZE/4,3*?WORLD_SIZE/4}, 0}, {{3*?WORLD_SIZE/4,3*?WORLD_SIZE/4},0},{{3*?WORLD_SIZE/4,?WORLD_SIZE/4},0}]});
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
