%%%-------------------------------------------------------------------
%% @doc gs_pc public API
%% @end
%%%-------------------------------------------------------------------

-module(gs_pc_app).

-behaviour(application).

-export([start/2, stop/1]).
-record(drone, {id, location, theta, speed}).


start(_StartType, _StartArgs) ->
    {ok, SupPid} = gs_pc_sup:start_link(),
    gs_server:start_link(),
    gen_server:call(gs_server, {launch_drones, 2}),
    % gen_server:call(gs_server, {drone_update, #drone{id = 1, location = {50,400}, theta = radian_to_degree(0), speed = 1}}),
    % timer:sleep(1000),
    % gen_server:call(gs_server, {drone_update, #drone{id = 2, location = {100,400}, theta = radian_to_degree(math:pi()/4), speed = 1}}),
    % timer:sleep(1000),
    % gen_server:call(gs_server, {drone_update, #drone{id = 3, location = {200,400}, theta = radian_to_degree(math:pi()/2), speed = 2}}),
    {ok, SupPid}.
    
radian_to_degree(Radian) ->
    DegreesFloat = Radian * (180/math:pi()),
    round(DegreesFloat).

stop(_State) ->
    ok.

%% internal functions
