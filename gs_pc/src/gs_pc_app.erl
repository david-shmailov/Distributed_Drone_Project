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
    gen_server:call(gs_server, {drone_update, #drone{id = 1, location = {5,2}, theta = 30, speed = 50}}),
    timer:sleep(1000),
    gen_server:call(gs_server, {drone_update, #drone{id = 1, location = {3,2}, theta = 20, speed = 60}}),
    timer:sleep(1000),
    gen_server:call(gs_server, {drone_update, #drone{id = 1, location = {1,2}, theta = 10, speed = 70}}),
    {ok, SupPid}.
    

stop(_State) ->
    ok.

%% internal functions
