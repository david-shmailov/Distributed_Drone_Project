%%%-------------------------------------------------------------------
%% @doc gs_pc public API
%% @end
%%%-------------------------------------------------------------------

-module(gs_pc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gs_pc_sup:start_link(),
    gs_server:start_link().

stop(_State) ->
    ok.

%% internal functions
