%%%-------------------------------------------------------------------
%% @doc erl_ide public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_ide_app).

-behaviour(application).
-import(ex2_311328322,[main/0]).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erl_ide_sup:start_link(),
    ex2_311328322:main(),
    stop(_StartType).

stop(_State) ->
    ok.

%% internal functions
