%%%-------------------------------------------------------------------
%% @doc erl_ide public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_ide_app).

-behaviour(application).
-import(ex_test,[test/0]).
% -import(project_test,[report/0]).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erl_ide_sup:start_link(),
    ex_test:test(),
    % project_test:report(),
    stop(_StartType).

stop(_State) ->
    ok.

%% internal functions
