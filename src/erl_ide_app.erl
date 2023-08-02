%%%-------------------------------------------------------------------
%% @doc erl_ide public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_ide_app).

-behaviour(application).
-import(gui_interface_server,[start_link/2]).
-import(gui_interface_server,[  init/1,
                                handle_call/3,
                                handle_cast/2,
                                handle_info/2,
                                terminate/2,
                                code_change/3]).
% -import(project_test,[report/0]).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erl_ide_sup:start_link(),
    start_python_gui(),
    {ok, Pid} = gui_interface_server:start_link("localhost", 8000),
    gen_server:call(Pid, {send_data, <<"hello world">>}),
    % project_test:report(),
    timer:sleep(10000),
    stop(_StartType).

stop(_State) ->
    ok. % todo causes bad return 

start_python_gui() ->
    io:format("Starting python GUI~n"),
    Command = "python3 GUI/gui.py",
    Port = open_port({spawn, Command}, []),
    Port ! {self(), close}.

init_tables() ->
    GS1_ETS = ets:new(gs1_ets, [named_table, public, {write_concurrency, true}]),
    GS2_ETS = ets:new(gs2_ets, [named_table, public, {write_concurrency, true}]),
    GS3_ETS = ets:new(gs3_ets, [named_table, public, {write_concurrency, true}]),
    GS4_ETS = ets:new(gs4_ets, [named_table, public, {write_concurrency, true}]), % Todo figure out if you really need write_concurrency
    {GS1_ETS, GS2_ETS, GS3_ETS, GS4_ETS}.

%% internal functions
