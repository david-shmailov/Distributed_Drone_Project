%%%-------------------------------------------------------------------
%% @doc gui_pc public API
%% @end
%%%-------------------------------------------------------------------

-module(gui_pc_app).
-include("../project_def.hrl").
-behaviour(application).
-import(gui_server,[start_link/2]).
-import(gui_server,[init/1,
                    handle_call/3,
                    handle_cast/2,
                    handle_info/2,
                    terminate/2,
                    code_change/3]).
% -import(project_test,[report/0]).
-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
    {ok, SupPid} = gui_pc_sup:start_link(),
    start_python_gui(),
    gui_server:start_link(?PORT_ERL2PY,?PORT_PY2ERL),
    {ok, SupPid}.

stop(_State) ->
    ok.  

start_python_gui() ->
    io:format("Starting python GUI~n"),
    Command = io_lib:format("python3 Qt_GUI/gui.py --in_port ~p --out_port ~p --world_size ~p --timeout ~p --search_radius ~p --step_size ~p &", 
        [?PORT_ERL2PY,?PORT_PY2ERL, ?WORLD_SIZE, ?TIMEOUT, ?SERACH_RADIUS, ?STEP_SIZE]),
    Str_command = lists:flatten(Command),
    Cmd_Port = open_port({spawn, Str_command}, []),
    Cmd_Port ! {self(), close}.



%% internal functions
