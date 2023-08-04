%%%-------------------------------------------------------------------
%% @doc gui_pc public API
%% @end
%%%-------------------------------------------------------------------

-module(gui_pc_app).

-behaviour(application).
-import(gui_server,[start_link/2]).
-import(gui_server,[  init/1,
                                handle_call/3,
                                handle_cast/2,
                                handle_info/2,
                                terminate/2,
                                code_change/3]).
% -import(project_test,[report/0]).
-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
    {ok, SupPid} = gui_pc_sup:start_link(),
    Port = 8000,
    start_python_gui(Port),
    gui_server:start_link(Port),
    {ok, SupPid}.

stop(_State) ->
    ok. % todo causes bad return 

start_python_gui(Port) ->
    io:format("Starting python GUI~n"),
    Command = "python3 Qt_GUI/gui.py --port " ++ integer_to_list(Port) ++ " &",
    Cmd_Port = open_port({spawn, Command}, []),
    Cmd_Port ! {self(), close}.



%% internal functions
