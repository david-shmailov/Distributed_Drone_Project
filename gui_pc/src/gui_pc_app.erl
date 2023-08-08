%%%-------------------------------------------------------------------
%% @doc gui_pc public API
%% @end
%%%-------------------------------------------------------------------

-module(gui_pc_app).
-include("../project_def.hrl").
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
    Port_erl2py = 8000,
    Port_py2erl = 8001,
    start_python_gui(Port_erl2py,Port_py2erl),
    gui_server:start_link(Port_erl2py,Port_py2erl),
    {ok, SupPid}.

stop(_State) ->
    ok. % todo causes bad return 

start_python_gui(Port_erl2py,Port_py2erl) ->
    io:format("Starting python GUI~n"),
    Command = io_lib:format("python3 Qt_GUI/gui.py --in_port ~p --out_port ~p &", [Port_erl2py,Port_py2erl]),
    Str_command = lists:flatten(Command),
    Cmd_Port = open_port({spawn, Str_command}, []),
    Cmd_Port ! {self(), close}.



%% internal functions
