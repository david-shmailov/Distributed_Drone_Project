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
    ping_gui_node(),
    gs_server:start_link(),
    {ok, SupPid}.
    

ping_gui_node() ->
    case init:get_argument(gui_node) of
        {ok, [[GUI_NODE_STR]]} -> 
            GUI_NODE = list_to_atom(GUI_NODE_STR),
            case net_adm:ping(GUI_NODE) of
                pong ->
                    io:format("GUI is up!~n");
                pang ->
                    io:format("GUI Node is down, trying again~n"),
                    timer:sleep(1000),
                    ping_gui_node()
            end;
        error -> io:format("Argument not provided~n")
    end.

stop(_State) ->
    ok.

%% internal functions
