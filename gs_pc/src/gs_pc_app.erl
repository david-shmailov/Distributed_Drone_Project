%%%-------------------------------------------------------------------
%% @doc gs_pc public API
%% @end
%%%-------------------------------------------------------------------

-module(gs_pc_app).
-include("../project_def.hrl").

-behaviour(application).

-export([start/2, stop/1,worker/0]).



start(_StartType, _StartArgs) ->
    {ok, SupPid} = gs_pc_sup:start_link(),
    ping_gui_node(),
    gs_server:start_link(),
    time_server:start_link(),
    receive
         shutdown ->
            % Perform cleanup or shutdown actions here
            io:format("Worker process shutting down.~n"),
            exit(normal);
        {'EXIT', _From, Reason} ->
            % Handle other process termination reasons
            io:format("Worker process terminated with reason: ~p~n", [Reason]),
            gs_server:start_link(1)
    end,
    {ok, SupPid}.
    

ping_gui_node() ->
    case init:get_argument(gui_node) of
        {ok, [[GUI_NODE_STR]]} -> 
            io:format("Pinging GUI node ~p~n", [GUI_NODE_STR]),
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

worker() ->
    receive
        shutdown ->
            % Perform cleanup or shutdown actions here
            io:format("Worker process shutting down.~n"),
            exit(normal);
        {'EXIT', _From, Reason} ->
            % Handle other process termination reasons
            io:format("Worker process terminated with reason: ~p~n", [Reason]),
            gs_server:start_link(1)
    end.
    

stop(_State) ->
    ok.

%% internal functions
