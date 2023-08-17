% my_definitions.hrl
% -define(GUI_NODE, 'gui@localhost').
% -define(GUI_NODE, 'gui@172.21.28.158').
-define(GUI_SERVER, 'gui_server').
% -define(GUI_GLOBAL, {global, ?GUI_SERVER}).
% -define(GUI_GLOBAL, {?GUI_SERVER, ?GUI_NODE}).
-define(STACK_SIZE, 0). % we might not need aggregation at all 
-define(WORLD_SIZE,650).
-define(INFINITY, ?WORLD_SIZE*1000).
-define(TIMEOUT, 400).
-define(RETRY_DELAY, ?TIMEOUT*2).
-define(INDENTATION,{0,20}).
-define(STEP_SIZE,4).
-define(SERACH_RADIUS,20).
-define(LOG_NAME,"log.txt").
-define(PORT_ERL2PY, 8000).
-define(PORT_PY2ERL, 8001).
-define(MAX_THETA, math:pi()/8).
-define(DEBUG_MODE, false).


-record(drone, {id, location, theta=0, speed=0, pid = undefined, next_waypoint={{0,0},0},
    gs_server=undefined,time_stamp=undefined, state = slave, borders = undefined, targets = [],
    waypoints_stack = [], indentation = {0,0}, followers = []}).
-record(area, {left_border, right_border}).
-record(mnesia_record, {id, pid}).
-record(log_message, {time, source, message}).
% -export([get_time/0]).
% get_time()->%%in milliseconds-needs to be verified
%     {MegaSecs, Secs, MicroSecs} = os:system_time(),
%     MegaSecs * 1000 + Secs * 1000 + MicroSecs / 1000.



