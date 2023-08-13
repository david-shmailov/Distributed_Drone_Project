% my_definitions.hrl
-define(RETRY_DELAY, 1000).
-define(GUI_NODE, 'gui@localhost').
-define(GUI_SERVER, 'gui_server').
% -define(GUI_GLOBAL, {global, ?GUI_SERVER}).
-define(GUI_GLOBAL, {?GUI_SERVER, ?GUI_NODE}).
-define(STACK_SIZE, 0). % we might not need aggregation at all 
-define(WORLD_SIZE,650).
-define(INFINITY, ?WORLD_SIZE*100).
-record(drone, {id, location, theta=0, speed=0, next_waypoint={{0,0},0},gs_server=undefined,time_stamp=undefined,pid = undefined}).
-record(area, {left_border, right_border}).
-record(mnesia_record, {id, pid}).
-record(log_message, {time, source, message}).
-define(TIMEOUT, 100).
-define(INDENTATION,{0,20}).
-define(STEP_SIZE,1).
-define(SERACH_RADIUS,20).
-define(LOG_NAME,"log.txt").
% -export([get_time/0]).
% get_time()->%%in milliseconds-needs to be verified
%     {MegaSecs, Secs, MicroSecs} = os:system_time(),
%     MegaSecs * 1000 + Secs * 1000 + MicroSecs / 1000.



