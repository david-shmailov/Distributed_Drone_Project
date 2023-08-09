% my_definitions.hrl
-define(RETRY_DELAY, 1000).
-define(GUI_NODE, 'gui@localhost').
-define(GUI_SERVER, 'gui_server').
-define(STACK_SIZE, 0). % we might not need aggregation at all 
-define(WORLD_SIZE,650).
-define(INFINITY, ?WORLD_SIZE*100).
-record(drone, {id, location, theta=0, speed=0, next_waypoint={{0,0},0}}).
-record(borders, {left, right, top, bottom}).
-record(mnesia_record, {id, pid}).
-define(TIMEOUT, 100).
-define(INDENTATION,{0,20}).
-define(STEP_SIZE,1).
-define(SERACH_RADIUS,50).
-define(FILE_NAME,"log.txt").


