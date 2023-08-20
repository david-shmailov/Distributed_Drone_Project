
-define(GUI_SERVER, 'gui_server').
-define(STACK_SIZE, 0). % we might not need aggregation at all 
-define(WORLD_SIZE,650).
-define(INFINITY, ?WORLD_SIZE*1000).
-define(TIMEOUT, 400). % in milliseconds, defines the time ticks of the simulation
-define(RETRY_DELAY, ?TIMEOUT*2). % delay that defines how long a caller will wait for a response from a callee before retrying, how long a message is valid before its outdated.
-define(INDENTATION,{0,20}). % relative vector that defines the shape of the flock formation. each drone keeps this position relative to its leader.
-define(STEP_SIZE,4). % defines the distance a drone moves in each step with speed of 1
-define(SERACH_RADIUS,20). % defines the radius in which a drone will search for targets
-define(LOG_NAME,"log.txt"). % name of the log file
-define(PORT_ERL2PY, 8000). % port for sending data to the python gui
-define(PORT_PY2ERL, 8001). % port for receiving data from the python gui
-define(MAX_THETA, math:pi()/8).  % defines the maximum angle a drone can turn in each step (not in use)
-define(DEBUG_MODE, false). % if set to true, will cause the drones to transmit every step

% drone state record
-record(drone, {id, location, theta=0, speed=0, pid = undefined, next_waypoint={{0,0},0},
    gs_server=undefined,time_stamp=undefined, state = slave, borders = undefined, targets = [],
    waypoints_stack = [], indentation = {0,0}, followers = []}).
% area record, defines the borders of an area
-record(area, {left_border, right_border}).
-record(log_message, {time, source, message}).



