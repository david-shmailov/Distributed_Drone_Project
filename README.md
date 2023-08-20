# Erlang Drone Flock Simulation

This distributed system project simulates a flock of drones in a coordinated environment. Each drone is represented by an individual Erlang process, and every ground station (GS) is an Erlang node. These nodes can be executed across different PCs within the same Local Area Network (LAN).

[Demo video](https://youtu.be/ekqY0pUqW48)

## Prerequisites:
- **Python:** Version 3.8 or later.
- **PyQt5:** For the graphical user interface. You can find installation instructions [here](https://pythonbasics.org/install-pyqt/).
- **Erlang:** OTP 25.0

## Setup Instructions:

1. **Clone the Repository:** Ensure you clone the repository onto each participating PC.

2. **Follow Detailed Instructions:** Navigate to the `gui_pc` and `gs_pc` directories. Each contains specific README files with detailed setup instructions. 

3. **Determine PC Roles:** One PC should act as the GUI control center, while the others serve as Ground Station PCs. While the setup order isn't strictly crucial, it's generally recommended to establish the GUI PC first.

## Operating the Simulation:

1. **Node Initialization:** After launching the nodes, each should perform a handshake with the GUI to join the Erlang cluster. Upon successful connection, you'll notice the node names appear in the GUI's dropdown menu.

2. **Launching Drones:** Select a node, specify the desired number of drones, and then hit the "Launch" button. Initially, the drones will be stationary.

3. **Waypoint Configuration:** 
   - Click on "Plot Route" to activate the waypoint plotting mode.
   - Define the route by clicking on specific points on the map.
   - Exit the plotting mode by clicking "Plot Route" again.
   - Click "Send Waypoints" to dispatch the waypoints to the leader drone. You can always add more waypoints following this procedure.

4. **Target Setting:**
   - Click "Set Target" to initiate the target setting mode.
   - Place your desired targets by clicking on the map. Targets are instantly relayed to the drones.
   - Drones will search for these targets. When a drone identifies a target, a distinct red circle will highlight the target on the map.

---

Happy simulating!
