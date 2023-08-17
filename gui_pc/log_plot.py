import re
from datetime import datetime
import matplotlib.pyplot as plt

# Read and parse log file
log_entries = []
with open('log.txt', 'r') as file:
    for line in file:
        match = re.match(r'{log_message,\{(\d+,\d+,\d+)},"(\w+)\s+(\w+)","(\d+)"', line)
        if match:
            timestamp_str, category, name, value_str = match.groups()
            timestamp = datetime.strptime(timestamp_str, "%H,%M,%S")
            value = int(value_str)
            log_entries.append((timestamp, category, name, value))

# Group and sum values by timestamp and category
time_sums = {}
for timestamp, category, name, value in log_entries:
    if timestamp not in time_sums:
        time_sums[timestamp] = {'server_server': 0, 'server_drone': 0,'drone_drone':0, 'total': 0}
    time_sums[timestamp][category] += value
    time_sums[timestamp]['total'] += value

# Convert data for plotting
timestamps = list(time_sums.keys())
server_values =         [data['server_server'] for data in time_sums.values()]
drone_server_values =   [data['server_drone'] for data in time_sums.values()]
drone_values =          [data['drone_drone'] for data in time_sums.values()]
total_values =          [data['total'] for data in time_sums.values()]

# Plot the data

plt.plot(timestamps, drone_values, label='Drone/Drone')
plt.plot(timestamps, drone_server_values, label='Drone/Server')
plt.plot(timestamps, server_values, label='Server/Server')
# plt.plot(timestamps, total_values, label='Total')
plt.xlabel('Time')
plt.ylabel('Msg/s', rotation=0, labelpad=20)
plt.title('Values over Time')
plt.xticks(rotation=45)
plt.legend()
plt.tight_layout()
plt.show()
