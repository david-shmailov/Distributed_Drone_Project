import re
from datetime import datetime
import matplotlib.pyplot as plt

# Read and parse log file
log_entries = []
with open('log.txt', 'r') as file:
    for line in file:
        match = re.match(r'{log_message,\{(\d+,\d+,\d+)\},"(\w+)\s+(\w+)","(\d+)"', line)
        if match:
            timestamp_str, source, category, value_str = match.groups()
            timestamp = datetime.strptime(timestamp_str, "%H,%M,%S")
            value = int(value_str)
            log_entries.append((timestamp, source, category, value))

# Group and sum values by timestamp and category
time_sums = {}
for timestamp, source, category, value in log_entries:
    if timestamp not in time_sums:
        time_sums[timestamp] = {'gs_server': 0, 'drone': 0, 'total': 0}
    if source == 'gs_server':
        time_sums[timestamp]['gs_server'] += value
    elif source == 'drone':
        time_sums[timestamp]['drone'] += value
    time_sums[timestamp]['total'] += value

# Convert data for plotting
timestamps = list(time_sums.keys())
gs_server_values = [data['gs_server'] for data in time_sums.values()]
drone_values = [data['drone'] for data in time_sums.values()]
total_values = [data['total'] for data in time_sums.values()]

# Plot the data
plt.plot(timestamps, gs_server_values, label='Drone/GS Server')
plt.plot(timestamps, drone_values, label='Drone/Drone')
# plt.plot(timestamps, total_values, label='Total')
plt.xlabel('Time')
plt.ylabel('Message/Second')
plt.title('Values over Time')
plt.xticks(rotation=45)
plt.legend()
plt.tight_layout()
plt.show()
