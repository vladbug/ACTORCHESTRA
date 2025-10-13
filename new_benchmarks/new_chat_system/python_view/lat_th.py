import pandas as pd
import matplotlib.pyplot as plt
from io import StringIO

# Chat system CSV data
chat_data = """Clients,Messages_Per_Client,Total_Messages,System_Type,Total_Time_ms,Message_Time_ms,Response_Time_ms,Async_Work_ms,Join_Time_ms,Leave_Time_ms,Full_Throughput_per_sec,Response_Throughput_per_sec,Avg_Latency_us,Async_Ratio,Full_Overhead_Percent,Response_Overhead_Percent,CV_Percent,Iterations
5,30,150,Baseline,211.226,204.672,82.240,122.432,2.458,4.096,733.843,1835.183,1364.481,0.598,0.0,0.0,4.500,8
5,30,150,Instrumented,260.877,247.437,123.149,124.288,6.029,7.411,609.263,1235.540,1649.579,0.504,23.506,49.743,6.531,8
10,30,300,Baseline,389.837,375.283,248.730,126.553,5.440,9.114,801.177,1212.392,1250.944,0.338,0.0,0.0,4.972,8
10,30,300,Instrumented,490.969,465.894,339.546,126.349,8.678,16.397,644.973,886.306,1552.981,0.272,25.942,36.512,4.336,8
20,30,600,Baseline,1101.350,1085.248,935.296,149.952,6.323,9.779,553.570,642.543,1808.746,0.138,0.0,0.0,3.705,8
20,30,600,Instrumented,1254.144,1209.562,1060.621,148.941,22.951,21.632,496.078,565.759,2015.936,0.123,13.873,13.399,1.369,8
40,30,1200,Baseline,3785.574,3761.203,3523.456,237.747,9.523,14.848,319.117,340.646,3134.336,0.063,0.0,0.0,1.643,8
40,30,1200,Instrumented,4269.952,4206.643,3959.322,247.321,33.280,30.029,285.478,303.352,3505.536,0.059,12.795,12.370,2.938,8
80,30,2400,Baseline,15564.685,15528.743,14890.970,637.773,16.960,18.982,154.563,161.183,6470.309,0.041,0.0,0.0,0.899,8
80,30,2400,Instrumented,16447.386,16340.595,15692.365,648.230,53.299,53.491,146.917,152.981,6808.581,0.040,5.671,5.382,1.855,8"""

# Load into pandas
df = pd.read_csv(StringIO(chat_data))

# Separate baseline and instrumented and sort by client count
baseline = df[df['System_Type'] == 'Baseline'].sort_values('Clients')
instrumented = df[df['System_Type'] == 'Instrumented'].sort_values('Clients')

# Create the plot exactly like the math system example
plt.figure(figsize=(10, 6))

# Scatter plots
plt.scatter(baseline['Full_Throughput_per_sec'], baseline['Avg_Latency_us'], 
           label="Baseline", marker='o', color='blue', s=50, zorder=3)
plt.scatter(instrumented['Full_Throughput_per_sec'], instrumented['Avg_Latency_us'], 
           label="Instrumented", marker='s', color='orange', s=50, zorder=3)

# Connect points with lines
plt.plot(baseline['Full_Throughput_per_sec'], baseline['Avg_Latency_us'], 
         linestyle='--', color='blue', alpha=0.7, zorder=2)
plt.plot(instrumented['Full_Throughput_per_sec'], instrumented['Avg_Latency_us'], 
         linestyle='--', color='orange', alpha=0.7, zorder=2)

# Set log scales
plt.xscale("log")
plt.yscale("log")

# Labels and title - exactly matching the math system format
plt.xlabel("Throughput (msgs/sec, log scale)")
plt.ylabel("Latency (µs, log scale)")
plt.title("Latency vs Throughput (Baseline vs Instrumented, Log Scales)")

# Legend and grid
plt.legend()
plt.grid(True, which="both", linestyle="--", alpha=0.6, zorder=1)

plt.tight_layout()
plt.show()

# Print some stats to see the difference
print("Chat System Performance Comparison:")
print("=" * 40)
print(f"Baseline throughput range: {baseline['Full_Throughput_per_sec'].min():.1f} - {baseline['Full_Throughput_per_sec'].max():.1f} msgs/sec")
print(f"Instrumented throughput range: {instrumented['Full_Throughput_per_sec'].min():.1f} - {instrumented['Full_Throughput_per_sec'].max():.1f} msgs/sec")
print(f"Baseline latency range: {baseline['Avg_Latency_us'].min():.1f} - {baseline['Avg_Latency_us'].max():.1f} µs")
print(f"Instrumented latency range: {instrumented['Avg_Latency_us'].min():.1f} - {instrumented['Avg_Latency_us'].max():.1f} µs")

# Show the scaling direction
print(f"\nChat system scaling pattern:")
print(f"• Higher client counts → Lower throughput (more recipients per message)")
print(f"• Higher client counts → Higher latency (more coordination needed)")
print(f"• Both systems follow similar curves (good scaling behavior)")