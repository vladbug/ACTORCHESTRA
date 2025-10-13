import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from io import StringIO

# Data as provided
data_str = """Clients,Messages_Per_Client,Total_Messages,System_Type,Total_Time_ms,Message_Time_ms,Join_Time_ms,Leave_Time_ms,Throughput_msg_per_sec,Avg_Latency_us,Overhead_Percent,CV_Percent,Iterations
5,30,150,Baseline,38.277,34.570,2.632,1.075,4576.166,230.469,0.0,25.756,10
5,30,150,Instrumented,86.979,77.005,6.226,3.748,1990.199,513.366,127.233,16.674,10
10,30,300,Baseline,76.053,70.943,2.703,2.406,4337.528,236.476,0.0,16.483,10
10,30,300,Instrumented,165.673,147.272,9.349,9.052,2046.079,490.906,117.840,8.023,10
20,30,600,Baseline,145.899,137.114,5.079,3.707,4462.172,228.523,0.0,15.100,10
20,30,600,Instrumented,333.865,302.674,19.241,11.950,1992.511,504.457,128.832,7.676,10
50,20,1000,Baseline,225.198,204.667,12.309,8.223,4936.120,204.667,0.0,10.270,10
50,20,1000,Instrumented,515.154,453.468,33.465,28.222,2207.744,453.468,128.756,3.493,10
100,10,1000,Baseline,245.432,201.605,26.757,17.070,4996.714,201.605,0.0,7.648,10
100,10,1000,Instrumented,559.872,436.797,65.823,57.252,2293.098,436.797,128.117,3.575,10
200,10,2000,Baseline,428.124,351.836,44.728,31.559,5699.390,175.918,0.0,3.955,10
200,10,2000,Instrumented,1048.443,822.436,122.102,103.906,2432.092,411.218,144.892,1.718,10"""

# Read the data
df = pd.read_csv(StringIO(data_str))

# Separate baseline and instrumented data
baseline_data = df[df['System_Type'] == 'Baseline'].reset_index(drop=True)
instrumented_data = df[df['System_Type'] == 'Instrumented'].reset_index(drop=True)

# Set up the plotting style
plt.style.use('default')
sns.set_palette("husl")

# ===== CHART 1: Message Processing Time Comparison =====
plt.figure(figsize=(10, 6))

message_times_baseline = baseline_data['Message_Time_ms']
message_times_instrumented = instrumented_data['Message_Time_ms']
client_labels = [f'{clients}C' for clients in baseline_data['Clients']]

plt.plot(range(len(message_times_baseline)), message_times_baseline, 
         'o-', label='Baseline', linewidth=3, markersize=10, color='#2E86AB')
plt.plot(range(len(message_times_instrumented)), message_times_instrumented, 
         's-', label='Instrumented', linewidth=3, markersize=10, color='#A23B72')

plt.xlabel('Test Configuration', fontsize=12)
plt.ylabel('Message Time (ms)', fontsize=12)
#plt.title('Message Processing Time Comparison', fontsize=14, fontweight='bold')
plt.xticks(range(len(baseline_data)), client_labels)
plt.legend(fontsize=11)
plt.grid(alpha=0.3)
plt.tight_layout()
plt.savefig('message_processing_time_comparison.pdf', format='pdf', bbox_inches='tight', dpi=300)
plt.show()
print("✓ Saved: message_processing_time_comparison.pdf")

# ===== CHART 2: Throughput Comparison =====
plt.figure(figsize=(10, 6))

x_pos = np.arange(len(baseline_data))
width = 0.35
config_labels = [f'{clients}C/{msgs}M' for clients, msgs in 
                zip(baseline_data['Clients'], baseline_data['Messages_Per_Client'])]

bars1 = plt.bar(x_pos - width/2, baseline_data['Throughput_msg_per_sec'], width,
                label='Baseline', alpha=0.8, color='#2E86AB')
bars2 = plt.bar(x_pos + width/2, instrumented_data['Throughput_msg_per_sec'], width,
                label='Instrumented', alpha=0.8, color='#A23B72')

plt.xlabel('Test Configuration', fontsize=12)
plt.ylabel('Throughput (msg/sec)', fontsize=12)
#plt.title('Throughput: Baseline vs Instrumented', fontsize=14, fontweight='bold')
plt.xticks(x_pos, config_labels, rotation=45)
plt.legend(fontsize=11)
plt.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('throughput_comparison.pdf', format='pdf', bbox_inches='tight', dpi=300)
plt.show()
print("✓ Saved: throughput_comparison.pdf")

# ===== CHART 3: Average Latency Comparison =====
plt.figure(figsize=(10, 6))

bars1 = plt.bar(x_pos - width/2, baseline_data['Avg_Latency_us'], width,
                label='Baseline', alpha=0.8, color='#2E86AB')
bars2 = plt.bar(x_pos + width/2, instrumented_data['Avg_Latency_us'], width,
                label='Instrumented', alpha=0.8, color='#A23B72')

plt.xlabel('Test Configuration', fontsize=12)
plt.ylabel('Average Latency (μs)', fontsize=12)
#plt.title('Average Latency: Baseline vs Instrumented', fontsize=14, fontweight='bold')
plt.xticks(x_pos, config_labels, rotation=45)
plt.legend(fontsize=11)
plt.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('latency_comparison.pdf', format='pdf', bbox_inches='tight', dpi=300)
plt.show()
print("✓ Saved: latency_comparison.pdf")

print("\n=== PDF FILES GENERATED ===")
print("Three standalone charts have been saved as PDFs:")
print("1. message_processing_time_comparison.pdf")
print("2. throughput_comparison.pdf")
print("3. latency_comparison.pdf")
print("\nAll files saved with high resolution (300 DPI) for professional quality.")