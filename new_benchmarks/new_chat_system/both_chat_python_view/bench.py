import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from io import StringIO

# No-broadcast data
no_broadcast_data_str = """Clients,Messages_Per_Client,Total_Messages,System_Type,Total_Time_ms,Message_Time_ms,Join_Time_ms,Leave_Time_ms,Throughput_msg_per_sec,Avg_Latency_us,Overhead_Percent,CV_Percent,Iterations
5,30,150,Baseline,33.475,26.706,5.786,0.983,5793.861,876.131,0.0,21.830,10
5,30,150,Instrumented,76.390,67.533,5.540,3.318,2290.896,2239.415,128.203,20.100,10
10,30,300,Baseline,68.690,59.781,6.799,2.110,5074.327,1964.646,0.0,14.044,10
10,30,300,Instrumented,159.518,145.500,8.745,5.273,2082.157,4806.484,132.230,9.827,10
20,30,600,Baseline,129.403,118.917,6.697,3.789,5176.928,3906.747,0.0,14.865,10
20,30,600,Instrumented,303.401,277.893,15.206,10.301,2162.636,9158.859,134.462,3.722,10
50,20,1000,Baseline,224.911,205.445,11.039,8.427,4907.577,10073.031,0.0,8.757,10
50,20,1000,Instrumented,534.313,461.681,43.254,29.378,2180.223,22624.916,137.566,7.105,10
100,10,1000,Baseline,259.799,212.603,27.300,19.896,4792.175,20428.592,0.0,13.037,10
100,10,1000,Instrumented,602.706,470.334,72.049,60.324,2136.907,45000.214,131.989,6.074,10
200,10,2000,Baseline,426.250,353.219,41.257,31.775,5676.481,33666.107,0.0,4.044,10
200,10,2000,Instrumented,1044.777,811.100,121.917,111.759,2465.962,77428.988,145.109,1.846,10"""

# Broadcast data
broadcast_data_str = """Clients,Messages_Per_Client,Total_Messages,System_Type,Total_Time_ms,Message_Time_ms,Join_Time_ms,Leave_Time_ms,Throughput_msg_per_sec,Avg_Latency_us,Overhead_Percent,CV_Percent,Iterations
5,30,150,Baseline,97.546,88.166,6.707,2.673,1711.136,2928.843,0.0,7.516,10
5,30,150,Instrumented,141.466,132.280,5.202,3.983,1156.516,4400.607,45.024,14.464,10
10,30,300,Baseline,285.502,270.797,6.523,8.182,1116.699,9012.601,0.0,9.115,10
10,30,300,Instrumented,348.611,329.124,8.407,11.080,913.409,10957.620,22.105,4.673,10
20,30,600,Baseline,930.519,896.164,5.315,29.041,669.995,29843.795,0.0,2.717,10
20,30,600,Instrumented,1112.422,1059.492,14.602,38.328,566.591,35290.934,19.549,2.478,10
50,20,1000,Baseline,3675.863,3478.395,11.499,185.969,287.666,173820.611,0.0,2.579,10
50,20,1000,Instrumented,4079.985,3812.188,39.137,228.659,262.370,190485.874,10.994,1.441,10
100,10,1000,Baseline,7524.454,6710.671,21.740,792.043,149.073,670606.028,0.0,2.258,10
100,10,1000,Instrumented,8017.500,7105.945,73.390,838.165,140.786,710029.291,6.553,2.343,10
200,10,2000,Baseline,31209.032,28017.818,42.158,3149.056,71.389,2800533.212,0.0,1.004,10
200,10,2000,Instrumented,32467.304,29097.288,116.121,3253.895,68.747,2908644.148,4.032,1.250,10"""

# Read the data
no_broadcast_df = pd.read_csv(StringIO(no_broadcast_data_str))
broadcast_df = pd.read_csv(StringIO(broadcast_data_str))

# Separate baseline and instrumented data for both systems
no_broadcast_baseline = no_broadcast_df[no_broadcast_df['System_Type'] == 'Baseline'].reset_index(drop=True)
no_broadcast_instrumented = no_broadcast_df[no_broadcast_df['System_Type'] == 'Instrumented'].reset_index(drop=True)

broadcast_baseline = broadcast_df[broadcast_df['System_Type'] == 'Baseline'].reset_index(drop=True)
broadcast_instrumented = broadcast_df[broadcast_df['System_Type'] == 'Instrumented'].reset_index(drop=True)

# Set up the plotting style
plt.style.use('default')
sns.set_palette("husl")

# Configuration labels
config_labels = [f'{clients}C/{msgs}M' for clients, msgs in 
                zip(no_broadcast_baseline['Clients'], no_broadcast_baseline['Messages_Per_Client'])]
x_pos = np.arange(len(config_labels))
width = 0.35

# ===== NO-BROADCAST SYSTEM PLOTS =====

# Chart 1: No-Broadcast Message Processing Time
plt.figure(figsize=(12, 6))
client_labels = [f'{clients}C' for clients in no_broadcast_baseline['Clients']]

plt.plot(range(len(no_broadcast_baseline)), no_broadcast_baseline['Total_Time_ms'], 
         'o-', label='Baseline', linewidth=3, markersize=10, color='#2E86AB')
plt.plot(range(len(no_broadcast_instrumented)), no_broadcast_instrumented['Total_Time_ms'], 
         's-', label='Instrumented', linewidth=3, markersize=10, color='#A23B72')

plt.xlabel('Test Configuration', fontsize=12)
plt.ylabel('Message Time (ms)', fontsize=12)
#plt.title('No-Broadcast System: Message Processing Time', fontsize=14, fontweight='bold')
plt.xticks(range(len(no_broadcast_baseline)), client_labels)
plt.legend(fontsize=11)
plt.grid(alpha=0.3)
plt.tight_layout()
plt.savefig('no_broadcast_message_time.pdf', format='pdf', bbox_inches='tight', dpi=300)
plt.show()

# Chart 2: No-Broadcast Throughput
plt.figure(figsize=(12, 6))
bars1 = plt.bar(x_pos - width/2, no_broadcast_baseline['Throughput_msg_per_sec'], width,
                label='Baseline', alpha=0.8, color='#2E86AB')
bars2 = plt.bar(x_pos + width/2, no_broadcast_instrumented['Throughput_msg_per_sec'], width,
                label='Instrumented', alpha=0.8, color='#A23B72')

plt.xlabel('Test Configuration', fontsize=12)
plt.ylabel('Throughput (msg/sec)', fontsize=12)
#plt.title('No-Broadcast System: Throughput Comparison', fontsize=14, fontweight='bold')
plt.xticks(x_pos, config_labels, rotation=45)
plt.legend(fontsize=11)
plt.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('no_broadcast_throughput.pdf', format='pdf', bbox_inches='tight', dpi=300)
plt.show()

# Chart 3: No-Broadcast Latency (converted to ms)
plt.figure(figsize=(12, 6))
bars1 = plt.bar(x_pos - width/2, no_broadcast_baseline['Avg_Latency_us'] / 1000, width,
                label='Baseline', alpha=0.8, color='#2E86AB')
bars2 = plt.bar(x_pos + width/2, no_broadcast_instrumented['Avg_Latency_us'] / 1000, width,
                label='Instrumented', alpha=0.8, color='#A23B72')

plt.xlabel('Test Configuration', fontsize=12)
plt.ylabel('Average Latency (ms)', fontsize=12)
#plt.title('No-Broadcast System: Average Latency Comparison', fontsize=14, fontweight='bold')
plt.xticks(x_pos, config_labels, rotation=45)
plt.legend(fontsize=11)
plt.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('no_broadcast_latency.pdf', format='pdf', bbox_inches='tight', dpi=300)
plt.show()

# ===== BROADCAST SYSTEM PLOTS =====

# Chart 4: Broadcast Message Processing Time
plt.figure(figsize=(12, 6))
plt.plot(range(len(broadcast_baseline)), broadcast_baseline['Total_Time_ms'], 
         'o-', label='Baseline', linewidth=3, markersize=10, color='#2E86AB')
plt.plot(range(len(broadcast_instrumented)), broadcast_instrumented['Total_Time_ms'], 
         's-', label='Instrumented', linewidth=3, markersize=10, color='#A23B72')

plt.xlabel('Test Configuration', fontsize=12)
plt.ylabel('Message Time (ms)', fontsize=12)
#plt.title('Broadcast System: Message Processing Time', fontsize=14, fontweight='bold')
plt.xticks(range(len(broadcast_baseline)), client_labels)
plt.yscale('log')
plt.legend(fontsize=11)
plt.grid(alpha=0.3)
plt.tight_layout()
plt.savefig('broadcast_message_time.pdf', format='pdf', bbox_inches='tight', dpi=300)
plt.show()

# Chart 5: Broadcast Throughput
plt.figure(figsize=(12, 6))
bars1 = plt.bar(x_pos - width/2, broadcast_baseline['Throughput_msg_per_sec'], width,
                label='Baseline', alpha=0.8, color='#2E86AB')
bars2 = plt.bar(x_pos + width/2, broadcast_instrumented['Throughput_msg_per_sec'], width,
                label='Instrumented', alpha=0.8, color='#A23B72')

plt.xlabel('Test Configuration', fontsize=12)
plt.ylabel('Throughput (msg/sec)', fontsize=12)
#plt.title('Broadcast System: Throughput Comparison', fontsize=14, fontweight='bold')
plt.xticks(x_pos, config_labels, rotation=45)
plt.legend(fontsize=11)
plt.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('broadcast_throughput.pdf', format='pdf', bbox_inches='tight', dpi=300)
plt.show()

# Chart 6: Broadcast Latency (converted to ms)
plt.figure(figsize=(12, 6))
bars1 = plt.bar(x_pos - width/2, broadcast_baseline['Avg_Latency_us'] / 1000, width,
                label='Baseline', alpha=0.8, color='#2E86AB')
bars2 = plt.bar(x_pos + width/2, broadcast_instrumented['Avg_Latency_us'] / 1000, width,
                label='Instrumented', alpha=0.8, color='#A23B72')

plt.xlabel('Test Configuration', fontsize=12)
plt.ylabel('Average Latency (ms)', fontsize=12)
#plt.title('Broadcast System: Average Latency Comparison', fontsize=14, fontweight='bold')
plt.xticks(x_pos, config_labels, rotation=45)
plt.legend(fontsize=11)
plt.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('broadcast_latency.pdf', format='pdf', bbox_inches='tight', dpi=300)
plt.show()

# ===== OVERHEAD ANALYSIS PLOT =====
plt.figure(figsize=(12, 8))

# Plot overhead percentages for both systems
plt.plot(range(len(no_broadcast_instrumented)), no_broadcast_instrumented['Overhead_Percent'], 
         'o-', label='No-Broadcast System', linewidth=3, markersize=10, color='#E74C3C')
plt.plot(range(len(broadcast_instrumented)), broadcast_instrumented['Overhead_Percent'], 
         's-', label='Broadcast System', linewidth=3, markersize=10, color='#27AE60')

plt.xlabel('Test Configuration', fontsize=12)
plt.ylabel('Overhead (%)', fontsize=12)
#plt.title('Instrumentation Overhead: Amortization Analysis', fontsize=14, fontweight='bold')
plt.xticks(range(len(config_labels)), config_labels, rotation=45)
plt.legend(fontsize=11)
plt.grid(alpha=0.3)

# Add annotations for key insights
#plt.annotate('No Amortization\n(~120-160% overhead)', 
             #xy=(2, 130), xytext=(1, 180),
             #arrowprops=dict(arrowstyle='->', color='#E74C3C', lw=2),
             #fontsize=10, ha='center', color='#E74C3C')

#plt.annotate('Excellent Amortization\n(45% → 4% overhead)', 
             #xy=(5, 10), xytext=(4, 60),
             #arrowprops=dict(arrowstyle='->', color='#27AE60', lw=2),
             #fontsize=10, ha='center', color='#27AE60')

plt.tight_layout()
plt.savefig('overhead_amortization_analysis.pdf', format='pdf', bbox_inches='tight', dpi=300)
plt.show()

# ===== SUMMARY STATISTICS =====
print("=== PERFORMANCE SUMMARY ===\n")

print("NO-BROADCAST SYSTEM:")
print(f"  Overhead range: {no_broadcast_instrumented['Overhead_Percent'].min():.1f}% - {no_broadcast_instrumented['Overhead_Percent'].max():.1f}%")
print(f"  Average overhead: {no_broadcast_instrumented['Overhead_Percent'].mean():.1f}%")
print(f"  Overhead trend: {'Decreasing' if no_broadcast_instrumented['Overhead_Percent'].iloc[-1] < no_broadcast_instrumented['Overhead_Percent'].iloc[0] else 'Stable/Increasing'}")

print("\nBROADCAST SYSTEM:")
print(f"  Overhead range: {broadcast_instrumented['Overhead_Percent'].min():.1f}% - {broadcast_instrumented['Overhead_Percent'].max():.1f}%")
print(f"  Average overhead: {broadcast_instrumented['Overhead_Percent'].mean():.1f}%")
print(f"  Overhead reduction: {broadcast_instrumented['Overhead_Percent'].iloc[0]:.1f}% → {broadcast_instrumented['Overhead_Percent'].iloc[-1]:.1f}%")
print(f"  Amortization factor: {broadcast_instrumented['Overhead_Percent'].iloc[0] / broadcast_instrumented['Overhead_Percent'].iloc[-1]:.1f}x improvement")

print("\n=== FILES GENERATED ===")
print("No-Broadcast System:")
print("  • no_broadcast_message_time.pdf")
print("  • no_broadcast_throughput.pdf") 
print("  • no_broadcast_latency.pdf")

print("\nBroadcast System:")
print("  • broadcast_message_time.pdf")
print("  • broadcast_throughput.pdf")
print("  • broadcast_latency.pdf")

print("\nComparative Analysis:")
print("  • overhead_amortization_analysis.pdf")

print("\nAll files saved with 300 DPI for publication quality.")