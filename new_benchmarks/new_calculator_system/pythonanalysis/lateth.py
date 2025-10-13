import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Read the CSV data
df = pd.read_csv('operations_scaling_results.csv')

# Separate baseline and instrumented data
baseline_data = df[df['system_type'] == 'baseline'].sort_values('operations_per_client')
instrumented_data = df[df['system_type'] == 'instrumented'].sort_values('operations_per_client')

# Create figure with two subplots
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 6))

# Plot 1: Baseline System - just scatter points
ax1.scatter(baseline_data['avg_throughput_ops_per_sec'], 
            baseline_data['avg_latency_us'],
            s=80,  # marker size
            color='#2E86AB',
            alpha=0.8,
            label='Baseline System')

ax1.set_xlabel('Throughput [ops/sec]', fontsize=12)
ax1.set_ylabel('Latency [μs]', fontsize=12)
ax1.set_title('Baseline System\nLatency vs Throughput', fontsize=13, fontweight='bold')
ax1.grid(True, alpha=0.3)
ax1.legend(fontsize=11)

# Plot 2: Instrumented System - just scatter points
ax2.scatter(instrumented_data['avg_throughput_ops_per_sec'], 
            instrumented_data['avg_latency_us'],
            s=80,  # marker size
            color='#A23B72',
            alpha=0.8,
            label='Instrumented System')

ax2.set_xlabel('Throughput [ops/sec]', fontsize=12)
ax2.set_ylabel('Latency [μs]', fontsize=12)
ax2.set_title('Instrumented System\nLatency vs Throughput', fontsize=13, fontweight='bold')
ax2.grid(True, alpha=0.3)
ax2.legend(fontsize=11)

# Make y-axes have the same scale for easy comparison
max_latency = max(baseline_data['avg_latency_us'].max(), instrumented_data['avg_latency_us'].max())
min_latency = min(baseline_data['avg_latency_us'].min(), instrumented_data['avg_latency_us'].min())
ax1.set_ylim(min_latency * 0.9, max_latency * 1.1)
ax2.set_ylim(min_latency * 0.9, max_latency * 1.1)

plt.tight_layout()
plt.show()

# Create a combined comparison plot as well
plt.figure(figsize=(12, 8))

# Plot both systems on the same graph for comparison - just scatter points
plt.scatter(baseline_data['avg_throughput_ops_per_sec'], 
            baseline_data['avg_latency_us'],
            s=80,
            color='#2E86AB',
            alpha=0.8,
            label='Baseline System')

plt.scatter(instrumented_data['avg_throughput_ops_per_sec'], 
            instrumented_data['avg_latency_us'],
            s=80,
            color='#A23B72',
            alpha=0.8,
            label='Instrumented System')

plt.xlabel('Throughput [ops/sec]', fontsize=12)
plt.ylabel('Latency [μs]', fontsize=12)
plt.title('Latency vs Throughput Comparison', fontsize=14, fontweight='bold')
plt.grid(True, alpha=0.3)
plt.legend(fontsize=11)

plt.tight_layout()
plt.show()

# Print key data points
print("=" * 60)
print("DATA POINTS")
print("=" * 60)

print("\nBaseline System (Throughput [ops/sec], Latency [μs]):")
for _, row in baseline_data.iterrows():
    print(f"  ({row['avg_throughput_ops_per_sec']:.0f}, {row['avg_latency_us']:.2f})")

print("\nInstrumented System (Throughput [ops/sec], Latency [μs]):")
for _, row in instrumented_data.iterrows():
    print(f"  ({row['avg_throughput_ops_per_sec']:.0f}, {row['avg_latency_us']:.2f})")

print("=" * 60)