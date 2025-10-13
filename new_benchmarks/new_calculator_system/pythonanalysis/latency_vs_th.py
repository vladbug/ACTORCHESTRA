import pandas as pd
import matplotlib.pyplot as plt
from io import StringIO

data = """test_name,system_type,test_type,num_clients,operations_per_client,total_operations,avg_latency_us,stddev_latency_us,avg_total_time_us,stddev_total_time_us,avg_throughput_ops_per_sec,stddev_throughput_ops_per_sec,success_rate_pct,overhead_vs_baseline_pct
matrix_1c_2000ops,baseline,unified,1,2000,20000,18.69,42.25,37857.40,4058.43,53542.82,7426.79,100.00,
matrix_1c_2000ops,instrumented,unified,1,2000,20000,33.09,49.60,66672.50,3538.87,30076.72,1663.55,100.00,77.04
matrix_2c_2000ops,baseline,unified,2,2000,40000,24.59,92.34,50872.30,7198.96,80118.93,11819.76,100.00,
matrix_2c_2000ops,instrumented,unified,2,2000,40000,46.10,56.34,93061.10,2682.02,43015.82,1284.78,100.00,87.48
matrix_4c_2000ops,baseline,unified,4,2000,64000,34.59,57.50,70195.25,6374.63,114871.91,11390.79,100.00,
matrix_4c_2000ops,instrumented,unified,4,2000,64000,71.18,60.04,143488.00,3646.91,55786.33,1463.82,100.00,105.75
matrix_8c_2000ops,baseline,unified,8,2000,128000,53.00,63.25,107494.25,3093.07,148959.41,4537.97,100.00,
matrix_8c_2000ops,instrumented,unified,8,2000,128000,120.68,131.14,244019.25,9968.30,65661.06,2587.50,100.00,127.71
matrix_16c_2000ops,baseline,unified,16,2000,160000,89.57,236.10,184258.60,34376.61,178319.60,31180.03,100.00,
matrix_16c_2000ops,instrumented,unified,16,2000,160000,230.29,68.59,462602.00,5859.45,69182.89,884.26,100.00,157.11
matrix_32c_2000ops,baseline,unified,32,2000,192000,124.40,85.54,252313.33,21993.96,255013.11,23406.90,100.00,
matrix_32c_2000ops,instrumented,unified,32,2000,192000,446.71,183.36,899038.00,17317.72,71204.84,1373.55,100.00,259.09
matrix_64c_2000ops,baseline,unified,64,2000,384000,229.14,197.24,464588.67,51344.07,277693.04,29635.81,100.00,
matrix_64c_2000ops,instrumented,unified,64,2000,384000,917.98,225.28,1839206.67,36397.32,69613.33,1372.46,100.00,300.63
matrix_128c_2000ops,baseline,unified,128,2000,768000,425.70,283.51,862310.00,33521.86,297173.74,11452.57,100.00,
matrix_128c_2000ops,instrumented,unified,128,2000,768000,2200.89,383.43,4412416.33,145683.33,58060.42,1923.32,100.00,417.01
matrix_256c_2000ops,baseline,unified,256,2000,1536000,991.26,222.36,2005162.67,61190.23,255502.01,7920.65,100.00,
matrix_256c_2000ops,instrumented,unified,256,2000,1536000,4427.09,409.76,8869375.67,30975.30,57727.19,201.54,100.00,346.61
matrix_512c_2000ops,baseline,unified,512,2000,3072000,2039.95,317.61,4132045.00,7364.78,247819.70,442.16,100.00,
matrix_512c_2000ops,instrumented,unified,512,2000,3072000,9727.52,964.50,19507643.67,267772.39,52498.79,715.06,100.00,376.85"""

# Load into pandas
df = pd.read_csv(StringIO(data))

# Separate baseline and instrumented
baseline = df[df['system_type'] == 'baseline']
instrumented = df[df['system_type'] == 'instrumented']

# Plot
plt.figure(figsize=(10, 6))
plt.scatter(baseline['avg_throughput_ops_per_sec'], baseline['avg_latency_us'], label="Baseline", marker='o')
plt.scatter(instrumented['avg_throughput_ops_per_sec'], instrumented['avg_latency_us'], label="Instrumented", marker='s')

# Connect points with lines
plt.plot(baseline['avg_throughput_ops_per_sec'], baseline['avg_latency_us'], linestyle='--', color='blue', alpha=0.7)
plt.plot(instrumented['avg_throughput_ops_per_sec'], instrumented['avg_latency_us'], linestyle='--', color='orange', alpha=0.7)

plt.xscale("log")
plt.yscale("log")

plt.xlabel("Throughput (ops/sec, log scale)")
plt.ylabel("Latency (µs, log scale)")
plt.title("Latency vs Throughput (Baseline vs Instrumented, Log Scales)")
plt.legend()
plt.grid(True, which="both", linestyle="--", alpha=0.6)
plt.tight_layout()
plt.show()
