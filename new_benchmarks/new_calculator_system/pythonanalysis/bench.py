import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from io import StringIO

# Raw CSV data
csv_data = """test_name,system_type,test_type,num_clients,operations_per_client,total_operations,avg_latency_us,stddev_latency_us,avg_total_time_us,stddev_total_time_us,avg_throughput_ops_per_sec,stddev_throughput_ops_per_sec,success_rate_pct,overhead_vs_baseline_pct
matrix_1c_500ops,baseline,unified,1,7500,7500,23.56,61.88,11906.53,3030.92,45712.35,17152.28,100.00,
matrix_1c_500ops,instrumented,unified,1,7500,7500,51.61,36.49,25887.47,2281.87,19440.40,1539.50,100.00,119.01
matrix_1c_1000ops,baseline,unified,1,15000,15000,21.44,41.64,21621.07,4975.25,50468.54,20578.74,100.00,
matrix_1c_1000ops,instrumented,unified,1,15000,15000,41.91,54.83,42060.00,4510.91,24058.33,2844.97,100.00,95.45
matrix_1c_2000ops,baseline,unified,1,20000,20000,18.18,9.22,36612.30,6716.66,56934.54,14068.23,100.00,
matrix_1c_2000ops,instrumented,unified,1,20000,20000,34.46,17.60,69138.00,3192.02,28983.27,1340.33,100.00,89.59
matrix_1c_5000ops,baseline,unified,1,50000,50000,12.96,8.22,65281.50,8723.53,78283.79,14205.04,100.00,
matrix_1c_5000ops,instrumented,unified,1,50000,50000,24.02,12.43,120455.80,6729.64,41624.14,2297.57,100.00,85.29
matrix_1c_10000ops,baseline,unified,1,80000,80000,10.16,7.45,102363.25,5809.39,98000.12,6223.45,100.00,
matrix_1c_10000ops,instrumented,unified,1,80000,80000,20.86,29.28,209269.25,21154.29,48153.45,4175.78,100.00,105.38
matrix_2c_500ops,baseline,unified,1,15000,15000,49.01,20.90,24748.93,4316.20,42541.52,13506.15,100.00,
matrix_2c_500ops,instrumented,unified,1,15000,15000,69.85,60.07,35101.60,9889.10,30981.24,9719.33,100.00,42.52
matrix_2c_1000ops,baseline,unified,1,20000,20000,42.59,18.14,42937.90,7826.85,49232.83,16240.65,100.00,
matrix_2c_1000ops,instrumented,unified,1,20000,20000,69.67,25.69,69955.60,5979.74,28813.70,2920.24,100.00,63.59
matrix_2c_2000ops,baseline,unified,1,40000,40000,33.45,14.72,67388.30,7842.44,60382.90,9806.60,100.00,
matrix_2c_2000ops,instrumented,unified,1,40000,40000,55.80,23.47,111972.00,1063.99,35726.10,336.90,100.00,66.81
matrix_2c_5000ops,baseline,unified,1,80000,80000,24.11,58.92,122083.88,21361.00,83728.11,12047.42,100.00,
matrix_2c_5000ops,instrumented,unified,1,80000,80000,46.16,18.85,231883.00,3085.66,43131.96,580.49,100.00,91.46
matrix_2c_10000ops,baseline,unified,1,160000,160000,20.12,16.24,202770.00,10242.32,98869.04,5324.66,100.00,
matrix_2c_10000ops,instrumented,unified,1,160000,160000,41.88,13.53,420825.75,7208.86,47538.21,840.71,100.00,108.13
matrix_4c_500ops,baseline,unified,1,20000,20000,68.73,34.10,34705.10,7792.95,60983.53,16763.10,100.00,
matrix_4c_500ops,instrumented,unified,1,20000,20000,126.77,49.42,63650.30,3899.81,31537.98,2113.71,100.00,84.45
matrix_4c_1000ops,baseline,unified,1,40000,40000,57.42,32.16,57939.60,10044.22,71845.52,18232.55,100.00,
matrix_4c_1000ops,instrumented,unified,1,40000,40000,94.82,38.71,95180.20,2670.63,42055.54,1188.20,100.00,65.14
matrix_4c_2000ops,baseline,unified,1,64000,64000,44.99,20.15,90565.75,2803.25,88407.96,2745.88,100.00,
matrix_4c_2000ops,instrumented,unified,1,64000,64000,80.23,82.58,161349.75,22109.01,50339.67,6448.11,100.00,78.33
matrix_4c_5000ops,baseline,unified,1,160000,160000,34.09,16.41,171675.63,12020.56,117036.91,8791.76,100.00,
matrix_4c_5000ops,instrumented,unified,1,160000,160000,70.40,24.00,353284.25,5720.90,56624.52,909.57,100.00,106.50
matrix_4c_10000ops,baseline,unified,1,200000,200000,31.58,19.23,318020.00,16388.15,126034.63,6223.88,100.00,
matrix_4c_10000ops,instrumented,unified,1,200000,200000,67.30,37.56,675570.00,20036.04,59249.60,1700.47,100.00,113.08
matrix_8c_500ops,baseline,unified,1,40000,40000,94.06,47.87,47493.10,9015.88,88241.03,24001.20,100.00,
matrix_8c_500ops,instrumented,unified,1,40000,40000,160.89,63.45,80820.90,2074.76,49521.51,1271.04,100.00,71.06
matrix_8c_1000ops,baseline,unified,1,64000,64000,72.79,30.48,73297.25,6320.58,109991.74,11251.44,100.00,
matrix_8c_1000ops,instrumented,unified,1,64000,64000,127.50,49.50,128086.88,9249.42,62743.81,4540.62,100.00,75.18
matrix_8c_2000ops,baseline,unified,1,128000,128000,59.01,92.55,118828.13,12395.15,135893.08,13740.18,100.00,
matrix_8c_2000ops,instrumented,unified,1,128000,128000,118.78,174.36,238591.75,39809.85,68296.03,8473.80,100.00,101.27
matrix_8c_5000ops,baseline,unified,1,200000,200000,46.92,18.55,236567.00,12204.43,169444.13,8700.34,100.00,
matrix_8c_5000ops,instrumented,unified,1,200000,200000,107.76,31.34,540534.60,8218.71,74014.73,1144.20,100.00,129.66
matrix_8c_10000ops,baseline,unified,1,240000,240000,45.04,16.70,454582.33,13843.59,176092.93,5280.99,100.00,
matrix_8c_10000ops,instrumented,unified,1,240000,240000,102.89,28.45,1031920.67,23843.33,77552.64,1772.12,100.00,128.45
matrix_16c_500ops,baseline,unified,1,64000,64000,127.11,53.84,64297.25,9322.51,127731.15,26157.46,100.00,
matrix_16c_500ops,instrumented,unified,1,64000,64000,233.40,89.33,117232.13,5334.80,68366.20,3155.16,100.00,83.63
matrix_16c_1000ops,baseline,unified,1,128000,128000,104.13,48.87,105128.38,9162.30,153418.89,16076.70,100.00,
matrix_16c_1000ops,instrumented,unified,1,128000,128000,207.89,68.79,208883.50,10095.58,76752.47,3662.12,100.00,99.65
matrix_16c_2000ops,baseline,unified,1,160000,160000,102.26,46.03,206340.60,20758.98,156314.96,15362.45,100.00,
matrix_16c_2000ops,instrumented,unified,1,160000,160000,234.90,187.51,471698.20,29088.13,68042.65,4114.64,100.00,129.70
matrix_16c_5000ops,baseline,unified,1,240000,240000,98.02,44.61,492759.67,64063.57,164071.08,19846.58,100.00,
matrix_16c_5000ops,instrumented,unified,1,240000,240000,227.15,67.33,1138484.33,18414.68,70281.01,1126.68,100.00,131.74
matrix_16c_10000ops,baseline,unified,1,480000,480000,93.99,40.58,945264.00,23002.85,169332.28,4154.55,100.00,
matrix_16c_10000ops,instrumented,unified,1,480000,480000,215.77,125.88,2166698.67,116230.82,73987.32,3979.62,100.00,129.57"""

def load_and_process_data():
    """Load and process the benchmark data"""
    df = pd.read_csv(StringIO(csv_data))
    
    # Extract client count and operation count from test name
    df['client_count'] = df['test_name'].str.extract(r'(\d+)c_').astype(int)
    df['ops_per_test'] = df['test_name'].str.extract(r'(\d+)ops').astype(int)
    df['total_time_ms'] = df['avg_total_time_us'] / 1000  # Convert to milliseconds
    
    return df

def plot_single_client_performance(df):
    """Create Figure 5.3: Single Client Performance - Baseline vs Instrumented"""
    # Filter for single client tests only
    single_client = df[df['client_count'] == 1].copy()
    
    # Pivot to get baseline and instrumented side by side
    pivot_data = single_client.pivot(index='ops_per_test', columns='system_type', values='total_time_ms')
    
    plt.figure(figsize=(10, 6))
    
    # Plot with the same style as the original
    plt.plot(pivot_data.index, pivot_data['baseline'], 'o-', 
             color='#1f77b4', linewidth=2, markersize=8, label='Baseline')
    plt.plot(pivot_data.index, pivot_data['instrumented'], 'o-', 
             color='#984ea3', linewidth=2, markersize=8, label='Instrumented')
    
    plt.xlabel('Number Of Requests', fontsize=12)
    plt.ylabel('Total Time (ms)', fontsize=12)
    #plt.title('Figure 5.3: Single Client Performance - Baseline vs Instrumented', fontsize=14, pad=20)
    
    # Set logarithmic scale like in the original
    plt.yscale('log')
    plt.xscale('log')
    
    # Grid and legend
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=11)
    
    # Format axes
    plt.gca().set_xticks([500, 1000, 2000, 5000, 10000])
    plt.gca().set_xticklabels(['500', '1000', '2000', '5000', '10000'])
    
    plt.tight_layout()
    plt.savefig('single_client_performance.pdf', dpi=300, bbox_inches='tight')
    plt.show()

def plot_multiple_clients_performance(df):
    """Create Figure 5.4: Multiple Clients Performance - Clean Subplot Approach"""
    
    # Create a grid of subplots - one for each client count
    fig, axes = plt.subplots(2, 3, figsize=(15, 8))
    fig.suptitle('Figure 5.4: Multiple Clients Performance - Baseline vs Instrumented', 
                 fontsize=16, y=0.95)
    
    # Flatten axes for easier iteration
    axes = axes.flatten()
    
    client_counts = sorted(df['client_count'].unique())
    colors = {'baseline': '#1f77b4', 'instrumented': '#d62728'}
    
    for i, client_count in enumerate(client_counts):
        ax = axes[i]
        client_data = df[df['client_count'] == client_count]
        
        # Plot baseline and instrumented for this client count
        for system_type in ['baseline', 'instrumented']:
            data = client_data[client_data['system_type'] == system_type].sort_values('total_operations')
            if len(data) > 0:
                line_style = '-' if system_type == 'baseline' else '--'
                marker = 'o' if system_type == 'baseline' else 's'
                
                ax.plot(data['total_operations'], data['total_time_ms'], 
                       line_style, color=colors[system_type], linewidth=2.5, 
                       marker=marker, markersize=6, label=system_type.capitalize(),
                       markerfacecolor='white' if system_type == 'baseline' else colors[system_type],
                       markeredgecolor=colors[system_type], markeredgewidth=1.5)
        
        # Configure each subplot
        ax.set_title(f'{client_count} Client{"s" if client_count > 1 else ""}', fontsize=12, pad=8)
        ax.set_xlabel('Total Requests', fontsize=10)
        ax.set_ylabel('Total Time (ms)', fontsize=10)
        
        # Logarithmic scale
        ax.set_yscale('log')
        ax.set_xscale('log')
        
        # Grid
        ax.grid(True, alpha=0.3)
        
        # Legend for first subplot only
        if i == 0:
            ax.legend(fontsize=9, loc='upper left')
    
    # Hide the last subplot since we only have 5 client counts
    axes[-1].set_visible(False)
    
    plt.tight_layout()
    plt.savefig('multiple_clients_performance.pdf', dpi=300, bbox_inches='tight')
    plt.show()
    
    # ALTERNATIVE: Create a cleaner single plot with better separation
    plt.figure(figsize=(12, 8))
    
    # Use very distinct colors for each client count
    client_colors = {
        1: '#e41a1c',   # Bright red
        2: '#377eb8',   # Bright blue  
        4: '#4daf4a',   # Bright green
        8: '#984ea3',   # Purple
        16: '#ff7f00'   # Orange
    }
    
    # Plot only baseline first, then instrumented - this creates natural separation
    for system_type in ['baseline', 'instrumented']:
        for client_count in client_counts:
            client_data = df[df['client_count'] == client_count]
            data = client_data[client_data['system_type'] == system_type].sort_values('total_operations')
            
            if len(data) > 0:
                line_style = '-' if system_type == 'baseline' else ':'
                linewidth = 3 if system_type == 'baseline' else 2.5
                alpha = 0.9 if system_type == 'baseline' else 0.8
                marker = 'o' if system_type == 'baseline' else 's'
                markersize = 7 if system_type == 'baseline' else 6
                
                label = f'{system_type.capitalize()} ({client_count}c)'
                
                plt.plot(data['total_operations'], data['total_time_ms'], 
                        line_style, color=client_colors[client_count], 
                        linewidth=linewidth, marker=marker, markersize=markersize,
                        label=label, alpha=alpha,
                        markerfacecolor='white' if system_type == 'baseline' else client_colors[client_count],
                        markeredgecolor=client_colors[client_count], markeredgewidth=1.5)
    
    plt.xlabel('Total Requests', fontsize=12)
    plt.ylabel('Total Time (ms)', fontsize=12)
    
    # Clean logarithmic scales
    plt.yscale('log')
    plt.xscale('log')
    
    # Clean grid
    plt.grid(True, alpha=0.3, linewidth=1)
    
    # Organized legend - baseline and instrumented grouped separately
    plt.legend(loc='upper left', fontsize=12, 
              frameon=True, fancybox=True, ncol=2, framealpha=0.9)
    
    plt.tight_layout()
    plt.savefig('multiple_clients_performance_combined.pdf', dpi=300, bbox_inches='tight')
    plt.show()

def create_data_table(df):
    """Create a comprehensive data table with all benchmark results"""
    # Reorder columns for better readability
    table_cols = [
        'test_name', 'system_type', 'client_count', 'ops_per_test', 
        'total_operations', 'avg_latency_us', 'stddev_latency_us',
        'avg_total_time_us', 'stddev_total_time_us', 'avg_throughput_ops_per_sec',
        'stddev_throughput_ops_per_sec', 'success_rate_pct', 'overhead_vs_baseline_pct'
    ]
    
    table_df = df[table_cols].copy()
    
    # Round numerical columns for better display
    numeric_cols = ['avg_latency_us', 'stddev_latency_us', 'avg_total_time_us', 
                   'stddev_total_time_us', 'avg_throughput_ops_per_sec', 
                   'stddev_throughput_ops_per_sec', 'overhead_vs_baseline_pct']
    
    for col in numeric_cols:
        table_df[col] = table_df[col].round(2)
    
    # Save to CSV
    table_df.to_csv('benchmark_results_table.csv', index=False)
    
    # Display first few rows
    print("Benchmark Results Table (first 10 rows):")
    print("=" * 120)
    print(table_df.head(10).to_string(index=False))
    print(f"\nFull table saved to: benchmark_results_table.csv")
    
    # Create summary statistics
    print("\n\nSummary Statistics by System Type:")
    print("=" * 50)
    summary = df.groupby('system_type').agg({
        'avg_latency_us': ['mean', 'std'],
        'avg_total_time_us': ['mean', 'std'], 
        'avg_throughput_ops_per_sec': ['mean', 'std'],
        'overhead_vs_baseline_pct': ['mean', 'std']
    }).round(2)
    print(summary)
    
    return table_df

def main():
    """Main function to generate all visualizations"""
    print("Loading benchmark data...")
    df = load_and_process_data()
    
    print(f"Loaded {len(df)} benchmark results")
    print(f"Client counts: {sorted(df['client_count'].unique())}")
    print(f"System types: {df['system_type'].unique().tolist()}")
    
    print("\nGenerating visualizations...")
    
    # 1. Single client performance plot
    print("Creating single client performance plot...")
    plot_single_client_performance(df)
    
    # 2. Multiple clients performance plot  
    print("Creating multiple clients performance plot...")
    plot_multiple_clients_performance(df)
    
    # 3. Data table
    print("Creating data table...")
    table_df = create_data_table(df)
    
    print("\nAll visualizations completed!")
    print("Files generated:")
    print("- single_client_performance.pdf")
    print("- multiple_clients_performance.pdf (split view)") 
    print("- multiple_clients_performance_combined.pdf (combined view)")
    print("- benchmark_results_table.csv")

if __name__ == "__main__":
    main()