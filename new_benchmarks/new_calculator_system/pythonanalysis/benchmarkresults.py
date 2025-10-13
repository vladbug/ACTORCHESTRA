import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Set clean academic plotting style
plt.style.use('default')
plt.rcParams['figure.figsize'] = (10, 6)
plt.rcParams['font.size'] = 12
plt.rcParams['axes.linewidth'] = 1
plt.rcParams['grid.alpha'] = 0.3

def load_and_prepare_data(csv_file):
    """Load CSV data and prepare it for plotting."""
    df = pd.read_csv(csv_file)
    
    # Convert overhead column to numeric, handling empty values
    df['overhead_vs_baseline_pct'] = pd.to_numeric(df['overhead_vs_baseline_pct'], errors='coerce')
    
    # Extract client count and operations from test_name for easier plotting
    df['client_count'] = df['test_name'].str.extract(r'matrix_(\d+)c_').astype(int)
    df['ops_per_client'] = df['test_name'].str.extract(r'_(\d+)ops').astype(int)
    
    # Convert microseconds to milliseconds for total time
    df['total_time_ms'] = df['avg_total_time_us'] / 1000
    
    return df

def plot_single_client(df):
    """Plot 1: Single client performance"""
    single_client = df[df['client_count'] == 1].copy()
    
    fig, ax = plt.subplots(1, 1, figsize=(10, 6))
    
    # Sort by operations for consistent x-axis
    baseline_1c = single_client[single_client['system_type'] == 'baseline'].sort_values('ops_per_client')
    instrumented_1c = single_client[single_client['system_type'] == 'instrumented'].sort_values('ops_per_client')
    
    # Plot with clean styling
    ax.plot(baseline_1c['ops_per_client'], baseline_1c['total_time_ms'], 
            'o-', color='#1f77b4', linewidth=2.5, markersize=7, label='Baseline')
    ax.plot(instrumented_1c['ops_per_client'], instrumented_1c['total_time_ms'], 
            's--', color='#d62728', linewidth=2.5, markersize=7, label='Instrumented')
    
    ax.set_xlabel('Number Of Requests')
    ax.set_ylabel('Total Time (ms)')
    ax.set_title('Single Client: Baseline vs Instrumented Time')
    ax.legend()
    ax.grid(True, alpha=0.3)
    ax.set_yscale('log')
    
    plt.tight_layout()
    plt.savefig('plot1_single_client.png', dpi=300, bbox_inches='tight')
    plt.show()

def plot_all_clients_time_overhead(df):
    """Plot 2: All clients showing baseline vs instrumented total time"""
    fig, ax = plt.subplots(1, 1, figsize=(12, 8))
    
    # Colors for different client counts
    colors = ['#d62728', '#1f77b4', '#ff7f0e', '#2ca02c', '#9467bd']
    client_counts = sorted(df['client_count'].unique())
    
    for i, client_count in enumerate(client_counts):
        client_data = df[df['client_count'] == client_count]
        baseline = client_data[client_data['system_type'] == 'baseline'].sort_values('total_operations')
        instrumented = client_data[client_data['system_type'] == 'instrumented'].sort_values('total_operations')
        
        color = colors[i % len(colors)]
        
        # Baseline: solid line with specific markers
        ax.plot(baseline['total_operations'], baseline['total_time_ms'], 
                'o-', color=color, linewidth=2.5, markersize=6,
                label=f'Baseline ({client_count} Client{"s" if client_count > 1 else ""})')
        
        # Instrumented: dashed line with different markers
        ax.plot(instrumented['total_operations'], instrumented['total_time_ms'], 
                's--', color=color, linewidth=2.5, markersize=6,
                label=f'Instrumented ({client_count} Client{"s" if client_count > 1 else ""})')
    
    ax.set_xlabel('Total Requests', fontsize=12)
    ax.set_ylabel('Total Time (ms)', fontsize=12)
    ax.set_title('Performance Comparison: Baseline vs Instrumented by Client Count', fontsize=13)
    ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=10)
    ax.grid(True, alpha=0.3)
    
    # Use log scales for better visualization
    ax.set_xscale('log')
    ax.set_yscale('log')
    
    # Improve axis limits and ticks for better readability
    ax.set_xlim(left=min(df['total_operations']) * 0.8, right=max(df['total_operations']) * 1.2)
    ax.set_ylim(bottom=min(df['total_time_ms']) * 0.8, top=max(df['total_time_ms']) * 1.2)
    
    # Add minor ticks for better log scale readability
    ax.tick_params(which='both', labelsize=10)
    
    plt.tight_layout()
    plt.savefig('plot2_time_overhead.png', dpi=300, bbox_inches='tight')
    plt.show()

def plot_overhead_percentage_separate(df):
    """Plot 2b: Overhead percentage as a separate clean plot"""
    fig, ax = plt.subplots(1, 1, figsize=(12, 7))
    
    # Get instrumented data with overhead information
    instrumented = df[df['system_type'] == 'instrumented'].dropna(subset=['overhead_vs_baseline_pct'])
    
    # Colors for different client counts
    colors = ['#d62728', '#1f77b4', '#ff7f0e', '#2ca02c', '#9467bd']
    client_counts = sorted(instrumented['client_count'].unique())
    
    for i, client_count in enumerate(client_counts):
        client_data = instrumented[instrumented['client_count'] == client_count].sort_values('ops_per_client')
        
        ax.plot(client_data['ops_per_client'], client_data['overhead_vs_baseline_pct'], 
                'o-', color=colors[i % len(colors)], linewidth=2.5, markersize=7,
                label=f'{client_count} Client{"s" if client_count > 1 else ""}')
    
    ax.set_xlabel('Operations per Client', fontsize=12)
    ax.set_ylabel('Time Overhead (%)', fontsize=12)
    ax.set_title('Instrumentation Time Overhead by Client Count', fontsize=13)
    ax.legend(title='Client Count', fontsize=10)
    ax.grid(True, alpha=0.3)
    
    # Use log scale for x-axis, linear for y-axis for overhead percentage
    ax.set_xscale('log')
    
    # Add reference lines
    ax.axhline(y=100, color='red', linestyle=':', alpha=0.7, linewidth=1.5, label='100% overhead')
    ax.axhline(y=50, color='orange', linestyle=':', alpha=0.5, linewidth=1, label='50% overhead')
    
    # Better y-axis limits
    y_max = max(instrumented['overhead_vs_baseline_pct']) * 1.1
    ax.set_ylim(bottom=0, top=y_max)
    
    plt.tight_layout()
    plt.savefig('plot2b_overhead_percentage.png', dpi=300, bbox_inches='tight')
    plt.show()

def plot_all_clients_throughput(df):
    """Plot 3: All clients showing throughput comparison"""
    fig, ax = plt.subplots(1, 1, figsize=(12, 7))
    
    # Colors for different client counts  
    colors = ['#d62728', '#1f77b4', '#ff7f0e', '#2ca02c', '#9467bd']
    client_counts = sorted(df['client_count'].unique())
    
    for i, client_count in enumerate(client_counts):
        client_data = df[df['client_count'] == client_count]
        baseline = client_data[client_data['system_type'] == 'baseline'].sort_values('total_operations')
        instrumented = client_data[client_data['system_type'] == 'instrumented'].sort_values('total_operations')
        
        color = colors[i % len(colors)]
        
        # Plot baseline and instrumented for each client count
        ax.plot(baseline['total_operations'], baseline['avg_throughput_ops_per_sec'], 
                'o-', color=color, linewidth=2.5, markersize=7, alpha=0.7,
                label=f'{client_count}c Baseline')
        ax.plot(instrumented['total_operations'], instrumented['avg_throughput_ops_per_sec'], 
                's--', color=color, linewidth=2.5, markersize=7, alpha=0.9,
                label=f'{client_count}c Instrumented')
    
    ax.set_xlabel('Total Operations')
    ax.set_ylabel('Throughput (ops/sec)')
    ax.set_title('Throughput Performance: Baseline vs Instrumented')
    ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
    ax.grid(True, alpha=0.3)
    ax.set_xscale('log')
    
    plt.tight_layout()
    plt.savefig('plot3_throughput.png', dpi=300, bbox_inches='tight')
    plt.show()

def print_summary_stats(df):
    """Print clean summary statistics"""
    print("=== PERFORMANCE SUMMARY ===")
    
    instrumented = df[df['system_type'] == 'instrumented'].dropna(subset=['overhead_vs_baseline_pct'])
    
    print(f"\nOverhead Statistics:")
    print(f"  Mean overhead: {instrumented['overhead_vs_baseline_pct'].mean():.1f}%")
    print(f"  Min overhead: {instrumented['overhead_vs_baseline_pct'].min():.1f}%")
    print(f"  Max overhead: {instrumented['overhead_vs_baseline_pct'].max():.1f}%")
    
    print(f"\nThroughput Impact:")
    baseline_avg = df[df['system_type'] == 'baseline']['avg_throughput_ops_per_sec'].mean()
    instrumented_avg = df[df['system_type'] == 'instrumented']['avg_throughput_ops_per_sec'].mean()
    throughput_loss = (baseline_avg - instrumented_avg) / baseline_avg * 100
    
    print(f"  Average baseline throughput: {baseline_avg:.0f} ops/sec")
    print(f"  Average instrumented throughput: {instrumented_avg:.0f} ops/sec")
    print(f"  Average throughput loss: {throughput_loss:.1f}%")

def main():
    # Load data
    csv_file = 'matrix_scaling_results.csv'  # Update this path if needed
    
    try:
        df = load_and_prepare_data(csv_file)
        print(f"Loaded {len(df)} data points")
        print(f"Client counts: {sorted(df['client_count'].unique())}")
        
        # Generate the 3 requested plots
        print("\nGenerating Plot 1: Single Client Performance...")
        plot_single_client(df)
        
        print("Generating Plot 2: Time Comparison (Baseline vs Instrumented)...")
        plot_all_clients_time_overhead(df)
        
        print("Generating Plot 2b: Overhead Percentage...")
        plot_overhead_percentage_separate(df)
        
        print("Generating Plot 3: Throughput Comparison...")
        plot_all_clients_throughput(df)
        
        # Print summary
        print_summary_stats(df)
        
        print("\n=== FILES GENERATED ===")
        print("- plot1_single_client.png")
        print("- plot2_time_overhead.png") 
        print("- plot3_throughput.png")
        
    except FileNotFoundError:
        print(f"Error: Could not find {csv_file}")
        print("Please ensure the CSV file is in the same directory as this script.")
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    main()