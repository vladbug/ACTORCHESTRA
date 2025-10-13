import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from io import StringIO

# Raw CSV data for messaging system
csv_data = """Clients,Messages_Per_Client,Total_Messages,System_Type,Total_Time_ms,Message_Time_ms,Join_Time_ms,Leave_Time_ms,Throughput_msg_per_sec,Avg_Latency_us,Overhead_Percent,CV_Percent,Iterations
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

def load_and_process_messaging_data():
    """Load and process the messaging system data"""
    df = pd.read_csv(StringIO(csv_data))
    
    # Sort by total messages for consistent plotting
    df = df.sort_values(['Total_Messages', 'System_Type'])
    
    return df

def plot_time_overhead_comparison(df):
    """Create time overhead comparison plot similar to previous benchmarks"""
    plt.figure(figsize=(12, 8))
    
    # Define colors for system types
    colors = {'Baseline': '#1f77b4', 'Instrumented': '#d62728'}
    markers = {'Baseline': 'o', 'Instrumented': 's'}
    
    # Plot both system types
    for system_type in ['Baseline', 'Instrumented']:
        data = df[df['System_Type'] == system_type].sort_values('Total_Messages')
        
        line_style = '-' if system_type == 'Baseline' else '--'
        
        plt.plot(data['Total_Messages'], data['Total_Time_ms'], 
                line_style, color=colors[system_type], linewidth=3, 
                marker=markers[system_type], markersize=8, 
                label=system_type,
                markerfacecolor='white' if system_type == 'Baseline' else colors[system_type],
                markeredgecolor=colors[system_type], markeredgewidth=2)
    
    plt.xlabel('Total Messages', fontsize=12)
    plt.ylabel('Total Time (ms)', fontsize=12)
    plt.title('Messaging System Performance - Time Overhead Comparison', fontsize=14, pad=15)
    
    # Use logarithmic scale for better visualization
    plt.yscale('log')
    plt.xscale('log')
    
    # Custom tick formatting
    plt.grid(True, alpha=0.3)
    plt.legend(loc='upper left', fontsize=11, frameon=True, framealpha=0.9)
    
    plt.tight_layout()
    plt.savefig('messaging_time_overhead.png', dpi=300, bbox_inches='tight')
    plt.show()

def plot_throughput_comparison(df):
    """Create throughput comparison using bar chart"""
    fig, ax = plt.subplots(figsize=(12, 8))
    
    # Get unique message counts for x-axis
    message_counts = sorted(df['Total_Messages'].unique())
    
    # Prepare data for grouped bar chart
    baseline_throughput = []
    instrumented_throughput = []
    
    for count in message_counts:
        baseline_val = df[(df['Total_Messages'] == count) & (df['System_Type'] == 'Baseline')]['Throughput_msg_per_sec'].iloc[0]
        instrumented_val = df[(df['Total_Messages'] == count) & (df['System_Type'] == 'Instrumented')]['Throughput_msg_per_sec'].iloc[0]
        
        baseline_throughput.append(baseline_val)
        instrumented_throughput.append(instrumented_val)
    
    # Set width of bars and positions
    bar_width = 0.35
    x_pos = np.arange(len(message_counts))
    
    # Create bars
    bars1 = ax.bar(x_pos - bar_width/2, baseline_throughput, bar_width, 
                   label='Baseline', color='#1f77b4', alpha=0.8, edgecolor='black', linewidth=1)
    bars2 = ax.bar(x_pos + bar_width/2, instrumented_throughput, bar_width,
                   label='Instrumented', color='#d62728', alpha=0.8, edgecolor='black', linewidth=1)
    
    # Add value labels on bars
    def add_value_labels(bars):
        for bar in bars:
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height + height*0.01,
                   f'{height:.0f}', ha='center', va='bottom', fontsize=9)
    
    add_value_labels(bars1)
    add_value_labels(bars2)
    
    # Customize the plot
    ax.set_xlabel('Total Messages', fontsize=12)
    ax.set_ylabel('Throughput (messages/sec)', fontsize=12)
    ax.set_title('Messaging System Throughput Comparison', fontsize=14, pad=15)
    ax.set_xticks(x_pos)
    ax.set_xticklabels(message_counts)
    
    # Add grid for better readability
    ax.grid(True, alpha=0.3, axis='y')
    ax.set_axisbelow(True)
    
    # Legend
    ax.legend(loc='upper right', fontsize=11, frameon=True, framealpha=0.9)
    
    plt.tight_layout()
    plt.savefig('messaging_throughput_comparison.png', dpi=300, bbox_inches='tight')
    plt.show()

def plot_detailed_throughput_by_clients(df):
    """Create a more detailed throughput plot showing client breakdown"""
    fig, ax = plt.subplots(figsize=(14, 8))
    
    # Group by clients for more detailed view
    client_counts = sorted(df['Clients'].unique())
    
    # Prepare data
    baseline_data = df[df['System_Type'] == 'Baseline'].sort_values('Clients')
    instrumented_data = df[df['System_Type'] == 'Instrumented'].sort_values('Clients')
    
    bar_width = 0.35
    x_pos = np.arange(len(client_counts))
    
    # Create bars
    bars1 = ax.bar(x_pos - bar_width/2, baseline_data['Throughput_msg_per_sec'], bar_width,
                   label='Baseline', color='#1f77b4', alpha=0.8, edgecolor='black', linewidth=1)
    bars2 = ax.bar(x_pos + bar_width/2, instrumented_data['Throughput_msg_per_sec'], bar_width,
                   label='Instrumented', color='#d62728', alpha=0.8, edgecolor='black', linewidth=1)
    
    # Add value labels
    def add_value_labels(bars):
        for bar in bars:
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height + height*0.01,
                   f'{height:.0f}', ha='center', va='bottom', fontsize=9)
    
    add_value_labels(bars1)
    add_value_labels(bars2)
    
    # Customize the plot
    ax.set_xlabel('Number of Clients', fontsize=12)
    ax.set_ylabel('Throughput (messages/sec)', fontsize=12)
    ax.set_title('Messaging System Throughput by Client Count', fontsize=14, pad=15)
    ax.set_xticks(x_pos)
    ax.set_xticklabels(client_counts)
    
    # Add grid for better readability
    ax.grid(True, alpha=0.3, axis='y')
    ax.set_axisbelow(True)
    
    # Legend
    ax.legend(loc='upper right', fontsize=11, frameon=True, framealpha=0.9)
    
    # Add secondary labels showing total messages
    for i, client_count in enumerate(client_counts):
        total_msgs = baseline_data.iloc[i]['Total_Messages']
        ax.text(i, -max(baseline_data['Throughput_msg_per_sec']) * 0.1, 
               f'{total_msgs} msgs', ha='center', va='top', fontsize=9, style='italic')
    
    plt.tight_layout()
    plt.savefig('messaging_throughput_by_clients.png', dpi=300, bbox_inches='tight')
    plt.show()

def plot_latency_comparison(df):
    """Create latency comparison plot"""
    fig, ax = plt.subplots(figsize=(12, 8))
    
    # Get unique client counts for x-axis
    client_counts = sorted(df['Clients'].unique())
    
    # Prepare data for grouped bar chart
    baseline_latency = []
    instrumented_latency = []
    
    for count in client_counts:
        baseline_val = df[(df['Clients'] == count) & (df['System_Type'] == 'Baseline')]['Avg_Latency_us'].iloc[0]
        instrumented_val = df[(df['Clients'] == count) & (df['System_Type'] == 'Instrumented')]['Avg_Latency_us'].iloc[0]
        
        baseline_latency.append(baseline_val)
        instrumented_latency.append(instrumented_val)
    
    # Set width of bars and positions
    bar_width = 0.35
    x_pos = np.arange(len(client_counts))
    
    # Create bars
    bars1 = ax.bar(x_pos - bar_width/2, baseline_latency, bar_width, 
                   label='Baseline', color='#1f77b4', alpha=0.8, edgecolor='black', linewidth=1)
    bars2 = ax.bar(x_pos + bar_width/2, instrumented_latency, bar_width,
                   label='Instrumented', color='#d62728', alpha=0.8, edgecolor='black', linewidth=1)
    
    # Add value labels on bars
    def add_value_labels(bars):
        for bar in bars:
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height + height*0.01,
                   f'{height:.0f}', ha='center', va='bottom', fontsize=9)
    
    add_value_labels(bars1)
    add_value_labels(bars2)
    
    # Customize the plot
    ax.set_xlabel('Number of Clients', fontsize=12)
    ax.set_ylabel('Average Latency (microseconds)', fontsize=12)
    ax.set_title('Messaging System Latency Comparison', fontsize=14, pad=15)
    ax.set_xticks(x_pos)
    ax.set_xticklabels(client_counts)
    
    # Add grid for better readability
    ax.grid(True, alpha=0.3, axis='y')
    ax.set_axisbelow(True)
    
    # Legend
    ax.legend(loc='upper left', fontsize=11, frameon=True, framealpha=0.9)
    
    plt.tight_layout()
    plt.savefig('messaging_latency_comparison.png', dpi=300, bbox_inches='tight')
    plt.show()

def create_summary_table(df):
    """Create a summary table of the messaging system data"""
    print("Messaging System Performance Summary:")
    print("=" * 100)
    
    # Create a nice formatted table
    summary_df = df[['Clients', 'Messages_Per_Client', 'Total_Messages', 'System_Type', 
                    'Total_Time_ms', 'Throughput_msg_per_sec', 'Avg_Latency_us', 'Overhead_Percent']].copy()
    
    # Round numerical values for better display
    summary_df['Total_Time_ms'] = summary_df['Total_Time_ms'].round(1)
    summary_df['Throughput_msg_per_sec'] = summary_df['Throughput_msg_per_sec'].round(1)
    summary_df['Avg_Latency_us'] = summary_df['Avg_Latency_us'].round(1)
    summary_df['Overhead_Percent'] = summary_df['Overhead_Percent'].round(2)
    
    print(summary_df.to_string(index=False))
    
    # Save to CSV
    summary_df.to_csv('messaging_system_summary.csv', index=False)
    print(f"\nSummary table saved to: messaging_system_summary.csv")
    
    # Performance impact analysis
    print("\n\nPerformance Impact Analysis:")
    print("=" * 50)
    
    for _, row in df[df['System_Type'] == 'Instrumented'].iterrows():
        clients = row['Clients']
        overhead = row['Overhead_Percent']
        baseline_throughput = df[(df['Clients'] == clients) & (df['System_Type'] == 'Baseline')]['Throughput_msg_per_sec'].iloc[0]
        instrumented_throughput = row['Throughput_msg_per_sec']
        throughput_loss = 100 - (instrumented_throughput / baseline_throughput * 100)
        
        print(f"{clients:3d} clients: {overhead:6.1f}% time overhead, {throughput_loss:5.1f}% throughput loss")
    
    # Calculate average overhead
    avg_overhead = df[df['System_Type'] == 'Instrumented']['Overhead_Percent'].mean()
    print(f"\nAverage time overhead: {avg_overhead:.1f}%")

def main():
    """Main function to generate all messaging system visualizations"""
    print("Loading messaging system data...")
    df = load_and_process_messaging_data()
    
    print(f"Loaded {len(df)} records")
    print(f"Client configurations: {sorted(df['Clients'].unique())}")
    print(f"System types: {df['System_Type'].unique().tolist()}")
    
    print("\nGenerating visualizations...")
    
    # 1. Time overhead comparison plot
    print("Creating time overhead comparison plot...")
    plot_time_overhead_comparison(df)
    
    # 2. Throughput comparison bar chart
    print("Creating throughput comparison bar chart...")
    plot_throughput_comparison(df)
    
    # 3. Detailed throughput by clients
    print("Creating detailed throughput by clients chart...")
    plot_detailed_throughput_by_clients(df)
    
    # 4. Latency comparison
    print("Creating latency comparison chart...")
    plot_latency_comparison(df)
    
    # 5. Summary table and analysis
    print("Creating summary table and analysis...")
    create_summary_table(df)
    
    print("\nAll visualizations completed!")
    print("Files generated:")
    print("- messaging_time_overhead.png")
    print("- messaging_throughput_comparison.png")
    print("- messaging_throughput_by_clients.png")
    print("- messaging_latency_comparison.png")
    print("- messaging_system_summary.csv")

if __name__ == "__main__":
    main()