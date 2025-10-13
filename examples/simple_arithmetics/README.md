# Arithmetic System

A concurrent arithmetic processing system where clients send numbers to a central server for processing.

## System Components

- **slow_central_server**: Main server that coordinates arithmetic operations
- **client**: Client process that sends numbers for processing
- **slow_proc_add10** & **slow_proc_mul2**: Worker processes (started automatically)

## Running Manually

### 1. Start the Erlang shell
```bash
erl
```

### 2. Compile the modules
```erlang
c(slow_central_server).
c(client).
```

### 3. Start the system
```erlang
%% Start the central server
{ok, _} = slow_central_server:start_link().

%% Start a client
{ok, _} = client:start_link(my_client).

%% Connect the client
gen_server:call(my_client, connect).
```

### 4. Send operations
```erlang
%% Send a number for processing
gen_server:call(my_client, {send_number, 42}).
```

### 5. Cleanup
```erlang
gen_server:call(my_client, disconnect).
gen_server:stop(my_client).
gen_server:stop(slow_central_server).
```

## Using the Test File

The `simple_concurrent_test.erl` file automatically tests the system with 1, 2, 4, and 8 concurrent clients.

### Compile and run:
```erlang
c(simple_concurrent_test).
simple_concurrent_test:run().
```

### Example output:
```
=== CONCURRENT SYSTEM DEMONSTRATION ===
Testing with 1, 2, 4, and 8 clients...

--- Testing with 1 client(s), 1000 operations each ---
  Total operations: 1000
  Time: 1234.56 ms
  Throughput: 810.23 ops/sec

--- Testing with 2 client(s), 1000 operations each ---
  Total operations: 2000
  Time: 678.90 ms
  Throughput: 2945.67 ops/sec
...
```

The test automatically:
- Starts the server
- Creates multiple clients
- Runs operations concurrently
- Measures performance
- Cleans up resources

All the interactions are logged into a `monitor_log.txt` file, where we can observe the context creation
and message association within the system interactions.

## Quick Test

For a quick manual test:
```erlang
c(manual_baseline_test).
manual_baseline_test:test_quick().
```
