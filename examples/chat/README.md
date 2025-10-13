# Chat System

A concurrent chat room system where multiple clients can join rooms and exchange messages.

## System Components

- **chat_server**: Main server that manages chat rooms
- **client**: Client process that can join rooms and send messages
- **chat_room**: Individual room process (created automatically)
- **client_registry**: Registry for tracking clients (started automatically)

## Running Manually

### 1. Start the Erlang shell
```bash
erl
```

### 2.1 Compile the modules
```erlang
c(chat_server).
c(client).
c(chat_room).
c(client_registry).
```

### 2.2 Compile the monitor and the conductor
```erlang
c(conductor).
c(monitor).
```

### 3. Start the system with monitoring
```erlang
%% Start the conductor
conductor:start_link().

%% Start the monitor
monitor:start().

%% Start the chat server
{ok, _} = chat_server:start_link().

%% Start clients
{ok, _} = client:start_link(alice).
{ok, _} = client:start_link(bob).

%% Register clients
gen_server:call(alice, register).
gen_server:call(bob, register).
```

### 4. Use the chat system
```erlang
%% Join a room
gen_server:call(alice, {join_room, lobby}).
gen_server:call(bob, {join_room, lobby}).

%% Send messages
gen_server:call(alice, {send_message, lobby, "Hello everyone!"}).
gen_server:call(bob, {send_message, lobby, "Hi Alice!"}).

%% Leave room
gen_server:call(alice, {leave_room, lobby}).
gen_server:call(bob, {leave_room, lobby}).
```

### 5. Cleanup
```erlang
gen_server:stop(alice).
gen_server:stop(bob).
gen_server:stop(chat_server).
```

## Using the Test File

The `simple_chat_test.erl` file automatically tests the system with 1, 2, and 4 concurrent clients.

### Compile and run:
```erlang
c(simple_chat_test).
simple_chat_test:run().
```

### Example output:
```
=== CHAT SYSTEM DEMONSTRATION ===
Testing with 1, 2, and 4 clients...

--- Testing 1 client(s), 5 messages each ---
  Total messages: 5
  Completed in: 45.32 ms

--- Testing 2 client(s), 5 messages each ---
  Total messages: 10
  Completed in: 52.18 ms

--- Testing 4 client(s), 5 messages each ---
  Total messages: 20
  Completed in: 68.45 ms

=== DEMONSTRATION COMPLETE ===
```

The test automatically:
- Starts the chat server
- Creates multiple clients
- Joins a room
- Sends messages concurrently
- Leaves the room
- Cleans up resources

All the interactions are logged into a `monitor_log.txt` and `sub_monitor_log.txt` files, where we can observe the context creation
and message association within the system interactions.

## Testing Multiple Rooms

You can manually test with multiple rooms:
```erlang
gen_server:call(alice, {join_room, room1}).
gen_server:call(bob, {join_room, room2}).
gen_server:call(alice, {send_message, room1, "In room 1"}).
gen_server:call(bob, {send_message, room2, "In room 2"}).
```
