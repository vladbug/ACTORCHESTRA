# ACTORCHESTRA

<div align="center">

```
♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♬ ♪ ♫
                      
♪ ♫ ♩  now playing...  ♫ ♪ ♩
  
     __          __     _   _______ ______
     \ \        / /\   | | |__   __|___  /
      \ \  /\  / /  \  | |    | |     / / 
       \ \/  \/ / /\ \ | |    | |    / /  
        \  /\  / ____ \| |____| |   / /__ 
         \/  \/_/    \_\______|_|  /_____|
         
           ~  A C T O R C H E S T R A  ~
                      
♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♫ ♩ ♬ ♪ ♬ ♪ ♫
```

</div>

---

## Overview

`ACTORCHESTRA` is a runtime verification framework built in `Erlang` that allows the specification of properties over an `Erlang` OTP client-server system. WALTZ is the specification language that allows the definition of formal properties that are compiled into `Erlang` executables that run alongside the system to capture the messages and verify the specified properties. 

Besides supporting WALTZ, `ACTORCHESTRA` manages context management of messages of systems that follow the OTP client-server behaviour by surgically injecting code in the handlers to allow a correlation token to be shared across a request chain that is causally related to allow the verification of properties in concurrent and distributed environments followed by the actor-based principles. 

The entity responsible for sharing and assigning these context values is the **conductor** and is the main entity responsible for the orchestration of the contexts through the system.

The main components of `ACTORCHESTRA` are:
- `context_injector.erl`
- `conductor.erl`
- WALTZ compiler

### Prerequesites and Setup

Before running the project, ensure that the following dependencies are installed on your system.

#### Erlang

Erlang is required to run parts of the system that depend on the Erlang runtime environment. Install via your package manager or from https://www.erlang.org/downloads

```bash
sudo apt install erlang    # Ubuntu/Debian
brew install erlang        # macOS
```

#### OCaml

OCaml is used to generate our monitors from the specification language. It is recommended to install it through `opam`:

```bash
sudo apt install opam              # Ubuntu/Debian
brew install opam                  # macOS
opam init
opam switch create <name> ocaml-base-compiler
opam switch <name>
eval $(opam env --switch=<name> --set-switch)
```

With this, you will have a switch with the `ocaml` compiler isntalled in your machine, and all
`opam` instalations will happen within the currently set switch.

##### Dune
Dune is the official build system for OCaml projects. We can use `opam` to install it:

```bash
opam install dune
```

Menhir is a parser generator used for compiling grammar definitions in OCaml. Install it via opam:

```bash
opam install menhir
```

A lexer tool (such as ocamllex) is needed for token generation. Install it via opam:

```bash
opam install ocamllex
```

Once all dependencies are installed, you can build and run the project using:

```bash
dune build
dune exec bin/main.exe
```

This is only needed for the monitor generation, not the interaction with the Erlang system. 

### Instructions
`ACTORCHESTRA` has some system examples in the `examples` folder to run and check the tool. In order to test the examples follow the instructions
in each one of them. Essentially, the tool will inject automatically code into all the `Erlang` modules that import the following instruction:

```
-compile({parse_transform, context_injector}).
```

This will instruct the compiler to change the AST accordingly to the rules defined in the `context_injector.erl` file. After this procedure is done,
the `.beam` files will already have the changes necessary. After that, we need to compile the system as usual, and also initialize the `condutor.erl` 
and the `monitor.erl` generated from the WALTZ compilation of the user defined property. Some examples of property specifications can be found in the
`README` file inside the WALTZ folder.

### Instructions

Before interacting with the system, both the Conductor and Monitor components must be initialized and running.
Follow the steps below carefully to ensure proper setup and integration.

#### Compile Required Modules

Open your Erlang shell from the project directory:

```bash
erl
```
Then compile the necessary source files:

```erlang
c(conductor).
c(monitor).
```

If your monitor property generates one or more sub-monitors, compile those as well:

```erlang
%% If a sub_monitor module exists:
c(sub_monitor).
```

#### Running compiled modules

Once compilation is complete, start both the conductor and the monitor.
The monitor will automatically handle spawning of any sub-monitors, so you don’t need to start them manually.

```erlang
conductor:start_link().
monitor:start().
```

This initializes both processes and sets up the system for monitoring and context management.
After the conductor and monitor are running, you can now interact with your system normally.
The context_injector ensures that all system operations automatically communicate with the conductor for proper context propagation and management.

### Curious about the injected code?

It is possible to check the updated code after the automatic changes to the AST by running the following comand on your terminal
to generate a `.P` file that can be consulted to see the changes. In order to do so, for the files adding the header `-compile({parse_transform, context_injector}).`
simply type:

```bash
elrc -P -pa . file_name.erl
```

## ✅ To-Do List

- [x] Fix the bug of UpperCase letter variables from map extraction
- [x] Fix the bug of message count not working correctly in the case of spawned nested operators
- [x] Fix the bug of incorrectly assignining context extraction on the client when not needed
- [ ] Refactor the code to avoid certain compilation repetition
- [ ] Prototype version of disjunction and conjunction
- [ ] io:format embeded into compilation process for blame registration
