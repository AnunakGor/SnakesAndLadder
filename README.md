# Snakes and Ladders Solver

## Introduction

This project implements a **Snakes and Ladders** solver in Erlang using **Breadth-First Search (BFS)**. The program determines the minimum number of moves required to reach the final cell of the board, accounting for **snakes and ladders** that modify the player's movement.

## Features

- **Breadth-First Search (BFS) Algorithm** for shortest path calculation.
- **Custom Board Input** via manual user entry.
- **Unit Tests** to validate core functionality.

## Usage

To execute the program, run:

```sh
erl
```

Then, in the Erlang shell:

```erl
c(snl_board), c(snl_main), c(snl_bfs), c(snl_utils), c(test).
snl_main:main().
```

For manual input mode:

```erl
snl_main:manual_input(). 
%% Add the 10*10 custom grid as argument to this function
```

## Modules

The project consists of the following Erlang modules:

### 1. **`snl_bfs.erl`** (Breadth-First Search Solver)
   - Implements BFS to find the shortest path from start to goal.
   - Handles board constraints (snakes and ladders).
   - Functions:
     - `solve/2`: Finds the minimum moves required.
     - `bfs/4`: Recursive BFS search.
     - `generate_next_nodes/4`: Determines valid next moves.

### 2. **`snl_board.erl`** (Board Processing)
   - Converts a 2D board representation into a 1D move list.
   - Functions:
     - `build_moves/2`: Constructs the board's move list.
     - `process_row/3`: Processes each row for snakes and ladders.

### 3. **`snl_main.erl`** (Main Entry Point)
   - Initializes the board and solves the game.
   - Functions:
     - `main/0`: Runs a predefined board.
     - `manual_input/0`: Allows user input for board configuration.

### 4. **`snl_utils.erl`** (Utility Functions)
   - Provides helper functions such as logging.
   - Functions:
     - `log/2`: Prints log messages.

### 5. **`test.erl`** (Unit Tests)
   - Implements **EUnit** tests for BFS and board processing.
   - Functions:
     - `plain_board_test/0`: Tests a board with no snakes or ladders.
     - `immediate_goal_test/0`: Tests reaching the goal instantly.
     - `ladder_board_test/0`: Simulates a board with a ladder.
     - `generate_next_nodes_plain_test/0`: Tests move generation.
     - `generate_next_nodes_with_ladder_test/0`: Tests move adjustments for ladders.
     - `unsolvable_board_test/0`: Tests an unsolvable board.

## Testing

Run unit tests using:

```sh
eunit:test(test).
```

