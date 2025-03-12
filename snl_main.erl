%% Main module for the Snakes and Ladders BFS solver.
%% It initializes a default board, converts it into a moves list, and
%% computes the minimum number of moves to reach the target cell.
-module(snl_main).
-export([main/0, manual_input/0]).



%% @doc Entry point for solving the Snakes and Ladders problem. Initializes a predefined board and prints the minimum moves.
-spec main() -> ok.
main() ->
    Board = [
        [".", ".", ".", ".", "16", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", "5", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", ".", ".", "."],
        ["11", ".", ".", ".", ".", ".", ".", ".", ".", "."]
    ],
    
    Moves = snl_board:build_moves(Board, 10),
    Result = snl_bfs:solve(Moves, 100),
    
    io:format("Minimum Moves to Reach 100: ~p~n", [Result]).

%% @doc Provides a manual input option for a 2×2 grid.
-spec manual_input() -> ok.
manual_input() ->
    io:format("Enter values for the 2x2 grid (use '.' for empty spaces):~n"),
    {ok, Line1} = io:read("Row 1 (comma-separated): "),
    {ok, Line2} = io:read("Row 2 (comma-separated): "),
    Row1 = string:split(Line1, ",", all),
    Row2 = string:split(Line2, ",", all),
    Grid = [Row1, Row2],
    io:format("Your 2x2 grid: ~p~n", [Grid]).