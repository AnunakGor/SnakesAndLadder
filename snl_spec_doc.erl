-module(snl_spec_doc).

-export([main/0, f/1, building_moves/5, adding_row_to_moves/3, bfs/4, next_nodes_Generaton/4]).

%% @doc
%% Runs the snake and ladder solver with a predefined board configuration.
%% Returns the result of printing the final moves count.
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
        [".", ".", ".", ".", ".", ".", ".", ".", ".", "."],
        [".", ".", ".", ".", ".", ".", ".", ".", ".", "."],
        ["11", ".", ".", ".", ".", ".", ".", ".", ".", "."]
    ],
    Result = f(Board),
    io:format("~p~n", [Result]).

%% @doc
%% Solves the snake and ladder game given a board configuration.
%% Returns the minimum number of moves required to reach the goal (cell 100),
%% or -1 if the goal is unreachable.
-spec f(Board :: list(list(string()))) -> integer().
f(Board) ->
    Moves = building_moves(Board, 10, 1, [], 0),
    bfs(Moves, 100).

%% @doc
%% Builds a list of moves mapping for each board cell.
%% The board is processed row by row, taking into account the alternating direction.
%% Returns a list of integers where -1 indicates no snake/ladder,
%% and any other number indicates the destination cell.
-spec building_moves(Board :: list(list(string())), N :: integer(), I :: integer(), Moves :: list(integer()), Row :: integer()) -> list(integer()).
building_moves(_, N, I, Moves, _) when I > N * N ->
    lists:reverse([0 | Moves]);
building_moves(Board, N, I, Moves, Row) ->
    RowList = case Row rem 2 of
        0 -> lists:nth(N - Row, Board);
        1 -> lists:reverse(lists:nth(N - Row, Board))
    end,
    NewMoves = adding_row_to_moves(RowList, I, Moves),
    building_moves(Board, N, I + length(RowList), NewMoves, Row + 1).

%% @doc
%% Converts a row of board strings to a moves mapping.
%% Each cell that contains "." is set to -1, otherwise the cell is converted to an integer.
-spec adding_row_to_moves(RowList :: list(string()), I :: integer(), Moves :: list(integer())) -> list(integer()).
adding_row_to_moves([], _, Moves) ->
    Moves;
adding_row_to_moves([H | T], I, Moves) ->
    Dest = case H of
        "." -> -1;
        _   -> list_to_integer(H)
    end,
    adding_row_to_moves(T, I + 1, [Dest | Moves]).

%% @doc
%% Initiates a breadth-first search to determine the minimum moves required to reach the goal.
%% It wraps the bfs/4 call with an initial queue and an empty visited map.
-spec bfs(Moves :: list(integer()), Goal :: integer()) -> integer().
bfs(Moves, Goal) ->
    bfs([{1, 0}], Moves, Goal, maps:new()).

%% @doc
%% Performs a breadth-first search using a queue of {Current, Steps} tuples,
%% a moves mapping, a goal cell, and a visited map to track already explored cells.
%% Returns the minimum number of moves to reach the goal or -1 if unreachable.
-spec bfs(Queue :: list({integer(), integer()}), Moves :: list(integer()), Goal :: integer(), Visited :: map()) -> integer().
bfs([], _, _, _) ->
    -1;
bfs([{Current, Steps} | Rest], Moves, Goal, Visited) ->
    case Current of
        Goal ->
            Steps;
        _ ->
            case maps:is_key(Current, Visited) of
                true ->
                    bfs(Rest, Moves, Goal, Visited);
                false ->
                    UpdatedVisited = maps:put(Current, true, Visited),
                    NextNodes = next_nodes_Generaton(Current, Steps, Moves, Goal),
                    bfs(Rest ++ NextNodes, Moves, Goal, UpdatedVisited)
            end
    end.

%% @doc
%% Generates the next possible moves from the current cell based on dice rolls (1 to 6).
%% If landing on a snake or ladder, the destination is adjusted accordingly.
%% Returns a list of tuples in the form {Destination, Steps + 1}.
-spec next_nodes_Generaton(Current :: integer(), Steps :: integer(), Moves :: list(integer()), Goal :: integer()) -> list({integer(), integer()}).
next_nodes_Generaton(Current, Steps, Moves, Goal) ->
    lists:foldl(fun(Offset, Acc) ->
        Next = Current + Offset,
        case Next =< Goal of
            false ->
                Acc;
            true ->
                Dest = case lists:nth(Next, Moves) of
                    -1 -> Next;
                    X  -> X
                end,
                Acc ++ [{Dest, Steps + 1}]
        end
    end, [], lists:seq(1, 6)).
