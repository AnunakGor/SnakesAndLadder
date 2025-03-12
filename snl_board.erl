%% Builds the moves list from the board. Converts a 2D board into 
%% a 1D list of moves, where each element represents the destination
-module(snl_board).
-export([build_moves/2, process_row/3]).


%% Type Definitions
-type board() :: [[string()]].
-type moves() :: [integer()].

%% @doc Converts the board into a moves list.
-spec build_moves(Board :: board(), N :: pos_integer()) -> moves().
build_moves(Board, N) ->
    build_moves(Board, N, 1, [], 0).
%% Private recursive function to build the moves list.
build_moves(_, N, I, Moves, _) when I > N * N ->
    lists:reverse([0 | Moves]);
build_moves(Board, N, I, Moves, Row) ->
    RowList = case Row rem 2 of
        0 -> lists:nth(N - Row, Board);
        1 -> lists:reverse(lists:nth(N - Row, Board))
    end,
    NewMoves = process_row(RowList, I, Moves),
    build_moves(Board, N, I + length(RowList), NewMoves, Row + 1).

%% @doc Processes a single row of the board.
-spec process_row(Row :: [string()], StartIndex :: pos_integer(), Moves :: moves()) -> moves().
process_row([], _, Moves) -> Moves;
process_row([H | T], I, Moves) ->
    Dest = case H of
        "." -> -1;
        _ -> list_to_integer(H)
    end,
    process_row(T, I + 1, [Dest | Moves]).
