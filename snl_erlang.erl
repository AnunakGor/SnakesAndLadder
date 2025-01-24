-module(snl_erlang).
-export([main/0, f/1, building_moves/5, adding_row_to_moves/3, bfs/4, next_nodes_Generaton/4]).

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

f(Board) ->
    Moves = building_moves(Board, 10, 1, [], 0),
    bfs(Moves, 100).

building_moves(_, N, I, Moves, _) when I > N * N ->
    lists:reverse([0 | Moves]);
building_moves(Board, N, I, Moves, Row) ->
    RowList = case Row rem 2 of
        0 -> lists:nth(N - Row, Board);
        1 -> lists:reverse(lists:nth(N - Row, Board))
    end,
    NewMoves = adding_row_to_moves(RowList, I, Moves),
    building_moves(Board, N, I + length(RowList), NewMoves, Row + 1).

adding_row_to_moves([], _, Moves) -> Moves;
adding_row_to_moves([H | T], I, Moves) ->
    Dest = case H of
        "." -> -1;
        _ -> list_to_integer(H)
    end,
    adding_row_to_moves(T, I + 1, [Dest | Moves]).

bfs(Moves, Goal) ->
    bfs([{1, 0}], Moves, Goal, maps:new()). %Here I am using maps to simulate visited array

bfs([], _, _, _) -> -1;
bfs([{Current, Steps} | Rest], Moves, Goal, Visited) ->
    case Current of
        Goal -> Steps;
        _ ->
            case maps:is_key(Current, Visited) of
                true -> bfs(Rest, Moves, Goal, Visited);
                false ->
                    UpdatedVisited = maps:put(Current, true, Visited),
                    NextNodes = next_nodes_Generaton(Current, Steps, Moves, Goal),
                    bfs(Rest ++ NextNodes, Moves, Goal, UpdatedVisited)
            end
    end.

next_nodes_Generaton(Current, Steps, Moves, Goal) ->
    lists:foldl(fun(Offset, Acc) ->
        Next = Current + Offset,
        case Next =< Goal of
            false -> Acc;
            true ->
                Dest = case lists:nth(Next, Moves) of
                    -1 -> Next;
                    X -> X
                end,
                Acc ++ [{Dest, Steps + 1}]
        end
    end, [], lists:seq(1, 6)).
