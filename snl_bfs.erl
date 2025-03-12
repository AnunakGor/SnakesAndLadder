%% Module implementing the BFS algorithm for the Snakes and Ladders game.
%% It calculates the minimum number of moves required to reach a given goal cell.
-module(snl_bfs).
-export([solve/2, bfs/4, generate_next_nodes/4]).

%% Type Definitions
-type moves() :: [integer()].
-type cell() :: pos_integer().
-type steps() :: non_neg_integer().
-type state() :: {cell(), steps()}.
-type visited() :: map().

%% @doc Solves the Snakes and Ladders problem using BFS.
-spec solve(Moves :: moves(), Goal :: cell()) -> steps() | -1.
solve(Moves, Goal) ->
    bfs([{1, 0}], Moves, Goal, maps:new()).

%% @doc Performs a breadth-first search to determine the shortest path.
-spec bfs(Queue :: [state()], Moves :: moves(), Goal :: cell(), Visited :: visited()) -> steps() | -1.
bfs([], _, _, _) -> -1;
bfs([{Current, Steps} | Rest], Moves, Goal, Visited) ->
    case Current of
        Goal -> Steps;
        _ ->
            case maps:is_key(Current, Visited) of
                true -> bfs(Rest, Moves, Goal, Visited);
                false ->
                    UpdatedVisited = maps:put(Current, true, Visited),
                    NextNodes = generate_next_nodes(Current, Steps, Moves, Goal),
                    bfs(Rest ++ NextNodes, Moves, Goal, UpdatedVisited)
            end
    end.
%% @doc Generates all valid next states from the current board cell.
%% It simulates dice rolls from 1 to 6 and accounts for ladders/snakes.
-spec generate_next_nodes(Current :: cell(), Steps :: steps(), Moves :: moves(), Goal :: cell()) -> [state()].
generate_next_nodes(Current, Steps, Moves, Goal) ->
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
