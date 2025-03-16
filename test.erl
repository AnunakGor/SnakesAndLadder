-module(test).

-include_lib("eunit/include/eunit.hrl").

%% Helper function to create a plain moves list.
plain_moves(Goal) ->
    [0] ++ lists:duplicate(Goal, -1).

%% Helper function to update the Nth element of a list 
update_nth(List, Index, Value) ->
    {Prefix, [_Old|Suffix]} = lists:split(Index - 1, List),
    Prefix ++ [Value] ++ Suffix.

%% Test case: plain board with no ladders or snakes.
%% For a 100-cell plain board, the minimum moves expected is 17.
plain_board_test() ->
    Moves = plain_moves(100),
    ?assertEqual(17, snl_bfs:solve(Moves, 100)).

%% Test case: trivial case where the starting cell is the goal.
immediate_goal_test() ->
    Moves = plain_moves(10),
    ?assertEqual(0, snl_bfs:solve(Moves, 1)).

%% Test case: board with a ladder.
%% We simulate a ladder from cell 2 to cell 100 so that from cell 1,
%% rolling a 1 will take us immediately to 100, i.e. in 1 move.
ladder_board_test() ->
    Moves0 = plain_moves(100),
    Moves = update_nth(Moves0, 2, 100),
    ?assertEqual(1, snl_bfs:solve(Moves, 100)).

%% Test case: generate_next_nodes in a plain board scenario.
%% For current cell 95 and steps 3, dice rolls 1-5 are valid (cell 101 is not).
%% Expected states: {96,4}, {97,4}, {98,4}, {99,4}, {100,4}.
generate_next_nodes_plain_test() ->
    Moves = plain_moves(100),
    NextNodes = snl_bfs:generate_next_nodes(95, 3, Moves, 100),
    Expected = [{96,4}, {97,4}, {98,4}, {99,4}, {100,4}],
    ?assertEqual(Expected, NextNodes).

%% Test case: generate_next_nodes with a ladder.
%% Here we simulate a ladder at cell 8 that sends to cell 31.
%% For current cell 7 and steps 2, the dice roll 1 (7+1=8) should return {31,3}.
%% Other dice outcomes (offsets 2..6) simply return the next cell.
generate_next_nodes_with_ladder_test() ->
    Moves0 = plain_moves(100),
    Moves = update_nth(Moves0, 8, 31),
    NextNodes = snl_bfs:generate_next_nodes(7, 2, Moves, 100),
    Expected = [{31,3}, {9,3}, {10,3}, {11,3}, {12,3}, {13,3}],
    ?assertEqual(Expected, NextNodes).

%% Test case: unsolvable board.
%% Here we create a board for a 10-cell game where every dice move leads back to cell 1,
%% making it impossible to progress. The expected result is -1.
unsolvable_board_test() ->
    %% Cells 2 to 10 are all forced back to 1.
    Moves = [0] ++ lists:duplicate(10, 1),
    ?assertEqual(-1, snl_bfs:solve(Moves, 10)).
