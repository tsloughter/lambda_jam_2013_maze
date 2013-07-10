-module(maze_gen).

-export([gen/2]).

-define(NORTH, 1).
-define(SOUTH, 2).
-define(EAST, 4).
-define(WEST, 8).

gen(X, Y) ->
    Maze = base_maze(X, Y),
    Positions = make_positions(X, Y, []),
    carve(Maze, Positions).

carve(Maze, []) ->
    Maze;
carve(Maze, Positions) ->
    io:format("Maze ~p~n", [Maze]),
    {X, Y} =  lists:nth(random:uniform(length(Positions)), Positions),
    NewPositions = lists:delete({X, Y}, Positions),
    {H, [Row | T]} = lists:split(Y-1, Maze),
    {RH, [_C | RT]} = lists:split(X-1, Row),
    carve(H ++ [RH ++ [?SOUTH] ++ RT] ++ T, NewPositions).

base_maze(X, Y) ->
    Row = make_row(X, []),
    add_row(Y, Row, []).

make_row(0, L) ->
    L;
make_row(X, L) ->
    make_row(X-1, [0 | L]).

add_row(0, _Row, L) ->
    L;
add_row(Y, Row, L) ->
    add_row(Y-1, Row, [Row | L]).

make_positions(_X, 0, L) ->
    lists:flatten(L);
make_positions(X, Y, L) ->
    Col = make_column(X, Y, L),
    make_positions(X, Y-1, [Col | L]).

make_column(0, _Y, L) ->
    L;
make_column(X, Y, L) ->
    make_column(X-1, Y, [{X, Y} | L]).
