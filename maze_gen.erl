-module(maze_gen).

-export([gen/2]).

-define(NORTH, 1).
-define(SOUTH, 2).
-define(EAST, 4).
-define(WEST, 8).

gen(X, Y) ->
    base_maze(X, Y).

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
