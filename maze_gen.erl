-module(maze_gen).

-export([gen/2]).

-define(NORTH, 1).
-define(SOUTH, 2).
-define(EAST, 4).
-define(WEST, 8).

-define(DIRECTIONS, [1,2,4,8]).

gen(X, Y) ->
    Maze = base_maze(X, Y),
    Positions = make_positions(X, Y, []),
    carve(Maze, Positions).

carve(Maze, []) ->
    Maze;
carve(Maze, Positions) ->
    {X, Y} =  lists:nth(random:uniform(length(Positions)), Positions),
    NewPositions = lists:delete({X, Y}, Positions),
    case uncarved_neighbor(X, Y, Maze, Positions) of
        none ->
            io:format("None~n", []),
            carve(Maze, NewPositions);
        {NX, NY} ->
            {H, [Row | T]} = lists:split(Y-1, Maze),
            {RH, [C | RT]} = lists:split(X-1, Row),

            if
                NX < X andalso NY == Y ->
                    carve(H ++ [RH ++ [C + ?WEST] ++ RT] ++ T, Positions);
                NX == X andalso NY < Y ->
                    carve(H ++ [RH ++ [C + ?NORTH] ++ RT] ++ T, Positions);
                NX == X andalso NY > Y ->
                    carve(H ++ [RH ++ [C + ?SOUTH] ++ RT] ++ T, Positions);
                NX > X andalso NY == Y ->
                    carve(H ++ [RH ++ [C + ?EAST] ++ RT] ++ T, Positions)
            end
    end.

uncarved_neighbor(X, Y, Maze, Positions) ->
    Neighbors = [{X, Y-1}, {X-1, Y}, {X+1, Y}, {X, Y+1}],
    find_uncarved_neighbor(Maze, Neighbors, Positions).

find_uncarved_neighbor(_, [], _Positions) ->
    none;
find_uncarved_neighbor(Maze, Neighbors, Positions) ->
    R = random:uniform(length(Neighbors)),
    {X, Y} = N = lists:nth(R, Neighbors),
    Cell = try
               Row = lists:nth(Y, Maze),
               lists:nth(X, Row)
           catch
               _:_ ->
                   -1
           end,
    case {Cell, lists:member(N, Positions)} of
        {0, true} ->
            N;
        {_, _} ->
            find_uncarved_neighbor(Maze, lists:delete(N, Neighbors), Positions)
    end.

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
