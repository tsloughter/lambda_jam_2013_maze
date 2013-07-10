%% Hi, I'm very slow
%% 3> maze_gen:gen(10, 10).
%% [[6,12,10,4,10,2,2,4,14,8],
%%  [1,4,13,12,11,3,3,2,5,10],
%%  [4,10,4,10,3,3,3,3,4,11],
%%  [4,13,12,13,11,5,13,11,4,11],
%%  [4,10,4,12,13,14,12,13,10,3],
%%  [4,15,14,12,12,9,2,4,15,9],
%%  [6,11,1,4,14,12,11,2,5,10],
%%  [1,3,4,14,9,4,15,9,2,3],
%%  [2,3,4,11,4,10,7,10,3,3],
%%  [5,9,4,9,4,13,9,5,13,9]]

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
    {C1, C2} =  lists:nth(random:uniform(length(Positions)), Positions),
    carve(Maze, Positions, [{C1, C2}], 0).

carve(Maze, [], _, _L) ->
    Maze;
carve(Maze, Positions, Carved, L) ->
    {X, Y} =  lists:nth(random:uniform(length(Carved)), Carved),
    NewPositions = lists:delete({X, Y}, Positions),
    case uncarved_neighbor(X, Y, Maze, Positions) of
        none ->
            carve(Maze, NewPositions, Carved, L+1);
        {NX, NY} ->
            Maze2 = replace_row(X, Y, NX, NY, Maze),
            carve(replace_row(NX, NY, X, Y, Maze2), Positions, [{NX, NY} | Carved], L+1)
    end.

replace_row(X, Y, NX, NY, Maze) ->
    {H, [Row | T]} = lists:split(Y-1, Maze),
    {RH, [C | RT]} = lists:split(X-1, Row),

    if
        NX < X andalso NY == Y ->
            H ++ [RH ++ [C + ?WEST] ++ RT] ++ T;
        NX == X andalso NY < Y ->
            H ++ [RH ++ [C + ?NORTH] ++ RT] ++ T;
        NX == X andalso NY > Y ->
            H ++ [RH ++ [C + ?SOUTH] ++ RT] ++ T;
        NX > X andalso NY == Y ->
            H ++ [RH ++ [C + ?EAST] ++ RT] ++ T
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
