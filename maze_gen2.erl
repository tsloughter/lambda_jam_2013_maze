-module(maze_gen2).

-export([gen/2,
         cell/3]).

-define(NORTH, 1).
-define(SOUTH, 2).
-define(EAST, 4).
-define(WEST, 8).

-include_lib("stdlib/include/qlc.hrl").

gen(X, Y) ->
    application:start(sasl),
    application:start(gproc),
    make_positions(X, Y),

    Q1 = qlc:q([XY || {{n, l, XY}, _, uncarved} <- gproc:table(names)]),
    Positions = qlc:eval(Q1),

    {C1, C2} =  lists:nth(random:uniform(length(Positions)), Positions),
    gproc:send({n, l, {C1, C2}}, {carved, self()}),
    receive
        ok ->
            ok
    end,
    carve(),

    Q2 = qlc:q([XY || {{n, l, XY}, _, _} <- gproc:table(names)]),
    H1 = qlc:eval(Q2),
    print(X, Y, get_value(H1, []), []).

print(_, 0, _, R) ->
    R;
print(X, Y, L, R) ->
    {Row, T} = lists:split(X, L),
    print(X, Y-1, T, [Row | R]).

get_value([], Values) ->
    Values;
get_value([H | T], Values) ->
    gproc:send({n, l, H}, {get_value, self()}),
    receive
        {ok, Value} ->
            get_value(T, [Value | Values])
    end.

carve() ->
    Q1 = qlc:q([XY || {{n, l, XY}, _, carved} <- gproc:table(names)]),

    case qlc:eval(Q1) of
        [] ->
            done;
        Positions ->
            {X, Y} =  lists:nth(random:uniform(length(Positions)), Positions),

            case uncarved_neighbor(X, Y) of
                none ->
                    gproc:send({n, l, {X, Y}}, {no_neighbors, self()}),
                    receive
                        ok ->
                            ok
                    end,
                    carve();
                {NX, NY} ->
                        if
                            NX < X andalso NY == Y ->
                                gproc:send({n, l, {X, Y}}, {carve, ?WEST}),
                                gproc:send({n, l, {NX, NY}}, {carve, ?EAST});
                            NX == X andalso NY < Y ->
                                gproc:send({n, l, {X, Y}}, {carve, ?SOUTH}),
                                gproc:send({n, l, {NX, NY}}, {carve, ?NORTH});
                            NX == X andalso NY > Y ->
                                gproc:send({n, l, {X, Y}}, {carve, ?NORTH}),
                                gproc:send({n, l, {NX, NY}}, {carve, ?SOUTH});
                            NX > X andalso NY == Y ->
                                gproc:send({n, l, {X, Y}}, {carve, ?EAST}),
                                gproc:send({n, l, {NX, NY}}, {carve, ?WEST})
                        end,

                    gproc:send({n, l, {NX, NY}}, {carved, self()}),
                    receive
                        ok ->
                            ok
                    end,
                    carve()
            end
    end.

uncarved_neighbor(X, Y) ->
    Neighbors = [{X, Y-1}, {X-1, Y}, {X+1, Y}, {X, Y+1}],
    Q1 = qlc:q([XY || {{n, l, XY}, _, uncarved} <- gproc:table(names)]),
    case qlc:eval(Q1) of
        [] ->
            none;
        Uncarved ->
            case sets:to_list(sets:intersection(sets:from_list(Neighbors), sets:from_list(Uncarved))) of
                [] ->
                    none;
                L ->
                    lists:nth(random:uniform(length(L)), L)
            end
    end.


make_positions(_X, 0) ->
    ok;
make_positions(X, Y) ->
    make_column(X, Y),
    make_positions(X, Y-1).

make_column(0, _Y) ->
    ok;
make_column(X, Y) ->
    gproc:reg_or_locate({n, l, {X, Y}}, uncarved, fun() -> ?MODULE:cell(X, Y, 0) end),
    make_column(X-1, Y).


cell(X, Y, Value) ->
    receive
        {carved, From} ->
            gproc:set_value({n, l, {X, Y}}, carved),
            From ! ok,
            cell(X, Y, Value);
        {no_neighbors, From} ->
            gproc:set_value({n, l, {X, Y}}, no_neighbors),
            From ! ok,
            cell(X, Y, Value);
        {carve, C} ->
            cell(X, Y, Value+C);
        {get_value, From} ->
            From ! {ok, Value},
            cell(X, Y, Value)
    end.
