-module(ppm).
% -export([blackOnWhite/1, redScale/1, greenScale/1, blueScale/1, write_ppm/3]).
-compile(export_all).

-include("fractal_constants.hrl").


blackOnWhite({inside})        -> black();
blackOnWhite({outside, _, _}) -> white().

white() -> ppmEntryGray(?MAX_COLOR).
black() -> ppmEntryGray(0).

redScale(X)   -> povScale(fun (C) -> ppmEntry(C, 0, 0) end, fun (C) -> ppmEntry(?MAX_COLOR, C, C) end, X).
greenScale(X) -> povScale(fun (C) -> ppmEntry(0, C, 0) end, fun (C) -> ppmEntry(C, ?MAX_COLOR, C) end, X).
blueScale(X)  -> povScale(fun (C) -> ppmEntry(0, 0, C) end, fun (C) -> ppmEntry(C, C, ?MAX_COLOR) end, X).

ppmEntry(R, G, B) -> string:join([integer_to_list(R), integer_to_list(G), integer_to_list(B)], " ").
ppmEntryGray(N) -> ppmEntry(N, N, N).

povScale(_, _, {inside}) -> black();
povScale(Plateau, Border, {outside, _, N}) ->
    HalfIters = ?MAX_ITERS / 2,
    if
        N < HalfIters -> Plateau(scaleIter(N));
        true          -> Border(scaleIter(N - HalfIters))
    end.

scaleIter(I) -> erlang:round(iterRatio(I) * ?MAX_COLOR).
iterRatio(I) -> (2 * (I-1)) / ?MAX_ITERS.

write_ppm(Colors, Width, Height) ->
    {ok, S} = file:open("mandelbrot.ppm", write),
    io:format(S, "P3~n", []),
    io:format(S, "~p ~p~n", [Width, Height]),
    io:format(S, "~p~n", [?MAX_COLOR]),
    lists:foreach(
        fun(Row) ->
            lists:foreach(fun(C) -> io:format(S, "~s ", [C]) end, Row),
            io:format(S, "~n", [])
        end,
        Colors),
    file:close(S).

