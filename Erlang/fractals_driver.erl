-module(fractals_driver).
-export([main/0]).

-import(complex).
-import(fractals, [complexGrid/3, plot/2, mandelbrot/1]).
-import(lists, [map/2]).
-import(ppm).

-include("fractal_constants.hrl").

main() ->
    Grid = complexGrid([?WIDTH, ?HEIGHT], ?UPPER_LEFT, ?LOWER_RIGHT),
    InsAndOuts = myMap2(fun (C) -> mandelbrot(C) end, Grid),
    Colors = myMap2(fun(A) -> ppm:greenScale(A) end, InsAndOuts),
    ppm:write_ppm(Colors, ?WIDTH, ?HEIGHT),
    void.

% myMap2(F, L) -> map(fun(Row) -> map(fun(X) -> F(X) end, Row) end, L).
myMap2(F, L) -> pmap(fun(Row) -> map(fun(X) -> F(X) end, Row) end, L).

% Borrowed from the Joe Armstrong book...
pmap(F, L) ->
    S = self(),
    Ref = erlang:make_ref(),
    Pids = map(fun(I) ->
		       spawn(fun() -> do_f(S, Ref, F, I) end)
	       end, L),
    gather(Pids, Ref).

do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, (catch F(I))}.

gather([Pid|T], Ref) ->
    receive
	{Pid, Ref, Ret} -> [Ret|gather(T, Ref)]
    end;
gather([], _) ->
    [].
