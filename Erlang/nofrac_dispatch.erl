% No warranty.  No guarantees of any sort.
% Creative Commons Attribution-Share Alike 3.0 United States License

-module(nofrac_dispatch).

-export([myMap2/2]).

-import(lists, [map/2]).

% myMap2(F, L) -> map(fun(Row) -> map(F, Row) end, L).
myMap2(F, L) -> pmap(fun(Row) -> map(F, Row) end, L).

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
