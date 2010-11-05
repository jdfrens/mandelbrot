-module(mandelbrot_server).
-export([start/0, compute/2]).

start() -> for(1, 4, fun (I) -> spawn(fun loop/0) end).

compute(Pid, What) ->
    rpc(Pid, What),
    timer:sleep(5000),
    rpc(Pid, What).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.


loop() ->
    receive
	{From, Other} ->
	    From ! {self(), {error,Other}},
	    loop()
    end.

for(Max, Max, F) -> [F(Max)];
for(I, Max, F)   -> [F(I) | for(I+1, Max, F)].
