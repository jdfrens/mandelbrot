-module(ppm).
-export([black_on_white/1, white_on_black/1, gray_scale/1, red_scale/1, green_scale/1, blue_scale/1, random_colors/0, write_ppm/3]).

-include("fractal_constants.hrl").

black_on_white({inside})        -> black();
black_on_white({outside, _, _}) -> white().

white_on_black({inside})        -> white();
white_on_black({outside, _, _}) -> black().

gray_scale({inside}) -> black();
gray_scale({outside, _, N}) ->
     Val = erlang:round(?MAX_COLOR * (N / ?MAX_ITERS)),
     ppm_entry_gray(Val).

red_scale(X)   -> pov_scale(fun (C) -> ppm_entry(C, 0, 0) end, fun (C) -> ppm_entry(?MAX_COLOR, C, C) end, X).
green_scale(X) -> pov_scale(fun (C) -> ppm_entry(0, C, 0) end, fun (C) -> ppm_entry(C, ?MAX_COLOR, C) end, X).
blue_scale(X)  -> pov_scale(fun (C) -> ppm_entry(0, 0, C) end, fun (C) -> ppm_entry(C, C, ?MAX_COLOR) end, X).

random_colors() ->
    random:seed(),
    Colors = random_colors_array(?MAX_ITERS),
    fun (X) ->
        case X of
            {inside}        -> black();
            {outside, _, N} -> lists:nth((N rem length(Colors)) + 1, Colors)
        end
    end.


%% **********
%% Helpers...

random_colors_array(0) -> [];
random_colors_array(N) ->
    Color = ppm_entry(random:uniform(?MAX_COLOR-1), random:uniform(?MAX_COLOR-1), random:uniform(?MAX_COLOR-1)),
    [Color | random_colors_array(N-1)].

white() -> ppm_entry_gray(?MAX_COLOR).
black() -> ppm_entry_gray(0).

ppm_entry(R, G, B) -> string:join([integer_to_list(R), integer_to_list(G), integer_to_list(B)], " ").
ppm_entry_gray(N) -> ppm_entry(N, N, N).

pov_scale(_, _, {inside}) -> black();
pov_scale(Plateau, Border, {outside, _, N}) ->
    HalfIters = ?MAX_ITERS / 2,
    if
        N < HalfIters -> Plateau(scaleIter(N));
        true          -> Border(scaleIter(N - HalfIters))
    end.

scaleIter(I) -> erlang:round(iterRatio(I) * ?MAX_COLOR).
iterRatio(I) -> (2 * (I-1)) / ?MAX_ITERS.

write_ppm(Filename, Colors, [Width, Height]) ->
    {ok, S} = file:open(Filename, write),
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

