-module(nofrac_config).

-export([color_function/1, dimensions/1, lower_right/1, output_file/1, upper_left/1]).

upper_left(Config) -> complex_param("upperleft", Config).
lower_right(Config) -> complex_param("lowerright", Config).

dimensions(Config) ->
    lists:map(fun to_int/1, string:tokens(lookup_value("size", Config), "x")).

output_file(Config) -> lookup_value("outputfile", Config).

%% **********
%% helpers...

color_function(Config) ->
    case lookup_value("color", Config) of
        "red"    -> fun ppm:red_scale/1;
        "green"  -> fun ppm:green_scale/1;
        "blue"   -> fun ppm:blue_scale/1;
        "bw"     -> fun ppm:black_on_white/1;
        "wb"     -> fun ppm:white_on_black/1;
        "gray"   -> fun ppm:gray_scale/1;
        "random" -> ppm:random_colors()
    end.

complex_param(Name, Config) ->
    complex:make(lookup_value(Name, Config)).

to_int(Str) -> {Value, _} = string:to_integer(Str), Value.

lookup_value(Key, Config) ->
    {_, Value} = lists:keyfind(Key, 1, Config),
    Value.