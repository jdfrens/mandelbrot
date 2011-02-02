% No warranty.  No guarantees of any sort.
% Creative Commons Attribution-Share Alike 3.0 United States License

-module(nofrac_config).

-export([color_function/1, dimensions/1, iterator/1, lower_right/1, output_file/1, upper_left/1]).

upper_left(Config) -> complex_param("upperleft", Config).
lower_right(Config) -> complex_param("lowerright", Config).

dimensions(Config) ->
    lists:map(fun to_int/1, string:tokens(lookup_value("size", Config), "x")).

output_file(Config) -> lookup_value("outputfile", Config).

iterator(Config) ->
    case lookup_value("type", Config) of
        "julia"       -> fractals:julia_iterator(complex_param("c", Config));
        "mandelbrot"  -> fun fractals:mandelbrot/1;
        "burningship" -> fun fractals:burningship/1;
        "newton"      -> fun fractals:newton/1
    end.

color_function(Config) ->
    case lookup_value("color", Config) of
        "red"    -> fun ppm:red_scale/1;
        "green"  -> fun ppm:green_scale/1;
        "blue"   -> fun ppm:blue_scale/1;
        "bw"     -> fun ppm:black_on_white/1;
        "wb"     -> fun ppm:white_on_black/1;
        "gray"   -> fun ppm:gray_scale/1;
        "random" -> set_random_seed(Config), ppm:random_colors()
    end.


%% **********
%% helpers...

set_random_seed(Config) ->
    case has_seed(Config) of
        true ->
            Seed = to_int(lookup_value("seed", Config)),
            random:seed(Seed, Seed * 1000, Seed * 2001);
        false ->
            {S1, S2, S3} = now(),
            random:seed(S1, S2, S3)
    end.

has_seed(Config) ->
    lists:keymember("seed", 1, Config).

complex_param(Name, Config) ->
    complex:make(lookup_value(Name, Config)).

to_int(Str) -> {Value, _} = string:to_integer(Str), Value.

lookup_value(Key, Config) ->
    {_, Value} = lists:keyfind(Key, 1, Config),
    Value.