-module(fractals_driver).
-export([main/1]).

-import(complex).
-import(nofrac_dispatch, [myMap2/2]).
-import(fractals, [complexGrid/3, plot/2, mandelbrot/1]).
-import(ppm).

-include("fractal_constants.hrl").

main(Config) ->
    Dimensions = nofrac_config:dimensions(Config),
    Grid = complexGrid(Dimensions, nofrac_config:upper_left(Config), nofrac_config:lower_right(Config)),
    InsAndOuts = myMap2(nofrac_config:iterator(Config), Grid),
    Colors = myMap2(nofrac_config:color_function(Config), InsAndOuts),
    ppm:write_ppm(nofrac_config:output_file(Config), Colors, Dimensions),
    void.
