defmodule Mandelbrot.Fractal do

  import Stream, only: [ concat: 2, iterate: 2, map: 2, take: 2, take_while: 2 ]

  @magnitude_cutoff         2.0
  @magnitude_cutoff_squared 4.0

  def generate(options) do
    concat(
      generate_header(options),
      generate_image(options)
      )
  end

  def generate_header(options) do
    %Mandelbrot.Size{ width: width, height: height } = options.size
    [
      "P3",
      Integer.to_string(width),
      Integer.to_string(height),
      "255"
    ]
  end

  def generate_image(options) do
    import Mandelbrot.NextFunction, only: [ next_function: 2 ]
    import Mandelbrot.Color, only: [ color_function: 1 ]
    import Mandelbrot.Grid, only: [ generate_grid: 1]

    color_func = color_function(options)
    options
    |> generate_grid
    |> map(&build_complex/1)
    |> map(fn   grid_point -> { grid_point, next_function(grid_point, options) } end)
    |> map(fn { grid_point, next_func } -> generate_pixel(grid_point, next_func, color_func) end)
  end

  def build_complex({ r, i }), do: %Complex{ real: r, imag: i }

  def generate_pixel(grid_point, next_func, color_func) do
    fractal_iterate(next_func, &cutoff/1, grid_point)
    |> in_or_out
    |> apply_color(color_func)
  end

  def cutoff(z) do
    Complex.magnitude_squared(z) < @magnitude_cutoff_squared
  end

  def apply_color(in_out, color_func) do
    color_func.(in_out)
  end

  def in_or_out({ z, iterations }) do
    status = if iterations >= 255, do: :inside, else: :outside
    { status, z, iterations }
  end

  def fractal_iterate(next, cutoff, grid_point) do
    grid_point
    |> iterate(next)
    |> take_while(cutoff)
    |> (&(concat([grid_point], &1))).()
    |> take(256)
    |> Stream.with_index
    |> take(-1)
    |> Enum.to_list
    |> List.last
  end

end
