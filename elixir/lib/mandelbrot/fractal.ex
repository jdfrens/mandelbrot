defmodule Mandelbrot.Fractal do

  import Stream, only: [ concat: 1, iterate: 2, map: 2, take: 2, take_while: 2, with_index: 1 ]

  @magnitude_cutoff         2.0
  @magnitude_cutoff_squared 4.0

  def generate(options) do
    concat([
      generate_header(options),
      generate_image(options)
      ])
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
    fractal_iterate(next_func, grid_point)
    |> in_or_out
    |> apply_color(color_func)
  end

  def apply_color(in_out, color_func) do
    color_func.(in_out)
  end

  def in_or_out(nil), do: { :outside, nil, 0 }
  def in_or_out({ z, iterations }) do
    status = if iterations >= 256, do: :inside, else: :outside
    { status, z, iterations }
  end

  # EXPERIMENT: magnitude v. magnitude_sqaured
  # EXPERIMENT: magnitude = 2.0 or 4.0
  def fractal_iterate(next, grid_point) do
    import Complex, only: [ magnitude_squared: 1 ]

    { grid_point, 1 }
    |> iterate(fn { z, i } -> { next.(z), i+1 } end)
    |> take_while(fn { z, _ } -> magnitude_squared(z) < @magnitude_cutoff_squared end)
    |> take(256)
    |> take(-1)
    |> Enum.to_list
    |> List.last
  end

end
