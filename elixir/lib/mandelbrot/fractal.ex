defmodule Mandelbrot.Fractal do

  import Enum, only: [ concat: 2, join: 2 ]

  # FIXME: this builds a list which is joined; it should build a stream
  def generate(options) do
    []
    |> concat(generate_header(options))
    |> concat(generate_image(options))
    |> join("\n")
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

    color_func = color_function(options)
    Mandelbrot.Grid.generate_grid(options)
    |> Enum.map(&build_complex/1)
    |> Enum.map(fn   grid_point -> { grid_point, next_function(grid_point, options) } end)
    |> Enum.map(fn { grid_point, next_func } -> generate_pixel(grid_point, next_func, color_func) end)
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

  def in_or_out({ z, iterations }) do
    status = if iterations >= 256, do: :inside, else: :outside
    { status, z, iterations }
  end

  def fractal_iterate(next, grid_point) do
    Stream.iterate({ grid_point, 1 }, fn { z, i } -> { next.(z), i+1 } end)
    |> Stream.take_while(fn { z, _ } -> Complex.magnitude(z) < 4.0 end)
    |> Stream.take(256)
    |> Stream.take(-1)
    |> Enum.to_list()
    |> List.last
  end

end
