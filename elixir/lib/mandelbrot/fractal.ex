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
    next_func  = Mandelbrot.NextFunction.next_function(options)
    color_func = Mandelbrot.Color.color_function(options)
    Mandelbrot.Grid.generate_grid(options)
    |> Enum.map(fn { r, i } -> %Complex{ real: r, imag: i } end)
    |> Enum.map(fn (z) -> generate_pixel(z, next_func, color_func) end)
  end

  def generate_pixel(c, next_func, color_func) do
    fractal_iterate(next_func, %Complex{ real: 0.0, imag: 0.0 }, c)
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

  def fractal_iterate(next, z, c) do
    Stream.iterate({ z, 1 }, fn { z, i } -> { next.(z, c), i+1 } end)
    |> Stream.take_while(fn { z, _ } -> Complex.magnitude(z) < 4.0 end)
    |> Stream.take(256)
    |> Stream.take(-1)
    |> Enum.to_list()
    |> List.last
  end

end
