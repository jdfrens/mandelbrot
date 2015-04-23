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
    generate_grid(options)
    |> Enum.map(&build_complex/1)
    |> Enum.map(&generate_mandelbrot_pixel/1)
  end

  def generate_grid(options) do
    for y <- ys(options), x <- xs(options), do: { x, y }
  end

  def build_complex({ real, imag }) do
    %Complex{ real: real, imag: imag }
  end

  def generate_mandelbrot_pixel(c) do
     z_n = fractal_iterate(%Complex{ real: 0.0, imag: 0.0 }, c)
    case in_or_out(z_n) do
      { :inside,  _ } -> "255 0 0 "
      { :outside, _ } -> "  0 0 0 "
    end
  end

  def in_or_out({ z, iterations }) do
    status = if iterations >= 256, do: :inside, else: :outside
    { status, z }
  end

  def fractal_iterate(z, c) do
    Stream.iterate({ z, 1 }, fn { z, i } -> { mandelbrot_next(z, c), i+1 } end)
    |> Stream.take_while(fn { z, _ } -> Complex.magnitude(z) < 4.0 end)
    |> Stream.take(256)
    |> Stream.take(-1)
    |> Enum.to_list()
    |> List.last
  end

  def xs(options) do
    %Mandelbrot.Options{
      size:        %Mandelbrot.Size{ width: width },
      upper_left:  %Complex{ real: x0 },
      lower_right: %Complex{ real: x1 }
    } = options
    float_sequence(width, x0, x1)
  end

  def ys(options) do
    %Mandelbrot.Options{
      size:        %Mandelbrot.Size{ height: height },
      upper_left:  %Complex{ imag: y1 },
      lower_right: %Complex{ imag: y0 }
    } = options
    float_sequence(height, y1, y0)
  end

  def float_sequence(count, first, last) do
    delta = (last - first) / (count - 1)
    Stream.iterate(first, &(&1 + delta)) |> Enum.take(count)
  end

  def mandelbrot_next(z, c) do
    z
    |> Complex.square()
    |> Complex.add(c)
  end

end
