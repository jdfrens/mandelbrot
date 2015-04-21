defmodule Mandelbrot.Fractal do

  import Enum, only: [ concat: 2, join: 2 ]

  def generate(options) do
    generate_header(options)
    |> concat(generate_image(options, generator(options)))
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

  def generate_image(options, generator) do
    for x <- xs(options), y <- ys(options), do: generator.(x, y)
  end

  def generator(options) do
    fn (_, _) -> "255 0 0 " end
  end

  def xs(options) do
    %Mandelbrot.Options{
      size:        %Mandelbrot.Size{ width: width },
      upper_left:  %Complex{ real: x0 },
      lower_right: %Complex{ real: x1 }
    } = options
    float_iterate(width, x0, x1)
  end

  def ys(options) do
    %Mandelbrot.Options{
      size:        %Mandelbrot.Size{ height: height },
      upper_left:  %Complex{ imag: y1 },
      lower_right: %Complex{ imag: y0 }
    } = options
    float_iterate(height, y1, y0)
  end


  def float_iterate(count, first, last) do
    delta = (last - first) / (count - 1)
    Stream.iterate(first, &(&1 + delta)) |> Enum.take(count)
  end

end
