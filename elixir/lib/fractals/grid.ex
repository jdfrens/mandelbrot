defmodule Mandelbrot.Grid do
  def generate_grid(options) do
    for y <- ys(options), x <- xs(options), do: { x, y }
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

end
