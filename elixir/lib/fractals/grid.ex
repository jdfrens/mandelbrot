defmodule Fractals.Grid do
  @moduledoc """
  Generates a grid of x and y values which can be turned into
  complex numbers.
  """

  import Complex, only: :macros

  def chunk(grid, options) do
    grid
    |> Stream.chunk(options.chunk_size, options.chunk_size, [])
    |> Stream.zip(1..options.chunk_count)
    |> Stream.map(fn {chunk, number} -> {number, chunk} end)
  end

  def grid(options) do
    for y <- ys(options), x <- xs(options), do: cmplx(x, y)
  end

  def xs(options) do
    %Fractals.Options{
      size:        %Fractals.Size{width: width},
      upper_left:  %Complex{real: x0},
      lower_right: %Complex{real: x1}
    } = options
    float_sequence(width, x0, x1)
  end

  def ys(options) do
    %Fractals.Options{
      size:        %Fractals.Size{height: height},
      upper_left:  %Complex{imag: y1},
      lower_right: %Complex{imag: y0}
    } = options
    float_sequence(height, y1, y0)
  end

  def float_sequence(count, first, last) do
    delta = (last - first) / (count - 1)
    first |> Stream.iterate(&(&1 + delta)) |> Enum.take(count)
  end
end
