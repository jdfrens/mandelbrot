defmodule Fractals.Grid do
  @moduledoc """
  Generates a grid of x and y values which can be turned into
  complex numbers.
  """

  alias Fractals.Params

  import Complex, only: :macros

  def chunk(grid, params) do
    grid
    |> Stream.chunk(params.chunk_size, params.chunk_size, [])
    |> Stream.zip(1..params.chunk_count)
    |> Stream.map(fn {chunk, number} -> {number, chunk} end)
  end

  def grid(params) do
    for y <- ys(params), x <- xs(params), do: cmplx(x, y)
  end

  def xs(params) do
    %Params{
      size:        %Fractals.Size{width: width},
      upper_left:  %Complex{real: x0},
      lower_right: %Complex{real: x1}
    } = params
    float_sequence(width, x0, x1)
  end

  def ys(params) do
    %Params{
      size:        %Fractals.Size{height: height},
      upper_left:  %Complex{imag: y1},
      lower_right: %Complex{imag: y0}
    } = params
    float_sequence(height, y1, y0)
  end

  def float_sequence(count, first, last) do
    delta = (last - first) / (count - 1)
    first |> Stream.iterate(&(&1 + delta)) |> Enum.take(count)
  end
end
