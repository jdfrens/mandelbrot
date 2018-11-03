defmodule Fractals.Grid do
  @moduledoc """
  Generates a grid of x and y values which can be turned into
  complex numbers.
  """

  @type t :: [Complex.complex()]

  alias Fractals.{Grid, Params}

  import Complex, only: :macros

  @spec chunked_grid(Params.t()) :: [Chunk.t()]
  def chunked_grid(params) do
    params |> grid |> chunk(params)
  end

  @spec chunk(Grid.t(), Params.t()) :: [Chunk.t()]
  def chunk(grid, params) do
    grid
    |> Stream.chunk_every(params.chunk_size, params.chunk_size, [])
    |> Stream.zip(1..params.chunk_count)
    |> Stream.map(fn {data, number} -> %Chunk{number: number, data: data, params: params} end)
    |> Enum.to_list()
  end

  @spec grid(Params.t()) :: Grid.t()
  def grid(params) do
    for y <- ys(params),
        x <- xs(params),
        do: Complex.new(x, y)
  end

  @spec xs(Params.t()) :: Enumerable.t()
  def xs(params) do
    %Params{
      size: %Fractals.Size{width: width},
      upper_left: %Complex{re: x0},
      lower_right: %Complex{re: x1}
    } = params

    float_sequence(width, x0, x1)
  end

  @spec ys(Params.t()) :: Enumerable.t()
  def ys(params) do
    %Params{
      size: %Fractals.Size{height: height},
      upper_left: %Complex{im: y1},
      lower_right: %Complex{im: y0}
    } = params

    float_sequence(height, y1, y0)
  end

  @spec float_sequence(non_neg_integer, float, float) :: Enumerable.t()
  def float_sequence(count, first, last) do
    delta = (last - first) / (count - 1)
    first |> Stream.iterate(&(&1 + delta)) |> Enum.take(count)
  end
end
