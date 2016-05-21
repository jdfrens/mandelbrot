defmodule Fractals.Generator.TasklessChunked do
  @moduledoc """
  Generates the pixels of the fractal.
  """

  import Fractals.Generator

  alias Fractals.Grid

  def generate(color_func, options) do
    options
    |> Grid.generate(&build_complex/2)
    |> Enum.chunk(100, 100, [])
    |> Enum.map(fn grid_points -> pixels(grid_points, color_func) end)
    |> Enum.concat
  end
end
