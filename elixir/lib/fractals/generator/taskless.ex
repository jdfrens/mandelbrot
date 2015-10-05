defmodule Fractals.Generator.Taskless do
  @moduledoc """
  Generates the pixels of the fractal.
  """

  import Fractals.Generator

  alias Fractals.Grid
  alias Fractals.Iterators

  def generate(color_func, options) do
    options
    |> Grid.generate(&build_complex/2)
    |> Enum.map(fn grid_point ->
      pixel(grid_point, Iterators.build(grid_point, options), color_func)
    end)
  end
end
