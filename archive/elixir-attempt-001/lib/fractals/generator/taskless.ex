defmodule Fractals.Generator.Taskless do
  @moduledoc """
  Generates the pixels of the fractal.
  """

  import Fractals.Generator

  alias Fractals.Grid

  def generate(options) do
    options
    |> Grid.generate(&build_complex/2)
    |> Enum.map(fn grid_point ->
      pixel(grid_point, options.iterator_builder.(grid_point), options.color_func)
    end)
  end
end
