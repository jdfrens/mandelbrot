defmodule Fractals.Generator.OriginalTasked do
  @moduledoc """
  This is the first way I thought of generating each pixel
  asynchronously.
  """

  import Fractals.Generator

  alias Fractals.Grid
  alias Fractals.Iterators

  def generate(color_func, options) do
    options
    |> Grid.generate(&build_complex/2)
    |> Enum.map(fn grid_point ->
      async_pixel(grid_point, options, color_func)
    end)
    |> Enum.map(&Task.await/1)
  end

  def async_pixel(grid_point, options, color_func) do
    Task.async(fn ->
      pixel(grid_point, Iterators.build(grid_point, options), color_func)
    end)
  end
end
