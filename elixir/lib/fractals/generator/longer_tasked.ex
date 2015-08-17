defmodule Fractals.Generator.LongerTasked do
  import Fractals.Generator

  import Complex

  alias Fractals.Grid
  alias Fractals.Iterators

  def generate(color_func, options) do
    options
    |> Grid.generate(&async_pixel(&1, &2, color_func, options))
    |> Enum.map(&Task.await/1)
  end

  def async_pixel(x, y, color_func, options) do
    Task.async(fn ->
      grid_point = cmplx(x, y)
      pixel(grid_point, Iterators.build(grid_point, options), color_func)
    end)
  end
end
