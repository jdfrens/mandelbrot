defmodule Fractals.Generator.OriginalTasked do
  @moduledoc """
  This is the first way I thought of generating each pixel
  asynchronously.
  """

  import Fractals.Generator

  alias Fractals.Grid

  def generate(options) do
    options
    |> Grid.generate(&build_complex/2)
    |> Enum.map(&async_pixel(&1, options))
    |> Enum.map(&Task.await/1)
  end

  def async_pixel(grid_point,
                  %Fractals.Options{color_func: color_func,
                                    iterator_builder: iterator_builder}) do
    Task.async(fn ->
      pixel(grid_point, iterator_builder.(grid_point), color_func)
    end)
  end
end
