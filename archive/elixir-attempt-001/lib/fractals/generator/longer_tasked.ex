defmodule Fractals.Generator.LongerTasked do
  @moduledoc """
  This module's `generate` tries to do more in each async task.

  And it runs SLOWER.

  Really?
  """

  import Fractals.Generator

  import Complex

  alias Fractals.Grid

  def generate(options) do
    options
    |> Grid.generate(&async_pixel(&1, &2, options))
    |> Enum.map(&Task.await/1)
  end

  def async_pixel(x, y,
                  %Fractals.Options{color_func: color_func,
                                    iterator_builder: iterator_builder}) do
    Task.async(fn ->
      grid_point = cmplx(x, y)
      pixel(grid_point, iterator_builder.(grid_point), color_func)
    end)
  end
end
