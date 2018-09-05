defmodule Fractals.EscapeTime.Mandelbrot do
  @moduledoc """
  Implements the iterated function for the Mandelbrot set.
  """

  import Complex

  use Fractals.EscapeTime

  @zero Complex.new(0.0, 0.0)

  def iterate(grid_point, _params) do
    Stream.iterate(@zero, &iterator(&1, grid_point))
  end

  def iterator(z, c) do
    z |> square |> add(c)
  end
end
