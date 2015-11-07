defmodule Fractals.Iterators do
  @moduledoc """
  Picks the right function for generating a requested fractal.
  """

  alias Fractals.Iterators.BurningShip
  alias Fractals.Iterators.Mandelbrot
  alias Fractals.Iterators.Julia
  alias Fractals.Iterators.Newton

  def build(options) do
    fn grid_point ->
      case options.fractal do
        :burningship -> BurningShip.iterator(grid_point)
        :mandelbrot  -> Mandelbrot.iterator(grid_point)
        :julia       -> Julia.iterator(options.c)
        :newton      -> Newton.iterator
      end
    end
  end
end
