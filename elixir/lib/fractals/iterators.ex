defmodule Fractals.Iterators do
  @moduledoc """
  Picks the right function for generating a requested fractal.
  """

  def build(grid_point, options) do
    case options.fractal do
      :burningship -> Fractals.Iterators.BurningShip.iterator(grid_point)
      :mandelbrot  -> Fractals.Iterators.Mandelbrot.iterator(grid_point)
      :julia       -> Fractals.Iterators.Julia.iterator(options.c)
      :newton      -> Fractals.Iterators.Newton.iterator()
      :nova        -> Fractals.Iterators.Nova.iterator(grid_point)
    end
  end

end
