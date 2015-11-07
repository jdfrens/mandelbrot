defmodule Fractals.Iterators.Mandelbrot do
  @moduledoc """
  Generates a Mandelbrot set.
  """

  import Complex

  def iterator(c) do
    fn (z) -> z |> square() |> add(c) end
  end
end
