defmodule Fractals.EscapeTime.Julia do
  @moduledoc """
  Implements the iterated function for a Julia set.
  """

  import Complex

  use Fractals.EscapeTime

  @spec iterate(Complex.complex(), Fractals.Params.t()) :: Enumerable.t()
  def iterate(grid_point, params) do
    Stream.iterate(grid_point, &iterator(&1, params.c))
  end

  @spec iterator(Complex.complex(), Complex.complex()) :: Complex.complex()
  def iterator(z, c) do
    z |> square |> add(c)
  end
end
