defmodule Fractals.EscapeTime.Julia do
  @moduledoc """
  Implements the iterated function for a Julia set.
  """

  import Complex

  use Fractals.EscapeTime

  def iterate(grid_point, params) do
    Stream.iterate(grid_point, &iterator(&1, params.c))
  end

  def iterator(z, c) do
    z |> square |> add(c)
  end
end
