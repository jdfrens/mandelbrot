defmodule Fractals.Iterators.Julia do
  @moduledoc """
  Generates a Julia set.
  """

  import Complex

  def iterator(c) do
    fn (z) -> z |> square() |> add(c) end
  end

end
