defmodule Fractals.Iterators.Julia do

  import Complex

  def iterator(c) do
    fn (z) -> z |> square() |> add(c) end
  end

end
