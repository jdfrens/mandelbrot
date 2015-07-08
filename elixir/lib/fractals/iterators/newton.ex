defmodule Fractals.Iterators.Newton do

  import Complex

  def iterator do
    fn (z) -> divide(p(z), p_prime(z)) end
  end

  def p(z) do
    z |> cube() |> subtract(%Complex{ real: -1, imag: 0 })
  end

  def p_prime(z) do
    z |> square() |> multiply(%Complex{ real: 3.0, imag: 0.0 })
  end

end
