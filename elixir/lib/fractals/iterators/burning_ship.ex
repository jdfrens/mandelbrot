defmodule Fractals.Iterators.BurningShip do

  import Complex

  def iterator(c) do
    fn (z) -> burn(z) |> square() |> add(c) end
  end

  def burn(%Complex{ real: real, imag: imag }) do
    # FIXME: not sure why I need to negate imag since Wikipedia doesn't
    %Complex{ real: abs(real), imag: -1 * abs(imag) }
  end

end
