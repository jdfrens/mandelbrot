defmodule Fractals.Iterators.Nova do
  @moduledoc """
  So... this computes some sort of fractal...

  Need to be able to compute z0^z1, raising a complex number
  to a complex *power*.  Yikes.

  z - r * (z ** p - 1) / (p * z ** (p - 1)) + c

  where z, r, and p are complex numbers.
  """

  # not a real (ha!) implementation
  def iterator(_options) do
    fn z -> z end
  end
end
