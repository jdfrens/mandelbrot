defmodule Fractals.Iterators.Nova do

  # FIXME: not implementing for now; z0^z1 is what, exactly?
  # z, r, and p are all Complex:
  # z - r * (z ** p - 1) / (p * z ** (p - 1)) + c
  def iterator(_options) do
    fn z -> z end
  end

end
