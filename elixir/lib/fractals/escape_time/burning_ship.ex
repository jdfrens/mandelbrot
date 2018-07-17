defmodule Fractals.EscapeTime.BurningShip do
  @moduledoc """
  Kind of an awesome fractal.  It looks a bit like a burning ship.
  """

  import Complex

  use Fractals.EscapeTime

  def iterate(grid_point, _params) do
    Stream.iterate(Complex.zero(), &iterator(&1, grid_point))
  end

  def iterator(z, c) do
    z |> burn |> square |> add(c)
  end

  def burn(%Complex{real: real, imag: imag}) do
    # TODO: not sure why I need to negate imag since Wikipedia doesn't
    cmplx(abs(real), -1 * abs(imag))
  end
end
