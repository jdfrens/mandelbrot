defmodule Fractals.EscapeTime.BurningShip do
  @moduledoc """
  Kind of an awesome fractal.  It looks a bit like a burning ship.
  """

  import Complex

  use Fractals.EscapeTime

  def iterate(grid_point, _params) do
    Stream.iterate(Complex.new(0.0), &iterator(&1, grid_point))
  end

  def iterator(z, c) do
    z |> burn |> square |> add(c)
  end

  def burn(%Complex{re: real, im: imag}) do
    # TODO: not sure why I need to negate imag since Wikipedia doesn't
    Complex.new(Kernel.abs(real), -1 * Kernel.abs(imag))
  end
end
