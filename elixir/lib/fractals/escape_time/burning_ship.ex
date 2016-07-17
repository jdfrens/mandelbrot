defmodule Fractals.EscapeTime.BurningShip do
  @moduledoc """
  Kind of an awesome fractal.  It looks a bit like a burning ship.
  """

  import Complex

  def pixels(grid_points, params) do
    Enum.map(grid_points, &escape_time(Complex.zero, &1, params))
  end

  def escaped?(z, params) do
    Complex.magnitude_squared(z) >= params.cutoff_squared
  end

  def escape_time(grid_point, c, params) do
    grid_point
    |> Stream.iterate(&iterator(&1,c))
    |> Stream.with_index
    |> Stream.drop_while(fn {z, i} -> !escaped?(z, params) && i < params.max_iterations end)
    |> Stream.take(1)
    |> Enum.to_list
    |> List.first
  end

  def iterator(z, c) do
    z |> burn |> square |> add(c)
  end

  def burn(%Complex{real: real, imag: imag}) do
    # FIXME: not sure why I need to negate imag since Wikipedia doesn't
    cmplx(abs(real), -1 * abs(imag))
  end
end
